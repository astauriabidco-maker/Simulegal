import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
import { WhatsappGateway } from './whatsapp.gateway';
import * as fs from 'fs';
import * as path from 'path';
import { randomUUID } from 'crypto';

// Dossier de stockage des médias entrants
const MEDIA_UPLOAD_DIR = path.resolve(__dirname, '..', '..', 'uploads', 'whatsapp');

@Injectable()
export class WhatsappService {
    private readonly logger = new Logger(WhatsappService.name);

    constructor(
        private prisma: PrismaService,
        private notificationsService: NotificationsService,
        private gateway: WhatsappGateway
    ) {
        // Créer le dossier uploads/whatsapp s'il n'existe pas
        if (!fs.existsSync(MEDIA_UPLOAD_DIR)) {
            fs.mkdirSync(MEDIA_UPLOAD_DIR, { recursive: true });
            this.logger.log(`📂 Created media upload directory: ${MEDIA_UPLOAD_DIR}`);
        }
    }

    /**
     * Handle incoming message from Twilio Webhook
     * Supporte les messages texte ET les pièces jointes (images, PDFs)
     */
    async handleIncoming(data: {
        from: string;
        body: string;
        messageSid: string;
        numMedia?: number;
        mediaItems?: { url: string; contentType: string }[];
    }) {
        const cleanPhone = data.from.replace('whatsapp:', '').replace('+', '').trim();
        this.logger.log(`📩 Received WhatsApp from ${cleanPhone}: ${data.body || '[MEDIA]'}`);

        // Try to find a matching Lead or Prospect
        const lead = await this.prisma.lead.findFirst({
            where: {
                phone: {
                    contains: cleanPhone.substring(cleanPhone.length - 9)
                }
            }
        });

        const prospect = !lead ? await this.prisma.prospect.findFirst({
            where: {
                phone: {
                    contains: cleanPhone.substring(cleanPhone.length - 9)
                }
            }
        }) : null;

        const senderName = lead?.name || (prospect ? `${prospect.firstName} ${prospect.lastName}`.trim() : 'Unknown');
        const results: any[] = [];

        // 1. Sauvegarder le message texte s'il y en a un
        if (data.body && data.body.trim()) {
            const communication = await this.prisma.communication.create({
                data: {
                    direction: 'INBOUND',
                    type: 'WHATSAPP',
                    content: data.body,
                    sender: data.from,
                    senderName,
                    leadId: lead?.id,
                    prospectId: prospect?.id
                }
            });
            results.push(communication);
        }

        // 2. Traiter les médias (images, PDFs, vidéos)
        if (data.mediaItems && data.mediaItems.length > 0) {
            for (const media of data.mediaItems) {
                try {
                    const savedMedia = await this.downloadAndSaveMedia(media.url, media.contentType);

                    const communication = await this.prisma.communication.create({
                        data: {
                            direction: 'INBOUND',
                            type: 'WHATSAPP',
                            content: data.body || '📎 Pièce jointe',
                            sender: data.from,
                            senderName,
                            mediaUrl: savedMedia.relativePath,
                            mediaType: media.contentType,
                            mediaFilename: savedMedia.filename,
                            leadId: lead?.id,
                            prospectId: prospect?.id
                        }
                    });

                    this.logger.log(`📎 Media saved: ${savedMedia.filename} (${media.contentType})`);
                    results.push(communication);

                } catch (mediaError) {
                    this.logger.error(`❌ Failed to save media: ${mediaError.message}`);
                    // Créer quand même un enregistrement sans le fichier
                    const communication = await this.prisma.communication.create({
                        data: {
                            direction: 'INBOUND',
                            type: 'WHATSAPP',
                            content: '📎 Pièce jointe (erreur téléchargement)',
                            sender: data.from,
                            senderName,
                            mediaUrl: media.url, // URL Twilio originale en fallback
                            mediaType: media.contentType,
                            leadId: lead?.id,
                            prospectId: prospect?.id
                        }
                    });
                    results.push(communication);
                }
            }
        }

        // Si aucun contenu (ni texte ni média), créer quand même un message
        if (results.length === 0) {
            const communication = await this.prisma.communication.create({
                data: {
                    direction: 'INBOUND',
                    type: 'WHATSAPP',
                    content: data.body || '(message vide)',
                    sender: data.from,
                    senderName,
                    leadId: lead?.id,
                    prospectId: prospect?.id
                }
            });
            results.push(communication);
        }

        // 🔌 Émettre chaque message en temps réel via WebSocket
        for (const comm of results) {
            this.gateway.emitNewMessage(comm);
        }

        // Mettre à jour la liste des conversations en temps réel
        try {
            const updatedConversations = await this.getConversations();
            this.gateway.emitConversationsUpdate(updatedConversations);
        } catch (e) {
            this.logger.warn(`Failed to emit conversations update: ${e.message}`);
        }

        return {
            success: true,
            communicationIds: results.map(r => r.id),
            matchedType: lead ? 'LEAD' : (prospect ? 'PROSPECT' : 'NONE'),
            matchedId: lead?.id || prospect?.id
        };
    }

    /**
     * Télécharge un média depuis l'URL Twilio et le sauvegarde localement
     */
    private async downloadAndSaveMedia(url: string, contentType: string): Promise<{ filename: string; relativePath: string; absolutePath: string }> {
        // Déterminer l'extension du fichier
        const ext = this.getExtensionFromMime(contentType);
        const filename = `${randomUUID()}${ext}`;
        const absolutePath = path.join(MEDIA_UPLOAD_DIR, filename);
        const relativePath = `/uploads/whatsapp/${filename}`;

        try {
            // Télécharger le fichier depuis Twilio
            const response = await fetch(url);
            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const buffer = Buffer.from(await response.arrayBuffer());
            fs.writeFileSync(absolutePath, buffer);

            this.logger.log(`✅ Downloaded media: ${filename} (${(buffer.length / 1024).toFixed(1)} KB)`);

            return { filename, relativePath, absolutePath };
        } catch (error) {
            this.logger.error(`Failed to download from ${url}: ${error.message}`);
            throw error;
        }
    }

    /**
     * Convertit un MIME type en extension de fichier
     */
    private getExtensionFromMime(mimeType: string): string {
        const map: Record<string, string> = {
            'image/jpeg': '.jpg',
            'image/png': '.png',
            'image/webp': '.webp',
            'image/gif': '.gif',
            'application/pdf': '.pdf',
            'video/mp4': '.mp4',
            'audio/ogg': '.ogg',
            'audio/mpeg': '.mp3',
            'application/vnd.openxmlformats-officedocument.wordprocessingml.document': '.docx',
            'application/msword': '.doc',
        };
        return map[mimeType] || '.bin';
    }

    /**
     * Get unique conversations (grouped by Lead or Prospect)
     */
    async getConversations() {
        const communications = await this.prisma.communication.findMany({
            orderBy: { createdAt: 'desc' },
            include: {
                lead: true,
                prospect: true
            }
        });

        const conversations: any[] = [];
        const seen = new Set();

        for (const comm of communications) {
            const id = comm.leadId || comm.prospectId;
            const type = comm.leadId ? 'LEAD' : (comm.prospectId ? 'PROSPECT' : 'UNKNOWN');

            if (id && !seen.has(`${type}:${id}`)) {
                seen.add(`${type}:${id}`);

                // Compter les messages non lus (entrants récents)
                const unreadCount = communications.filter(c =>
                    ((type === 'LEAD' && c.leadId === id) || (type === 'PROSPECT' && c.prospectId === id)) &&
                    c.direction === 'INBOUND'
                ).length;

                conversations.push({
                    id,
                    type,
                    name: comm.lead?.name || (comm.prospect ? `${comm.prospect.firstName} ${comm.prospect.lastName}`.trim() : comm.senderName || 'Unknown'),
                    lastMessage: comm.mediaType ? `📎 ${this.getMediaLabel(comm.mediaType)}` : comm.content,
                    lastAt: comm.createdAt,
                    phone: comm.sender,
                    hasMedia: !!comm.mediaUrl,
                    unreadCount
                });
            }
        }

        return conversations;
    }

    /**
     * Get messages for a specific Lead or Prospect
     */
    async getMessages(type: 'LEAD' | 'PROSPECT', id: string) {
        return this.prisma.communication.findMany({
            where: type === 'LEAD' ? { leadId: id } : { prospectId: id },
            orderBy: { createdAt: 'asc' }
        });
    }

    /**
     * Send manual reply
     */
    async sendMessage(type: 'LEAD' | 'PROSPECT', id: string, content: string) {
        let phone = '';
        const metadata: any = {};

        if (type === 'LEAD') {
            const lead = await this.prisma.lead.findUnique({ where: { id } });
            if (!lead) throw new Error('Lead not found');
            phone = lead.phone;
            metadata.leadId = id;
        } else {
            const prospect = await this.prisma.prospect.findUnique({ where: { id } });
            if (!prospect) throw new Error('Prospect not found');
            phone = prospect.phone;
            metadata.prospectId = id;
        }

        return this.notificationsService.sendWhatsApp(phone, 'manual_reply', { message: content }, metadata);
    }

    /**
     * Retourne un label lisible pour le type de média
     */
    private getMediaLabel(mimeType: string): string {
        if (mimeType.startsWith('image/')) return 'Photo';
        if (mimeType.startsWith('video/')) return 'Vidéo';
        if (mimeType.startsWith('audio/')) return 'Audio';
        if (mimeType === 'application/pdf') return 'Document PDF';
        return 'Fichier';
    }
}
