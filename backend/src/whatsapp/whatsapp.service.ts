import { Injectable, Logger, Inject, forwardRef } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
import { WhatsappGateway } from './whatsapp.gateway';
import { LeadsService } from '../leads/leads.service';
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
        private gateway: WhatsappGateway,
        @Inject(forwardRef(() => LeadsService))
        private leadsService: LeadsService,
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

                    // ════════════════════════════════════════════════════════
                    // 📎 AUTO-ATTACH : rattacher le média au dossier Lead
                    // ════════════════════════════════════════════════════════
                    if (lead && this.isDocumentMedia(media.contentType)) {
                        try {
                            const fileBuffer = fs.readFileSync(savedMedia.absolutePath);
                            await this.attachMediaToLeadDossier(
                                lead,
                                fileBuffer,
                                savedMedia.filename,
                                media.contentType,
                                savedMedia.relativePath,
                                data.body
                            );
                        } catch (attachErr: any) {
                            this.logger.warn(`⚠️ Auto-attach failed: ${attachErr.message}`);
                        }
                    }

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
     * Vérifie si un média est un document (image ou PDF)
     */
    private isDocumentMedia(contentType: string): boolean {
        return contentType.startsWith('image/') || contentType === 'application/pdf';
    }

    /**
     * 📎 Rattache un fichier envoyé par WhatsApp au dossier Lead
     *
     * Logique intelligente :
     *   1. Vérifie que le Lead est en étape COLLECTING (ou PAID/NEW)
     *   2. Tente de matcher le fichier avec un document requis non encore déposé
     *   3. Lance l'OCR automatique (Tesseract/Ollama) pour validation
     *   4. Met à jour Lead.documents
     *   5. Notifie le client du résultat
     */
    private async attachMediaToLeadDossier(
        lead: any,
        fileBuffer: Buffer,
        filename: string,
        mimeType: string,
        savedPath: string,
        messageBody?: string
    ): Promise<void> {
        // S'assurer que le Lead est en étape de collecte
        const activeStages = ['NEW', 'PAID', 'COLLECTING'];
        if (!activeStages.includes(lead.status)) {
            this.logger.log(`[✋ WhatsApp Attach] Lead ${lead.id} n'est pas en collecte (${lead.status}) — fichier non rattaché au dossier`);
            return;
        }

        // Charger les documents requis et déjà déposés
        const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
        const existingDocs: any[] = lead.documents ? JSON.parse(lead.documents) : [];

        // Trouver le prochain document requis non encore déposé
        const missingDoc = requiredDocs.find((rd: any) => {
            const already = existingDocs.find((d: any) => d.id === rd.id && d.status !== 'REJECTED');
            return !already;
        });

        // Tenter de déduire le type de document depuis le message ou le contexte
        let targetDocId = missingDoc?.id || `whatsapp_doc_${Date.now()}`;
        let targetDocName = missingDoc?.name || 'Document envoyé par WhatsApp';

        // Si le client a écrit un message accompagnant le fichier, tenter de matcher
        if (messageBody && requiredDocs.length > 0) {
            const matchedByMessage = this.matchDocByMessage(messageBody, requiredDocs, existingDocs);
            if (matchedByMessage) {
                targetDocId = matchedByMessage.id;
                targetDocName = matchedByMessage.name;
            }
        }

        this.logger.log(`📎 [WhatsApp → Dossier] Lead ${lead.id}: rattachement à "${targetDocName}" (${targetDocId})`);

        // Utiliser le handleDocumentUpload de LeadsService (avec OCR intégré)
        const result = await this.leadsService.handleDocumentUpload(
            lead.id,
            targetDocId,
            fileBuffer,
            filename,
            mimeType
        );

        // Notifier le client du résultat directement par WhatsApp
        const statusEmoji = result.ocrResult?.status === 'VALID' ? '✅'
            : result.ocrResult?.status === 'REJECTED' ? '❌' : '⏳';

        const replyMessage = `${statusEmoji} *Document reçu* — ${targetDocName}\n\n${result.message}`;

        await this.notificationsService.sendWhatsApp(
            lead.phone,
            'document_receipt_confirmation',
            { name: lead.name, message: replyMessage },
            { leadId: lead.id }
        );
    }

    /**
     * Tente de matcher un message texte avec un document requis
     * Ex: "Voici mon passeport" → match avec le doc requis "passeport"
     */
    private matchDocByMessage(
        message: string,
        requiredDocs: any[],
        existingDocs: any[]
    ): { id: string; name: string } | null {
        const msg = message.toLowerCase();

        // Mots-clés pour chaque type de document
        const keywords: Record<string, string[]> = {
            'passeport': ['passeport', 'passport'],
            'carte_identite': ['carte d\'identité', 'cni', 'carte identite', 'identity card'],
            'titre_sejour': ['titre de séjour', 'carte de séjour', 'titre sejour', 'residence permit'],
            'acte_naissance': ['acte de naissance', 'birth certificate', 'naissance'],
            'acte_mariage': ['acte de mariage', 'mariage', 'marriage'],
            'justif_domicile': ['justificatif de domicile', 'domicile', 'facture', 'edf', 'quittance'],
            'photos_identite': ['photo', 'photos d\'identité', 'photos identite'],
            'recepisse': ['récépissé', 'recepisse', 'récépissé'],
            'cerfa': ['cerfa', 'formulaire'],
            'avis_imposition': ['avis d\'imposition', 'avis imposition', 'impôt', 'impot'],
            'contrat_travail': ['contrat de travail', 'contrat travail', 'emploi'],
            'attestation_hebergement': ['attestation d\'hébergement', 'hebergement'],
        };

        for (const rd of requiredDocs) {
            const alreadyDone = existingDocs.find((d: any) => d.id === rd.id && d.status !== 'REJECTED');
            if (alreadyDone) continue;

            // Match par l'ID du document
            const docKeywords = keywords[rd.id] || [];

            // Match aussi par le nom du document
            const nameWords = (rd.name || '').toLowerCase().split(/\s+/);
            const allKeywords = [...docKeywords, ...nameWords.filter((w: string) => w.length > 3)];

            if (allKeywords.some(kw => msg.includes(kw))) {
                return { id: rd.id, name: rd.name };
            }
        }

        return null;
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
