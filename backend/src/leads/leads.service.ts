import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
import { PipelineAutomationService } from '../pipeline-automation/pipeline-automation.service';
import * as jwt from 'jsonwebtoken';
import * as fs from 'fs';
import * as path from 'path';

const UPLOAD_DIR = path.resolve(__dirname, '..', '..', 'uploads', 'documents');
const JWT_SECRET = process.env.JWT_SECRET || 'dev_secret_change_in_production';
const FRONTEND_URL = process.env.FRONTEND_URL || 'http://localhost:3000';

@Injectable()
export class LeadsService {
    private readonly logger = new Logger(LeadsService.name);

    constructor(
        private prisma: PrismaService,
        private notifications: NotificationsService,
        private pipelineAutomation: PipelineAutomationService,
    ) {
        if (!fs.existsSync(UPLOAD_DIR)) {
            fs.mkdirSync(UPLOAD_DIR, { recursive: true });
            this.logger.log(`📂 Created document upload directory: ${UPLOAD_DIR}`);
        }
    }

    async findAll() {
        const leads = await this.prisma.lead.findMany({
            include: { notes: true, originAgency: true },
            orderBy: { createdAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    async findByAgency(agencyId: string) {
        const leads = await this.prisma.lead.findMany({
            where: { originAgencyId: agencyId },
            include: { notes: true },
            orderBy: { createdAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    async findOne(id: string) {
        const lead = await this.prisma.lead.findUnique({
            where: { id },
            include: { notes: true, originAgency: true }
        });
        if (!lead) return null;
        return this.mapLead(lead);
    }

    private mapLead(lead: any) {
        return {
            ...lead,
            documents: lead.documents ? JSON.parse(lead.documents) : [],
            contract: lead.contract ? JSON.parse(lead.contract) : null,
            requiredDocs: lead.requiredDocs ? JSON.parse(lead.requiredDocs) : null
        };
    }

    async updateStatus(id: string, status: any) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (lead) {
            console.log(`[LeadsService] Updating lead ${id} status from ${lead.status} to ${status}`);
            await this.notifications.onStageChange(lead, lead.status, status);
            // Trigger dynamic pipeline automations
            await this.pipelineAutomation.onStageChange(lead, lead.status, status);
        }

        const updatedLead = await this.prisma.lead.update({
            where: { id },
            data: { status }
        });

        return this.mapLead(updatedLead);
    }

    async assignUser(id: string, userId: string) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        const user = await this.prisma.user.findUnique({ where: { id: userId } });

        if (lead && user) {
            await this.notifications.onJuristAssigned(lead, user.name);
        }

        return this.prisma.lead.update({
            where: { id },
            data: { assignedUserId: userId }
        });
    }

    async updateDocuments(id: string, documents: any[]) {
        const existingLead = await this.prisma.lead.findUnique({ where: { id } });

        if (existingLead) {
            const oldDocs = JSON.parse(existingLead.documents || '[]');
            const requiredDocs = existingLead.requiredDocs ? JSON.parse(existingLead.requiredDocs) : [];

            // ── Détecter les changements de statut des documents ──
            for (const newDoc of documents) {
                const oldDoc = oldDocs.find((d: any) => d.id === newDoc.id);
                const docLabel = newDoc.docType || newDoc.name || newDoc.id;

                // 🔴 Document REJETÉ
                if (newDoc.status === 'REJECTED' && oldDoc?.status !== 'REJECTED') {
                    // Générer un magic link de re-upload pour ce document spécifique
                    const reuploadToken = jwt.sign(
                        { leadId: id, docId: newDoc.id, docName: docLabel, purpose: 'doc_upload' },
                        JWT_SECRET,
                        { expiresIn: '30d' }
                    );
                    const reuploadUrl = `${FRONTEND_URL}/upload/${reuploadToken}`;

                    await this.notifications.onDocumentRejected(
                        existingLead,
                        docLabel,
                        newDoc.rejectionReason || undefined,
                        reuploadUrl
                    );
                    this.logger.log(`🔴 Document ${docLabel} REJETÉ pour ${existingLead.name} — lien de re-upload envoyé`);
                }

                // 🟢 Document VALIDÉ
                if (newDoc.status === 'VALID' && oldDoc?.status !== 'VALID') {
                    await this.notifications.onDocumentValidated(existingLead, docLabel);
                    this.logger.log(`🟢 Document ${docLabel} VALIDÉ pour ${existingLead.name}`);
                }
            }

            // ── Vérifier si TOUS les docs requis sont validés (dossier complet) ──
            if (requiredDocs.length > 0) {
                const allRequiredValidated = requiredDocs.every((req: any) => {
                    const doc = documents.find((d: any) => d.id === req.id);
                    return doc?.status === 'VALID';
                });

                // Vérifier que ce n'était pas déjà le cas avant (éviter doublons)
                const wasAlreadyComplete = requiredDocs.every((req: any) => {
                    const doc = oldDocs.find((d: any) => d.id === req.id);
                    return doc?.status === 'VALID';
                });

                if (allRequiredValidated && !wasAlreadyComplete) {
                    await this.notifications.onAllDocumentsValidated(existingLead);
                    this.logger.log(`🎉 DOSSIER COMPLET pour ${existingLead.name} — tous les documents sont validés`);

                    // Trigger pipeline automations on docs complete (auto-transition, notifications)
                    await this.pipelineAutomation.onDocsComplete(existingLead);

                    // Mettre à jour automatiquement le statut du lead en PROCESSING
                    await this.prisma.lead.update({
                        where: { id },
                        data: { status: 'PROCESSING' }
                    });
                    this.logger.log(`📋 Lead ${id} passé en PROCESSING automatiquement`);
                }
            }
        }

        const lead = await this.prisma.lead.update({
            where: { id },
            data: { documents: JSON.stringify(documents) }
        });

        return this.mapLead(lead);
    }

    async addNote(leadId: string, data: { content: string, author: string }) {
        return this.prisma.leadNote.create({
            data: {
                content: data.content,
                author: data.author,
                leadId
            }
        });
    }

    async create(data: any) {
        const { currentStage, contract, documents, requiredDocuments, ...rest } = data;
        const leadId = data.id || `SL-${Math.floor(Math.random() * 90000 + 10000)}`;
        const { name, email, phone, serviceId, serviceName, status, amountPaid, originAgencyId } = rest;

        const lead = await this.prisma.lead.create({
            data: {
                id: leadId,
                name,
                email,
                phone,
                serviceId,
                serviceName,
                status: status || currentStage || 'NEW',
                amountPaid: amountPaid || 0,
                originAgencyId,
                contract: contract ? JSON.stringify(contract) : null,
                documents: documents ? JSON.stringify(documents) : '[]',
                requiredDocs: requiredDocuments ? JSON.stringify(requiredDocuments) : null,
                data: JSON.stringify(rest)
            }
        });

        return this.mapLead(lead);
    }

    async recordPayment(id: string, data: { amount: number, method: string, reference?: string }) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (!lead) throw new Error('Lead not found');

        const newAmount = (lead.amountPaid || 0) + data.amount;
        const invoiceNumber = lead.invoiceNumber || `FAC-${new Date().getFullYear()}-${id.split('-').pop()}`;

        const updatedLead = await this.prisma.lead.update({
            where: { id },
            data: {
                amountPaid: newAmount,
                paymentMethod: data.method,
                paymentDate: new Date(),
                paymentRef: data.reference,
                invoiceNumber,
                status: lead.status === 'NEW' ? 'COLLECTING' : lead.status
            }
        });

        return this.mapLead(updatedLead);
    }

    async delete(id: string) {
        return this.prisma.lead.delete({
            where: { id }
        });
    }

    // ═══════════════════════════════════════════════════════════
    // MAGIC LINKS — Upload de documents ciblés
    // ═══════════════════════════════════════════════════════════

    /**
     * Génère un magic link JWT pour chaque document de la checklist d'un lead.
     * Chaque lien est spécifique à UN document et expire après 30 jours.
     */
    generateDocumentUploadLinks(leadId: string, requiredDocs: any[]): { docId: string; docName: string; url: string }[] {
        return requiredDocs.map((doc: any) => {
            const token = jwt.sign(
                {
                    leadId,
                    docId: doc.id,
                    docName: doc.name || doc.label,
                    purpose: 'doc_upload'
                },
                JWT_SECRET,
                { expiresIn: '30d' }
            );

            return {
                docId: doc.id,
                docName: doc.name || doc.label,
                url: `${FRONTEND_URL}/upload/${token}`
            };
        });
    }

    /**
     * Vérifie un token d'upload et retourne les informations du document
     */
    verifyUploadToken(token: string): { leadId: string; docId: string; docName: string } | null {
        try {
            const payload = jwt.verify(token, JWT_SECRET) as any;
            if (payload.purpose !== 'doc_upload') return null;
            return {
                leadId: payload.leadId,
                docId: payload.docId,
                docName: payload.docName
            };
        } catch (e) {
            this.logger.warn(`❌ Invalid upload token: ${e.message}`);
            return null;
        }
    }

    /**
     * Traite l'upload d'un document via magic link.
     * Sauvegarde le fichier et met à jour la checklist du lead.
     */
    async handleDocumentUpload(
        leadId: string,
        docId: string,
        fileBuffer: Buffer,
        originalFilename: string,
        mimeType: string
    ): Promise<{ success: boolean; message: string }> {
        // Vérifier que le lead existe
        const lead = await this.prisma.lead.findUnique({ where: { id: leadId } });
        if (!lead) {
            return { success: false, message: 'Dossier introuvable' };
        }

        // Sauvegarder le fichier
        const ext = path.extname(originalFilename) || this.getExtFromMime(mimeType);
        const safeFilename = `${leadId}_${docId}_${Date.now()}${ext}`;
        const filePath = path.join(UPLOAD_DIR, safeFilename);
        fs.writeFileSync(filePath, fileBuffer);

        const fileUrl = `/uploads/documents/${safeFilename}`;
        this.logger.log(`📎 Document uploaded: ${safeFilename} for Lead ${leadId} / Doc ${docId}`);

        // Mettre à jour la checklist du lead
        const documents: any[] = JSON.parse(lead.documents || '[]');

        const existingDocIndex = documents.findIndex((d: any) => d.id === docId);
        const docEntry = {
            id: docId,
            docType: docId,
            status: 'PENDING',
            fileUrl,
            originalFilename,
            mimeType,
            uploadedAt: new Date().toISOString(),
            uploadMethod: 'MAGIC_LINK'
        };

        if (existingDocIndex >= 0) {
            // Remplacer le document existant (re-upload après rejet par exemple)
            documents[existingDocIndex] = { ...documents[existingDocIndex], ...docEntry, status: 'PENDING' };
        } else {
            documents.push(docEntry);
        }

        await this.prisma.lead.update({
            where: { id: leadId },
            data: { documents: JSON.stringify(documents) }
        });

        this.logger.log(`✅ Lead ${leadId}: Document ${docId} rattaché au dossier (${documents.length} docs total)`);

        return { success: true, message: 'Document déposé avec succès' };
    }

    /**
     * Construit le message WhatsApp avec boutons interactifs.
     * Retourne le texte du message ET les boutons CTA séparément.
     * 
     * Les boutons seront affichés :
     *  - En production : comme de vrais boutons cliquables WhatsApp (via Content Template Twilio)
     *  - En dev/sandbox : comme des liens formatés visuellement dans le texte
     */
    buildWhatsAppChecklistMessage(
        serviceLabel: string,
        clientSpaceUrl: string,
        uploadLinks: { docId: string; docName: string; url: string }[]
    ): { message: string; buttons: { title: string; url: string }[] } {
        // Message principal (sans les URLs brutes — elles sont dans les boutons)
        let msg = `✅ *Paiement confirmé pour ${serviceLabel} !*\n\n`;
        msg += `Bonjour, votre dossier a été enregistré avec succès.\n\n`;
        msg += `📋 *Pièces à fournir :*\n`;

        uploadLinks.forEach((link, i) => {
            msg += `${i + 1}. ${link.docName}\n`;
        });

        msg += `\nCliquez sur le bouton ci-dessous pour accéder à votre espace sécurisé et déposer vos documents.\n`;
        msg += `\n🔒 _Liens sécurisés, valables 30 jours._`;

        // Boutons CTA interactifs
        const buttons: { title: string; url: string }[] = [
            { title: '📂 Ouvrir mon espace client', url: clientSpaceUrl }
        ];

        // Ajouter les 2 premiers documents comme boutons directs (WhatsApp limite à 3 boutons CTA)
        const directUploadButtons = uploadLinks.slice(0, 2).map(link => ({
            title: `📤 ${link.docName.substring(0, 20)}`,
            url: link.url
        }));

        buttons.push(...directUploadButtons);

        return { message: msg, buttons };
    }

    // ═══════════════════════════════════════════════════════════
    // ESPACE CLIENT — Accès sans login via magic link
    // ═══════════════════════════════════════════════════════════

    /**
     * Génère un token JWT pour l'accès à l'espace client complet.
     * Expire après 90 jours (plus long que les liens doc individuels).
     */
    generateClientSpaceToken(leadId: string): string {
        return jwt.sign(
            { leadId, purpose: 'client_space' },
            JWT_SECRET,
            { expiresIn: '90d' }
        );
    }

    /**
     * Génère l'URL complète de l'espace client
     */
    generateClientSpaceUrl(leadId: string): string {
        const token = this.generateClientSpaceToken(leadId);
        return `${FRONTEND_URL}/client/${token}`;
    }

    /**
     * Vérifie un token d'espace client
     */
    verifyClientSpaceToken(token: string): { leadId: string } | null {
        try {
            const payload = jwt.verify(token, JWT_SECRET) as any;
            if (payload.purpose !== 'client_space') return null;
            return { leadId: payload.leadId };
        } catch (e) {
            this.logger.warn(`❌ Invalid client space token: ${e.message}`);
            return null;
        }
    }

    /**
     * Retourne les données complètes du dossier pour l'espace client.
     * Filtre les informations sensibles (pas d'accès aux notes internes, etc.)
     */
    async getClientSpaceData(leadId: string) {
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId }
        });
        if (!lead) return null;

        const documents = JSON.parse(lead.documents || '[]');
        const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];

        // Construire la checklist enrichie (merge required + uploaded)
        const checklist = requiredDocs.map((req: any) => {
            const uploaded = documents.find((d: any) => d.id === req.id);
            return {
                id: req.id,
                name: req.name || req.label,
                description: req.description || '',
                category: req.category || 'OTHER',
                required: req.required !== false,
                status: uploaded?.status || 'NOT_UPLOADED',
                fileUrl: uploaded?.fileUrl || null,
                uploadedAt: uploaded?.uploadedAt || null,
                rejectionReason: uploaded?.rejectionReason || null,
                // Générer un magic link individuel pour chaque doc
                uploadToken: jwt.sign(
                    { leadId, docId: req.id, docName: req.name || req.label, purpose: 'doc_upload' },
                    JWT_SECRET,
                    { expiresIn: '30d' }
                )
            };
        });

        // Calculer la progression
        const totalRequired = checklist.filter((d: any) => d.required).length;
        const validated = checklist.filter((d: any) => d.status === 'VALID').length;
        const pending = checklist.filter((d: any) => d.status === 'PENDING').length;
        const rejected = checklist.filter((d: any) => d.status === 'REJECTED').length;
        const notUploaded = checklist.filter((d: any) => d.status === 'NOT_UPLOADED').length;

        // Mapper le statut du dossier en français
        const statusLabels: Record<string, string> = {
            'NEW': 'Nouveau',
            'COLLECTING': 'Collecte des pièces',
            'REVIEWING': 'En cours d\'examen',
            'PROCESSING': 'En cours de traitement',
            'SUBMITTED': 'Dossier déposé',
            'COMPLETED': 'Terminé',
            'REJECTED': 'Refusé',
        };

        return {
            leadId: lead.id,
            name: lead.name,
            serviceName: lead.serviceName,
            status: lead.status,
            statusLabel: statusLabels[lead.status] || lead.status,
            createdAt: lead.createdAt,
            checklist,
            progress: {
                total: totalRequired,
                validated,
                pending,
                rejected,
                notUploaded,
                percentage: totalRequired > 0 ? Math.round((validated / totalRequired) * 100) : 0
            }
        };
    }

    private getExtFromMime(mime: string): string {
        const map: Record<string, string> = {
            'image/jpeg': '.jpg', 'image/png': '.png', 'image/webp': '.webp',
            'application/pdf': '.pdf', 'image/gif': '.gif',
        };
        return map[mime] || '.bin';
    }
}
