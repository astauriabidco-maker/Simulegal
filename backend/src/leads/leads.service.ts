import { Injectable, Logger } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
import { PipelineAutomationService } from '../pipeline-automation/pipeline-automation.service';
import { DocumentsService } from '../documents/documents.service';
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
        private documentsService: DocumentsService,
        private eventEmitter: EventEmitter2,
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
        const stageHistory = lead.stageHistory ? JSON.parse(lead.stageHistory) : [];
        const stageEnteredAt = lead.stageEnteredAt || lead.createdAt;
        const daysInStage = Math.floor((Date.now() - new Date(stageEnteredAt).getTime()) / (1000 * 60 * 60 * 24));
        const slaLimit = this.getSlaLimit(lead.status);

        return {
            ...lead,
            documents: lead.documents ? JSON.parse(lead.documents) : [],
            contract: lead.contract ? JSON.parse(lead.contract) : null,
            requiredDocs: lead.requiredDocs ? JSON.parse(lead.requiredDocs) : null,
            stageHistory,
            stageEnteredAt,
            daysInStage,
            sla: {
                limitDays: slaLimit,
                daysInStage,
                isOverdue: slaLimit > 0 && daysInStage > slaLimit,
                isWarning: slaLimit > 0 && daysInStage > slaLimit * 0.7,
                remainingDays: slaLimit > 0 ? Math.max(0, slaLimit - daysInStage) : null,
            }
        };
    }

    // ── SLA LIMITS par étape (en jours) ──
    private getSlaLimit(status: string): number {
        const SLA_LIMITS: Record<string, number> = {
            'COLLECTING': 7,    // 7 jours pour envoyer les docs
            'REVIEW': 3,        // 3 jours pour vérifier
            'HUNTING': 14,      // 14 jours pour trouver un créneau
            'DRAFTING': 5,      // 5 jours pour rédiger
            'SUBMITTED': 0,     // Pas de SLA (dépend de l'administration)
            'INSTRUCTION': 0,
            'DECISION_WAIT': 0,
            'SCHEDULING': 5,
        };
        return SLA_LIMITS[status] || 0;
    }

    // ── VALIDATION MÉTIER avant transition ──
    private validateTransition(lead: any, newStatus: string): { valid: boolean; reason?: string } {
        const docs = lead.documents ? JSON.parse(lead.documents) : [];
        const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];

        // Impossible de quitter COLLECTING sans documents requis validés
        if (lead.status === 'COLLECTING' && ['REVIEW', 'DRAFTING', 'HUNTING'].includes(newStatus)) {
            if (requiredDocs.length > 0) {
                const allRequired = requiredDocs.filter((r: any) => r.required !== false);
                const allValid = allRequired.every((r: any) => {
                    const uploaded = docs.find((d: any) => d.id === r.id);
                    return uploaded && (uploaded.status === 'VALID' || uploaded.status === 'PENDING');
                });
                if (!allValid) {
                    return { valid: false, reason: 'Tous les documents requis doivent être déposés avant de passer à l\'étape suivante.' };
                }
            }
        }

        // Impossible de quitter REVIEW sans juriste assigné
        if (lead.status === 'REVIEW' && !['COLLECTING', 'NEW', 'PAID'].includes(newStatus)) {
            if (!lead.assignedUserId) {
                return { valid: false, reason: 'Un juriste doit être assigné au dossier avant de passer à l\'étape suivante.' };
            }
        }

        return { valid: true };
    }

    async updateStatus(id: string, status: any) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (!lead) throw new Error('Lead not found');

        // Validation métier
        const validation = this.validateTransition(lead, status);
        if (!validation.valid) {
            this.logger.warn(`[BLOCKED] ${lead.name}: ${lead.status} → ${status} — ${validation.reason}`);
            throw new Error(validation.reason);
        }

        const oldStatus = lead.status;
        this.logger.log(`[LeadsService] ${lead.name}: ${oldStatus} → ${status}`);

        // Notifications + automations
        await this.notifications.onStageChange(lead, oldStatus, status);
        await this.pipelineAutomation.onStageChange(lead, oldStatus, status);

        // Construire l'historique
        const history = lead.stageHistory ? JSON.parse(lead.stageHistory) : [];
        history.push({
            from: oldStatus,
            to: status,
            at: new Date().toISOString(),
            daysInPreviousStage: Math.floor((Date.now() - new Date(lead.stageEnteredAt || lead.createdAt).getTime()) / (1000 * 60 * 60 * 24)),
        });

        const updatedLead = await this.prisma.lead.update({
            where: { id },
            data: {
                status,
                stageEnteredAt: new Date(),
                stageHistory: JSON.stringify(history),
            }
        });

        return this.mapLead(updatedLead);
    }

    // ── PORTEFEUILLE JURISTE ──
    async findByAssignedUser(userId: string) {
        const leads = await this.prisma.lead.findMany({
            where: { assignedUserId: userId },
            include: { notes: true, originAgency: true },
            orderBy: { updatedAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    // ── HISTORIQUE D'ÉTAPES ──
    async getStageHistory(id: string) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (!lead) return null;
        return JSON.parse(lead.stageHistory || '[]');
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
                status: (lead.status === 'NEW' || lead.status === 'PAID') ? 'COLLECTING' : lead.status
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
     * Sauvegarde le fichier, lance l'OCR automatique, puis met à jour la checklist.
     */
    async handleDocumentUpload(
        leadId: string,
        docId: string,
        fileBuffer: Buffer,
        originalFilename: string,
        mimeType: string
    ): Promise<{ success: boolean; message: string; ocrResult?: any }> {
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

        // ════════════════════════════════════════════════════════
        // 🛡️ FIX 2 — Anti-doublons EARLY CHECK (avant OCR pour économiser du compute)
        // ════════════════════════════════════════════════════════
        const earlyDocs: any[] = JSON.parse(lead.documents || '[]');
        const earlyExisting = earlyDocs.find((d: any) => d.id === docId);
        if (earlyExisting && earlyExisting.status === 'VALID') {
            this.logger.log(`🛡️ [Anti-doublon] Lead ${leadId}: Document ${docId} déjà validé — skip OCR`);
            // Supprimer le fichier inutile
            try { fs.unlinkSync(filePath); } catch { }
            return {
                success: true,
                message: `Ce document est déjà validé ✅. Inutile de le renvoyer.`,
                ocrResult: { status: 'VALID', confidence: 100, message: 'Déjà validé', extractedData: earlyExisting.ocrData }
            };
        }

        // ════════════════════════════════════════════════════════
        // 🤖 AGENT OCR — Vérification automatique du document
        // ════════════════════════════════════════════════════════
        let ocrStatus: 'PENDING' | 'VALID' | 'REJECTED' = 'PENDING';
        let ocrMessage = '';
        let ocrData: any = null;
        let ocrConfidence = 0;

        try {
            const multerFile = {
                buffer: fileBuffer,
                originalname: originalFilename,
                mimetype: mimeType,
            } as Express.Multer.File;

            this.logger.log(`🤖 [OCR Agent] Analyse en cours: ${originalFilename}...`);
            const analysis = await this.documentsService.analyze(multerFile);
            ocrData = analysis.extractedData;
            ocrConfidence = analysis.confidence;

            if (analysis.status === 'VALID') {
                // ── Vérification supplémentaire : date d'expiration ──
                if (ocrData?.expiryDate) {
                    const expiryDate = new Date(ocrData.expiryDate);
                    if (expiryDate < new Date()) {
                        ocrStatus = 'REJECTED';
                        ocrMessage = `Document expiré le ${expiryDate.toLocaleDateString('fr-FR')}. Veuillez fournir un document en cours de validité.`;
                        this.logger.warn(`🤖 [OCR Agent] ❌ EXPIRÉ: ${docId} — ${ocrMessage}`);
                    } else {
                        ocrStatus = 'VALID';
                        ocrMessage = `Document valide (confiance: ${ocrConfidence}%).`;
                        // ── Vérification nom du client ──
                        if (ocrData?.lastName && lead.name) {
                            const extractedName = (ocrData.lastName || '').toUpperCase();
                            const leadName = lead.name.toUpperCase();
                            const nameMatch = leadName.includes(extractedName) || extractedName.includes(leadName.split(' ').pop() || '');
                            if (!nameMatch && ocrConfidence < 85) {
                                // Doute — passer en PENDING pour vérif manuelle
                                ocrStatus = 'PENDING';
                                ocrMessage = `Vérification manuelle requise : le nom extrait (${ocrData.lastName}) ne correspond pas exactement au client (${lead.name}).`;
                                this.logger.warn(`🤖 [OCR Agent] ⚠️ NOM MISMATCH: ${extractedName} vs ${leadName}`);
                            }
                        }
                        if (ocrStatus === 'VALID') {
                            this.logger.log(`🤖 [OCR Agent] ✅ AUTO-VALIDÉ: ${docId} (${ocrConfidence}%)`);
                        }
                    }
                } else {
                    // Pas de date d'expiration détectée — valider si confiance suffisante
                    ocrStatus = ocrConfidence >= 70 ? 'VALID' : 'PENDING';
                    ocrMessage = ocrConfidence >= 70
                        ? `Document validé automatiquement (confiance: ${ocrConfidence}%).`
                        : `Confiance insuffisante (${ocrConfidence}%). Vérification manuelle requise.`;
                    this.logger.log(`🤖 [OCR Agent] ${ocrStatus === 'VALID' ? '✅' : '⏳'} ${docId}: ${ocrMessage}`);
                }
            } else if (analysis.status === 'REJECTED_BLURRY') {
                ocrStatus = 'REJECTED';
                ocrMessage = 'Document illisible ou trop flou. Veuillez reprendre la photo avec plus de lumière.';
                this.logger.warn(`🤖 [OCR Agent] ❌ FLOU: ${docId}`);
            } else if (analysis.status === 'REJECTED_EXPIRED') {
                ocrStatus = 'REJECTED';
                ocrMessage = 'Document expiré. Veuillez fournir un document en cours de validité.';
                this.logger.warn(`🤖 [OCR Agent] ❌ EXPIRÉ: ${docId}`);
            } else if (analysis.status === 'REJECTED_WRONG_TYPE') {
                ocrStatus = 'REJECTED';
                ocrMessage = 'Le document ne correspond pas au type demandé. Vérifiez le document attendu.';
                this.logger.warn(`🤖 [OCR Agent] ❌ MAUVAIS TYPE: ${docId}`);
            } else if (analysis.status === 'REJECTED_INCOMPLETE') {
                ocrStatus = 'REJECTED';
                ocrMessage = 'Document incomplet. Merci de capturer l\'intégralité du document.';
                this.logger.warn(`🤖 [OCR Agent] ❌ INCOMPLET: ${docId}`);
            }
        } catch (ocrError: any) {
            this.logger.warn(`🤖 [OCR Agent] ⚠️ Erreur OCR (fallback PENDING): ${ocrError.message}`);
            ocrStatus = 'PENDING';
            ocrMessage = 'Analyse automatique indisponible. Le document sera vérifié manuellement.';
        }

        // ════════════════════════════════════════════════════════
        // 🛡️ FIX 2 — Anti-doublons : skip si déjà VALID
        // ════════════════════════════════════════════════════════
        const documents: any[] = JSON.parse(lead.documents || '[]');
        const existingDoc = documents.find((d: any) => d.id === docId);
        if (existingDoc && existingDoc.status === 'VALID') {
            this.logger.log(`🛡️ [Anti-doublon] Lead ${leadId}: Document ${docId} déjà validé — skip re-upload`);
            return {
                success: true,
                message: `Ce document (${docId}) est déjà validé ✅. Inutile de le renvoyer.`,
                ocrResult: { status: 'VALID', confidence: 100, message: 'Déjà validé', extractedData: existingDoc.ocrData }
            };
        }

        // ════════════════════════════════════════════════════════
        // 🔄 FIX 1 — Réassignation intelligente post-OCR
        // ════════════════════════════════════════════════════════
        // Si l'OCR a détecté un type de document différent de celui assigné,
        // réassigner au bon slot dans la checklist
        let finalDocId = docId;
        if (ocrStatus !== 'REJECTED' && ocrData?.documentType) {
            const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
            const betterSlot = this.findBetterDocSlot(ocrData.documentType, docId, requiredDocs, documents);
            if (betterSlot) {
                this.logger.log(`🔄 [Smart Reassign] "${docId}" → "${betterSlot.id}" (OCR détecte: ${ocrData.documentType})`);
                finalDocId = betterSlot.id;
            }
        }

        const existingDocIndex = documents.findIndex((d: any) => d.id === finalDocId);
        const docEntry = {
            id: finalDocId,
            docType: finalDocId,
            status: ocrStatus,
            fileUrl,
            originalFilename,
            mimeType,
            uploadedAt: new Date().toISOString(),
            uploadMethod: 'MAGIC_LINK',
            ocrConfidence,
            ocrMessage,
            ocrData,
            reassignedFrom: finalDocId !== docId ? docId : undefined,
        };

        if (existingDocIndex >= 0) {
            documents[existingDocIndex] = { ...documents[existingDocIndex], ...docEntry };
        } else {
            documents.push(docEntry);
        }

        await this.prisma.lead.update({
            where: { id: leadId },
            data: { documents: JSON.stringify(documents) }
        });

        this.logger.log(`📋 Lead ${leadId}: Document ${docId} → ${ocrStatus} (${documents.length} docs total)`);

        // ════════════════════════════════════════════════════════
        // 📲 NOTIFICATIONS selon le résultat OCR
        // ════════════════════════════════════════════════════════
        if (ocrStatus === 'REJECTED') {
            // Notifier le client du rejet avec la raison
            const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
            const docLabel = requiredDocs.find((r: any) => r.id === docId)?.name || docId;
            const reuploadUrl = this.generateDocumentUploadLink(leadId, docId);
            await this.notifications.onDocumentRejected(lead, docLabel, ocrMessage, reuploadUrl);
        } else if (ocrStatus === 'VALID') {
            // Notifier la validation
            const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
            const docLabel = requiredDocs.find((r: any) => r.id === docId)?.name || docId;
            await this.notifications.onDocumentValidated(lead, docLabel);

            // 🤖 Déclencher l'Agent de Supervision en arrière-plan
            this.eventEmitter.emit('lead.document.validated', { leadId });
        }

        // ════════════════════════════════════════════════════════
        // 🔄 AUTO-ADVANCE : COLLECTING → REVIEW si tous les docs validés
        // ════════════════════════════════════════════════════════
        await this.checkAutoAdvance(leadId);

        return {
            success: ocrStatus !== 'REJECTED',
            message: ocrStatus === 'REJECTED'
                ? `Document refusé : ${ocrMessage}`
                : ocrStatus === 'VALID'
                    ? `Document validé automatiquement ! ${ocrMessage}`
                    : `Document déposé. ${ocrMessage}`,
            ocrResult: { status: ocrStatus, confidence: ocrConfidence, message: ocrMessage, extractedData: ocrData }
        };
    }

    /**
     * 🔄 Vérifie si tous les documents requis sont validés et avance automatiquement le Lead.
     */
    private async checkAutoAdvance(leadId: string): Promise<void> {
        const lead = await this.prisma.lead.findUnique({ where: { id: leadId } });
        if (!lead || lead.status !== 'COLLECTING') return;

        const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
        const documents = lead.documents ? JSON.parse(lead.documents) : [];

        if (requiredDocs.length === 0) return;

        const mandatoryDocs = requiredDocs.filter((r: any) => r.required !== false);
        const allMandatoryValid = mandatoryDocs.every((r: any) => {
            const uploaded = documents.find((d: any) => d.id === r.id);
            return uploaded && uploaded.status === 'VALID';
        });

        if (allMandatoryValid) {
            this.logger.log(`🎉 Lead ${leadId}: Tous les documents obligatoires validés → AUTO-ADVANCE vers REVIEW`);
            await this.updateStatus(leadId, 'REVIEW' as any);
            await this.notifications.onAllDocumentsValidated(lead);
        }
    }

    /**
     * Génère un magic link d'upload pour un document spécifique.
     */
    private generateDocumentUploadLink(leadId: string, docId: string): string {
        try {
            const token = jwt.sign(
                { leadId, docId, purpose: 'document_upload' },
                JWT_SECRET,
                { expiresIn: '30d' }
            );
            return `${FRONTEND_URL}/upload/${token}`;
        } catch {
            return '';
        }
    }

    /**
     * 🔄 FIX 1 — Réassignation intelligente post-OCR
     *
     * Si l'OCR a détecté un type de document (ex: "Passeport") mais qu'il a été
     * assigné au mauvais slot (ex: "justif_domicile"), cette méthode trouve
     * le bon slot dans la checklist de documents requis.
     */
    private findBetterDocSlot(
        detectedType: string,
        currentDocId: string,
        requiredDocs: any[],
        existingDocs: any[]
    ): { id: string; name: string } | null {
        // Mapping type OCR → IDs de documents possibles
        const typeToDocIds: Record<string, string[]> = {
            'Passeport': ['passeport', 'passport'],
            "Carte d'identité": ['carte_identite', 'cni', 'carte_nationale_identite'],
            'Titre de séjour': ['titre_sejour', 'carte_sejour', 'titre_de_sejour'],
            'Récépissé': ['recepisse', 'recipisse'],
            'Acte de naissance': ['acte_naissance', 'acte_de_naissance'],
            'Acte de mariage': ['acte_mariage', 'acte_de_mariage'],
            'Justificatif de domicile': ['justif_domicile', 'justificatif_domicile', 'attestation_domicile'],
            'Quittance de loyer': ['quittance_loyer', 'quittance'],
            "Avis d'imposition": ['avis_imposition', 'avis_impot'],
            'Formulaire CERFA': ['cerfa', 'formulaire_cerfa'],
            'Certificat de nationalité': ['certificat_nationalite'],
            'Facture': ['facture', 'facture_edf', 'facture_energie'],
            'Attestation': ['attestation', 'attestation_hebergement'],
        };

        const possibleDocIds = typeToDocIds[detectedType];
        if (!possibleDocIds) return null;

        // Vérifier si le docId actuel est déjà un match correct
        if (possibleDocIds.includes(currentDocId)) return null;

        // Chercher un slot requis non rempli qui correspond au type détecté
        for (const rd of requiredDocs) {
            if (possibleDocIds.includes(rd.id)) {
                // Vérifier que ce slot n'est pas déjà rempli (ou est REJECTED)
                const already = existingDocs.find((d: any) => d.id === rd.id && d.status !== 'REJECTED');
                if (!already) {
                    return { id: rd.id, name: rd.name };
                }
            }
        }

        return null;
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
