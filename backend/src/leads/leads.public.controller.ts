import { Controller, Post, Body, Get, Param, Query, BadRequestException, Inject, forwardRef, NotFoundException, UseInterceptors, UploadedFile } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { LeadsService } from './leads.service';
import { EmailService } from '../email/email.service';
import { PaymentsService } from '../payments/payments.service';
import { NotificationsService } from '../notifications/notifications.service';

@Controller('public/leads')
export class PublicLeadsController {
    constructor(
        private readonly leadsService: LeadsService,
        private readonly emailService: EmailService,
        @Inject(forwardRef(() => PaymentsService)) private readonly paymentsService: PaymentsService,
        private readonly notificationsService: NotificationsService
    ) { }

    @Post()
    async create(@Body() leadData: any) {
        if (!leadData.name || (!leadData.email && !leadData.phone)) {
            throw new BadRequestException('Contact information missing');
        }

        const secureData = {
            ...leadData,
            status: 'NEW',
            originAgencyId: leadData.forceAgencyId || undefined,
            id: leadData.id || undefined
        };

        return this.leadsService.create(secureData);
    }

    @Post('checkout')
    async createCheckoutSession(@Body() body: { leadId: string, amount: number, label: string, successUrl: string, cancelUrl: string }) {
        const { leadId, amount, label, successUrl, cancelUrl } = body;

        const lead = await this.leadsService.findOne(leadId);
        if (!lead) throw new BadRequestException('Lead not found');

        // Si Stripe est configuré, on utilise le vrai service
        if (this.paymentsService.isStripeConfigured()) {
            return this.paymentsService.createCheckoutSession(leadId, successUrl, cancelUrl, amount, label);
        }

        // MOCK STRIPE: Si Stripe n'est pas configuré
        await new Promise(resolve => setTimeout(resolve, 800));
        const mockSessionId = `cs_test_${leadId}_${Date.now()}`;
        const clientSpaceUrl = this.leadsService.generateClientSpaceUrl(leadId);

        // ENVOI EMAIL DE CONFIRMATION
        if (lead.email) {
            await this.emailService.sendOrderConfirmation(lead.email, lead.name, label, amount, mockSessionId, lead.requiredDocs, clientSpaceUrl);
            await this.emailService.sendMandateCopy(lead.email, lead.name);
        } else {
            console.warn(`[Checkout] Lead ${leadId} has no email, skipping confirmation email.`);
        }

        // ENVOI WHATSAPP AVEC BOUTONS INTERACTIFS + ESPACE CLIENT
        if (lead.phone && lead.requiredDocs && lead.requiredDocs.length > 0) {
            const uploadLinks = this.leadsService.generateDocumentUploadLinks(leadId, lead.requiredDocs);
            const clientSpaceUrl = this.leadsService.generateClientSpaceUrl(leadId);
            const { message, buttons } = this.leadsService.buildWhatsAppChecklistMessage(label, clientSpaceUrl, uploadLinks);

            await this.notificationsService.sendWhatsApp(
                lead.phone,
                'order_checklist',
                { message },
                { leadId: lead.id },
                buttons
            );
        }

        return {
            url: `${successUrl}?session_id=${mockSessionId}&mock_payment=true`,
            sessionId: mockSessionId
        };
    }

    // ═══════════════════════════════════════════════════════════
    // MAGIC LINK — Endpoints publics d'upload de documents
    // ═══════════════════════════════════════════════════════════

    /**
     * GET /public/leads/upload/:token
     * Vérifie un token d'upload et retourne les infos du document attendu.
     * Utilisé par la page frontend pour afficher le nom du document.
     */
    @Get('upload/:token')
    async verifyUploadToken(@Param('token') token: string) {
        const info = this.leadsService.verifyUploadToken(token);
        if (!info) {
            throw new BadRequestException('Lien invalide ou expiré. Contactez votre conseiller.');
        }

        // Vérifier que le lead existe toujours
        const lead = await this.leadsService.findOne(info.leadId);
        if (!lead) {
            throw new NotFoundException('Dossier introuvable');
        }

        // Vérifier si ce document a déjà été déposé
        const existingDoc = lead.documents?.find((d: any) => d.id === info.docId);
        const alreadyUploaded = existingDoc?.fileUrl && existingDoc?.status !== 'REJECTED';

        return {
            valid: true,
            docId: info.docId,
            docName: info.docName,
            leadName: lead.name,
            serviceName: lead.serviceName,
            alreadyUploaded,
            existingStatus: existingDoc?.status || null
        };
    }

    /**
     * POST /public/leads/upload/:token
     * Reçoit le fichier uploadé via magic link et le rattache au dossier.
     * Pas besoin d'authentification — le token JWT fait office de preuve.
     */
    @Post('upload/:token')
    @UseInterceptors(FileInterceptor('file'))
    async uploadDocument(
        @Param('token') token: string,
        @UploadedFile() file: any
    ) {
        // 1. Vérifier le token
        const info = this.leadsService.verifyUploadToken(token);
        if (!info) {
            throw new BadRequestException('Lien invalide ou expiré.');
        }

        // 2. Vérifier qu'un fichier a été envoyé
        if (!file) {
            throw new BadRequestException('Aucun fichier reçu.');
        }

        // 3. Vérifier le type de fichier (images et PDF uniquement)
        const allowedTypes = ['image/jpeg', 'image/png', 'image/webp', 'image/gif', 'application/pdf'];
        if (!allowedTypes.includes(file.mimetype)) {
            throw new BadRequestException('Type de fichier non autorisé. Veuillez envoyer une image (JPG, PNG) ou un PDF.');
        }

        // 4. Vérifier la taille (max 15 MB)
        const maxSize = 15 * 1024 * 1024;
        if (file.size > maxSize) {
            throw new BadRequestException('Fichier trop volumineux (15 MB maximum).');
        }

        // 5. Sauvegarder et rattacher au dossier
        const result = await this.leadsService.handleDocumentUpload(
            info.leadId,
            info.docId,
            file.buffer,
            file.originalname,
            file.mimetype
        );

        return result;
    }

    // ═══════════════════════════════════════════════════════════
    // ESPACE CLIENT — Dashboard sans login
    // ═══════════════════════════════════════════════════════════

    /**
     * GET /public/leads/client-space/:token
     * Retourne les données complètes du dossier pour l'espace client.
     * Pas d'authentification requise — le token JWT suffit.
     */
    @Get('client-space/:token')
    async getClientSpace(@Param('token') token: string) {
        const info = this.leadsService.verifyClientSpaceToken(token);
        if (!info) {
            throw new BadRequestException('Lien d\'accès invalide ou expiré. Contactez votre conseiller.');
        }

        const data = await this.leadsService.getClientSpaceData(info.leadId);
        if (!data) {
            throw new NotFoundException('Dossier introuvable');
        }

        return data;
    }

    @Get('debug/last-email')
    async getLastEmailSent() {
        return this.emailService.getLastEmail();
    }
}
