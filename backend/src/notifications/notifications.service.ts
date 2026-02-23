import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as nodemailer from 'nodemailer';
import Twilio from 'twilio';
import { EmailTemplatesService } from './email-templates.service';
import { SettingsService } from '../settings/settings.service';
import { PrismaService } from '../prisma/prisma.service';
import { getContentSid, WhatsAppProvider } from './whatsapp-templates.config';
import { MetaWhatsAppService } from './meta-whatsapp.service';

interface SmtpConfig {
    host: string;
    port: number;
    user: string;
    pass: string;
}

@Injectable()
export class NotificationsService {
    private twilioClient: Twilio.Twilio;
    private metaClient: MetaWhatsAppService | null = null;
    private whatsappProvider: WhatsAppProvider = 'NONE';
    private cachedSmtpConfig: SmtpConfig | null = null;
    private lastConfigCheck: number = 0;
    private readonly CONFIG_TTL = 60000; // Refresh config every 60 seconds

    constructor(
        private configService: ConfigService,
        private emailTemplates: EmailTemplatesService,
        private settingsService: SettingsService,
        private prisma: PrismaService
    ) {
        // ═══════════════════════════════════════════════════
        // 1. Tenter d'initialiser Meta WhatsApp Cloud API (prioritaire)
        // ═══════════════════════════════════════════════════
        const metaPhoneId = this.configService.get<string>('META_WHATSAPP_PHONE_ID');
        const metaToken = this.configService.get<string>('META_WHATSAPP_TOKEN');

        if (metaPhoneId && metaToken) {
            this.metaClient = new MetaWhatsAppService({
                phoneNumberId: metaPhoneId,
                accessToken: metaToken,
                apiVersion: this.configService.get<string>('META_WHATSAPP_API_VERSION') || 'v21.0',
                businessAccountId: this.configService.get<string>('META_WHATSAPP_BUSINESS_ID'),
            });
            this.whatsappProvider = 'META';
            console.log('[WhatsApp] 🟢 Provider: Meta Cloud API (direct)');
        }

        // ═══════════════════════════════════════════════════
        // 2. Fallback sur Twilio si Meta n'est pas configuré
        // ═══════════════════════════════════════════════════
        const accountSid = this.configService.get<string>('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get<string>('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = Twilio(accountSid, authToken);
            if (this.whatsappProvider === 'NONE') {
                this.whatsappProvider = 'TWILIO';
                console.log('[WhatsApp] 🟢 Provider: Twilio');
            }
        }

        if (this.whatsappProvider === 'NONE') {
            console.warn('[WhatsApp] ⚠️ No WhatsApp provider configured. Messages will be logged only.');
        }
    }

    /**
     * Retourne le provider WhatsApp actif
     */
    getWhatsAppProvider(): WhatsAppProvider {
        return this.whatsappProvider;
    }

    /**
     * Get SMTP config from database (with caching) or fallback to env vars
     */
    private async getSmtpConfig(): Promise<SmtpConfig | null> {
        const now = Date.now();

        // Return cached config if still valid
        if (this.cachedSmtpConfig && (now - this.lastConfigCheck) < this.CONFIG_TTL) {
            return this.cachedSmtpConfig;
        }

        try {
            // Try to load from database
            const settings = await this.settingsService.getSettings();
            const dbConfig = settings.notifications;

            if (dbConfig?.smtpHost && dbConfig?.smtpUser && dbConfig?.smtpPass !== '******') {
                this.cachedSmtpConfig = {
                    host: dbConfig.smtpHost,
                    port: dbConfig.smtpPort || 587,
                    user: dbConfig.smtpUser,
                    pass: dbConfig.smtpPass
                };
                this.lastConfigCheck = now;
                console.log('[SMTP] 📧 Using database configuration');
                return this.cachedSmtpConfig;
            }
        } catch (error) {
            console.warn('[SMTP] Could not load settings from DB:', error);
        }

        // Fallback to environment variables
        const envHost = this.configService.get<string>('SMTP_HOST');
        const envUser = this.configService.get<string>('SMTP_USER');
        const envPass = this.configService.get<string>('SMTP_PASS');

        if (envHost && envUser) {
            this.cachedSmtpConfig = {
                host: envHost,
                port: this.configService.get<number>('SMTP_PORT') || 587,
                user: envUser,
                pass: envPass || ''
            };
            this.lastConfigCheck = now;
            console.log('[SMTP] 📧 Using environment configuration');
            return this.cachedSmtpConfig;
        }

        return null;
    }

    /**
     * Create a transporter on-demand with current config
     */
    private async createTransporter(): Promise<nodemailer.Transporter | null> {
        const config = await this.getSmtpConfig();
        if (!config) return null;

        return nodemailer.createTransport({
            host: config.host,
            port: config.port,
            secure: config.port === 465,
            auth: {
                user: config.user,
                pass: config.pass,
            },
        });
    }

    /**
     * Force refresh of SMTP configuration (call after settings update)
     */
    async refreshSmtpConfig(): Promise<void> {
        this.cachedSmtpConfig = null;
        this.lastConfigCheck = 0;
        console.log('[SMTP] 🔄 Configuration cache cleared');
    }

    /**
     * Envoi WhatsApp — Multi-provider (Meta Cloud API / Twilio / Fallback texte)
     * 
     * Routing automatique :
     * - META  : Utilise l'API WhatsApp Cloud de Meta (direct, moins cher)
     * - TWILIO: Utilise Twilio comme proxy (Content Templates)
     * - NONE  : Log uniquement (dev sans credentials)
     * 
     * @param phone - Numéro de téléphone du destinataire
     * @param template - Nom du template (ex: 'order_checklist')
     * @param params - Paramètres du message (message, contentVariables, etc.)
     * @param metadata - Métadonnées pour la persistence (leadId, prospectId)
     * @param buttons - Boutons CTA [{title, url}]
     * @param contentSid - SID du Content Template (Twilio only, optionnel)
     */
    async sendWhatsApp(
        phone: string,
        template: string,
        params: any,
        metadata?: { leadId?: string, prospectId?: string },
        buttons?: { title: string; url: string }[],
        contentSid?: string
    ) {
        const resolvedContentSid = contentSid || getContentSid(template);

        // Construire le message texte (utilisé pour persistence + fallback)
        let finalMessage = params.message || 'Template message';

        // Ajouter les boutons comme liens texte (pour persistence DB + mode fallback)
        if (buttons && buttons.length > 0) {
            const buttonsText = '\n\n' + buttons.map(btn =>
                `━━━━━━━━━━━━━━━━\n▶️ *${btn.title}*\n${btn.url}`
            ).join('\n') + '\n━━━━━━━━━━━━━━━━';

            // Ajouter au message seulement si pas de template résolu (fallback texte)
            if (!resolvedContentSid && this.whatsappProvider !== 'META') {
                finalMessage += buttonsText;
            }
        }

        // ═══ Persistence DB (identique pour tous les providers) ═══
        const sender = this.whatsappProvider === 'META' ? 'META_CLOUD_API' : 'TWILIO';
        try {
            await this.prisma.communication.create({
                data: {
                    direction: 'OUTBOUND',
                    type: 'WHATSAPP',
                    content: finalMessage,
                    sender: sender,
                    senderName: 'SYSTEM',
                    leadId: metadata?.leadId,
                    prospectId: metadata?.prospectId
                }
            });
        } catch (persistError) {
            console.warn('[WhatsApp] Persistence failed:', persistError.message);
        }

        // ═══ Routing vers le bon provider ═══
        switch (this.whatsappProvider) {
            case 'META':
                return this.sendWhatsAppViaMeta(phone, template, params, finalMessage, buttons, resolvedContentSid);

            case 'TWILIO':
                return this.sendWhatsAppViaTwilio(phone, template, params, finalMessage, buttons, resolvedContentSid);

            case 'NONE':
            default:
                console.log(`[WhatsApp] 📝 LOG ONLY (no provider) — template: ${template}`);
                console.log(`[WhatsApp] To: ${phone}`);
                console.log(`[WhatsApp] Message: ${finalMessage.substring(0, 200)}...`);
                return { success: true, messageId: 'log-only' };
        }
    }

    /**
     * Envoi via Meta WhatsApp Cloud API
     */
    private async sendWhatsAppViaMeta(
        phone: string,
        template: string,
        params: any,
        finalMessage: string,
        buttons?: { title: string; url: string }[],
        templateId?: string | null
    ) {
        if (!this.metaClient) {
            console.warn('[Meta WhatsApp] Client not initialized.');
            return { success: false };
        }

        try {
            // Si on a un template approuvé configuré, l'utiliser
            if (templateId) {
                // Pour Meta, le templateId EST le nom du template (pas un SID)
                // Extraire les variables du message pour les passer au template
                const bodyParams = params.templateVariables || [];

                // Construire les paramètres des boutons URL
                const buttonParams = (buttons || []).map((btn, index) => {
                    // Extraire la partie dynamique de l'URL (le token JWT)
                    const urlParts = btn.url.split('/');
                    const dynamicPart = urlParts[urlParts.length - 1] || '';
                    return {
                        index,
                        sub_type: 'url',
                        parameters: [{ type: 'text', text: dynamicPart }]
                    };
                });

                const result = await this.metaClient.sendTemplate(
                    phone,
                    templateId, // Nom du template Meta (ex: 'simulegal_order_checklist')
                    'fr',
                    bodyParams.length > 0 ? bodyParams : undefined,
                    buttonParams.length > 0 ? buttonParams : undefined
                );

                if (result.success) {
                    console.log(`[WhatsApp/Meta] 🟢 Template "${template}" sent to ${phone}: ${result.messageId}`);
                }
                return result;
            }

            // Sinon, envoyer un message texte simple
            const result = await this.metaClient.sendText(phone, finalMessage);
            if (result.success) {
                console.log(`[WhatsApp/Meta] 🟢 Text message sent to ${phone}: ${result.messageId}`);
            }
            return result;
        } catch (error) {
            console.error('[WhatsApp/Meta] Error:', error);
            return { success: false, error };
        }
    }

    /**
     * Envoi via Twilio (legacy)
     */
    private async sendWhatsAppViaTwilio(
        phone: string,
        template: string,
        params: any,
        finalMessage: string,
        buttons?: { title: string; url: string }[],
        resolvedContentSid?: string | null
    ) {
        if (!this.twilioClient) {
            console.warn('[WhatsApp/Twilio] Client not initialized. Check env vars.');
            return { success: false };
        }

        const from = this.configService.get<string>('TWILIO_WHATSAPP_NUMBER') || 'whatsapp:+14155238886';
        const to = `whatsapp:${phone.replace(/^0/, '+33').replace(/\s/g, '')}`;

        try {
            // Si on a un contentSid (production), utiliser le Content Template
            if (resolvedContentSid) {
                const result = await this.twilioClient.messages.create({
                    contentSid: resolvedContentSid,
                    contentVariables: JSON.stringify(params.contentVariables || {}),
                    from: from,
                    to: to
                });
                console.log(`[WhatsApp/Twilio] 🟢 Template "${template}" sent to ${to}: ${result.sid}`);
                return { success: true, messageId: result.sid };
            }

            // Sinon, envoi du message texte simple
            const result = await this.twilioClient.messages.create({
                body: finalMessage,
                from: from,
                to: to
            });
            console.log(`[WhatsApp/Twilio] 🟢 Text sent to ${to}: ${result.sid}`);
            return { success: true, messageId: result.sid };
        } catch (error) {
            console.error('[WhatsApp/Twilio] Error:', error);
            return { success: false, error };
        }
    }

    /**
     * Send email with optional HTML content
     * Uses dynamic SMTP configuration from database or env vars
     */
    async sendEmail(to: string, subject: string, body: string, html?: string) {
        const transporter = await this.createTransporter();

        if (!transporter) {
            // Log email content for dev environment (no SMTP configured)
            console.log(`[Mailer] 📧 DEV MODE - Would send to ${to}:`);
            console.log(`  Subject: ${subject}`);
            console.log(`  Body preview: ${body.substring(0, 100)}...`);
            return { success: true, messageId: 'dev-mode-no-smtp' };
        }

        try {
            const info = await transporter.sendMail({
                from: '"SimuLegal" <no-reply@simulegal.fr>', // sender address
                to: to, // list of receivers
                subject: subject, // Subject line
                text: body, // plain text body
                html: html || body.replace(/\n/g, '<br>'), // HTML body
            });
            console.log(`[Email] 📧 Sent to ${to}: ${info.messageId}`);
            return { success: true, messageId: info.messageId };
        } catch (error) {
            console.error('[Email] Error:', error);
            return { success: false, error };
        }
    }

    // ========================================
    // TRANSACTIONAL EMAIL METHODS
    // ========================================

    /**
     * Send welcome email with temporary credentials
     */
    async sendWelcomeEmail(user: { name: string; email: string }, tempPassword: string) {
        const template = this.emailTemplates.renderWelcome(user.name, user.email, tempPassword);
        console.log(`[Transactional] 📧 Sending WELCOME email to ${user.email}`);
        return this.sendEmail(user.email, template.subject, `Bienvenue ${user.name}! Vos accès: ${user.email} / ${tempPassword}`, template.html);
    }

    /**
     * Send diagnostic invitation with magic link
     */
    async sendDiagnosticInvitation(prospect: { name: string; email: string }, magicLink: string) {
        const template = this.emailTemplates.renderDiagnosticInvitation(prospect.name, magicLink);
        console.log(`[Transactional] 📧 Sending DIAGNOSTIC INVITATION to ${prospect.email}`);
        return this.sendEmail(prospect.email, template.subject, `${prospect.name}, passez votre diagnostic: ${magicLink}`, template.html);
    }

    /**
     * Send appointment confirmation with rich HTML
     */
    async sendAppointmentConfirmationEmail(
        lead: { name: string; email: string },
        appointment: { start: Date; type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY'; meetingLink?: string; agencyAddress?: string }
    ) {
        const template = this.emailTemplates.renderAppointmentConfirmation(
            lead.name,
            new Date(appointment.start),
            appointment.type,
            appointment.meetingLink,
            appointment.agencyAddress
        );
        console.log(`[Transactional] 📧 Sending APPOINTMENT CONFIRMATION to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `RDV confirmé pour ${lead.name}`, template.html);
    }

    /**
     * Send payment confirmation with refund code (B2C)
     */
    async sendPaymentConfirmation(lead: { name: string; email: string }, amount: number, refundCode: string) {
        const template = this.emailTemplates.renderPaymentConfirmation(lead.name, amount, refundCode);
        console.log(`[Transactional] 📧 Sending PAYMENT CONFIRMATION to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `Paiement de ${amount}€ reçu. Code: ${refundCode}`, template.html);
    }

    /**
     * Send appointment reminder (24h before)
     */
    async sendAppointmentReminder(
        lead: { name: string; email: string },
        appointment: { start: Date; type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY'; meetingLink?: string }
    ) {
        const template = this.emailTemplates.renderAppointmentReminder(
            lead.name,
            new Date(appointment.start),
            appointment.type,
            appointment.meetingLink
        );
        console.log(`[Transactional] 📧 Sending APPOINTMENT REMINDER to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `Rappel: RDV demain`, template.html);
    }

    async sendSMS(phone: string, message: string) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized.');
            return { success: false };
        }

        const from = this.configService.get<string>('TWILIO_CALLER_ID'); // Can use same number for Voice and SMS often
        const to = phone.replace(/^0/, '+33').replace(/\s/g, '');

        try {
            const result = await this.twilioClient.messages.create({
                body: message,
                from: from,
                to: to
            });
            console.log(`[SMS] 📱 Sent to ${to}: ${result.sid}`);
            return { success: true, messageId: result.sid };
        } catch (error) {
            console.error('[SMS] Error:', error);
            return { success: false, error };
        }
    }

    async sendAppointmentConfirmation(lead: any, appointment: any) {
        const dateStr = new Date(appointment.start).toLocaleString('fr-FR', {
            weekday: 'long',
            day: 'numeric',
            month: 'long',
            hour: '2-digit',
            minute: '2-digit'
        });

        const meetingInfo = appointment.meetingLink
            ? `\nLien Visio : ${appointment.meetingLink}`
            : `\nLieu : En agence (${appointment.agencyId || 'Siège'})`;

        const message = `Bonjour ${lead.name}, votre RDV SimuLegal est confirmé pour le ${dateStr}.${meetingInfo}`;

        console.log(`[Notifications] Processing confirmation for ${lead.phone}/${lead.email}`);

        // Send across all channels
        await Promise.all([
            // WhatsApp (fail silent)
            this.sendWhatsApp(lead.phone || '0600000000', 'booking_confirmation', { message }, { leadId: lead.id }),
            // Email
            lead.email ? this.sendEmail(lead.email, 'Confirmation de votre Rendez-vous SimuLegal', message) : Promise.resolve(),
            // SMS
            this.sendSMS(lead.phone || '0600000000', message)
        ]);
    }

    /**
     * Logique de trigger sur changement d'étape
     */
    async onStageChange(lead: any, oldStage: string, newStage: string) {
        console.log(`[Backend-NotificationTrigger] Dossier ${lead.id} (${lead.name}): ${oldStage} -> ${newStage}`);

        if (newStage === 'OFII_INVESTIGATION') {
            await this.sendWhatsApp(lead.phone, 'coach_ofii_alert', {
                name: lead.name,
                message: `⚠️ Important : Votre dossier est à l'étape Enquête Logement/OFII. Préparez votre logement. Checklist : simulegal.fr/guide-ofii`
            }, { leadId: lead.id });
        }

        if (newStage === 'HUNTING') {
            await this.sendWhatsApp(lead.phone, 'hunting_start', {
                name: lead.name,
                message: `⚡️ Recherche activée. Nous surveillons les créneaux de RDV pour vous.`
            }, { leadId: lead.id });
        }

        if (newStage === 'BOOKED') {
            await this.sendWhatsApp(lead.phone, 'booking_success', {
                name: lead.name,
                message: `✅ RDV RÉSERVÉ ! Détails disponibles dans votre espace client.`
            }, { leadId: lead.id });
        }
    }

    /**
     * Trigger lors du rejet d'un document — envoie un bouton de re-upload direct
     */
    async onDocumentRejected(lead: any, docLabel: string, rejectionReason?: string, reuploadUrl?: string) {
        console.log(`[Notifications] Document rejected for ${lead.name}: ${docLabel}`);

        const reasonText = rejectionReason
            ? `\n\n💬 Motif : _${rejectionReason}_`
            : '';

        const message = `⚠️ *Document refusé*\n\n` +
            `Bonjour ${lead.name},\n` +
            `Votre document « *${docLabel}* » n'a pas pu être validé par notre équipe.` +
            reasonText + `\n\n` +
            `Merci de renvoyer ce document via le bouton ci-dessous.`;

        const buttons: { title: string; url: string }[] = [];

        if (reuploadUrl) {
            buttons.push({ title: '📤 Renvoyer ce document', url: reuploadUrl });
        }

        // Ajouter aussi le lien vers l'espace client
        const clientSpaceUrl = this.generateClientSpaceUrl(lead.id);
        if (clientSpaceUrl) {
            buttons.push({ title: '📂 Mon espace client', url: clientSpaceUrl });
        }

        await this.sendWhatsApp(
            lead.phone,
            'document_rejected',
            { message },
            { leadId: lead.id },
            buttons
        );
    }

    /**
     * Trigger lors de la validation d'un document
     */
    async onDocumentValidated(lead: any, docLabel: string) {
        console.log(`[Notifications] Document validated for ${lead.name}: ${docLabel}`);

        const message = `✅ *Document validé*\n\n` +
            `Bonjour ${lead.name},\n` +
            `Votre document « *${docLabel}* » a été validé par notre équipe.\n\n` +
            `Vous pouvez suivre l'avancement de votre dossier dans votre espace client.`;

        const clientSpaceUrl = this.generateClientSpaceUrl(lead.id);
        const buttons: { title: string; url: string }[] = [];

        if (clientSpaceUrl) {
            buttons.push({ title: '📂 Voir mon dossier', url: clientSpaceUrl });
        }

        await this.sendWhatsApp(
            lead.phone,
            'document_validated',
            { message },
            { leadId: lead.id },
            buttons
        );
    }

    /**
     * Trigger quand TOUS les documents requis sont validés.
     * → WhatsApp + Email au client
     * → Notification interne au juriste assigné
     */
    async onAllDocumentsValidated(lead: any) {
        console.log(`[Notifications] All documents validated for ${lead.name} 🎉`);

        const clientSpaceUrl = this.generateClientSpaceUrl(lead.id);

        // ══════════════════════════════════════════
        // 1. WHATSAPP AU CLIENT
        // ══════════════════════════════════════════
        const whatsappMessage = `🎉 *Dossier complet !*\n\n` +
            `Bonjour ${lead.name},\n` +
            `Tous vos documents ont été vérifiés et validés. Votre dossier est maintenant *en cours de traitement* par notre équipe juridique.\n\n` +
            `Nous vous tiendrons informé(e) de l'avancement.\n\n` +
            `Merci de votre confiance ! 🙏`;

        const buttons: { title: string; url: string }[] = [];
        if (clientSpaceUrl) {
            buttons.push({ title: '📂 Suivre mon dossier', url: clientSpaceUrl });
        }

        await this.sendWhatsApp(
            lead.phone,
            'all_documents_validated',
            { message: whatsappMessage },
            { leadId: lead.id },
            buttons
        );

        // ══════════════════════════════════════════
        // 2. EMAIL AU CLIENT
        // ══════════════════════════════════════════
        if (lead.email) {
            const clientLink = clientSpaceUrl || 'https://simulegal.fr/client';
            const emailSubject = `🎉 Dossier complet — ${lead.serviceName || 'Simulegal'}`;
            const emailBody = `Bonjour ${lead.name},\n\n` +
                `Excellente nouvelle ! Tous les documents de votre dossier "${lead.serviceName || ''}" ont été vérifiés et validés par notre équipe.\n\n` +
                `VOTRE DOSSIER EST MAINTENANT EN COURS DE TRAITEMENT\n\n` +
                `Notre équipe juridique va désormais constituer votre dossier complet et procéder aux démarches nécessaires.\n\n` +
                `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n` +
                `🔗 SUIVRE L'AVANCEMENT :\n` +
                `${clientLink}\n` +
                `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n` +
                `Nous vous contacterons si nous avons besoin d'informations complémentaires.\n\n` +
                `Cordialement,\n` +
                `L'équipe Simulegal`;

            await this.sendEmail(lead.email, emailSubject, emailBody);
        }

        // ══════════════════════════════════════════
        // 3. NOTIFICATION INTERNE AU JURISTE ASSIGNÉ
        // ══════════════════════════════════════════
        if (lead.assignedUserId) {
            try {
                const jurist = await this.prisma.user.findUnique({
                    where: { id: lead.assignedUserId }
                });

                if (jurist) {
                    const internalMessage = `📋 *Dossier prêt à traiter*\n\n` +
                        `Le dossier de *${lead.name}* (${lead.serviceName || 'N/A'}) est complet.\n` +
                        `Tous les documents ont été validés. Le dossier passe en traitement.\n\n` +
                        `📌 Réf : ${lead.id}`;

                    // Email interne
                    if (jurist.email) {
                        await this.sendEmail(
                            jurist.email,
                            `📋 Dossier complet — ${lead.name} (${lead.serviceName || ''})`,
                            `Bonjour ${jurist.name},\n\n` +
                            `Le dossier de ${lead.name} est complet. Tous les documents requis ont été validés.\n\n` +
                            `Référence : ${lead.id}\n` +
                            `Service : ${lead.serviceName || 'N/A'}\n\n` +
                            `Le dossier est maintenant en statut PROCESSING.\n\n` +
                            `— Simulegal`
                        );
                    }

                    // WhatsApp interne (si le juriste a un numéro)
                    if ((jurist as any).phone) {
                        await this.sendWhatsApp(
                            (jurist as any).phone,
                            'internal_dossier_complete',
                            { message: internalMessage }
                        );
                    }

                    console.log(`[Notifications] Internal notification sent to jurist ${jurist.name} for lead ${lead.id}`);
                }
            } catch (e) {
                console.warn(`[Notifications] Failed to notify jurist: ${e.message}`);
            }
        }
    }

    /**
     * Helper: génère l'URL de l'espace client pour un lead
     * (utilise le même JWT_SECRET que LeadsService)
     */
    private generateClientSpaceUrl(leadId: string): string | null {
        try {
            const jwt = require('jsonwebtoken');
            const secret = process.env.JWT_SECRET || 'dev_secret_change_in_production';
            const frontendUrl = process.env.FRONTEND_URL || 'http://localhost:3000';
            const token = jwt.sign({ leadId, purpose: 'client_space' }, secret, { expiresIn: '90d' });
            return `${frontendUrl}/client/${token}`;
        } catch {
            return null;
        }
    }

    /**
     * Trigger lors de l'assignation d'un juriste
     */
    async onJuristAssigned(lead: any, juristName: string) {
        console.log(`[Notifications] Jurist assigned to ${lead.name}: ${juristName}`);

        const message = `💼 *Juriste assigné*\n\n` +
            `Bonjour ${lead.name},\n` +
            `Votre dossier est maintenant pris en charge par *${juristName}*.\n\n` +
            `Vous pouvez suivre l'avancement dans votre espace client.`;

        const clientSpaceUrl = this.generateClientSpaceUrl(lead.id);
        const buttons: { title: string; url: string }[] = [];

        if (clientSpaceUrl) {
            buttons.push({ title: '📂 Mon espace client', url: clientSpaceUrl });
        }

        await this.sendWhatsApp(
            lead.phone,
            'jurist_assigned',
            { message },
            { leadId: lead.id },
            buttons
        );
    }

    /**
     * Trigger lors de l'onboarding d'une nouvelle franchise
     * Envoie le Kit d'Ouverture complet : email HTML riche + WhatsApp
     */
    async onFranchiseOnboarding(lead: any, tempPassword: string, agency?: any) {
        const agencyId = agency?.id || lead.convertedAgencyId || '—';
        const agencyName = agency?.name || lead.companyName || lead.name;
        const kioskUrl = agency?.kioskUrl || `https://simulegal.fr/kiosk/${agencyId}`;
        const adminUrl = 'https://admin.simulegal.fr';

        // === Email HTML professionnel — Kit d'Ouverture ===
        const html = `
        <div style="font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; max-width: 640px; margin: 0 auto; background: #f8fafc; padding: 0;">
            <!-- Header -->
            <div style="background: linear-gradient(135deg, #4f46e5, #7c3aed); padding: 32px; border-radius: 16px 16px 0 0;">
                <h1 style="color: white; margin: 0; font-size: 24px;">🎉 Bienvenue chez SimuLegal !</h1>
                <p style="color: #c7d2fe; margin: 8px 0 0; font-size: 14px;">Votre Kit d'Ouverture Franchisé</p>
            </div>

            <!-- Body -->
            <div style="background: white; padding: 32px; border: 1px solid #e2e8f0;">
                <p style="font-size: 15px; color: #334155;">Bonjour <strong>${lead.name}</strong>,</p>
                <p style="font-size: 14px; color: #475569; line-height: 1.6;">
                    Félicitations ! Votre contrat de franchise a été signé avec succès. Votre agence 
                    <strong>${agencyName}</strong> (ID: <code style="background:#f1f5f9; padding:2px 6px; border-radius:4px;">${agencyId}</code>) 
                    est maintenant active sur le réseau SimuLegal.
                </p>

                <!-- Identifiants -->
                <div style="background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 12px; padding: 20px; margin: 20px 0;">
                    <h3 style="margin: 0 0 12px; color: #1e40af; font-size: 14px;">🔑 Vos Identifiants de Connexion</h3>
                    <table style="width: 100%; border-collapse: collapse;">
                        <tr><td style="padding: 6px 0; color: #64748b; font-size: 13px; width: 140px;">Plateforme</td><td style="font-weight: bold; color: #1e293b; font-size: 13px;"><a href="${adminUrl}" style="color: #4f46e5;">${adminUrl}</a></td></tr>
                        <tr><td style="padding: 6px 0; color: #64748b; font-size: 13px;">Email</td><td style="font-weight: bold; color: #1e293b; font-size: 13px;">${lead.email}</td></tr>
                        <tr><td style="padding: 6px 0; color: #64748b; font-size: 13px;">Mot de passe temporaire</td><td style="font-weight: bold; color: #dc2626; font-size: 13px; font-family: monospace;">${tempPassword}</td></tr>
                    </table>
                    <p style="margin: 12px 0 0; font-size: 11px; color: #94a3b8;">⚠️ Changez votre mot de passe dès la première connexion.</p>
                </div>

                ${agency?.kioskUrl ? `
                <!-- Borne / Kiosk -->
                <div style="background: #f5f3ff; border: 1px solid #ddd6fe; border-radius: 12px; padding: 20px; margin: 20px 0;">
                    <h3 style="margin: 0 0 8px; color: #6d28d9; font-size: 14px;">🖥️ Votre Borne SimuLegal</h3>
                    <p style="font-size: 13px; color: #475569; margin: 0;">URL d'accès : <a href="${kioskUrl}" style="color: #7c3aed; font-weight: bold;">${kioskUrl}</a></p>
                </div>
                ` : ''}

                <!-- Checklist d'onboarding -->
                <div style="background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 12px; padding: 20px; margin: 20px 0;">
                    <h3 style="margin: 0 0 16px; color: #166534; font-size: 14px;">📋 Checklist d'Ouverture (8 étapes)</h3>
                    <table style="width: 100%; border-collapse: collapse;">
                        ${[
                { n: 1, label: 'Se connecter à la plateforme et changer le mot de passe', cat: 'Immédiat' },
                { n: 2, label: 'Compléter le profil de votre agence (horaires, adresse, photo)', cat: 'Jour 1' },
                { n: 3, label: 'Suivre la formation en ligne (module Juriste SimuLegal)', cat: 'Semaine 1' },
                { n: 4, label: 'Commander les supports marketing (PLV, cartes de visite)', cat: 'Semaine 1' },
                { n: 5, label: 'Configurer les services proposés dans votre zone', cat: 'Semaine 2' },
                { n: 6, label: 'Tester le parcours client (simulation d\'un dossier test)', cat: 'Semaine 2' },
                { n: 7, label: 'Organiser l\'événement d\'inauguration', cat: 'Mois 1' },
                { n: 8, label: 'Premier reporting mensuel au siège', cat: 'Mois 1' },
            ].map(s => `
                            <tr>
                                <td style="padding: 8px 0; vertical-align: top; width: 30px;">
                                    <span style="display:inline-block; width:22px; height:22px; border-radius:50%; background:#dcfce7; border:1px solid #86efac; text-align:center; line-height:22px; font-size:11px; color:#166534; font-weight:bold;">${s.n}</span>
                                </td>
                                <td style="padding: 8px 0; font-size: 13px; color: #334155;">${s.label}</td>
                                <td style="padding: 8px 0; font-size: 11px; color: #94a3b8; text-align: right;">${s.cat}</td>
                            </tr>
                        `).join('')}
                    </table>
                </div>

                <!-- Résumé Contrat -->
                <div style="background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px; margin: 20px 0;">
                    <h3 style="margin: 0 0 12px; color: #334155; font-size: 14px;">📄 Résumé de votre Contrat</h3>
                    <table style="width: 100%; border-collapse: collapse; font-size: 13px;">
                        <tr><td style="padding: 6px 0; color: #64748b; width: 160px;">Type</td><td style="color: #1e293b; font-weight: 500;">${agency?.type === 'CORNER' ? 'Corner / Borne' : 'Franchise Standard'}</td></tr>
                        <tr><td style="padding: 6px 0; color: #64748b;">Zone</td><td style="color: #1e293b; font-weight: 500;">${lead.targetCity} (${lead.region})</td></tr>
                        <tr><td style="padding: 6px 0; color: #64748b;">Commission</td><td style="color: #1e293b; font-weight: 500;">${agency?.commissionRate || 15}%</td></tr>
                        <tr><td style="padding: 6px 0; color: #64748b;">Agence ID</td><td style="color: #1e293b; font-weight: 500;">${agencyId}</td></tr>
                    </table>
                </div>

                <!-- Contact -->
                <div style="text-align: center; padding: 20px 0 0; border-top: 1px solid #e2e8f0;">
                    <p style="font-size: 13px; color: #475569; margin: 0;">Un référent franchise vous contactera sous 48h pour organiser votre kickoff.</p>
                    <p style="font-size: 12px; color: #94a3b8; margin: 8px 0 0;">SimuLegal HQ — 8 Rue de la Paix, 75002 Paris — franchise@simulegal.fr</p>
                </div>
            </div>

            <!-- Footer -->
            <div style="text-align: center; padding: 16px; font-size: 11px; color: #94a3b8;">
                © ${new Date().getFullYear()} SimuLegal — Ce message est confidentiel et destiné uniquement à son destinataire.
            </div>
        </div>`;

        const textBody = `Bienvenue chez SimuLegal !\n\n` +
            `Bonjour ${lead.name},\n\n` +
            `Votre agence ${agencyName} (ID: ${agencyId}) est active.\n\n` +
            `Identifiants :\n` +
            `- URL : ${adminUrl}\n` +
            `- Email : ${lead.email}\n` +
            `- Mot de passe temporaire : ${tempPassword}\n\n` +
            `Checklist d'ouverture :\n` +
            `1. Changer votre mot de passe\n` +
            `2. Compléter le profil agence\n` +
            `3. Suivre la formation en ligne\n` +
            `4. Commander les supports marketing\n` +
            `5. Configurer les services\n` +
            `6. Tester le parcours client\n` +
            `7. Organiser l'inauguration\n` +
            `8. Premier reporting mensuel\n\n` +
            `Un référent franchise vous contactera sous 48h.`;

        await Promise.all([
            this.sendEmail(
                lead.email,
                `🎉 Kit d'Ouverture SimuLegal — Bienvenue ${lead.name} !`,
                textBody,
                html
            ),
            this.sendWhatsApp(lead.phone, 'franchise_welcome', {
                name: lead.name,
                agencyName,
                agencyId,
                password: tempPassword,
                adminUrl
            }, { leadId: lead.id })
        ]);
    }
}
