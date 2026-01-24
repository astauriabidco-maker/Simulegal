import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as nodemailer from 'nodemailer';
import Twilio from 'twilio';
import { EmailTemplatesService } from './email-templates.service';
import { SettingsService } from '../settings/settings.service';
import { PrismaService } from '../prisma/prisma.service';

interface SmtpConfig {
    host: string;
    port: number;
    user: string;
    pass: string;
}

@Injectable()
export class NotificationsService {
    private twilioClient: Twilio.Twilio;
    private cachedSmtpConfig: SmtpConfig | null = null;
    private lastConfigCheck: number = 0;
    private readonly CONFIG_TTL = 60000; // Refresh config every 60 seconds

    constructor(
        private configService: ConfigService,
        private emailTemplates: EmailTemplatesService,
        private settingsService: SettingsService,
        private prisma: PrismaService
    ) {
        // Init Twilio (still from env vars for security)
        const accountSid = this.configService.get<string>('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get<string>('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = Twilio(accountSid, authToken);
        }
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
     * Envoi WhatsApp via Twilio
     */
    async sendWhatsApp(phone: string, template: string, params: any, metadata?: { leadId?: string, prospectId?: string }) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized. Check env vars.');
            return { success: false };
        }

        const from = this.configService.get<string>('TWILIO_WHATSAPP_NUMBER') || 'whatsapp:+14155238886'; // Default sandbox
        // Ensure phone is in E.164 and prefixed with whatsapp:
        const to = `whatsapp:${phone.replace(/^0/, '+33').replace(/\s/g, '')}`;

        // Persist the outgoing message (Attempt)
        try {
            await this.prisma.communication.create({
                data: {
                    direction: 'OUTBOUND',
                    type: 'WHATSAPP',
                    content: params.message || 'Template message',
                    sender: from,
                    senderName: 'SYSTEM',
                    leadId: metadata?.leadId,
                    prospectId: metadata?.prospectId
                }
            });
        } catch (persistError) {
            console.warn('[WhatsApp] Persistence failed:', persistError.message);
        }

        try {
            const result = await this.twilioClient.messages.create({
                body: params.message, // For sandbox, simpler to just send body. Templates are complex in Twilio.
                from: from,
                to: to
            });
            console.log(`[WhatsApp] 🟢 Sent to ${to}: ${result.sid}`);
            return { success: true, messageId: result.sid };
        } catch (error) {
            console.error('[WhatsApp] Error:', error);
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
     * Trigger lors du rejet d'un document
     */
    async onDocumentRejected(lead: any, docLabel: string) {
        console.log(`[Notifications] Document rejected for ${lead.name}: ${docLabel}`);
        await this.sendWhatsApp(lead.phone, 'document_rejected', {
            name: lead.name,
            message: `⚠️ Attention ${lead.name} : Votre document "${docLabel}" a été refusé par nos services. Merci de vous connecter à votre espace client pour le renvoyer.`
        }, { leadId: lead.id });
    }

    /**
     * Trigger lors de l'assignation d'un juriste
     */
    async onJuristAssigned(lead: any, juristName: string) {
        console.log(`[Notifications] Jurist assigned to ${lead.name}: ${juristName}`);
        await this.sendWhatsApp(lead.phone, 'jurist_assigned', {
            name: lead.name,
            message: `💼 Bonne nouvelle ${lead.name} : Votre dossier est maintenant pris en charge par ${juristName}. Vous pouvez suivre l'avancement dans votre espace.`
        }, { leadId: lead.id });
    }

    /**
     * Trigger lors de l'onboarding d'une nouvelle franchise
     */
    async onFranchiseOnboarding(lead: any, tempPassword: string) {
        const message = `Félicitations ${lead.name} ! Votre agence SimuLegal est maintenant active.\n\n` +
            `Vos accès :\n` +
            `Email : ${lead.email}\n` +
            `Mot de passe temporaire : ${tempPassword}\n\n` +
            `Connectez-vous sur : https://admin.simulegal.fr`;

        await Promise.all([
            this.sendEmail(lead.email, 'Bienvenue chez SimuLegal - Vos accès Gérant', message),
            this.sendWhatsApp(lead.phone, 'franchise_welcome', { message }, { leadId: lead.id }) // franchise leads might exist as leads
        ]);
    }
}
