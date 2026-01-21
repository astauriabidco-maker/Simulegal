import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as nodemailer from 'nodemailer';
import Twilio from 'twilio';

@Injectable()
export class NotificationsService {
    private twilioClient: Twilio.Twilio;
    private mailTransporter: nodemailer.Transporter;

    constructor(private configService: ConfigService) {
        // Init Twilio
        const accountSid = this.configService.get<string>('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get<string>('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = Twilio(accountSid, authToken);
        }

        // Init Nodemailer
        const smtpHost = this.configService.get<string>('SMTP_HOST');
        const smtpPort = this.configService.get<number>('SMTP_PORT');
        const smtpUser = this.configService.get<string>('SMTP_USER');
        const smtpPass = this.configService.get<string>('SMTP_PASS');

        if (smtpHost && smtpUser) {
            this.mailTransporter = nodemailer.createTransport({
                host: smtpHost,
                port: smtpPort || 587,
                secure: false, // true for 465, false for other ports
                auth: {
                    user: smtpUser,
                    pass: smtpPass,
                },
            });
        }
    }

    /**
     * Envoi WhatsApp via Twilio
     */
    async sendWhatsApp(phone: string, template: string, params: any) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized. Check env vars.');
            return { success: false };
        }

        const from = this.configService.get<string>('TWILIO_WHATSAPP_NUMBER') || 'whatsapp:+14155238886'; // Default sandbox
        // Ensure phone is in E.164 and prefixed with whatsapp:
        const to = `whatsapp:${phone.replace(/^0/, '+33').replace(/\s/g, '')}`;

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

    async sendEmail(to: string, subject: string, body: string) {
        if (!this.mailTransporter) {
            console.warn('[Mailer] Transporter not initialized. Check env vars.');
            return { success: false };
        }

        try {
            const info = await this.mailTransporter.sendMail({
                from: '"SimuLegal Notif" <no-reply@simulegal.fr>', // sender address
                to: to, // list of receivers
                subject: subject, // Subject line
                text: body, // plain text body
                html: body.replace(/\n/g, '<br>'), // simple html conversion
            });
            console.log(`[Email] 📧 Sent to ${to}: ${info.messageId}`);
            return { success: true, messageId: info.messageId };
        } catch (error) {
            console.error('[Email] Error:', error);
            return { success: false, error };
        }
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
            this.sendWhatsApp(lead.phone || '0600000000', 'booking_confirmation', { message }),
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
            });
        }

        if (newStage === 'HUNTING') {
            await this.sendWhatsApp(lead.phone, 'hunting_start', {
                name: lead.name,
                message: `⚡️ Recherche activée. Nous surveillons les créneaux de RDV pour vous.`
            });
        }

        if (newStage === 'BOOKED') {
            await this.sendWhatsApp(lead.phone, 'booking_success', {
                name: lead.name,
                message: `✅ RDV RÉSERVÉ ! Détails disponibles dans votre espace client.`
            });
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
        });
    }

    /**
     * Trigger lors de l'assignation d'un juriste
     */
    async onJuristAssigned(lead: any, juristName: string) {
        console.log(`[Notifications] Jurist assigned to ${lead.name}: ${juristName}`);
        await this.sendWhatsApp(lead.phone, 'jurist_assigned', {
            name: lead.name,
            message: `💼 Bonne nouvelle ${lead.name} : Votre dossier est maintenant pris en charge par ${juristName}. Vous pouvez suivre l'avancement dans votre espace.`
        });
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
            this.sendWhatsApp(lead.phone, 'franchise_welcome', { message })
        ]);
    }
}
