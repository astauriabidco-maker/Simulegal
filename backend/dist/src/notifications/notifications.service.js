"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.NotificationsService = void 0;
const common_1 = require("@nestjs/common");
const config_1 = require("@nestjs/config");
const nodemailer = __importStar(require("nodemailer"));
const twilio_1 = __importDefault(require("twilio"));
const email_templates_service_1 = require("./email-templates.service");
const settings_service_1 = require("../settings/settings.service");
const prisma_service_1 = require("../prisma/prisma.service");
let NotificationsService = class NotificationsService {
    configService;
    emailTemplates;
    settingsService;
    prisma;
    twilioClient;
    cachedSmtpConfig = null;
    lastConfigCheck = 0;
    CONFIG_TTL = 60000;
    constructor(configService, emailTemplates, settingsService, prisma) {
        this.configService = configService;
        this.emailTemplates = emailTemplates;
        this.settingsService = settingsService;
        this.prisma = prisma;
        const accountSid = this.configService.get('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = (0, twilio_1.default)(accountSid, authToken);
        }
    }
    async getSmtpConfig() {
        const now = Date.now();
        if (this.cachedSmtpConfig && (now - this.lastConfigCheck) < this.CONFIG_TTL) {
            return this.cachedSmtpConfig;
        }
        try {
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
        }
        catch (error) {
            console.warn('[SMTP] Could not load settings from DB:', error);
        }
        const envHost = this.configService.get('SMTP_HOST');
        const envUser = this.configService.get('SMTP_USER');
        const envPass = this.configService.get('SMTP_PASS');
        if (envHost && envUser) {
            this.cachedSmtpConfig = {
                host: envHost,
                port: this.configService.get('SMTP_PORT') || 587,
                user: envUser,
                pass: envPass || ''
            };
            this.lastConfigCheck = now;
            console.log('[SMTP] 📧 Using environment configuration');
            return this.cachedSmtpConfig;
        }
        return null;
    }
    async createTransporter() {
        const config = await this.getSmtpConfig();
        if (!config)
            return null;
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
    async refreshSmtpConfig() {
        this.cachedSmtpConfig = null;
        this.lastConfigCheck = 0;
        console.log('[SMTP] 🔄 Configuration cache cleared');
    }
    async sendWhatsApp(phone, template, params, metadata) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized. Check env vars.');
            return { success: false };
        }
        const from = this.configService.get('TWILIO_WHATSAPP_NUMBER') || 'whatsapp:+14155238886';
        const to = `whatsapp:${phone.replace(/^0/, '+33').replace(/\s/g, '')}`;
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
        }
        catch (persistError) {
            console.warn('[WhatsApp] Persistence failed:', persistError.message);
        }
        try {
            const result = await this.twilioClient.messages.create({
                body: params.message,
                from: from,
                to: to
            });
            console.log(`[WhatsApp] 🟢 Sent to ${to}: ${result.sid}`);
            return { success: true, messageId: result.sid };
        }
        catch (error) {
            console.error('[WhatsApp] Error:', error);
            return { success: false, error };
        }
    }
    async sendEmail(to, subject, body, html) {
        const transporter = await this.createTransporter();
        if (!transporter) {
            console.log(`[Mailer] 📧 DEV MODE - Would send to ${to}:`);
            console.log(`  Subject: ${subject}`);
            console.log(`  Body preview: ${body.substring(0, 100)}...`);
            return { success: true, messageId: 'dev-mode-no-smtp' };
        }
        try {
            const info = await transporter.sendMail({
                from: '"SimuLegal" <no-reply@simulegal.fr>',
                to: to,
                subject: subject,
                text: body,
                html: html || body.replace(/\n/g, '<br>'),
            });
            console.log(`[Email] 📧 Sent to ${to}: ${info.messageId}`);
            return { success: true, messageId: info.messageId };
        }
        catch (error) {
            console.error('[Email] Error:', error);
            return { success: false, error };
        }
    }
    async sendWelcomeEmail(user, tempPassword) {
        const template = this.emailTemplates.renderWelcome(user.name, user.email, tempPassword);
        console.log(`[Transactional] 📧 Sending WELCOME email to ${user.email}`);
        return this.sendEmail(user.email, template.subject, `Bienvenue ${user.name}! Vos accès: ${user.email} / ${tempPassword}`, template.html);
    }
    async sendDiagnosticInvitation(prospect, magicLink) {
        const template = this.emailTemplates.renderDiagnosticInvitation(prospect.name, magicLink);
        console.log(`[Transactional] 📧 Sending DIAGNOSTIC INVITATION to ${prospect.email}`);
        return this.sendEmail(prospect.email, template.subject, `${prospect.name}, passez votre diagnostic: ${magicLink}`, template.html);
    }
    async sendAppointmentConfirmationEmail(lead, appointment) {
        const template = this.emailTemplates.renderAppointmentConfirmation(lead.name, new Date(appointment.start), appointment.type, appointment.meetingLink, appointment.agencyAddress);
        console.log(`[Transactional] 📧 Sending APPOINTMENT CONFIRMATION to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `RDV confirmé pour ${lead.name}`, template.html);
    }
    async sendPaymentConfirmation(lead, amount, refundCode) {
        const template = this.emailTemplates.renderPaymentConfirmation(lead.name, amount, refundCode);
        console.log(`[Transactional] 📧 Sending PAYMENT CONFIRMATION to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `Paiement de ${amount}€ reçu. Code: ${refundCode}`, template.html);
    }
    async sendAppointmentReminder(lead, appointment) {
        const template = this.emailTemplates.renderAppointmentReminder(lead.name, new Date(appointment.start), appointment.type, appointment.meetingLink);
        console.log(`[Transactional] 📧 Sending APPOINTMENT REMINDER to ${lead.email}`);
        return this.sendEmail(lead.email, template.subject, `Rappel: RDV demain`, template.html);
    }
    async sendSMS(phone, message) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized.');
            return { success: false };
        }
        const from = this.configService.get('TWILIO_CALLER_ID');
        const to = phone.replace(/^0/, '+33').replace(/\s/g, '');
        try {
            const result = await this.twilioClient.messages.create({
                body: message,
                from: from,
                to: to
            });
            console.log(`[SMS] 📱 Sent to ${to}: ${result.sid}`);
            return { success: true, messageId: result.sid };
        }
        catch (error) {
            console.error('[SMS] Error:', error);
            return { success: false, error };
        }
    }
    async sendAppointmentConfirmation(lead, appointment) {
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
        await Promise.all([
            this.sendWhatsApp(lead.phone || '0600000000', 'booking_confirmation', { message }, { leadId: lead.id }),
            lead.email ? this.sendEmail(lead.email, 'Confirmation de votre Rendez-vous SimuLegal', message) : Promise.resolve(),
            this.sendSMS(lead.phone || '0600000000', message)
        ]);
    }
    async onStageChange(lead, oldStage, newStage) {
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
    async onDocumentRejected(lead, docLabel) {
        console.log(`[Notifications] Document rejected for ${lead.name}: ${docLabel}`);
        await this.sendWhatsApp(lead.phone, 'document_rejected', {
            name: lead.name,
            message: `⚠️ Attention ${lead.name} : Votre document "${docLabel}" a été refusé par nos services. Merci de vous connecter à votre espace client pour le renvoyer.`
        }, { leadId: lead.id });
    }
    async onJuristAssigned(lead, juristName) {
        console.log(`[Notifications] Jurist assigned to ${lead.name}: ${juristName}`);
        await this.sendWhatsApp(lead.phone, 'jurist_assigned', {
            name: lead.name,
            message: `💼 Bonne nouvelle ${lead.name} : Votre dossier est maintenant pris en charge par ${juristName}. Vous pouvez suivre l'avancement dans votre espace.`
        }, { leadId: lead.id });
    }
    async onFranchiseOnboarding(lead, tempPassword) {
        const message = `Félicitations ${lead.name} ! Votre agence SimuLegal est maintenant active.\n\n` +
            `Vos accès :\n` +
            `Email : ${lead.email}\n` +
            `Mot de passe temporaire : ${tempPassword}\n\n` +
            `Connectez-vous sur : https://admin.simulegal.fr`;
        await Promise.all([
            this.sendEmail(lead.email, 'Bienvenue chez SimuLegal - Vos accès Gérant', message),
            this.sendWhatsApp(lead.phone, 'franchise_welcome', { message }, { leadId: lead.id })
        ]);
    }
};
exports.NotificationsService = NotificationsService;
exports.NotificationsService = NotificationsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [config_1.ConfigService,
        email_templates_service_1.EmailTemplatesService,
        settings_service_1.SettingsService,
        prisma_service_1.PrismaService])
], NotificationsService);
//# sourceMappingURL=notifications.service.js.map