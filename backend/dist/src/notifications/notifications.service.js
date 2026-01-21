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
let NotificationsService = class NotificationsService {
    configService;
    twilioClient;
    mailTransporter;
    constructor(configService) {
        this.configService = configService;
        const accountSid = this.configService.get('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = (0, twilio_1.default)(accountSid, authToken);
        }
        const smtpHost = this.configService.get('SMTP_HOST');
        const smtpPort = this.configService.get('SMTP_PORT');
        const smtpUser = this.configService.get('SMTP_USER');
        const smtpPass = this.configService.get('SMTP_PASS');
        if (smtpHost && smtpUser) {
            this.mailTransporter = nodemailer.createTransport({
                host: smtpHost,
                port: smtpPort || 587,
                secure: false,
                auth: {
                    user: smtpUser,
                    pass: smtpPass,
                },
            });
        }
    }
    async sendWhatsApp(phone, template, params) {
        if (!this.twilioClient) {
            console.warn('[Twilio] Client not initialized. Check env vars.');
            return { success: false };
        }
        const from = this.configService.get('TWILIO_WHATSAPP_NUMBER') || 'whatsapp:+14155238886';
        const to = `whatsapp:${phone.replace(/^0/, '+33').replace(/\s/g, '')}`;
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
    async sendEmail(to, subject, body) {
        if (!this.mailTransporter) {
            console.warn('[Mailer] Transporter not initialized. Check env vars.');
            return { success: false };
        }
        try {
            const info = await this.mailTransporter.sendMail({
                from: '"SimuLegal Notif" <no-reply@simulegal.fr>',
                to: to,
                subject: subject,
                text: body,
                html: body.replace(/\n/g, '<br>'),
            });
            console.log(`[Email] 📧 Sent to ${to}: ${info.messageId}`);
            return { success: true, messageId: info.messageId };
        }
        catch (error) {
            console.error('[Email] Error:', error);
            return { success: false, error };
        }
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
            this.sendWhatsApp(lead.phone || '0600000000', 'booking_confirmation', { message }),
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
    async onDocumentRejected(lead, docLabel) {
        console.log(`[Notifications] Document rejected for ${lead.name}: ${docLabel}`);
        await this.sendWhatsApp(lead.phone, 'document_rejected', {
            name: lead.name,
            message: `⚠️ Attention ${lead.name} : Votre document "${docLabel}" a été refusé par nos services. Merci de vous connecter à votre espace client pour le renvoyer.`
        });
    }
    async onJuristAssigned(lead, juristName) {
        console.log(`[Notifications] Jurist assigned to ${lead.name}: ${juristName}`);
        await this.sendWhatsApp(lead.phone, 'jurist_assigned', {
            name: lead.name,
            message: `💼 Bonne nouvelle ${lead.name} : Votre dossier est maintenant pris en charge par ${juristName}. Vous pouvez suivre l'avancement dans votre espace.`
        });
    }
    async onFranchiseOnboarding(lead, tempPassword) {
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
};
exports.NotificationsService = NotificationsService;
exports.NotificationsService = NotificationsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [config_1.ConfigService])
], NotificationsService);
//# sourceMappingURL=notifications.service.js.map