"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.NotificationsController = void 0;
const common_1 = require("@nestjs/common");
const notifications_service_1 = require("./notifications.service");
const jwt_auth_guard_1 = require("../auth/jwt-auth.guard");
const config_1 = require("@nestjs/config");
let NotificationsController = class NotificationsController {
    notificationsService;
    configService;
    constructor(notificationsService, configService) {
        this.notificationsService = notificationsService;
        this.configService = configService;
    }
    async sendWhatsApp(data) {
        return this.notificationsService.sendWhatsApp(data.phone, data.template, data.params);
    }
    async sendSms(data) {
        return this.notificationsService.sendSMS(data.phone, data.message);
    }
    async sendProspectLink(data) {
        const baseUrl = this.configService.get('FRONTEND_URL') || 'https://app.simulegal.fr';
        const link = `${baseUrl}/simulation?ref=${data.prospectId}`;
        const message = `Bonjour ${data.prospectFirstName}, voici votre lien de simulation personnalisé SimuLegal :\n${link}\n\nCe lien est valable 48h.`;
        if (data.channel === 'WHATSAPP') {
            return this.notificationsService.sendWhatsApp(data.prospectPhone, 'simulation_link', { message });
        }
        else {
            return this.notificationsService.sendSMS(data.prospectPhone, message);
        }
    }
    async triggerStageChange(data) {
        return this.notificationsService.onStageChange(data.lead, data.oldStage, data.newStage);
    }
    async testEmail(data) {
        const testUser = { name: 'Jean Dupont', email: data.to };
        const testAppointment = {
            start: new Date(Date.now() + 24 * 60 * 60 * 1000),
            type: 'VISIO_JURISTE',
            meetingLink: 'https://meet.google.com/abc-defg-hij'
        };
        switch (data.template) {
            case 'welcome':
                return this.notificationsService.sendWelcomeEmail(testUser, 'TempPass123!');
            case 'diagnostic':
                return this.notificationsService.sendDiagnosticInvitation(testUser, 'https://simulegal.fr/diagnostic?token=abc123');
            case 'appointment':
                return this.notificationsService.sendAppointmentConfirmationEmail(testUser, testAppointment);
            case 'payment':
                return this.notificationsService.sendPaymentConfirmation(testUser, 9.90, 'REFUND-ABC123');
            case 'reminder':
                return this.notificationsService.sendAppointmentReminder(testUser, testAppointment);
            default:
                return { error: 'Invalid template. Use: welcome, diagnostic, appointment, payment, reminder' };
        }
    }
    async refreshSmtp() {
        await this.notificationsService.refreshSmtpConfig();
        return { success: true, message: 'SMTP configuration cache refreshed' };
    }
    async testSmtp(data) {
        const testEmail = data.to || 'test@example.com';
        const result = await this.notificationsService.sendEmail(testEmail, 'Test SMTP - SimuLegal', 'Ceci est un email de test pour vérifier la configuration SMTP.', '<h1>✅ Configuration SMTP fonctionnelle</h1><p>Cet email confirme que votre serveur SMTP est correctement configuré.</p>');
        return {
            success: result.success,
            messageId: result.messageId,
            message: result.success
                ? `Email de test envoyé à ${testEmail}`
                : 'Échec de l\'envoi (voir logs)'
        };
    }
};
exports.NotificationsController = NotificationsController;
__decorate([
    (0, common_1.Post)('send-whatsapp'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "sendWhatsApp", null);
__decorate([
    (0, common_1.Post)('send-sms'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "sendSms", null);
__decorate([
    (0, common_1.Post)('send-prospect-link'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "sendProspectLink", null);
__decorate([
    (0, common_1.Post)('trigger-stage-change'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "triggerStageChange", null);
__decorate([
    (0, common_1.Post)('test-email'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "testEmail", null);
__decorate([
    (0, common_1.Post)('refresh-smtp'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "refreshSmtp", null);
__decorate([
    (0, common_1.Post)('test-smtp'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], NotificationsController.prototype, "testSmtp", null);
exports.NotificationsController = NotificationsController = __decorate([
    (0, common_1.Controller)('notifications'),
    __metadata("design:paramtypes", [notifications_service_1.NotificationsService,
        config_1.ConfigService])
], NotificationsController);
//# sourceMappingURL=notifications.controller.js.map