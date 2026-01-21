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
exports.NotificationsController = NotificationsController = __decorate([
    (0, common_1.Controller)('notifications'),
    __metadata("design:paramtypes", [notifications_service_1.NotificationsService,
        config_1.ConfigService])
], NotificationsController);
//# sourceMappingURL=notifications.controller.js.map