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
exports.PublicLeadsController = void 0;
const common_1 = require("@nestjs/common");
const leads_service_1 = require("./leads.service");
const email_service_1 = require("../email/email.service");
let PublicLeadsController = class PublicLeadsController {
    leadsService;
    emailService;
    constructor(leadsService, emailService) {
        this.leadsService = leadsService;
        this.emailService = emailService;
    }
    async create(leadData) {
        if (!leadData.name || (!leadData.email && !leadData.phone)) {
            throw new common_1.BadRequestException('Contact information missing');
        }
        const secureData = {
            ...leadData,
            status: 'NEW',
            originAgencyId: leadData.forceAgencyId || undefined,
            id: leadData.id || undefined
        };
        return this.leadsService.create(secureData);
    }
    async createCheckoutSession(body) {
        const { leadId, amount, label, successUrl, cancelUrl } = body;
        const lead = await this.leadsService.findOne(leadId);
        if (!lead)
            throw new common_1.BadRequestException('Lead not found');
        await new Promise(resolve => setTimeout(resolve, 800));
        const mockSessionId = `cs_test_${leadId}_${Date.now()}`;
        if (lead.email) {
            await this.emailService.sendOrderConfirmation(lead.email, lead.name, label, amount, mockSessionId);
            await this.emailService.sendMandateCopy(lead.email, lead.name);
        }
        else {
            console.warn(`[Checkout] Lead ${leadId} has no email, skipping confirmation email.`);
        }
        return {
            url: `${successUrl}?session_id=${mockSessionId}&mock_payment=true`,
            sessionId: mockSessionId
        };
    }
};
exports.PublicLeadsController = PublicLeadsController;
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], PublicLeadsController.prototype, "create", null);
__decorate([
    (0, common_1.Post)('checkout'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], PublicLeadsController.prototype, "createCheckoutSession", null);
exports.PublicLeadsController = PublicLeadsController = __decorate([
    (0, common_1.Controller)('public/leads'),
    __metadata("design:paramtypes", [leads_service_1.LeadsService,
        email_service_1.EmailService])
], PublicLeadsController);
//# sourceMappingURL=leads.public.controller.js.map