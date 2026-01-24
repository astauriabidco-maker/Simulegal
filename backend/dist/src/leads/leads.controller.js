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
exports.LeadsController = void 0;
const common_1 = require("@nestjs/common");
const leads_service_1 = require("./leads.service");
const invoices_service_1 = require("../invoices/invoices.service");
const passport_1 = require("@nestjs/passport");
let LeadsController = class LeadsController {
    leadsService;
    invoicesService;
    constructor(leadsService, invoicesService) {
        this.leadsService = leadsService;
        this.invoicesService = invoicesService;
    }
    findAll(req, agencyId) {
        if (req.user.role === 'AGENCY_MANAGER' || req.user.role === 'KIOSK_AGENT') {
            return this.leadsService.findByAgency(req.user.agencyId);
        }
        if (agencyId) {
            return this.leadsService.findByAgency(agencyId);
        }
        return this.leadsService.findAll();
    }
    remove(req, id) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Action réservée au siège');
        }
        return this.leadsService.delete(id);
    }
    recordPayment(id, data) {
        return this.leadsService.recordPayment(id, data);
    }
    async getInvoice(id) {
        const invoice = await this.invoicesService.getInvoiceData(id);
        if (!invoice)
            throw new Error('Invoice not found');
        return invoice;
    }
    async downloadPdf(id) {
        return this.invoicesService.generatePdf(id);
    }
    findOne(id) {
        return this.leadsService.findOne(id);
    }
    create(data) {
        return this.leadsService.create(data);
    }
    updateStatus(id, status) {
        return this.leadsService.updateStatus(id, status);
    }
    addNote(id, data) {
        return this.leadsService.addNote(id, data);
    }
    assignUser(id, userId) {
        return this.leadsService.assignUser(id, userId);
    }
    updateDocuments(id, documents) {
        return this.leadsService.updateDocuments(id, documents);
    }
};
exports.LeadsController = LeadsController;
__decorate([
    (0, common_1.Get)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Query)('agencyId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "findAll", null);
__decorate([
    (0, common_1.Delete)(':id'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "remove", null);
__decorate([
    (0, common_1.Post)(':id/payment'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "recordPayment", null);
__decorate([
    (0, common_1.Get)(':id/invoice'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Promise)
], LeadsController.prototype, "getInvoice", null);
__decorate([
    (0, common_1.Get)(':id/invoice/pdf'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Promise)
], LeadsController.prototype, "downloadPdf", null);
__decorate([
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "findOne", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "create", null);
__decorate([
    (0, common_1.Patch)(':id/status'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)('status')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "updateStatus", null);
__decorate([
    (0, common_1.Post)(':id/notes'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "addNote", null);
__decorate([
    (0, common_1.Patch)(':id/assign'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)('userId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "assignUser", null);
__decorate([
    (0, common_1.Patch)(':id/documents'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)('documents')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Array]),
    __metadata("design:returntype", void 0)
], LeadsController.prototype, "updateDocuments", null);
exports.LeadsController = LeadsController = __decorate([
    (0, common_1.Controller)('leads'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [leads_service_1.LeadsService,
        invoices_service_1.InvoicesService])
], LeadsController);
//# sourceMappingURL=leads.controller.js.map