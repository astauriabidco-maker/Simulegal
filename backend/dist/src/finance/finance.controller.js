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
exports.FinanceController = void 0;
const common_1 = require("@nestjs/common");
const finance_service_1 = require("./finance.service");
const passport_1 = require("@nestjs/passport");
let FinanceController = class FinanceController {
    financeService;
    constructor(financeService) {
        this.financeService = financeService;
    }
    getStats(req) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getGlobalStats();
    }
    getBalance(req, agencyId) {
        const id = agencyId || req.user.agencyId;
        if (!id)
            throw new common_1.ForbiddenException('ID Agence manquant');
        if (req.user.role === 'AGENCY_MANAGER' && id !== req.user.agencyId) {
            throw new common_1.ForbiddenException('Accès refusé');
        }
        return this.financeService.getAgencyBalance(id);
    }
    getPayouts(req) {
        if (req.user.role === 'AGENCY_MANAGER') {
        }
        return this.financeService.getAllPayouts();
    }
    createPayout(req, data) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Action réservée au siège');
        }
        return this.financeService.createPayout(data);
    }
    getSettlements(req, month, year) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getMonthlySettlement(month, year);
    }
    getPerformanceTrends(req, agencyId) {
        const id = agencyId || req.user.agencyId;
        if (!id)
            throw new common_1.ForbiddenException('ID Agence manquant');
        if (req.user.role === 'AGENCY_MANAGER' && id !== req.user.agencyId) {
            throw new common_1.ForbiddenException('Accès refusé');
        }
        return this.financeService.getAgencyPerformanceTrends(id);
    }
    getInvoices(req) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getInvoices();
    }
    getTransactions(req) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getTransactions();
    }
    getCreditNotes(req) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new common_1.ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getCreditNotes();
    }
};
exports.FinanceController = FinanceController;
__decorate([
    (0, common_1.Get)('stats'),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getStats", null);
__decorate([
    (0, common_1.Get)('balance'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Query)('agencyId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getBalance", null);
__decorate([
    (0, common_1.Get)('payouts'),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getPayouts", null);
__decorate([
    (0, common_1.Post)('payouts'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "createPayout", null);
__decorate([
    (0, common_1.Get)('settlements'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Query)('month')),
    __param(2, (0, common_1.Query)('year')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String, String]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getSettlements", null);
__decorate([
    (0, common_1.Get)('performance-trends'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Query)('agencyId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getPerformanceTrends", null);
__decorate([
    (0, common_1.Get)('invoices'),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getInvoices", null);
__decorate([
    (0, common_1.Get)('transactions'),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getTransactions", null);
__decorate([
    (0, common_1.Get)('credit-notes'),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FinanceController.prototype, "getCreditNotes", null);
exports.FinanceController = FinanceController = __decorate([
    (0, common_1.Controller)('finance'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [finance_service_1.FinanceService])
], FinanceController);
//# sourceMappingURL=finance.controller.js.map