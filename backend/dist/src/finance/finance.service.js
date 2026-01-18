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
Object.defineProperty(exports, "__esModule", { value: true });
exports.FinanceService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let FinanceService = class FinanceService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async getGlobalStats() {
        const leads = await this.prisma.lead.findMany({
            where: { status: 'PAID' },
            include: { originAgency: true }
        });
        const payouts = await this.prisma.payout.findMany();
        const totalGMV = leads.reduce((sum, lead) => sum + lead.amountPaid, 0);
        const totalCommissionsPaid = payouts.reduce((sum, p) => sum + p.amount, 0);
        const totalCommissionsEarned = leads.reduce((sum, lead) => {
            const rate = lead.originAgency?.commissionRate || 0;
            return sum + (lead.amountPaid * rate / 100);
        }, 0);
        const totalCommissionsPending = totalCommissionsEarned - totalCommissionsPaid;
        return {
            totalGMV,
            totalPartnerDebt: totalCommissionsPending,
            totalCommissionsPaid,
            netRevenue: totalGMV - totalCommissionsEarned
        };
    }
    async getAgencyBalance(agencyId) {
        const leads = await this.prisma.lead.findMany({
            where: { originAgencyId: agencyId, status: 'PAID' }
        });
        const agency = await this.prisma.agency.findUnique({ where: { id: agencyId } });
        const rate = agency?.commissionRate || 0;
        const totalEarned = leads.reduce((sum, lead) => sum + (lead.amountPaid * rate / 100), 0);
        const payouts = await this.prisma.payout.findMany({
            where: { agencyId }
        });
        const totalPaid = payouts.reduce((sum, p) => sum + p.amount, 0);
        return {
            balance: totalEarned - totalPaid,
            totalEarned,
            totalPaid
        };
    }
    async getAllPayouts() {
        return this.prisma.payout.findMany({
            include: { agency: true },
            orderBy: { createdAt: 'desc' }
        });
    }
    async createPayout(data) {
        return this.prisma.payout.create({
            data: {
                ...data,
                status: 'PAID',
                paidAt: new Date(),
                reference: `VIR-${Math.random().toString(36).substr(2, 9).toUpperCase()}`
            }
        });
    }
};
exports.FinanceService = FinanceService;
exports.FinanceService = FinanceService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], FinanceService);
//# sourceMappingURL=finance.service.js.map