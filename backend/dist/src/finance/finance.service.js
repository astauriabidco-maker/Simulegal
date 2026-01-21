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
        const totalGMV = leads.reduce((sum, lead) => sum + (lead.amountPaid || 0), 0);
        const totalCommissionsPaid = payouts.reduce((sum, p) => sum + (p.amount || 0), 0);
        const totalCommissionsEarned = leads.reduce((sum, lead) => {
            const rate = this.getCommissionRate(lead.originAgency, lead.serviceId);
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
    getCommissionRate(agency, serviceId) {
        if (!agency)
            return 0;
        let rate = agency.commissionRate || 15;
        if (agency.serviceCommissionOverrides) {
            try {
                const overrides = JSON.parse(agency.serviceCommissionOverrides);
                if (overrides[serviceId] !== undefined) {
                    rate = overrides[serviceId];
                }
            }
            catch (e) {
            }
        }
        return rate;
    }
    async getAgencyBalance(agencyId) {
        const leads = await this.prisma.lead.findMany({
            where: { originAgencyId: agencyId, status: 'PAID' }
        });
        const agency = await this.prisma.agency.findUnique({ where: { id: agencyId } });
        const totalEarned = leads.reduce((sum, lead) => {
            const rate = this.getCommissionRate(agency, lead.serviceId);
            return sum + (lead.amountPaid * rate / 100);
        }, 0);
        const payouts = await this.prisma.payout.findMany({
            where: { agencyId }
        });
        const totalPaid = payouts.reduce((sum, p) => sum + (p.amount || 0), 0);
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
    async getMonthlySettlement(month, year) {
        const startDate = new Date(parseInt(year), parseInt(month) - 1, 1);
        const endDate = new Date(parseInt(year), parseInt(month), 0, 23, 59, 59);
        const leads = await this.prisma.lead.findMany({
            where: {
                status: 'PAID',
                updatedAt: {
                    gte: startDate,
                    lte: endDate
                }
            },
            include: { originAgency: true }
        });
        const settlements = {};
        leads.forEach((lead) => {
            const agencyId = lead.originAgencyId || 'HQ';
            if (!settlements[agencyId]) {
                settlements[agencyId] = {
                    agencyId,
                    agencyName: lead.originAgency?.name || 'Vente Directe / HQ',
                    totalGMV: 0,
                    commissionDue: 0,
                    leadCount: 0,
                    details: []
                };
            }
            const rate = this.getCommissionRate(lead.originAgency, lead.serviceId);
            const commission = lead.amountPaid * rate / 100;
            settlements[agencyId].totalGMV += lead.amountPaid;
            settlements[agencyId].commissionDue += commission;
            settlements[agencyId].leadCount += 1;
            settlements[agencyId].details.push({
                leadId: lead.id,
                leadName: lead.name,
                service: lead.serviceName,
                amount: lead.amountPaid,
                rate,
                commission
            });
        });
        return Object.values(settlements);
    }
    async getAgencyPerformanceTrends(agencyId) {
        const last6Months = [];
        for (let i = 5; i >= 0; i--) {
            const d = new Date();
            d.setMonth(d.getMonth() - i);
            last6Months.push({
                month: d.getMonth() + 1,
                year: d.getFullYear(),
                label: d.toLocaleString('fr-FR', { month: 'short' })
            });
        }
        const trends = await Promise.all(last6Months.map(async (period) => {
            const startDate = new Date(period.year, period.month - 1, 1);
            const endDate = new Date(period.year, period.month, 0, 23, 59, 59);
            const leads = await this.prisma.lead.findMany({
                where: {
                    originAgencyId: agencyId === 'HQ' ? null : agencyId,
                    status: 'PAID',
                    updatedAt: {
                        gte: startDate,
                        lte: endDate
                    }
                },
                include: { originAgency: true }
            });
            const agency = agencyId === 'HQ' ? null : await this.prisma.agency.findUnique({ where: { id: agencyId } });
            const totalGMV = leads.reduce((sum, lead) => sum + (lead.amountPaid || 0), 0);
            const totalCommission = leads.reduce((sum, lead) => {
                const rate = this.getCommissionRate(lead.originAgency || agency, lead.serviceId);
                return sum + (lead.amountPaid * rate / 100);
            }, 0);
            return {
                period: period.label,
                gmv: totalGMV,
                commission: totalCommission,
                count: leads.length
            };
        }));
        return trends;
    }
};
exports.FinanceService = FinanceService;
exports.FinanceService = FinanceService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], FinanceService);
//# sourceMappingURL=finance.service.js.map