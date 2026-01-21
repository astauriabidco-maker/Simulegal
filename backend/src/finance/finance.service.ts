import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class FinanceService {
    constructor(private prisma: PrismaService) { }

    async getGlobalStats() {
        const leads = await this.prisma.lead.findMany({
            where: { status: 'PAID' },
            include: { originAgency: true }
        });

        const payouts = await this.prisma.payout.findMany();

        const totalGMV = leads.reduce((sum: number, lead: any) => sum + (lead.amountPaid || 0), 0);
        const totalCommissionsPaid = payouts.reduce((sum: number, p: any) => sum + (p.amount || 0), 0);

        const totalCommissionsEarned = leads.reduce((sum: number, lead: any) => {
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

    private getCommissionRate(agency: any, serviceId: string): number {
        if (!agency) return 0;
        let rate = agency.commissionRate || 15;

        if (agency.serviceCommissionOverrides) {
            try {
                const overrides = JSON.parse(agency.serviceCommissionOverrides);
                if (overrides[serviceId] !== undefined) {
                    rate = overrides[serviceId];
                }
            } catch (e) {
                // Ignore parsing errors
            }
        }
        return rate;
    }

    async getAgencyBalance(agencyId: string) {
        const leads = await this.prisma.lead.findMany({
            where: { originAgencyId: agencyId, status: 'PAID' }
        });

        const agency = await this.prisma.agency.findUnique({ where: { id: agencyId } });

        const totalEarned = leads.reduce((sum: number, lead: any) => {
            const rate = this.getCommissionRate(agency, lead.serviceId);
            return sum + (lead.amountPaid * rate / 100);
        }, 0);

        const payouts = await this.prisma.payout.findMany({
            where: { agencyId }
        });
        const totalPaid = payouts.reduce((sum: number, p: any) => sum + (p.amount || 0), 0);

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

    async createPayout(data: { agencyId: string, amount: number, period: string }) {
        return this.prisma.payout.create({
            data: {
                ...data,
                status: 'PAID',
                paidAt: new Date(),
                reference: `VIR-${Math.random().toString(36).substr(2, 9).toUpperCase()}`
            }
        });
    }

    async getMonthlySettlement(month: string, year: string) {
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

        const settlements: any = {};

        leads.forEach((lead: any) => {
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

    async getAgencyPerformanceTrends(agencyId: string) {
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

            const totalGMV = leads.reduce((sum: number, lead: any) => sum + (lead.amountPaid || 0), 0);
            const totalCommission = leads.reduce((sum: number, lead: any) => {
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
}
