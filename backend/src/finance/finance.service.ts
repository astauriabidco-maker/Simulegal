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

        const totalGMV = leads.reduce((sum, lead) => sum + lead.amountPaid, 0);
        const totalCommissionsPaid = payouts.reduce((sum, p) => sum + p.amount, 0);

        // Calcul simplifié pour démo : commission sur chaque lead payé
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

    async getAgencyBalance(agencyId: string) {
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
}
