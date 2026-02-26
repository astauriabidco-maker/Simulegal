import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { SettingsService } from '../settings/settings.service';

@Injectable()
export class FinanceService {
    constructor(
        private prisma: PrismaService,
        private settings: SettingsService
    ) { }

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

    async getInvoices() {
        return this.prisma.lead.findMany({
            where: { invoiceNumber: { not: null } },
            include: { originAgency: true },
            orderBy: { paymentDate: 'desc' }
        });
    }

    async getTransactions() {
        return this.prisma.transaction.findMany({
            include: { lead: true },
            orderBy: { createdAt: 'desc' }
        });
    }

    async getCreditNotes() {
        return this.prisma.creditNote.findMany({
            include: { lead: true },
            orderBy: { createdAt: 'desc' }
        });
    }

    // ════════════════════════════════════════
    // FINANCIAL SUMMARY — KPIs enrichis
    // ════════════════════════════════════════

    async getFinancialSummary() {
        const now = new Date();
        const thisMonthStart = new Date(now.getFullYear(), now.getMonth(), 1);
        const lastMonthStart = new Date(now.getFullYear(), now.getMonth() - 1, 1);
        const lastMonthEnd = new Date(now.getFullYear(), now.getMonth(), 0, 23, 59, 59);

        // Current month leads
        const thisMonthLeads = await this.prisma.lead.findMany({
            where: { status: 'PAID', updatedAt: { gte: thisMonthStart } },
            include: { originAgency: true }
        });
        const lastMonthLeads = await this.prisma.lead.findMany({
            where: { status: 'PAID', updatedAt: { gte: lastMonthStart, lte: lastMonthEnd } },
            include: { originAgency: true }
        });

        const allPaidLeads = await this.prisma.lead.findMany({
            where: { status: 'PAID' }, include: { originAgency: true }
        });
        const allPayouts = await this.prisma.payout.findMany();

        const totalGMV = allPaidLeads.reduce((s, l) => s + (l.amountPaid || 0), 0);
        const thisMonthGMV = thisMonthLeads.reduce((s, l) => s + (l.amountPaid || 0), 0);
        const lastMonthGMV = lastMonthLeads.reduce((s, l) => s + (l.amountPaid || 0), 0);
        const momGrowth = lastMonthGMV > 0 ? Math.round(((thisMonthGMV - lastMonthGMV) / lastMonthGMV) * 100) : 0;

        const totalCommissionsEarned = allPaidLeads.reduce((s, l) => {
            return s + (l.amountPaid * this.getCommissionRate(l.originAgency, l.serviceId) / 100);
        }, 0);
        const totalPaid = allPayouts.reduce((s, p) => s + (p.amount || 0), 0);

        const marginPct = totalGMV > 0 ? Math.round(((totalGMV - totalCommissionsEarned) / totalGMV) * 100) : 0;

        // Top agency this month
        const agencyMap: Record<string, { name: string; gmv: number }> = {};
        thisMonthLeads.forEach(l => {
            const key = l.originAgencyId || 'HQ';
            if (!agencyMap[key]) agencyMap[key] = { name: l.originAgency?.name || 'Vente Directe', gmv: 0 };
            agencyMap[key].gmv += l.amountPaid;
        });
        const topAgency = Object.values(agencyMap).sort((a, b) => b.gmv - a.gmv)[0] || null;

        // Credit notes sum
        const creditNotes = await this.prisma.creditNote.findMany();
        const totalRefunds = creditNotes.reduce((s, c) => s + c.amount, 0);

        return {
            totalGMV, thisMonthGMV, lastMonthGMV, momGrowth,
            totalCommissionsEarned: Math.round(totalCommissionsEarned),
            totalCommissionsPaid: totalPaid,
            totalCommissionsPending: Math.round(totalCommissionsEarned - totalPaid),
            netRevenue: Math.round(totalGMV - totalCommissionsEarned),
            marginPct,
            totalRefunds,
            totalDeals: allPaidLeads.length,
            thisMonthDeals: thisMonthLeads.length,
            avgDealValue: allPaidLeads.length > 0 ? Math.round(totalGMV / allPaidLeads.length) : 0,
            topAgency,
        };
    }

    // ════════════════════════════════════════
    // REVENUE BREAKDOWN — Ventilation CA
    // ════════════════════════════════════════

    async getRevenueBreakdown() {
        const leads = await this.prisma.lead.findMany({
            where: { status: 'PAID' },
            include: { originAgency: true }
        });

        // By agency type
        const byType: Record<string, number> = {};
        // By service
        const byService: Record<string, number> = {};
        // By region
        const byRegion: Record<string, number> = {};

        for (const l of leads) {
            const type = l.originAgency?.type || 'DIRECT';
            byType[type] = (byType[type] || 0) + l.amountPaid;
            byService[l.serviceName] = (byService[l.serviceName] || 0) + l.amountPaid;
            const region = l.originAgency?.region || 'HQ';
            byRegion[region] = (byRegion[region] || 0) + l.amountPaid;
        }

        // Monthly (last 12 months)
        const monthly: { month: string; gmv: number; commission: number; net: number; count: number }[] = [];
        for (let i = 11; i >= 0; i--) {
            const d = new Date();
            d.setMonth(d.getMonth() - i);
            const start = new Date(d.getFullYear(), d.getMonth(), 1);
            const end = new Date(d.getFullYear(), d.getMonth() + 1, 0, 23, 59, 59);
            const label = d.toLocaleDateString('fr-FR', { month: 'short', year: '2-digit' });
            const mLeads = leads.filter(l => { const u = new Date(l.updatedAt); return u >= start && u <= end; });
            const gmv = mLeads.reduce((s, l) => s + l.amountPaid, 0);
            const commission = mLeads.reduce((s, l) => s + l.amountPaid * this.getCommissionRate(l.originAgency, l.serviceId) / 100, 0);
            monthly.push({ month: label, gmv, commission: Math.round(commission), net: Math.round(gmv - commission), count: mLeads.length });
        }

        return { byType, byService, byRegion, monthly };
    }

    // ════════════════════════════════════════
    // CASH FLOW PROJECTION — 3 months forecast
    // ════════════════════════════════════════

    async getCashFlowProjection() {
        const leads = await this.prisma.lead.findMany({
            where: { status: 'PAID' },
            include: { originAgency: true }
        });
        const payouts = await this.prisma.payout.findMany();

        // Compute last 3 months average
        const monthlyGMVs: number[] = [];
        const monthlyCommissions: number[] = [];
        for (let i = 2; i >= 0; i--) {
            const d = new Date();
            d.setMonth(d.getMonth() - i);
            const start = new Date(d.getFullYear(), d.getMonth(), 1);
            const end = new Date(d.getFullYear(), d.getMonth() + 1, 0, 23, 59, 59);
            const mLeads = leads.filter(l => { const u = new Date(l.updatedAt); return u >= start && u <= end; });
            monthlyGMVs.push(mLeads.reduce((s, l) => s + l.amountPaid, 0));
            monthlyCommissions.push(mLeads.reduce((s, l) => s + l.amountPaid * this.getCommissionRate(l.originAgency, l.serviceId) / 100, 0));
        }

        const avgGMV = monthlyGMVs.length > 0 ? Math.round(monthlyGMVs.reduce((a, b) => a + b, 0) / monthlyGMVs.length) : 0;
        const avgCommission = monthlyCommissions.length > 0 ? Math.round(monthlyCommissions.reduce((a, b) => a + b, 0) / monthlyCommissions.length) : 0;
        const avgNet = avgGMV - avgCommission;

        // Projection for next 3 months
        const projection: { month: string; estGMV: number; estCommission: number; estNet: number }[] = [];
        for (let i = 1; i <= 3; i++) {
            const d = new Date();
            d.setMonth(d.getMonth() + i);
            const growthFactor = 1 + (i * 0.02); // 2% monthly growth assumption
            projection.push({
                month: d.toLocaleDateString('fr-FR', { month: 'long', year: 'numeric' }),
                estGMV: Math.round(avgGMV * growthFactor),
                estCommission: Math.round(avgCommission * growthFactor),
                estNet: Math.round(avgNet * growthFactor),
            });
        }

        // Current outstanding debt
        const totalGMV = leads.reduce((s, l) => s + l.amountPaid, 0);
        const totalCommissions = leads.reduce((s, l) => s + l.amountPaid * this.getCommissionRate(l.originAgency, l.serviceId) / 100, 0);
        const totalPaid = payouts.reduce((s, p) => s + (p.amount || 0), 0);

        return {
            outstandingDebt: Math.round(totalCommissions - totalPaid),
            avgMonthlyGMV: avgGMV,
            avgMonthlyNet: avgNet,
            projection,
        };
    }

    /**
     * Generate SEPA XML for a single payout (ISO 20022 PAIN.001.001.03)
     */
    async generatePayoutSepaXml(payoutId: string) {
        const payout = await this.prisma.payout.findUnique({
            where: { id: payoutId },
            include: { agency: true }
        });

        if (!payout) throw new Error('Payout non trouvé');
        if (!payout.agency?.iban) throw new Error('IBAN manquant pour cette agence');

        const sysSettings = await this.settings.getSettings();
        const company = sysSettings.company;

        // Use HQ banking info from settings if available, else default placeholders
        const debtorIban = (sysSettings as any).banking?.iban || 'FR7600000000000000000000000';
        const debtorBic = (sysSettings as any).banking?.bic || 'XXXXXXXXXXX';

        const msgId = `MSG-${payout.reference}`;
        const pmtId = `PMT-${payout.id}`;
        const now = new Date().toISOString();
        const executionDate = new Date().toISOString().split('T')[0];

        const xml = `<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <CstmrCdtTrfInitn>
    <GrpHdr>
      <MsgId>${msgId}</MsgId>
      <CreDtTm>${now}</CreDtTm>
      <NbOfTxs>1</NbOfTxs>
      <CtrlSum>${payout.amount.toFixed(2)}</CtrlSum>
      <InitgPty>
        <Nm>${this.escapeXml(company.name)}</Nm>
      </InitgPty>
    </GrpHdr>
    <PmtInf>
      <PmtInfId>${pmtId}</PmtInfId>
      <PmtMtd>TRF</PmtMtd>
      <NbOfTxs>1</NbOfTxs>
      <CtrlSum>${payout.amount.toFixed(2)}</CtrlSum>
      <PmtTpInf>
        <SvcLvl>
          <Cd>SEPA</Cd>
        </SvcLvl>
      </PmtTpInf>
      <ReqdExctnDt>${executionDate}</ReqdExctnDt>
      <Dbtr>
        <Nm>${this.escapeXml(company.name)}</Nm>
      </Dbtr>
      <DbtrAcct>
        <Id>
          <IBAN>${debtorIban}</IBAN>
        </Id>
      </DbtrAcct>
      <DbtrAgt>
        <FinInstnId>
          <BIC>${debtorBic}</BIC>
        </FinInstnId>
      </DbtrAgt>
      <ChrgBr>SLEV</ChrgBr>
      <CdtTrfTxInf>
        <PmtId>
          <EndToEndId>${payout.reference}</EndToEndId>
        </PmtId>
        <Amt>
          <InstdAmt Ccy="EUR">${payout.amount.toFixed(2)}</InstdAmt>
        </Amt>
        <CdtrAgt>
          <FinInstnId>
            <BIC>${payout.agency.bic || 'XXXXXXXXXXX'}</BIC>
          </FinInstnId>
        </CdtrAgt>
        <Cdtr>
          <Nm>${this.escapeXml(payout.agency.name)}</Nm>
        </Cdtr>
        <CdtrAcct>
          <Id>
            <IBAN>${payout.agency.iban}</IBAN>
          </Id>
        </CdtrAcct>
        <RmtInf>
          <Ustrd>COMMISSION SIMULEGAL - ${payout.period}</Ustrd>
        </RmtInf>
      </CdtTrfTxInf>
    </PmtInf>
  </CstmrCdtTrfInitn>
</Document>`;

        return xml;
    }

    private escapeXml(unsafe: string) {
        return unsafe.replace(/[<>&'"]/g, (c) => {
            switch (c) {
                case '<': return '&lt;';
                case '>': return '&gt;';
                case '&': return '&amp;';
                case '\'': return '&apos;';
                case '"': return '&quot;';
            }
            return c;
        });
    }
}
