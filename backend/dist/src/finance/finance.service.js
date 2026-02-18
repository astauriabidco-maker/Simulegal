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
const settings_service_1 = require("../settings/settings.service");
let FinanceService = class FinanceService {
    prisma;
    settings;
    constructor(prisma, settings) {
        this.prisma = prisma;
        this.settings = settings;
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
    async generatePayoutSepaXml(payoutId) {
        const payout = await this.prisma.payout.findUnique({
            where: { id: payoutId },
            include: { agency: true }
        });
        if (!payout)
            throw new Error('Payout non trouvé');
        if (!payout.agency?.iban)
            throw new Error('IBAN manquant pour cette agence');
        const sysSettings = await this.settings.getSettings();
        const company = sysSettings.company;
        const debtorIban = sysSettings.banking?.iban || 'FR7600000000000000000000000';
        const debtorBic = sysSettings.banking?.bic || 'XXXXXXXXXXX';
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
    escapeXml(unsafe) {
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
};
exports.FinanceService = FinanceService;
exports.FinanceService = FinanceService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        settings_service_1.SettingsService])
], FinanceService);
//# sourceMappingURL=finance.service.js.map