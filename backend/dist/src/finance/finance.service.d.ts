import { PrismaService } from '../prisma/prisma.service';
import { SettingsService } from '../settings/settings.service';
export declare class FinanceService {
    private prisma;
    private settings;
    constructor(prisma: PrismaService, settings: SettingsService);
    getGlobalStats(): Promise<{
        totalGMV: any;
        totalPartnerDebt: number;
        totalCommissionsPaid: any;
        netRevenue: number;
    }>;
    private getCommissionRate;
    getAgencyBalance(agencyId: string): Promise<{
        balance: number;
        totalEarned: number;
        totalPaid: any;
    }>;
    getAllPayouts(): Promise<({
        agency: {
            id: string;
            name: string;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            serviceCommissionOverrides: string | null;
            contactEmail: string;
            iban: string | null;
            bic: string | null;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        };
    } & {
        id: string;
        status: string;
        createdAt: Date;
        agencyId: string;
        amount: number;
        period: string;
        paidAt: Date | null;
        reference: string;
    })[]>;
    createPayout(data: {
        agencyId: string;
        amount: number;
        period: string;
    }): Promise<{
        id: string;
        status: string;
        createdAt: Date;
        agencyId: string;
        amount: number;
        period: string;
        paidAt: Date | null;
        reference: string;
    }>;
    getMonthlySettlement(month: string, year: string): Promise<unknown[]>;
    getAgencyPerformanceTrends(agencyId: string): Promise<{
        period: string;
        gmv: any;
        commission: number;
        count: number;
    }[]>;
    getInvoices(): Promise<({
        originAgency: {
            id: string;
            name: string;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            serviceCommissionOverrides: string | null;
            contactEmail: string;
            iban: string | null;
            bic: string | null;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        } | null;
    } & {
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        data: string;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        paymentMethod: string | null;
        paymentDate: Date | null;
        paymentRef: string | null;
        invoiceNumber: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    })[]>;
    getTransactions(): Promise<({
        lead: {
            id: string;
            name: string;
            status: import(".prisma/client").$Enums.LeadStatus;
            createdAt: Date;
            updatedAt: Date;
            data: string;
            email: string;
            phone: string;
            serviceId: string;
            serviceName: string;
            amountPaid: number;
            paymentMethod: string | null;
            paymentDate: Date | null;
            paymentRef: string | null;
            invoiceNumber: string | null;
            contract: string | null;
            documents: string;
            requiredDocs: string | null;
            originAgencyId: string | null;
            assignedUserId: string | null;
        };
    } & {
        id: string;
        type: string;
        createdAt: Date;
        invoiceNumber: string | null;
        leadId: string;
        amount: number;
        reference: string | null;
        method: string | null;
    })[]>;
    getCreditNotes(): Promise<({
        lead: {
            id: string;
            name: string;
            status: import(".prisma/client").$Enums.LeadStatus;
            createdAt: Date;
            updatedAt: Date;
            data: string;
            email: string;
            phone: string;
            serviceId: string;
            serviceName: string;
            amountPaid: number;
            paymentMethod: string | null;
            paymentDate: Date | null;
            paymentRef: string | null;
            invoiceNumber: string | null;
            contract: string | null;
            documents: string;
            requiredDocs: string | null;
            originAgencyId: string | null;
            assignedUserId: string | null;
        };
    } & {
        number: string;
        id: string;
        createdAt: Date;
        leadId: string;
        amount: number;
        reason: string;
    })[]>;
    generatePayoutSepaXml(payoutId: string): Promise<string>;
    private escapeXml;
}
