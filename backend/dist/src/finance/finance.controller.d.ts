import type { Response } from 'express';
import { FinanceService } from './finance.service';
export declare class FinanceController {
    private readonly financeService;
    constructor(financeService: FinanceService);
    getStats(req: any): Promise<{
        totalGMV: any;
        totalPartnerDebt: number;
        totalCommissionsPaid: any;
        netRevenue: number;
    }>;
    getBalance(req: any, agencyId?: string): Promise<{
        balance: number;
        totalEarned: number;
        totalPaid: any;
    }>;
    getPayouts(req: any): Promise<({
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
    createPayout(req: any, data: {
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
    getSettlements(req: any, month: string, year: string): Promise<unknown[]>;
    getPerformanceTrends(req: any, agencyId?: string): Promise<{
        period: string;
        gmv: any;
        commission: number;
        count: number;
    }[]>;
    getInvoices(req: any): Promise<({
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
    getTransactions(req: any): Promise<({
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
    getCreditNotes(req: any): Promise<({
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
    downloadSepa(req: any, id: string, res: Response): Promise<Response<any, Record<string, any>>>;
}
