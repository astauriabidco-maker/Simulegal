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
}
