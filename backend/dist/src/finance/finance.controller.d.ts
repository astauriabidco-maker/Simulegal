import { FinanceService } from './finance.service';
export declare class FinanceController {
    private readonly financeService;
    constructor(financeService: FinanceService);
    getStats(req: any): Promise<{
        totalGMV: number;
        totalPartnerDebt: number;
        totalCommissionsPaid: number;
        netRevenue: number;
    }>;
    getBalance(req: any, agencyId?: string): Promise<{
        balance: number;
        totalEarned: number;
        totalPaid: number;
    }>;
    getPayouts(req: any): Promise<({
        agency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        };
    } & {
        agencyId: string;
        id: string;
        createdAt: Date;
        status: string;
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
        agencyId: string;
        id: string;
        createdAt: Date;
        status: string;
        amount: number;
        period: string;
        paidAt: Date | null;
        reference: string;
    }>;
}
