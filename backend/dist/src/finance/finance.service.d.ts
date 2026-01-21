import { PrismaService } from '../prisma/prisma.service';
export declare class FinanceService {
    private prisma;
    constructor(prisma: PrismaService);
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
}
