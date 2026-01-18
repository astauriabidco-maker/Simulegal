import { PrismaService } from '../prisma/prisma.service';
export declare class FinanceService {
    private prisma;
    constructor(prisma: PrismaService);
    getGlobalStats(): Promise<{
        totalGMV: number;
        totalPartnerDebt: number;
        totalCommissionsPaid: number;
        netRevenue: number;
    }>;
    getAgencyBalance(agencyId: string): Promise<{
        balance: number;
        totalEarned: number;
        totalPaid: number;
    }>;
    getAllPayouts(): Promise<({
        agency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        };
    } & {
        id: string;
        agencyId: string;
        createdAt: Date;
        status: string;
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
        agencyId: string;
        createdAt: Date;
        status: string;
        amount: number;
        period: string;
        paidAt: Date | null;
        reference: string;
    }>;
}
