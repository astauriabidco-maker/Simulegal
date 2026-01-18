import { PrismaService } from '../prisma/prisma.service';
export declare class DashboardService {
    private prisma;
    constructor(prisma: PrismaService);
    getGlobalStats(): Promise<{
        overview: {
            totalLeads: number;
            totalRevenue: any;
            conversionRate: number;
            pendingDossiers: number;
            completedToday: number;
            fleetHealth: number;
            franchisePipeline: number;
        };
        network: {
            id: string;
            name: string;
            _count: {
                leads: number;
            };
            region: string;
            type: import(".prisma/client").$Enums.AgencyType;
            city: string;
        }[];
        serviceDistribution: {
            id: string;
            count: number;
        }[];
        franchiseStats: {
            new: number;
            meeting: number;
            signed: number;
        };
    }>;
}
