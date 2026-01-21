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
            type: import(".prisma/client").$Enums.AgencyType;
            region: string;
            city: string;
            _count: {
                leads: number;
            };
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
