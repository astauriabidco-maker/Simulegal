import { DashboardService } from './dashboard.service';
export declare class DashboardController {
    private readonly dashboardService;
    constructor(dashboardService: DashboardService);
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
