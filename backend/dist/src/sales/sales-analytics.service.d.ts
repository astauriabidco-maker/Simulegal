import { PrismaService } from '../prisma/prisma.service';
export declare class SalesAnalyticsService {
    private prisma;
    constructor(prisma: PrismaService);
    getDashboardStats(period?: 'TODAY' | 'WEEK' | 'MONTH'): Promise<{
        period: "WEEK" | "TODAY" | "MONTH";
        kpis: {
            totalLeads: number;
            newLeads: number;
            convertedLeads: number;
            conversionRate: number;
            pipelineValue: number;
        };
        funnel: {
            status: import(".prisma/client").$Enums.ProspectStatus;
            count: number;
        }[];
        sources: {
            source: string;
            count: number;
        }[];
    }>;
    private estimatePipelineValue;
}
