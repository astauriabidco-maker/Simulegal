import { SalesService } from './sales.service';
import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';
export declare class SalesController {
    private readonly salesService;
    private readonly analyticsService;
    private readonly assignmentService;
    constructor(salesService: SalesService, analyticsService: SalesAnalyticsService, assignmentService: AssignmentService);
    getAnalytics(period?: 'TODAY' | 'WEEK' | 'MONTH'): Promise<{
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
    findAll(page?: number, limit?: number, status?: string, agencyId?: string, source?: string, dateFrom?: string, dateTo?: string): Promise<{
        data: ({
            notes: {
                id: string;
                createdAt: Date;
                text: string;
                prospectId: string;
                authorId: string;
            }[];
        } & {
            id: string;
            status: import(".prisma/client").$Enums.ProspectStatus;
            createdAt: Date;
            updatedAt: Date;
            agencyId: string;
            email: string | null;
            phone: string;
            source: string;
            firstName: string;
            lastName: string;
            campaignName: string | null;
            interestServiceId: string | null;
            score: number;
            assignedToSalesId: string | null;
            lastContactAt: Date | null;
        })[];
        meta: {
            total: number;
            page: number;
            limit: number;
            totalPages: number;
        };
    }>;
    exportProspects(agencyId?: string, source?: string, dateFrom?: string, dateTo?: string): Promise<{
        filename: string;
        content: string;
        contentType: string;
    }>;
    findOne(id: string): Promise<({
        notes: {
            id: string;
            createdAt: Date;
            text: string;
            prospectId: string;
            authorId: string;
        }[];
    } & {
        id: string;
        status: import(".prisma/client").$Enums.ProspectStatus;
        createdAt: Date;
        updatedAt: Date;
        agencyId: string;
        email: string | null;
        phone: string;
        source: string;
        firstName: string;
        lastName: string;
        campaignName: string | null;
        interestServiceId: string | null;
        score: number;
        assignedToSalesId: string | null;
        lastContactAt: Date | null;
    }) | null>;
    create(data: any): Promise<{
        id: string;
        status: import(".prisma/client").$Enums.ProspectStatus;
        createdAt: Date;
        updatedAt: Date;
        agencyId: string;
        email: string | null;
        phone: string;
        source: string;
        firstName: string;
        lastName: string;
        campaignName: string | null;
        interestServiceId: string | null;
        score: number;
        assignedToSalesId: string | null;
        lastContactAt: Date | null;
    }>;
    update(id: string, data: any): Promise<{
        id: string;
        status: import(".prisma/client").$Enums.ProspectStatus;
        createdAt: Date;
        updatedAt: Date;
        agencyId: string;
        email: string | null;
        phone: string;
        source: string;
        firstName: string;
        lastName: string;
        campaignName: string | null;
        interestServiceId: string | null;
        score: number;
        assignedToSalesId: string | null;
        lastContactAt: Date | null;
    } | null>;
    addNote(id: string, req: any, data: {
        text: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        text: string;
        prospectId: string;
        authorId: string;
    }>;
    reassignProspect(id: string, data: {
        salesUserId?: string;
    }): Promise<{
        success: boolean;
        error: string;
        assignedTo?: undefined;
        prospect?: undefined;
    } | {
        success: boolean;
        assignedTo: string;
        prospect: {
            id: string;
            status: import(".prisma/client").$Enums.ProspectStatus;
            createdAt: Date;
            updatedAt: Date;
            agencyId: string;
            email: string | null;
            phone: string;
            source: string;
            firstName: string;
            lastName: string;
            campaignName: string | null;
            interestServiceId: string | null;
            score: number;
            assignedToSalesId: string | null;
            lastContactAt: Date | null;
        } | null;
        error?: undefined;
    }>;
    importProspects(file: any): Promise<number>;
}
