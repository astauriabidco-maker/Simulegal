import { PrismaService } from '../prisma/prisma.service';
import { AssignmentService } from './assignment.service';
export declare class SalesService {
    private prisma;
    private assignmentService;
    constructor(prisma: PrismaService, assignmentService: AssignmentService);
    findAll(params: {
        page: number;
        limit: number;
        status?: string;
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
    }): Promise<{
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
    exportToCSV(filters?: {
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
    }): Promise<string>;
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
    private calculateScore;
    private triggerAutomation;
    addNote(prospectId: string, authorId: string, text: string): Promise<{
        id: string;
        createdAt: Date;
        text: string;
        prospectId: string;
        authorId: string;
    }>;
    importFromCSV(buffer: Buffer): Promise<number>;
}
