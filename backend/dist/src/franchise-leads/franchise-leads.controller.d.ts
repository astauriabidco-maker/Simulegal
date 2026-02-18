import type { Response } from 'express';
import { FranchiseLeadsService } from './franchise-leads.service';
export declare class FranchiseLeadsController {
    private readonly franchiseLeadsService;
    constructor(franchiseLeadsService: FranchiseLeadsService);
    create(body: any): Promise<any>;
    findAll(): Promise<any[]>;
    findOne(id: string): Promise<any>;
    update(id: string, body: any): Promise<any>;
    signContract(id: string): Promise<{
        lead: {
            id: string;
            name: string;
            status: import(".prisma/client").$Enums.FranchiseLeadStatus;
            region: string;
            createdAt: Date;
            updatedAt: Date;
            email: string;
            phone: string;
            documents: string;
            targetCity: string;
            companyName: string | null;
            siret: string | null;
            legalForm: string | null;
            contractDetails: string;
            contractHistory: string;
            convertedAgencyId: string | null;
            rejectionReason: string | null;
        };
        agency: any;
        user: any;
    }>;
    getContract(id: string, res: Response): Promise<void>;
    addNote(id: string, body: {
        content: string;
        author: string;
        type?: 'NOTE' | 'CALL' | 'EMAIL';
    }): Promise<{
        id: string;
        type: string;
        createdAt: Date;
        content: string;
        leadId: string;
        author: string;
    }>;
    getAnalytics(): Promise<{
        total: number;
        statusCounts: Record<string, number>;
        regionCounts: Record<string, number>;
        conversionRate: number;
        monthlyTrend: {
            month: string;
            count: number;
            signed: number;
        }[];
    }>;
    exportCSV(res: Response): Promise<void>;
}
