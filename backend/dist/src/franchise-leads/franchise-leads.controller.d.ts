import type { Response } from 'express';
import { FranchiseLeadsService } from './franchise-leads.service';
export declare class FranchiseLeadsController {
    private readonly franchiseLeadsService;
    constructor(franchiseLeadsService: FranchiseLeadsService);
    create(body: any): Promise<{
        id: string;
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        region: string;
        phone: string;
        targetCity: string;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        convertedAgencyId: string | null;
    }>;
    findAll(): Promise<{
        id: string;
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        region: string;
        phone: string;
        targetCity: string;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        convertedAgencyId: string | null;
    }[]>;
    findOne(id: string): Promise<({
        notes: {
            id: string;
            createdAt: Date;
            type: string;
            content: string;
            author: string;
            leadId: string;
        }[];
        convertedAgency: {
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
        } | null;
    } & {
        id: string;
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        region: string;
        phone: string;
        targetCity: string;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        convertedAgencyId: string | null;
    }) | null>;
    update(id: string, body: any): Promise<{
        id: string;
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        region: string;
        phone: string;
        targetCity: string;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        convertedAgencyId: string | null;
    }>;
    signContract(id: string): Promise<{
        lead: {
            id: string;
            email: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            status: import(".prisma/client").$Enums.FranchiseLeadStatus;
            region: string;
            phone: string;
            targetCity: string;
            companyName: string | null;
            siret: string | null;
            legalForm: string | null;
            contractDetails: string;
            convertedAgencyId: string | null;
        };
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
        user: {
            tempPassword: string;
            agencyId: string | null;
            role: import(".prisma/client").$Enums.UserRole;
            id: string;
            email: string;
            password: string;
            name: string;
            roleId: string | null;
            homeAgencyId: string | null;
            scopeAgencyIds: string;
            permissions: string;
            createdAt: Date;
            updatedAt: Date;
            lastLogin: Date | null;
        };
    }>;
    getContract(id: string, res: Response): Promise<void>;
    addNote(id: string, body: {
        content: string;
        author: string;
        type?: 'NOTE' | 'CALL' | 'EMAIL';
    }): Promise<{
        id: string;
        createdAt: Date;
        type: string;
        content: string;
        author: string;
        leadId: string;
    }>;
}
