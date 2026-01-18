import type { Response } from 'express';
import { FranchiseLeadsService } from './franchise-leads.service';
export declare class FranchiseLeadsController {
    private readonly franchiseLeadsService;
    constructor(franchiseLeadsService: FranchiseLeadsService);
    create(body: any): Promise<{
        id: string;
        name: string;
        email: string;
        phone: string;
        targetCity: string;
        region: string;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        createdAt: Date;
        updatedAt: Date;
        convertedAgencyId: string | null;
    }>;
    findAll(): Promise<{
        id: string;
        name: string;
        email: string;
        phone: string;
        targetCity: string;
        region: string;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        createdAt: Date;
        updatedAt: Date;
        convertedAgencyId: string | null;
    }[]>;
    findOne(id: string): Promise<({
        convertedAgency: {
            id: string;
            name: string;
            region: string;
            status: import(".prisma/client").$Enums.AgencyStatus;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
        notes: {
            id: string;
            createdAt: Date;
            type: string;
            content: string;
            author: string;
            leadId: string;
        }[];
    } & {
        id: string;
        name: string;
        email: string;
        phone: string;
        targetCity: string;
        region: string;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        createdAt: Date;
        updatedAt: Date;
        convertedAgencyId: string | null;
    }) | null>;
    update(id: string, body: any): Promise<{
        id: string;
        name: string;
        email: string;
        phone: string;
        targetCity: string;
        region: string;
        status: import(".prisma/client").$Enums.FranchiseLeadStatus;
        companyName: string | null;
        siret: string | null;
        legalForm: string | null;
        contractDetails: string;
        createdAt: Date;
        updatedAt: Date;
        convertedAgencyId: string | null;
    }>;
    signContract(id: string): Promise<{
        lead: {
            id: string;
            name: string;
            email: string;
            phone: string;
            targetCity: string;
            region: string;
            status: import(".prisma/client").$Enums.FranchiseLeadStatus;
            companyName: string | null;
            siret: string | null;
            legalForm: string | null;
            contractDetails: string;
            createdAt: Date;
            updatedAt: Date;
            convertedAgencyId: string | null;
        };
        agency: {
            id: string;
            name: string;
            region: string;
            status: import(".prisma/client").$Enums.AgencyStatus;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        };
        user: {
            tempPassword: string;
            id: string;
            name: string;
            email: string;
            createdAt: Date;
            updatedAt: Date;
            password: string;
            role: import(".prisma/client").$Enums.UserRole;
            homeAgencyId: string | null;
            scopeAgencyIds: string;
            permissions: string;
            lastLogin: Date | null;
            roleId: string | null;
            agencyId: string | null;
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
