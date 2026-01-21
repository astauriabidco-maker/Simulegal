import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
export declare class LeadsService {
    private prisma;
    private notifications;
    constructor(prisma: PrismaService, notifications: NotificationsService);
    findAll(): Promise<({
        originAgency: {
            id: string;
            name: string;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            serviceCommissionOverrides: string | null;
            contactEmail: string;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        } | null;
        notes: {
            id: string;
            createdAt: Date;
            content: string;
            author: string;
            leadId: string;
        }[];
    } & {
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    })[]>;
    findByAgency(agencyId: string): Promise<({
        notes: {
            id: string;
            createdAt: Date;
            content: string;
            author: string;
            leadId: string;
        }[];
    } & {
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    })[]>;
    findOne(id: string): Promise<({
        originAgency: {
            id: string;
            name: string;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            serviceCommissionOverrides: string | null;
            contactEmail: string;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        } | null;
        notes: {
            id: string;
            createdAt: Date;
            content: string;
            author: string;
            leadId: string;
        }[];
    } & {
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }) | null>;
    updateStatus(id: string, status: any): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
    assignUser(id: string, userId: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
    updateDocuments(id: string, documents: any[]): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
    addNote(leadId: string, data: {
        content: string;
        author: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        content: string;
        author: string;
        leadId: string;
    }>;
    create(data: any): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
}
