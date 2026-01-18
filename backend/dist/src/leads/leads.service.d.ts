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
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
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
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.LeadStatus;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        originAgencyId: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
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
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.LeadStatus;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        originAgencyId: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
    })[]>;
    findOne(id: string): Promise<({
        originAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
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
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.LeadStatus;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        originAgencyId: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
    }) | null>;
    updateStatus(id: string, status: any): Promise<{
        id: string;
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.LeadStatus;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        originAgencyId: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
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
        email: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        status: import(".prisma/client").$Enums.LeadStatus;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        originAgencyId: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
    }>;
}
