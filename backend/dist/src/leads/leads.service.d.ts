import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
export declare class LeadsService {
    private prisma;
    private notifications;
    constructor(prisma: PrismaService, notifications: NotificationsService);
    findAll(): Promise<any[]>;
    findByAgency(agencyId: string): Promise<any[]>;
    findOne(id: string): Promise<any>;
    private mapLead;
    updateStatus(id: string, status: any): Promise<any>;
    assignUser(id: string, userId: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        data: string;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        paymentMethod: string | null;
        paymentDate: Date | null;
        paymentRef: string | null;
        invoiceNumber: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
    updateDocuments(id: string, documents: any[]): Promise<any>;
    addNote(leadId: string, data: {
        content: string;
        author: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        content: string;
        leadId: string;
        author: string;
    }>;
    create(data: any): Promise<any>;
    recordPayment(id: string, data: {
        amount: number;
        method: string;
        reference?: string;
    }): Promise<any>;
    delete(id: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        data: string;
        email: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        paymentMethod: string | null;
        paymentDate: Date | null;
        paymentRef: string | null;
        invoiceNumber: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    }>;
}
