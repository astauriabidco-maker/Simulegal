import { LeadsService } from './leads.service';
import { InvoicesService } from '../invoices/invoices.service';
export declare class LeadsController {
    private readonly leadsService;
    private readonly invoicesService;
    constructor(leadsService: LeadsService, invoicesService: InvoicesService);
    findAll(req: any, agencyId?: string): Promise<any[]>;
    remove(req: any, id: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        data: string;
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
    recordPayment(id: string, data: {
        amount: number;
        method: string;
        reference?: string;
    }): Promise<any>;
    getInvoice(id: string): Promise<{
        invoiceNumber: string | null;
        date: Date;
        client: {
            name: string;
            email: string;
            phone: string;
        };
        service: {
            name: string;
            amount: number;
        };
        agency: {
            name: string;
            city: string;
        };
        payment: {
            method: string | null;
            reference: string | null;
        };
    }>;
    downloadPdf(id: string): Promise<{
        filename: string;
        content: string;
    } | null>;
    findOne(id: string): Promise<any>;
    create(data: any): Promise<any>;
    updateStatus(id: string, status: string): Promise<any>;
    addNote(id: string, data: {
        content: string;
        author: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        leadId: string;
        content: string;
        author: string;
    }>;
    assignUser(id: string, userId: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        data: string;
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
}
