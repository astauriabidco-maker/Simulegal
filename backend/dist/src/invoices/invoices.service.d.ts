import { PrismaService } from '../prisma/prisma.service';
export declare class InvoicesService {
    private prisma;
    constructor(prisma: PrismaService);
    getInvoiceData(leadId: string): Promise<{
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
    } | null>;
    generatePdf(leadId: string): Promise<{
        filename: string;
        content: string;
    } | null>;
}
