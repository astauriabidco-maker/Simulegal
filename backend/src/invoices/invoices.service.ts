import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class InvoicesService {
    constructor(private prisma: PrismaService) { }

    async getInvoiceData(leadId: string) {
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId },
            include: { originAgency: true }
        });

        if (!lead) return null;

        return {
            invoiceNumber: lead.invoiceNumber,
            date: lead.paymentDate || lead.updatedAt,
            client: {
                name: lead.name,
                email: lead.email,
                phone: lead.phone
            },
            service: {
                name: lead.serviceName,
                amount: lead.amountPaid
            },
            agency: lead.originAgency ? {
                name: lead.originAgency.name,
                city: lead.originAgency.city
            } : {
                name: 'SimuLegal HQ',
                city: 'Paris'
            },
            payment: {
                method: lead.paymentMethod,
                reference: lead.paymentRef
            }
        };
    }

    // Simulation de génération PDF (retourne un message ou buffer)
    async generatePdf(leadId: string) {
        const data = await this.getInvoiceData(leadId);
        if (!data) return null;

        console.log(`[INVOICE] 📄 PDF Generated for ${data.invoiceNumber}`);
        return {
            filename: `${data.invoiceNumber}.pdf`,
            content: `SIMULEGAL INVOICE\nRef: ${data.invoiceNumber}\nClient: ${data.client.name}\nAmount: ${data.service.amount / 100}€`
        };
    }
}
