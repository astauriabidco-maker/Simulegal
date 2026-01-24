import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';

@Injectable()
export class LeadsService {
    constructor(
        private prisma: PrismaService,
        private notifications: NotificationsService
    ) { }

    async findAll() {
        const leads = await this.prisma.lead.findMany({
            include: { notes: true, originAgency: true },
            orderBy: { createdAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    async findByAgency(agencyId: string) {
        const leads = await this.prisma.lead.findMany({
            where: { originAgencyId: agencyId },
            include: { notes: true },
            orderBy: { createdAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    async findOne(id: string) {
        const lead = await this.prisma.lead.findUnique({
            where: { id },
            include: { notes: true, originAgency: true }
        });
        if (!lead) return null;
        return this.mapLead(lead);
    }

    private mapLead(lead: any) {
        return {
            ...lead,
            documents: lead.documents ? JSON.parse(lead.documents) : [],
            contract: lead.contract ? JSON.parse(lead.contract) : null,
            requiredDocs: lead.requiredDocs ? JSON.parse(lead.requiredDocs) : null
        };
    }

    async updateStatus(id: string, status: any) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (lead) {
            console.log(`[LeadsService] Updating lead ${id} status from ${lead.status} to ${status}`);
            await this.notifications.onStageChange(lead, lead.status, status);
        }

        const updatedLead = await this.prisma.lead.update({
            where: { id },
            data: { status }
        });

        return this.mapLead(updatedLead);
    }

    async assignUser(id: string, userId: string) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        const user = await this.prisma.user.findUnique({ where: { id: userId } });

        if (lead && user) {
            await this.notifications.onJuristAssigned(lead, user.name);
        }

        return this.prisma.lead.update({
            where: { id },
            data: { assignedUserId: userId }
        });
    }

    async updateDocuments(id: string, documents: any[]) {
        const existingLead = await this.prisma.lead.findUnique({ where: { id } });

        if (existingLead) {
            const oldDocs = JSON.parse(existingLead.documents || '[]');
            // Trouver les documents qui viennent d'être rejetés
            for (const newDoc of documents) {
                const oldDoc = oldDocs.find((d: any) => d.id === newDoc.id);
                if (newDoc.status === 'REJECTED' && oldDoc?.status !== 'REJECTED') {
                    await this.notifications.onDocumentRejected(existingLead, newDoc.docType);
                }
            }
        }

        const lead = await this.prisma.lead.update({
            where: { id },
            data: { documents: JSON.stringify(documents) }
        });

        return this.mapLead(lead);
    }

    async addNote(leadId: string, data: { content: string, author: string }) {
        return this.prisma.leadNote.create({
            data: {
                content: data.content,
                author: data.author,
                leadId
            }
        });
    }

    async create(data: any) {
        // Nettoyage et mappage des données pour Prisma
        const { currentStage, contract, documents, requiredDocuments, ...rest } = data;

        // Si l'ID n'est pas fourni, on en génère un type SL-XXXXX
        const leadId = data.id || `SL-${Math.floor(Math.random() * 90000 + 10000)}`;

        const { name, email, phone, serviceId, serviceName, status, amountPaid, originAgencyId } = rest;

        const lead = await this.prisma.lead.create({
            data: {
                id: leadId,
                name,
                email,
                phone,
                serviceId,
                serviceName,
                status: status || currentStage || 'NEW',
                amountPaid: amountPaid || 0,
                originAgencyId,
                contract: contract ? JSON.stringify(contract) : null,
                documents: documents ? JSON.stringify(documents) : '[]',
                requiredDocs: requiredDocuments ? JSON.stringify(requiredDocuments) : null,
                data: JSON.stringify(rest)
            }
        });

        return this.mapLead(lead);
    }

    async recordPayment(id: string, data: { amount: number, method: string, reference?: string }) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (!lead) throw new Error('Lead not found');

        const newAmount = (lead.amountPaid || 0) + data.amount;

        // On génère un numéro de facture si c'est le premier paiement
        const invoiceNumber = lead.invoiceNumber || `FAC-${new Date().getFullYear()}-${id.split('-').pop()}`;

        const updatedLead = await this.prisma.lead.update({
            where: { id },
            data: {
                amountPaid: newAmount,
                paymentMethod: data.method,
                paymentDate: new Date(),
                paymentRef: data.reference,
                invoiceNumber,
                status: 'PAID'
            }
        });

        return this.mapLead(updatedLead);
    }

    async delete(id: string) {
        return this.prisma.lead.delete({
            where: { id }
        });
    }
}
