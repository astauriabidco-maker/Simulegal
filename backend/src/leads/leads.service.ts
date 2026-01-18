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
        return this.prisma.lead.findMany({
            include: { notes: true, originAgency: true },
            orderBy: { createdAt: 'desc' }
        });
    }

    async findByAgency(agencyId: string) {
        return this.prisma.lead.findMany({
            where: { originAgencyId: agencyId },
            include: { notes: true },
            orderBy: { createdAt: 'desc' }
        });
    }

    async findOne(id: string) {
        return this.prisma.lead.findUnique({
            where: { id },
            include: { notes: true, originAgency: true }
        });
    }

    async updateStatus(id: string, status: any) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (lead) {
            await this.notifications.onStageChange(lead, lead.status, status);
        }

        return this.prisma.lead.update({
            where: { id },
            data: { status }
        });
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

        return this.prisma.lead.create({
            data: {
                ...rest,
                id: leadId,
                status: currentStage || rest.status || 'NEW',
                contract: contract ? JSON.stringify(contract) : null,
                documents: documents ? JSON.stringify(documents) : '[]',
                requiredDocs: requiredDocuments ? JSON.stringify(requiredDocuments) : null,
            }
        });
    }
}
