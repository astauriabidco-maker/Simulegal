import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Prospect, ProspectStatus } from '@prisma/client';
import { AssignmentService } from './assignment.service';

@Injectable()
export class SalesService {
    constructor(
        private prisma: PrismaService,
        private assignmentService: AssignmentService
    ) { }

    async findAll(params: {
        page: number;
        limit: number;
        status?: string;
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
    }) {
        const { page, limit, status, agencyId, source, dateFrom, dateTo } = params;
        const skip = (page - 1) * limit;

        const where: any = {};

        // Status filter
        if (status) {
            where.status = status;
        }

        // Agency filter
        if (agencyId) {
            where.agencyId = agencyId;
        }

        // Source filter
        if (source) {
            where.source = source;
        }

        // Date range filter
        if (dateFrom || dateTo) {
            where.createdAt = {};
            if (dateFrom) {
                where.createdAt.gte = new Date(dateFrom);
            }
            if (dateTo) {
                where.createdAt.lte = new Date(dateTo);
            }
        }

        const [data, total] = await Promise.all([
            this.prisma.prospect.findMany({
                where,
                skip,
                take: limit,
                include: { notes: true },
                orderBy: { createdAt: 'desc' },
            }),
            this.prisma.prospect.count({ where })
        ]);

        return {
            data,
            meta: {
                total,
                page,
                limit,
                totalPages: Math.ceil(total / limit)
            }
        };
    }

    async exportToCSV(filters?: { agencyId?: string; source?: string; dateFrom?: string; dateTo?: string }): Promise<string> {
        const where: any = {};

        if (filters?.agencyId) where.agencyId = filters.agencyId;
        if (filters?.source) where.source = filters.source;
        if (filters?.dateFrom || filters?.dateTo) {
            where.createdAt = {};
            if (filters.dateFrom) where.createdAt.gte = new Date(filters.dateFrom);
            if (filters.dateTo) where.createdAt.lte = new Date(filters.dateTo);
        }

        const prospects = await this.prisma.prospect.findMany({
            where,
            orderBy: { createdAt: 'desc' },
        });

        // Build CSV
        const headers = ['ID', 'Prénom', 'Nom', 'Téléphone', 'Email', 'Source', 'Agence', 'Statut', 'Score', 'Date création'];
        const rows = prospects.map(p => [
            p.id,
            p.firstName,
            p.lastName,
            p.phone,
            p.email || '',
            p.source,
            p.agencyId,
            p.status,
            p.score.toString(),
            new Date(p.createdAt).toLocaleDateString('fr-FR')
        ].map(v => `"${(v || '').replace(/"/g, '""')}"`).join(';'));

        return [headers.join(';'), ...rows].join('\n');
    }

    async findOne(id: string) {
        return this.prisma.prospect.findUnique({
            where: { id },
            include: { notes: true },
        });
    }

    async create(data: any) {
        const score = this.calculateScore(data);

        // Round-robin assignment to a sales agent
        const assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId || null);

        const prospect = await this.prisma.prospect.create({
            data: {
                ...data,
                score,
                status: 'NEW',
                assignedToSalesId,
            },
        });

        await this.triggerAutomation(prospect, 'NEW');
        return prospect;
    }

    async update(id: string, data: any) {
        const oldProspect = await this.prisma.prospect.findUnique({ where: { id } });
        if (!oldProspect) return null;

        const updatedProspect = await this.prisma.prospect.update({
            where: { id },
            data,
        });

        if (data.status && data.status !== oldProspect.status) {
            await this.triggerAutomation(updatedProspect, data.status);
        }

        return updatedProspect;
    }

    private calculateScore(prospect: any): number {
        let score = 0;

        // Source rules
        if (prospect.source === 'GOOGLE_ADS') score += 30;
        if (prospect.source === 'META_ADS') score += 20;
        if (prospect.source === 'TIKTOK_ADS') score += 15;

        // Info rules
        if (prospect.email && prospect.email.length > 5) score += 10;
        if (prospect.phone && prospect.phone.length > 8) score += 10;

        // Interest rules
        if (prospect.interestServiceId) score += 10;

        return Math.min(score, 100);
    }

    private async triggerAutomation(prospect: Prospect, status: ProspectStatus) {
        console.log(`[BACKEND AUTOMATION] Trigger for ${prospect.firstName} -> ${status}`);

        // Mocking automation calls
        switch (status) {
            case 'NEW':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, merci de votre intérêt pour Simulegal. Un expert va vous rappeler ds les 2h."`);
                break;
            case 'MEETING_BOOKED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Votre RDV Simulegal est confirmé."`);
                break;
            case 'NO_SHOW':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, nous avons remarqué que vous n'avez pas pu venir. Souhaitez-vous reprogrammer ?"`);
                break;
            case 'SIGNED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Bienvenue chez Simulegal ! Votre dossier est ouvert."`);
                break;
        }
    }

    async addNote(prospectId: string, authorId: string, text: string) {
        return this.prisma.prospectNote.create({
            data: {
                prospectId,
                authorId,
                text,
            },
        });
    }

    async importFromCSV(buffer: Buffer): Promise<number> {
        const content = buffer.toString('utf-8');
        const lines = content.split(/\r?\n/);

        let count = 0;
        const headers = lines[0]?.toLowerCase().split(/[;,]/).map(h => h.trim().replace(/"/g, ''));

        // Basic validation
        if (!headers || !headers.includes('firstname') || !headers.includes('lastname')) {
            throw new Error('Invalid CSV headers. Expected firstName, lastName, phone, email');
        }

        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;

            const values = line.split(/[;,]/).map(v => v.trim().replace(/"/g, ''));
            const data: any = {};

            headers.forEach((header, index) => {
                const val = values[index];
                if (header === 'firstname') data.firstName = val;
                if (header === 'lastname') data.lastName = val;
                if (header === 'email') data.email = val;
                if (header === 'phone') data.phone = val;
                if (header === 'source') data.source = val;
            });

            if (data.firstName && data.lastName) {
                // Set defaults
                data.source = data.source || 'CSV_IMPORT';
                data.status = 'NEW';
                data.score = 10; // Base score for imported
                data.agencyId = data.agencyId || 'HQ-001'; // Default to HQ

                // Round-robin assignment
                data.assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId);

                await this.prisma.prospect.create({ data });
                count++;
            }
        }

        return count;
    }
}
