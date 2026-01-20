import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Prospect, ProspectStatus } from '@prisma/client';

@Injectable()
export class SalesService {
    constructor(private prisma: PrismaService) { }

    async findAll(params: { page: number; limit: number; status?: string }) {
        const { page, limit, status } = params;
        const skip = (page - 1) * limit;

        const where: any = {};
        if (status) {
            where.status = status;
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

    async findOne(id: string) {
        return this.prisma.prospect.findUnique({
            where: { id },
            include: { notes: true },
        });
    }

    async create(data: any) {
        const score = this.calculateScore(data);

        const prospect = await this.prisma.prospect.create({
            data: {
                ...data,
                score,
                status: 'TO_CALL',
            },
        });

        await this.triggerAutomation(prospect, 'TO_CALL');
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
            case 'TO_CALL':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, merci de votre intérêt pour Simulegal. Un expert va vous rappeler ds les 2h."`);
                break;
            case 'MEETING_BOOKED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Votre RDV Simulegal est confirmé."`);
                break;
            case 'LINK_SENT':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Voici votre lien sécurisé: https://simulegal.fr/pay/${prospect.id}"`);
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
                data.status = 'TO_CALL';
                data.score = 10; // Base score for imported
                data.agencyId = 'HQ-001'; // Default to HQ, or could be passed in params

                await this.prisma.prospect.create({ data });
                count++;
            }
        }

        return count;
    }
}
