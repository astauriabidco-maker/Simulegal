import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Prospect, ProspectStatus } from '@prisma/client';

@Injectable()
export class SalesService {
    constructor(private prisma: PrismaService) { }

    async findAll() {
        return this.prisma.prospect.findMany({
            include: { notes: true },
            orderBy: { createdAt: 'desc' },
        });
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
}
