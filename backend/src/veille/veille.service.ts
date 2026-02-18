import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class VeilleService {
    private readonly logger = new Logger(VeilleService.name);

    constructor(private prisma: PrismaService) { }

    async findAll() {
        return this.prisma.legalUpdate.findMany({
            orderBy: { createdAt: 'desc' },
        });
    }

    async findPending() {
        return this.prisma.legalUpdate.findMany({
            where: { applied: false },
            orderBy: { createdAt: 'desc' },
        });
    }

    async create(data: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
    }) {
        const note = await this.prisma.legalUpdate.create({
            data: {
                title: data.title,
                summary: data.summary,
                category: data.category,
                severity: data.severity || 'medium',
                sourceUrl: data.sourceUrl || null,
                authorName: data.authorName || null,
            },
        });
        this.logger.log(`✅ Note créée: "${note.title}" (${note.id})`);
        return note;
    }

    async update(id: string, data: Partial<{
        title: string;
        summary: string;
        category: string;
        severity: string;
        sourceUrl: string;
        authorName: string;
        applied: boolean;
    }>) {
        const note = await this.prisma.legalUpdate.update({
            where: { id },
            data,
        });
        this.logger.log(`✅ Note mise à jour: "${note.title}" (${note.id})`);
        return note;
    }

    async markAsApplied(id: string) {
        return this.prisma.legalUpdate.update({
            where: { id },
            data: { applied: true, appliedAt: new Date() },
        });
    }

    async remove(id: string) {
        await this.prisma.legalUpdate.delete({ where: { id } });
        this.logger.log(`🗑 Note supprimée: ${id}`);
        return { deleted: true };
    }

    async getStats() {
        const [total, applied, pending] = await Promise.all([
            this.prisma.legalUpdate.count(),
            this.prisma.legalUpdate.count({ where: { applied: true } }),
            this.prisma.legalUpdate.count({ where: { applied: false } }),
        ]);

        const latest = await this.prisma.legalUpdate.findFirst({
            orderBy: { createdAt: 'desc' },
            select: { createdAt: true },
        });

        return {
            totalCount: total,
            appliedCount: applied,
            pendingCount: pending,
            conformityPercent: total > 0 ? Math.round((applied / total) * 100) : 100,
            lastUpdate: latest?.createdAt || null,
        };
    }
}
