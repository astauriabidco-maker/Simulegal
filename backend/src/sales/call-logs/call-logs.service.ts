import { Injectable } from '@nestjs/common';
import { PrismaService } from '../../prisma/prisma.service';
import { ProspectPipelineService } from '../prospect-pipeline.service';

@Injectable()
export class CallLogsService {
    constructor(
        private prisma: PrismaService,
        private prospectPipeline: ProspectPipelineService,
    ) { }

    async create(data: {
        prospectId: string;
        userId: string;
        direction?: string;
        twilioCallSid?: string;
    }) {
        const callLog = await this.prisma.callLog.create({
            data: {
                prospectId: data.prospectId,
                userId: data.userId,
                direction: data.direction || 'OUTBOUND',
                status: 'INITIATED',
                twilioCallSid: data.twilioCallSid,
            },
        });

        // ─── RÈGLE 1 : Premier appel → NEW → CONTACTED ───────
        await this.prospectPipeline.onCallStarted(data.prospectId);

        return callLog;
    }

    async update(id: string, data: {
        status?: string;
        duration?: number;
        notes?: string;
        endedAt?: Date;
    }) {
        return this.prisma.callLog.update({
            where: { id },
            data: {
                status: data.status,
                duration: data.duration,
                notes: data.notes,
                endedAt: data.endedAt,
            },
        });
    }

    async findByProspect(prospectId: string) {
        return this.prisma.callLog.findMany({
            where: { prospectId },
            orderBy: { startedAt: 'desc' },
        });
    }

    async findOne(id: string) {
        return this.prisma.callLog.findUnique({
            where: { id },
        });
    }
}
