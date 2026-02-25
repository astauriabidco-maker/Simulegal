import { Injectable } from '@nestjs/common';
import { PrismaService } from '../../prisma/prisma.service';
import { ProspectPipelineService } from '../prospect-pipeline.service';
import { SalesTrackingService } from '../../sales-tracking/sales-tracking.service';

@Injectable()
export class CallLogsService {
    constructor(
        private prisma: PrismaService,
        private prospectPipeline: ProspectPipelineService,
        private salesTracking: SalesTrackingService,
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

        // ─── PONT → SUIVI COMMERCIAL : log appel ───
        try {
            const prospect = await this.prisma.prospect.findUnique({
                where: { id: data.prospectId },
                select: { firstName: true, lastName: true, agencyId: true },
            });
            await this.salesTracking.logActivity({
                userId: data.userId,
                agencyId: prospect?.agencyId || undefined,
                activityType: 'CALL',
                prospectId: data.prospectId,
                prospectName: prospect ? `${prospect.firstName} ${prospect.lastName}` : undefined,
                notes: `Appel ${data.direction || 'OUTBOUND'}`,
                metadata: { callLogId: callLog.id, twilioCallSid: data.twilioCallSid },
            });
        } catch (e) { console.warn('[TRACKING] call log failed:', e); }

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
