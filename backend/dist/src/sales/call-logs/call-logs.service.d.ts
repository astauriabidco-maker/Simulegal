import { PrismaService } from '../../prisma/prisma.service';
export declare class CallLogsService {
    private prisma;
    constructor(prisma: PrismaService);
    create(data: {
        prospectId: string;
        userId: string;
        direction?: string;
        twilioCallSid?: string;
    }): Promise<{
        id: string;
        status: string;
        notes: string | null;
        userId: string;
        prospectId: string;
        direction: string;
        duration: number;
        twilioCallSid: string | null;
        startedAt: Date;
        endedAt: Date | null;
    }>;
    update(id: string, data: {
        status?: string;
        duration?: number;
        notes?: string;
        endedAt?: Date;
    }): Promise<{
        id: string;
        status: string;
        notes: string | null;
        userId: string;
        prospectId: string;
        direction: string;
        duration: number;
        twilioCallSid: string | null;
        startedAt: Date;
        endedAt: Date | null;
    }>;
    findByProspect(prospectId: string): Promise<{
        id: string;
        status: string;
        notes: string | null;
        userId: string;
        prospectId: string;
        direction: string;
        duration: number;
        twilioCallSid: string | null;
        startedAt: Date;
        endedAt: Date | null;
    }[]>;
    findOne(id: string): Promise<{
        id: string;
        status: string;
        notes: string | null;
        userId: string;
        prospectId: string;
        direction: string;
        duration: number;
        twilioCallSid: string | null;
        startedAt: Date;
        endedAt: Date | null;
    } | null>;
}
