import { PrismaService } from '../prisma/prisma.service';
export declare class AppointmentsSlotsService {
    private prisma;
    constructor(prisma: PrismaService);
    createSlots(juristId: string, slots: {
        start: string;
        end: string;
    }[]): Promise<{
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    }[]>;
    findAvailableSlots(startDate: string, endDate: string): Promise<({
        jurist: {
            id: string;
            name: string;
        };
    } & {
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    })[]>;
    lockSlot(slotId: string, leadId?: string): Promise<{
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    }>;
    bookSlot(slotId: string, leadId: string): Promise<{
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    }>;
    releaseExpiredLocks(): Promise<import(".prisma/client").Prisma.BatchPayload>;
}
