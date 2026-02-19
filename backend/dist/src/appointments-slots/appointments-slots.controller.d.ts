import { AppointmentsSlotsService } from './appointments-slots.service';
export declare class AppointmentsSlotsController {
    private readonly slotsService;
    constructor(slotsService: AppointmentsSlotsService);
    getAvailableSlots(start: string, end: string): Promise<({
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
    createSlots(body: {
        juristId: string;
        slots: {
            start: string;
            end: string;
        }[];
    }): Promise<{
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    }[]>;
    lockSlot(id: string, body: {
        leadId?: string;
    }): Promise<{
        id: string;
        status: string;
        start: Date;
        end: Date;
        lockedAt: Date | null;
        juristId: string;
        leadId: string | null;
    }>;
}
