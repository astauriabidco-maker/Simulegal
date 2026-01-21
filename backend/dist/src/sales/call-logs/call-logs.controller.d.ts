import { CallLogsService } from './call-logs.service';
export declare class CallLogsController {
    private readonly callLogsService;
    constructor(callLogsService: CallLogsService);
    create(req: any, data: {
        prospectId: string;
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
}
