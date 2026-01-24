import { AbsencesService } from './absences.service';
export declare class AbsencesController {
    private readonly absencesService;
    constructor(absencesService: AbsencesService);
    findAll(userId?: string, start?: string, end?: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        end: Date;
        userId: string;
        reason: string | null;
        start: Date;
    }[]>;
    create(data: {
        userId: string;
        start: string;
        end: string;
        reason?: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        end: Date;
        userId: string;
        reason: string | null;
        start: Date;
    }>;
    remove(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        end: Date;
        userId: string;
        reason: string | null;
        start: Date;
    }>;
}
