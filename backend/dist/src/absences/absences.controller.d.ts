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
        start: Date;
        reason: string | null;
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
        start: Date;
        reason: string | null;
    }>;
    remove(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        end: Date;
        userId: string;
        start: Date;
        reason: string | null;
    }>;
}
