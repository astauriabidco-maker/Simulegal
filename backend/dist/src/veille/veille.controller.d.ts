import { VeilleService } from './veille.service';
export declare class VeilleController {
    private readonly veilleService;
    constructor(veilleService: VeilleService);
    findAll(): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        summary: string;
        title: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
        linkedRuleIds: string;
    }[]>;
    findPending(): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        summary: string;
        title: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
        linkedRuleIds: string;
    }[]>;
    getStats(): Promise<{
        totalCount: number;
        appliedCount: number;
        pendingCount: number;
        conformityPercent: number;
        lastUpdate: Date | null;
    }>;
    create(body: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        summary: string;
        title: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
        linkedRuleIds: string;
    }>;
    update(id: string, body: any): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        summary: string;
        title: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
        linkedRuleIds: string;
    }>;
    markAsApplied(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        summary: string;
        title: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
        linkedRuleIds: string;
    }>;
    remove(id: string): Promise<{
        deleted: boolean;
    }>;
}
