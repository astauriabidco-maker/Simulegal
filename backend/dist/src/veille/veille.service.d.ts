import { PrismaService } from '../prisma/prisma.service';
export declare class VeilleService {
    private prisma;
    private readonly logger;
    constructor(prisma: PrismaService);
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
    create(data: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
        linkedRuleIds?: string[];
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
    update(id: string, data: Partial<{
        title: string;
        summary: string;
        category: string;
        severity: string;
        sourceUrl: string;
        authorName: string;
        applied: boolean;
        linkedRuleIds: string[];
    }>): Promise<{
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
    getStats(): Promise<{
        totalCount: number;
        appliedCount: number;
        pendingCount: number;
        conformityPercent: number;
        lastUpdate: Date | null;
    }>;
}
