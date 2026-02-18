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
        title: string;
        summary: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
    }[]>;
    findPending(): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        title: string;
        summary: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
    }[]>;
    create(data: {
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
        title: string;
        summary: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
    }>;
    update(id: string, data: Partial<{
        title: string;
        summary: string;
        category: string;
        severity: string;
        sourceUrl: string;
        authorName: string;
        applied: boolean;
    }>): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        title: string;
        summary: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
    }>;
    markAsApplied(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        category: string;
        title: string;
        summary: string;
        severity: string;
        sourceUrl: string | null;
        authorName: string | null;
        applied: boolean;
        appliedAt: Date | null;
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
