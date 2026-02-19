import { PrismaService } from '../prisma/prisma.service';
export declare class EligibilityService {
    private prisma;
    private readonly logger;
    private readonly rootSpecsPath;
    private readonly localSpecsPath;
    constructor(prisma: PrismaService);
    private getSpecsDir;
    private getFileNames;
    getThresholds(): any;
    getRules(category: string): any;
    evaluateEligibility(userProfile: any, category: string): Promise<any>;
    updateRule(category: string, ruleId: string, newConditions: any, changedBy: string, changeDetails?: string): Promise<any>;
    updateThresholds(newThresholds: any, changedBy: string, changeDetails?: string): Promise<any>;
    getAuditLog(limit?: number): Promise<{
        id: string;
        createdAt: Date;
        category: string;
        ruleId: string;
        ruleName: string;
        action: string;
        changedBy: string;
        previousValue: string | null;
        newValue: string | null;
        changeDetails: string | null;
    }[]>;
    getRuleHistory(category: string, ruleId: string): Promise<{
        id: string;
        createdAt: Date;
        category: string;
        ruleId: string;
        ruleName: string;
        action: string;
        changedBy: string;
        previousValue: string | null;
        newValue: string | null;
        changeDetails: string | null;
    }[]>;
    getDocumentCatalog(): any;
    generateChecklist(userProfile: any, category: string): Promise<any>;
    runConsistencyCheck(): Promise<{
        error: string;
        summary?: undefined;
        orphanRules?: undefined;
        highOverlap?: undefined;
        profileResults?: undefined;
    } | {
        summary: {
            totalProfiles: any;
            totalRules: number;
            orphanRulesCount: number;
            highOverlapCount: number;
            profilesWithIssues: number;
        };
        orphanRules: string[];
        highOverlap: {
            rule: string;
            matchedByProfiles: number;
        }[];
        profileResults: any[];
        error?: undefined;
    }>;
    checkThresholdsStaleness(): {
        status: string;
        message: string;
        fileLastModified?: undefined;
        daysSinceFileUpdate?: undefined;
        meta?: undefined;
        staleWarning?: undefined;
        alerts?: undefined;
        recommendation?: undefined;
    } | {
        status: string;
        fileLastModified: string;
        daysSinceFileUpdate: number;
        meta: {
            lastReviewed: any;
            nextReviewDue: any;
            reviewOverdue: boolean;
        };
        staleWarning: string | null;
        alerts: any[];
        recommendation: string;
        message?: undefined;
    };
}
