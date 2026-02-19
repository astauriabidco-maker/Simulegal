import { EligibilityService } from './eligibility.service';
export declare class EligibilityController {
    private readonly service;
    constructor(service: EligibilityService);
    getThresholds(): any;
    getRules(category: string): any;
    evaluate(category: string, userProfile: any): Promise<any>;
    updateRule(category: string, ruleId: string, body: {
        conditions: any;
        changedBy: string;
        changeDetails?: string;
    }): Promise<any>;
    updateThresholds(body: {
        thresholds: any;
        changedBy: string;
        changeDetails?: string;
    }): Promise<any>;
    getAuditLog(limit?: string): Promise<{
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
    generateChecklist(category: string, userProfile: any): Promise<any>;
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
    checkThresholdsHealth(): {
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
