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
}
