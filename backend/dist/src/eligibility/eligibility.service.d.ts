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
}
