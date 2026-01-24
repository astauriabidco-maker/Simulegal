export declare class EligibilityService {
    private readonly logger;
    private readonly rootSpecsPath;
    private readonly localSpecsPath;
    private getSpecsDir;
    getThresholds(): any;
    getRules(category: string): any;
    evaluateEligibility(userProfile: any, category: string): Promise<any>;
}
