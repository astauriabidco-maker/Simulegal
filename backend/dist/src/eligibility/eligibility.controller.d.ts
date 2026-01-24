import { EligibilityService } from './eligibility.service';
export declare class EligibilityController {
    private readonly service;
    constructor(service: EligibilityService);
    getThresholds(): any;
    getRules(category: string): any;
    evaluate(category: string, userProfile: any): Promise<any>;
}
