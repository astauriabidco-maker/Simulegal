export interface RuleCondition {
    AND?: RuleCondition[];
    OR?: RuleCondition[];
    var?: string;
    op?: 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE' | 'IN';
    val?: any;
}
export declare function evaluateRule(data: any, condition: RuleCondition, thresholds?: any): boolean;
