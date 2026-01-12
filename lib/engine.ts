import { UserProfile, RuleCondition } from '../types/index';
import { resolveThreshold } from './config';

/**
 * Retrieves a nested value from an object using a dot-separated path.
 */
function getValueByPath(obj: any, path: string): any {
    return path.split('.').reduce((acc, part) => acc && acc[part], obj);
}

/**
 * Evaluates a single rule condition against a user profile.
 * Supports recursive AND/OR blocks and various comparison operators.
 */
export function evaluateRule(user: UserProfile, condition: RuleCondition): boolean {
    // 1. Logic Blocks: AND
    if (condition.AND) {
        return condition.AND.every((subCondition) => evaluateRule(user, subCondition));
    }

    // 2. Logic Blocks: OR
    if (condition.OR) {
        return condition.OR.some((subCondition) => evaluateRule(user, subCondition));
    }

    // 3. Leaf Condition: Variable comparison
    if (condition.var && condition.op) {
        const userValue = getValueByPath(user, condition.var);
        const targetValue = resolveThreshold(condition.val);

        // Handle undefined user values (false by default)
        if (userValue === undefined || userValue === null) {
            return false;
        }

        switch (condition.op) {
            case 'EQ':
                return userValue === targetValue;
            case 'NEQ':
                return userValue !== targetValue;
            case 'GTE':
                return userValue >= targetValue;
            case 'GT':
                return userValue > targetValue;
            case 'LTE':
                return userValue <= targetValue;
            case 'LT':
                return userValue < targetValue;
            case 'IN':
                return Array.isArray(targetValue) && targetValue.includes(userValue);
            default:
                console.warn(`[Engine] Unsupported operator: ${condition.op}`);
                return false;
        }
    }

    return false;
}
