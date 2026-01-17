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
            if (process.env.NODE_ENV === 'development') {
                console.log(`[Engine] Rule fail: ${condition.var} is undefined/null`);
            }
            return false;
        }

        let result = false;
        switch (condition.op) {
            case 'EQ':
                result = userValue === targetValue;
                break;
            case 'NEQ':
                result = userValue !== targetValue;
                break;
            case 'GTE':
                result = userValue >= targetValue;
                break;
            case 'GT':
                result = userValue > targetValue;
                break;
            case 'LTE':
                result = userValue <= targetValue;
                break;
            case 'LT':
                result = userValue < targetValue;
                break;
            case 'IN':
                result = Array.isArray(targetValue) && targetValue.includes(userValue);
                break;
            default:
                console.warn(`[Engine] Unsupported operator: ${condition.op}`);
                result = false;
        }

        if (!result && process.env.NODE_ENV === 'development') {
            console.log(`[Engine] Rule fail: ${condition.var} (${userValue}) ${condition.op} ${targetValue}`);
        }

        return result;
    }

    return false;
}
