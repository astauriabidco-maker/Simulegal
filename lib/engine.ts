import { UserProfile, RuleCondition } from '../types/index';
import { resolveThreshold } from './config';

/**
 * Ordre sémantique des niveaux CECRL (Cadre Européen).
 * Utilisé pour les comparaisons GTE/GT/LTE/LT sur les niveaux de français.
 */
const FRENCH_LEVEL_ORDER: Record<string, number> = {
    A1: 1, A2: 2, B1: 3, B2: 4, C1: 5, C2: 6
};

/**
 * Compare deux niveaux de français sémantiquement.
 * Retourne un nombre négatif si a < b, 0 si a === b, positif si a > b.
 * Retourne null si l'un des deux n'est pas un niveau reconnu.
 */
function compareFrenchLevel(a: string, b: string): number | null {
    const orderA = FRENCH_LEVEL_ORDER[a];
    const orderB = FRENCH_LEVEL_ORDER[b];
    if (orderA === undefined || orderB === undefined) return null;
    return orderA - orderB;
}

/**
 * Retrieves a nested value from an object using a dot-separated path.
 */
function getValueByPath(obj: unknown, path: string): unknown {
    return path.split('.').reduce((acc: unknown, part: string) => (acc as Record<string, unknown>)?.[part], obj);
}

/**
 * Evaluates a single rule condition against a user profile.
 * Supports recursive AND/OR blocks and various comparison operators.
 */
export function evaluateRule(user: UserProfile, condition: RuleCondition): boolean {
    try {
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

            // Utiliser la comparaison sémantique pour les niveaux de français
            const isFrenchLevel = condition.var === 'integration.french_level';
            const frenchCmp = isFrenchLevel ? compareFrenchLevel(String(userValue), String(targetValue)) : null;

            switch (condition.op) {
                case 'EQ':
                    result = userValue === targetValue;
                    break;
                case 'NEQ':
                    result = userValue !== targetValue;
                    break;
                case 'GTE':
                    result = frenchCmp !== null ? frenchCmp >= 0 : (userValue as number) >= (targetValue as number);
                    break;
                case 'GT':
                    result = frenchCmp !== null ? frenchCmp > 0 : (userValue as number) > (targetValue as number);
                    break;
                case 'LTE':
                    result = frenchCmp !== null ? frenchCmp <= 0 : (userValue as number) <= (targetValue as number);
                    break;
                case 'LT':
                    result = frenchCmp !== null ? frenchCmp < 0 : (userValue as number) < (targetValue as number);
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
    } catch (err) {
        console.error('[Engine] Error evaluating rule condition:', err, condition);
        return false;
    }
}

/**
 * Finds the first leaf condition that fails within an AND block.
 * Returns the failed condition's { var, op, val } plus the actual userValue.
 * Returns null if all conditions pass.
 */
export function findFirstFailedCondition(
    user: UserProfile,
    condition: RuleCondition
): { var: string; op: string; val: unknown; userValue: unknown } | null {
    if (condition.AND) {
        for (const sub of condition.AND) {
            const failed = findFirstFailedCondition(user, sub);
            if (failed) return failed;
        }
        return null;
    }
    if (condition.OR) {
        // An OR fails only if ALL children fail — return the first child failure if ALL fail
        const allFail = condition.OR.every((sub) => !evaluateRule(user, sub));
        if (allFail && condition.OR.length > 0) {
            return findFirstFailedCondition(user, condition.OR[0]);
        }
        return null;
    }
    if (condition.var && condition.op) {
        if (!evaluateRule(user, condition)) {
            const userValue = getValueByPath(user, condition.var);
            return { var: condition.var, op: condition.op, val: condition.val, userValue };
        }
    }
    return null;
}
