import { Logger } from '@nestjs/common';

const logger = new Logger('RuleEngine');

export interface RuleCondition {
    AND?: RuleCondition[];
    OR?: RuleCondition[];
    var?: string;
    op?: 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE' | 'IN';
    val?: any;
}

/**
 * Ordre sémantique des niveaux CECRL (Cadre Européen).
 */
const FRENCH_LEVEL_ORDER: Record<string, number> = {
    A1: 1, A2: 2, B1: 3, B2: 4, C1: 5, C2: 6
};

/**
 * Compare deux niveaux de français sémantiquement.
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
function getValueByPath(obj: any, path: string): any {
    return path.split('.').reduce((acc, part) => acc && acc[part], obj);
}

/**
 * Resolves configuration thresholds if needed.
 */
function resolveValue(val: any, thresholds: any): any {
    if (typeof val === 'string' && val.startsWith('@config:')) {
        const path = val.replace('@config:', '').split('.');
        let current = thresholds;
        for (const key of path) {
            if (current && typeof current === 'object' && key in current) {
                current = current[key];
            } else {
                return undefined;
            }
        }
        return current;
    }
    return val;
}

/**
 * Evaluates a rule condition against a data object.
 */
export function evaluateRule(data: any, condition: RuleCondition, thresholds: any = {}): boolean {
    if (condition.AND) {
        return condition.AND.every(c => evaluateRule(data, c, thresholds));
    }

    if (condition.OR) {
        return condition.OR.some(c => evaluateRule(data, c, thresholds));
    }

    if (condition.var && condition.op) {
        const userValue = getValueByPath(data, condition.var);
        const targetValue = resolveValue(condition.val, thresholds);

        if (userValue === undefined || userValue === null) {
            return false;
        }

        // Comparaison sémantique pour les niveaux de français
        const isFrenchLevel = condition.var === 'integration.french_level';
        const frenchCmp = isFrenchLevel ? compareFrenchLevel(String(userValue), String(targetValue)) : null;

        switch (condition.op) {
            case 'EQ': return userValue === targetValue;
            case 'NEQ': return userValue !== targetValue;
            case 'GT': return frenchCmp !== null ? frenchCmp > 0 : userValue > targetValue;
            case 'GTE': return frenchCmp !== null ? frenchCmp >= 0 : userValue >= targetValue;
            case 'LT': return frenchCmp !== null ? frenchCmp < 0 : userValue < targetValue;
            case 'LTE': return frenchCmp !== null ? frenchCmp <= 0 : userValue <= targetValue;
            case 'IN': return Array.isArray(targetValue) && targetValue.includes(userValue);
            default:
                logger.warn(`Unsupported operator: ${condition.op}`);
                return false;
        }
    }

    return false;
}

