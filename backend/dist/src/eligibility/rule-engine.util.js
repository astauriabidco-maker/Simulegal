"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.evaluateRule = evaluateRule;
const common_1 = require("@nestjs/common");
const logger = new common_1.Logger('RuleEngine');
const FRENCH_LEVEL_ORDER = {
    A1: 1, A2: 2, B1: 3, B2: 4, C1: 5, C2: 6
};
function compareFrenchLevel(a, b) {
    const orderA = FRENCH_LEVEL_ORDER[a];
    const orderB = FRENCH_LEVEL_ORDER[b];
    if (orderA === undefined || orderB === undefined)
        return null;
    return orderA - orderB;
}
function getValueByPath(obj, path) {
    return path.split('.').reduce((acc, part) => acc && acc[part], obj);
}
function resolveValue(val, thresholds) {
    if (typeof val === 'string' && val.startsWith('@config:')) {
        const path = val.replace('@config:', '').split('.');
        let current = thresholds;
        for (const key of path) {
            if (current && typeof current === 'object' && key in current) {
                current = current[key];
            }
            else {
                return undefined;
            }
        }
        return current;
    }
    return val;
}
function evaluateRule(data, condition, thresholds = {}) {
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
//# sourceMappingURL=rule-engine.util.js.map