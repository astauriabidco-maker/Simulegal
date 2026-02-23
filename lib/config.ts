import EligibilityStore from '../services/EligibilityStore';

/**
 * Resolves a value that might be a reference to the configuration thresholds.
 * If the value starts with "@config:", it traverses the thresholds from EligibilityStore.
 */
export function resolveThreshold(value: unknown): number | unknown {
    if (typeof value === 'string' && value.startsWith('@config:')) {
        const path = value.replace('@config:', '').split('.');
        const config = EligibilityStore.getThresholds();
        let current: unknown = config;

        for (const key of path) {
            if (current && typeof current === 'object' && key in current) {
                current = (current as Record<string, unknown>)[key];
            } else {
                console.warn(`[Config] Path not found: ${value}`);
                return undefined;
            }
        }
        return current;
    }
    return value;
}
