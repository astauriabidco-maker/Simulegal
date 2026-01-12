import config from '../specs/config_thresholds.json';

/**
 * Resolves a value that might be a reference to the configuration thresholds.
 * If the value starts with "@config:", it traverses the config_thresholds.json file.
 */
export function resolveThreshold(value: any): number | any {
    if (typeof value === 'string' && value.startsWith('@config:')) {
        const path = value.replace('@config:', '').split('.');
        let current: any = config;

        for (const key of path) {
            if (current && typeof current === 'object' && key in current) {
                current = current[key];
            } else {
                console.warn(`[Config] Path not found: ${value}`);
                return undefined;
            }
        }
        return current;
    }
    return value;
}
