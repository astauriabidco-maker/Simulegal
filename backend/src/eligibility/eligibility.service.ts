import { Injectable, Logger } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';

@Injectable()
export class EligibilityService {
    private readonly logger = new Logger(EligibilityService.name);
    private readonly rootSpecsPath = path.join(process.cwd(), '..', 'specs');
    private readonly localSpecsPath = path.join(process.cwd(), 'specs');

    private getSpecsDir(): string {
        if (fs.existsSync(this.rootSpecsPath)) return this.rootSpecsPath;
        return this.localSpecsPath;
    }

    getThresholds() {
        const specsDir = this.getSpecsDir();
        try {
            const filePath = path.join(specsDir, 'config_thresholds.json');
            if (fs.existsSync(filePath)) {
                return JSON.parse(fs.readFileSync(filePath, 'utf8'));
            }
        } catch (error) {
            this.logger.error('Failed to load thresholds', error);
        }

        throw new Error('Critical: Legal specification "config_thresholds.json" not found.');
    }

    getRules(category: string) {
        const specsDir = this.getSpecsDir();
        const fileNames: Record<string, string> = {
            'sejour': 'rules_sejour.json',
            'naturalisation': 'rules_naturalisation.json',
            'family': 'rules_family.json'
        };

        const fileName = fileNames[category];
        if (!fileName) return [];

        try {
            const filePath = path.join(specsDir, fileName);
            if (fs.existsSync(filePath)) {
                return JSON.parse(fs.readFileSync(filePath, 'utf8'));
            }
        } catch (error) {
            this.logger.error(`Failed to load rules for category ${category}`, error);
        }

        return [];
    }

    async evaluateEligibility(userProfile: any, category: string) {
        const rules = this.getRules(category);
        const thresholds = this.getThresholds();
        const { evaluateRule } = require('./rule-engine.util');

        const eligibleRules = rules.filter((rule: any) =>
            evaluateRule(userProfile, rule.conditions, thresholds)
        );

        return eligibleRules.sort((a: any, b: any) => (b.priority || 0) - (a.priority || 0));
    }
}
