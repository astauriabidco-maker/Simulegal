import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import * as fs from 'fs';
import * as path from 'path';

@Injectable()
export class EligibilityService {
    private readonly logger = new Logger(EligibilityService.name);
    private readonly rootSpecsPath = path.join(process.cwd(), '..', 'specs');
    private readonly localSpecsPath = path.join(process.cwd(), 'specs');

    constructor(private prisma: PrismaService) { }

    private getSpecsDir(): string {
        if (fs.existsSync(this.rootSpecsPath)) return this.rootSpecsPath;
        return this.localSpecsPath;
    }

    private getFileNames(): Record<string, string> {
        return {
            'sejour': 'rules_sejour.json',
            'naturalisation': 'rules_naturalisation.json',
            'family': 'rules_family.json',
        };
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
        const fileName = this.getFileNames()[category];
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

    // ─── Audit Trail ─────────────────────────────────────────────

    /**
     * Met à jour une règle dans le JSON et crée un audit log
     */
    async updateRule(category: string, ruleId: string, newConditions: any, changedBy: string, changeDetails?: string) {
        const specsDir = this.getSpecsDir();
        const fileName = this.getFileNames()[category];
        if (!fileName) throw new Error(`Unknown category: ${category}`);

        const filePath = path.join(specsDir, fileName);
        const rules = JSON.parse(fs.readFileSync(filePath, 'utf8'));
        const ruleIndex = rules.findIndex((r: any) => r.id === ruleId);
        if (ruleIndex === -1) throw new Error(`Rule ${ruleId} not found in ${category}`);

        const previousRule = { ...rules[ruleIndex] };
        rules[ruleIndex].conditions = newConditions;

        // Save to file
        fs.writeFileSync(filePath, JSON.stringify(rules, null, 4), 'utf8');

        // Create audit log
        await this.prisma.ruleAuditLog.create({
            data: {
                category,
                ruleId,
                ruleName: previousRule.title || previousRule.label || ruleId,
                action: 'UPDATE',
                changedBy,
                previousValue: JSON.stringify(previousRule.conditions),
                newValue: JSON.stringify(newConditions),
                changeDetails: changeDetails || `Conditions modifiées par ${changedBy}`,
            },
        });

        this.logger.log(`✅ Rule ${ruleId} updated in ${category} by ${changedBy}`);
        return rules[ruleIndex];
    }

    /**
     * Met à jour les seuils de configuration et crée un audit log
     */
    async updateThresholds(newThresholds: any, changedBy: string, changeDetails?: string) {
        const specsDir = this.getSpecsDir();
        const filePath = path.join(specsDir, 'config_thresholds.json');

        let previousThresholds = {};
        try {
            previousThresholds = JSON.parse(fs.readFileSync(filePath, 'utf8'));
        } catch (e) { }

        fs.writeFileSync(filePath, JSON.stringify(newThresholds, null, 4), 'utf8');

        await this.prisma.ruleAuditLog.create({
            data: {
                category: 'config',
                ruleId: 'thresholds',
                ruleName: 'Seuils de configuration',
                action: 'UPDATE',
                changedBy,
                previousValue: JSON.stringify(previousThresholds),
                newValue: JSON.stringify(newThresholds),
                changeDetails: changeDetails || `Seuils mis à jour par ${changedBy}`,
            },
        });

        this.logger.log(`✅ Thresholds updated by ${changedBy}`);
        return newThresholds;
    }

    /**
     * Récupère l'audit log des modifications de règles
     */
    async getAuditLog(limit = 50) {
        return this.prisma.ruleAuditLog.findMany({
            orderBy: { createdAt: 'desc' },
            take: limit,
        });
    }

    /**
     * Récupère l'audit log pour une règle spécifique
     */
    async getRuleHistory(category: string, ruleId: string) {
        return this.prisma.ruleAuditLog.findMany({
            where: { category, ruleId },
            orderBy: { createdAt: 'desc' },
        });
    }
}
