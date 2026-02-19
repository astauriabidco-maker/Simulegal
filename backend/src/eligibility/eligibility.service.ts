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
            'asile': 'rules_asile.json',
            'permis': 'rules_permis.json',
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

    // ─── Catalogue de documents ──────────────────────────────────

    /**
     * Retourne le catalogue complet des documents
     */
    getDocumentCatalog() {
        const specsDir = this.getSpecsDir();
        try {
            const filePath = path.join(specsDir, 'document_catalog.json');
            if (fs.existsSync(filePath)) {
                return JSON.parse(fs.readFileSync(filePath, 'utf8'));
            }
        } catch (error) {
            this.logger.error('Failed to load document catalog', error);
        }
        return {};
    }

    // ─── Checklist personnalisée ─────────────────────────────────

    /**
     * Génère une checklist personnalisée basée sur le profil utilisateur
     */
    async generateChecklist(userProfile: any, category: string) {
        const eligibleRules = await this.evaluateEligibility(userProfile, category);
        const catalog = this.getDocumentCatalog();

        return eligibleRules.map((rule: any) => ({
            ruleId: rule.id,
            ruleName: rule.name,
            description: rule.description,
            priority: rule.priority,
            tier: rule.tier,
            source_ref: rule.source_ref,
            documents: (rule.documents || []).map((docCode: string) => ({
                code: docCode,
                ...(catalog[docCode] || { name: docCode, description: '', category: 'other' }),
            })),
        }));
    }

    // ─── Consistency Check (Batch Simulator) ─────────────────────

    /**
     * Exécute tous les profils-types contre toutes les règles
     * et détecte : règles orphelines, chevauchements excessifs, profils sans match
     */
    async runConsistencyCheck() {
        const specsDir = this.getSpecsDir();
        const profilesPath = path.join(specsDir, 'test_profiles.json');

        if (!fs.existsSync(profilesPath)) {
            return { error: 'test_profiles.json not found' };
        }

        const profiles = JSON.parse(fs.readFileSync(profilesPath, 'utf8'));
        const thresholds = this.getThresholds();
        const { evaluateRule } = require('./rule-engine.util');
        const categories = Object.keys(this.getFileNames());

        // Load all rules
        const allRulesMap: Record<string, any[]> = {};
        let totalRules = 0;
        for (const cat of categories) {
            allRulesMap[cat] = this.getRules(cat);
            totalRules += allRulesMap[cat].length;
        }

        // Track which rules are matched by at least one profile
        const ruleMatchCount: Record<string, number> = {};
        for (const cat of categories) {
            for (const rule of allRulesMap[cat]) {
                ruleMatchCount[`${cat}/${rule.id}`] = 0;
            }
        }

        // Run each profile against all categories
        const profileResults: any[] = [];
        for (const tp of profiles) {
            const matchesByCategory: Record<string, string[]> = {};
            let totalMatches = 0;

            for (const cat of categories) {
                const matched = allRulesMap[cat].filter((rule: any) =>
                    evaluateRule(tp.profile, rule.conditions, thresholds)
                );
                matchesByCategory[cat] = matched.map((r: any) => r.id);
                totalMatches += matched.length;

                for (const m of matched) {
                    ruleMatchCount[`${cat}/${m.id}`]++;
                }
            }

            const expectedCats = tp.expected_categories || [];
            const actualCats = Object.entries(matchesByCategory)
                .filter(([, ids]) => (ids as string[]).length > 0)
                .map(([cat]) => cat);

            const missingExpected = expectedCats.filter((c: string) => !actualCats.includes(c));
            const unexpectedCats = actualCats.filter((c: string) => !expectedCats.includes(c));

            profileResults.push({
                profileId: tp.id,
                profileName: tp.name,
                totalMatches,
                matchesByCategory,
                expectedCategories: expectedCats,
                actualCategories: actualCats,
                missingExpected,
                unexpectedCategories: unexpectedCats,
                status: missingExpected.length === 0 && unexpectedCats.length === 0 ? '✅' : '⚠️',
            });
        }

        // Identify orphan rules (never matched by any profile)
        const orphanRules = Object.entries(ruleMatchCount)
            .filter(([, count]) => count === 0)
            .map(([key]) => key);

        // Identify heavily overlapping rules (matched by many profiles)
        const highOverlap = Object.entries(ruleMatchCount)
            .filter(([, count]) => count >= Math.ceil(profiles.length * 0.7))
            .map(([key, count]) => ({ rule: key, matchedByProfiles: count }));

        return {
            summary: {
                totalProfiles: profiles.length,
                totalRules,
                orphanRulesCount: orphanRules.length,
                highOverlapCount: highOverlap.length,
                profilesWithIssues: profileResults.filter(p => p.status === '⚠️').length,
            },
            orphanRules,
            highOverlap,
            profileResults,
        };
    }

    // ─── Threshold Staleness Check ───────────────────────────────

    /**
     * Vérifie la fraîcheur des seuils de configuration
     */
    checkThresholdsStaleness() {
        const specsDir = this.getSpecsDir();
        const filePath = path.join(specsDir, 'config_thresholds.json');

        if (!fs.existsSync(filePath)) {
            return { status: 'ERROR', message: 'config_thresholds.json not found' };
        }

        const stat = fs.statSync(filePath);
        const lastModified = stat.mtime;
        const daysSinceFileUpdate = Math.floor((Date.now() - lastModified.getTime()) / (1000 * 60 * 60 * 24));

        const thresholds = JSON.parse(fs.readFileSync(filePath, 'utf8'));

        // Read _meta if present
        const meta = thresholds._meta || {};
        const lastReviewed = meta.last_reviewed || null;
        const nextReviewDue = meta.next_review_due || null;
        const reviewOverdue = nextReviewDue ? new Date(nextReviewDue) < new Date() : false;

        // Check individual thresholds
        const checks = [
            { label: 'SMIC mensuel brut', path: 'financial_thresholds.salary_monthly_gross.smic' },
            { label: 'Passeport Talent salarié qualifié', path: 'financial_thresholds.salary_annual_gross.passeport_talent_salarie_qualifie' },
            { label: 'Carte Bleue UE seuil', path: 'financial_thresholds.salary_annual_gross.passeport_talent_carte_bleue_eu' },
            { label: 'Investisseur économique', path: 'financial_thresholds.investments.investisseur_eco_fonds' },
            { label: 'Création entreprise', path: 'financial_thresholds.investments.creation_entreprise_fonds' },
        ];

        const currentYear = new Date().getFullYear();
        const alerts: any[] = [];

        for (const check of checks) {
            const parts = check.path.split('.');
            let val: any = thresholds;
            for (const p of parts) { val = val?.[p]; }

            // Handle versioned (object with .value) vs legacy (raw number)
            const isVersioned = val && typeof val === 'object' && 'value' in val;
            const numericValue = isVersioned ? val.value : val;
            const validFrom = isVersioned ? val.valid_from : null;
            const sourceUrl = isVersioned ? val.source_url : null;
            const sourceRef = isVersioned ? val.source_ref : null;

            // Stale if valid_from is from a previous year
            const validFromYear = validFrom ? new Date(validFrom).getFullYear() : null;
            const isStale = validFromYear !== null && validFromYear < currentYear;

            alerts.push({
                label: check.label,
                currentValue: numericValue,
                validFrom,
                sourceUrl,
                sourceRef,
                isVersioned,
                isStale,
                severity: isStale ? 'warning' : 'ok',
            });
        }

        const staleCount = alerts.filter(a => a.isStale).length;

        return {
            status: staleCount > 0 || reviewOverdue ? 'WARNING' : 'OK',
            fileLastModified: lastModified.toISOString(),
            daysSinceFileUpdate,
            meta: {
                lastReviewed,
                nextReviewDue,
                reviewOverdue,
            },
            staleWarning: daysSinceFileUpdate > 180 ? `⚠️ Fichier non modifié depuis ${daysSinceFileUpdate} jours` : null,
            alerts,
            recommendation: staleCount > 0
                ? `${staleCount} seuil(s) datent de l'année précédente. Vérifiez les montants officiels sur service-public.fr.`
                : reviewOverdue
                    ? 'La date de prochaine revue est dépassée. Planifiez une revue des seuils.'
                    : 'Tous les seuils paraissent à jour.',
        };
    }
}
