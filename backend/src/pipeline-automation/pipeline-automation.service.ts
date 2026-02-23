import { Injectable, OnModuleInit } from '@nestjs/common';
import { Cron, CronExpression } from '@nestjs/schedule';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';

// ═══════════════════════════════════════════════
// TYPES POUR LES AUTOMATISATIONS PIPELINE
// ═══════════════════════════════════════════════

export type AutomationType =
    | 'RELANCE'
    | 'ESCALADE'
    | 'AUTO_TRANSITION'
    | 'NOTIFICATION'
    | 'ALERTE_INTERNE';

export interface AutomationRule {
    id: string;
    name: string;
    description: string;
    type: AutomationType;
    enabled: boolean;
    priority: number;
    trigger: {
        event: 'STAGE_CHANGE' | 'STALE_DAYS' | 'DOCS_COMPLETE' | 'PAYMENT_RECEIVED' | 'MANUAL';
        stage?: string;
        fromStage?: string;
        toStage?: string;
        staleDays?: number;
        pipelineTemplates?: string[];
        categories?: string[];
    };
    action: {
        type: 'WHATSAPP' | 'EMAIL' | 'MOVE_STAGE' | 'INTERNAL_ALERT' | 'ASSIGN_USER';
        template?: string;
        message?: string;
        targetStage?: string;
        alertLevel?: 'INFO' | 'WARNING' | 'CRITICAL';
        assignTo?: 'MANAGER' | 'ADMIN' | 'SPECIFIC_USER';
        maxExecutions?: number;
        cooldownHours?: number;
    };
    createdAt: string;
    updatedAt: string;
    createdBy?: string;
    executionCount?: number;
}

export interface AutomationLog {
    id: string;
    ruleId: string;
    ruleName: string;
    leadId: string;
    leadName: string;
    action: string;
    result: 'SUCCESS' | 'FAILED' | 'SKIPPED';
    details?: string;
    executedAt: string;
}

@Injectable()
export class PipelineAutomationService implements OnModuleInit {
    private rules: AutomationRule[] = [];
    private executionLogs: AutomationLog[] = [];
    private executionTracker: Map<string, { count: number; lastExecution: Date }> = new Map();

    constructor(
        private prisma: PrismaService,
        private notifications: NotificationsService,
    ) { }

    async onModuleInit() {
        await this.loadRules();
        console.log(`[PipelineAutomation] ${this.rules.length} automation rules loaded`);
    }

    // ═══════════════════════════════════════════════
    // CHARGEMENT / SAUVEGARDE DES RÈGLES (DB)
    // ═══════════════════════════════════════════════

    async loadRules(): Promise<AutomationRule[]> {
        const settings = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } }) as any;
        if (settings?.pipelineAutomations) {
            try {
                const data = JSON.parse(settings.pipelineAutomations);
                this.rules = data.rules || this.getDefaultRules();
                this.executionLogs = (data.logs || []).slice(-500);
            } catch {
                this.rules = this.getDefaultRules();
            }
        } else {
            this.rules = this.getDefaultRules();
            await this.saveRules();
        }
        return this.rules;
    }

    async saveRules(): Promise<void> {
        const payload = JSON.stringify({
            rules: this.rules,
            logs: this.executionLogs.slice(-500),
        });
        await this.prisma.systemSettings.upsert({
            where: { id: 'GLOBAL' },
            create: { id: 'GLOBAL', pipelineAutomations: payload } as any,
            update: { pipelineAutomations: payload } as any,
        });
    }

    // ═══════════════════════════════════════════════
    // CRUD DES RÈGLES D'AUTOMATISATION
    // ═══════════════════════════════════════════════

    async getRules(): Promise<AutomationRule[]> {
        await this.loadRules();
        return this.rules;
    }

    async getRule(id: string): Promise<AutomationRule | undefined> {
        await this.loadRules();
        return this.rules.find(r => r.id === id);
    }

    async createRule(rule: Partial<AutomationRule>): Promise<AutomationRule> {
        await this.loadRules();
        const newRule: AutomationRule = {
            id: `auto_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`,
            name: rule.name || 'Nouvelle automatisation',
            description: rule.description || '',
            type: rule.type || 'NOTIFICATION',
            enabled: rule.enabled ?? true,
            priority: rule.priority || 3,
            trigger: rule.trigger || { event: 'STAGE_CHANGE' },
            action: rule.action || { type: 'INTERNAL_ALERT', alertLevel: 'INFO' },
            createdAt: new Date().toISOString(),
            updatedAt: new Date().toISOString(),
            createdBy: rule.createdBy,
            executionCount: 0,
        };
        this.rules.push(newRule);
        await this.saveRules();
        return newRule;
    }

    async updateRule(id: string, updates: Partial<AutomationRule>): Promise<AutomationRule | null> {
        await this.loadRules();
        const idx = this.rules.findIndex(r => r.id === id);
        if (idx === -1) return null;
        this.rules[idx] = {
            ...this.rules[idx],
            ...updates,
            id: this.rules[idx].id,
            updatedAt: new Date().toISOString(),
        };
        await this.saveRules();
        return this.rules[idx];
    }

    async deleteRule(id: string): Promise<boolean> {
        await this.loadRules();
        const defaultRule = this.rules.find(r => r.id === id && r.id.startsWith('default_'));
        if (defaultRule) {
            defaultRule.enabled = false;
            defaultRule.updatedAt = new Date().toISOString();
            await this.saveRules();
            return true;
        }
        const len = this.rules.length;
        this.rules = this.rules.filter(r => r.id !== id);
        await this.saveRules();
        return this.rules.length < len;
    }

    async toggleRule(id: string): Promise<AutomationRule | null> {
        await this.loadRules();
        const rule = this.rules.find(r => r.id === id);
        if (!rule) return null;
        rule.enabled = !rule.enabled;
        rule.updatedAt = new Date().toISOString();
        await this.saveRules();
        return rule;
    }

    // ═══════════════════════════════════════════════
    // LOGS D'EXÉCUTION
    // ═══════════════════════════════════════════════

    async getLogs(limit = 100): Promise<AutomationLog[]> {
        await this.loadRules();
        return this.executionLogs.slice(-limit).reverse();
    }

    private addLog(log: Omit<AutomationLog, 'id' | 'executedAt'>): void {
        this.executionLogs.push({
            ...log,
            id: `log_${Date.now()}_${Math.random().toString(36).slice(2, 6)}`,
            executedAt: new Date().toISOString(),
        });
        if (this.executionLogs.length > 500) {
            this.executionLogs = this.executionLogs.slice(-500);
        }
    }

    // ═══════════════════════════════════════════════
    // EXÉCUTION : CHANGEMENT D'ÉTAPE
    // ═══════════════════════════════════════════════

    async onStageChange(lead: any, oldStage: string, newStage: string): Promise<void> {
        await this.loadRules();
        const applicable = this.rules.filter(r =>
            r.enabled &&
            r.trigger.event === 'STAGE_CHANGE' &&
            this.matchesStage(r, oldStage, newStage) &&
            this.matchesCategory(r, lead) &&
            this.canExecute(r, lead.id)
        );
        applicable.sort((a, b) => a.priority - b.priority);
        for (const rule of applicable) {
            await this.executeAction(rule, lead);
        }
        if (applicable.length > 0) await this.saveRules();
    }

    async onDocsComplete(lead: any): Promise<void> {
        await this.loadRules();
        const applicable = this.rules.filter(r =>
            r.enabled &&
            r.trigger.event === 'DOCS_COMPLETE' &&
            this.matchesCategory(r, lead) &&
            this.canExecute(r, lead.id)
        );
        for (const rule of applicable) {
            await this.executeAction(rule, lead);
        }
        if (applicable.length > 0) await this.saveRules();
    }

    async onPaymentReceived(lead: any): Promise<void> {
        await this.loadRules();
        const applicable = this.rules.filter(r =>
            r.enabled &&
            r.trigger.event === 'PAYMENT_RECEIVED' &&
            this.matchesCategory(r, lead) &&
            this.canExecute(r, lead.id)
        );
        for (const rule of applicable) {
            await this.executeAction(rule, lead);
        }
        if (applicable.length > 0) await this.saveRules();
    }

    // ═══════════════════════════════════════════════
    // CRON : RELANCES & ESCALADES AUTOMATIQUES
    // ═══════════════════════════════════════════════

    @Cron(CronExpression.EVERY_HOUR)
    async checkStaleLeads(): Promise<{ processed: number; actions: number }> {
        await this.loadRules();
        const staleRules = this.rules.filter(r => r.enabled && r.trigger.event === 'STALE_DAYS');
        if (staleRules.length === 0) return { processed: 0, actions: 0 };

        const leads = await this.prisma.lead.findMany({
            where: { status: { notIn: ['DONE', 'ARCHIVED', 'CANCELLED'] as any[] } },
        });

        let actionsExecuted = 0;
        for (const lead of leads) {
            const daysSinceUpdate = this.daysSince(lead.updatedAt);
            for (const rule of staleRules) {
                if (
                    rule.trigger.staleDays &&
                    daysSinceUpdate >= rule.trigger.staleDays &&
                    this.matchesStageForStale(rule, lead.status) &&
                    this.matchesCategory(rule, lead) &&
                    this.canExecute(rule, lead.id)
                ) {
                    await this.executeAction(rule, lead);
                    actionsExecuted++;
                }
            }
        }

        if (actionsExecuted > 0) {
            await this.saveRules();
            console.log(`[PipelineAutomation] CRON: ${actionsExecuted} actions sur ${leads.length} dossiers`);
        }
        return { processed: leads.length, actions: actionsExecuted };
    }

    // ═══════════════════════════════════════════════
    // EXÉCUTION D'UNE ACTION
    // ═══════════════════════════════════════════════

    private async executeAction(rule: AutomationRule, lead: any): Promise<void> {
        const trackerKey = `${rule.id}_${lead.id}`;
        try {
            const message = this.interpolateMessage(rule.action.message || '', lead, rule);

            switch (rule.action.type) {
                case 'WHATSAPP':
                    if (lead.phone) {
                        await this.notifications.sendWhatsApp(
                            lead.phone,
                            rule.action.template || 'generic_notification',
                            { name: lead.name || 'Client', message },
                        );
                    }
                    break;

                case 'EMAIL':
                    if (lead.email) {
                        await this.notifications.sendEmail(
                            lead.email,
                            `Simulegal — ${rule.name}`,
                            message,
                        );
                    }
                    break;

                case 'MOVE_STAGE':
                    if (rule.action.targetStage) {
                        await this.prisma.lead.update({
                            where: { id: lead.id },
                            data: { status: rule.action.targetStage as any },
                        });
                    }
                    break;

                case 'INTERNAL_ALERT':
                    console.log(`[ALERTE ${rule.action.alertLevel || 'INFO'}] ${rule.name}: ${lead.name} (${lead.id}) — ${message}`);
                    break;

                case 'ASSIGN_USER':
                    console.log(`[ASSIGN] ${rule.name}: ${lead.name} → ${rule.action.assignTo}`);
                    break;
            }

            // Track execution
            const tracker = this.executionTracker.get(trackerKey) || { count: 0, lastExecution: new Date(0) };
            tracker.count++;
            tracker.lastExecution = new Date();
            this.executionTracker.set(trackerKey, tracker);
            rule.executionCount = (rule.executionCount || 0) + 1;

            this.addLog({
                ruleId: rule.id,
                ruleName: rule.name,
                leadId: lead.id,
                leadName: lead.name || lead.email || 'Inconnu',
                action: `${rule.action.type}${rule.action.targetStage ? ` → ${rule.action.targetStage}` : ''}`,
                result: 'SUCCESS',
                details: message.substring(0, 200),
            });
        } catch (err: any) {
            this.addLog({
                ruleId: rule.id,
                ruleName: rule.name,
                leadId: lead.id,
                leadName: lead.name || 'Inconnu',
                action: rule.action.type,
                result: 'FAILED',
                details: err?.message || 'Erreur inconnue',
            });
        }
    }

    // ═══════════════════════════════════════════════
    // HELPERS
    // ═══════════════════════════════════════════════

    private matchesStage(rule: AutomationRule, oldStage: string, newStage: string): boolean {
        if (rule.trigger.fromStage && rule.trigger.fromStage !== '*' && rule.trigger.fromStage !== oldStage) return false;
        if (rule.trigger.toStage && rule.trigger.toStage !== '*' && rule.trigger.toStage !== newStage) return false;
        if (!rule.trigger.fromStage && !rule.trigger.toStage && rule.trigger.stage) {
            return rule.trigger.stage === '*' || rule.trigger.stage === newStage;
        }
        return true;
    }

    private matchesStageForStale(rule: AutomationRule, currentStage: string): boolean {
        if (!rule.trigger.stage || rule.trigger.stage === '*') return true;
        return rule.trigger.stage === currentStage;
    }

    private matchesCategory(rule: AutomationRule, lead: any): boolean {
        if (!rule.trigger.categories || rule.trigger.categories.length === 0) return true;
        const leadCategory = lead.serviceCategory || lead.category || '';
        return rule.trigger.categories.includes(leadCategory);
    }

    private canExecute(rule: AutomationRule, leadId: string): boolean {
        const trackerKey = `${rule.id}_${leadId}`;
        const tracker = this.executionTracker.get(trackerKey);
        if (!tracker) return true;
        if (rule.action.maxExecutions && rule.action.maxExecutions > 0 && tracker.count >= rule.action.maxExecutions) {
            return false;
        }
        if (rule.action.cooldownHours && rule.action.cooldownHours > 0) {
            const hoursSinceLast = (Date.now() - tracker.lastExecution.getTime()) / (1000 * 60 * 60);
            if (hoursSinceLast < rule.action.cooldownHours) return false;
        }
        return true;
    }

    private daysSince(date: Date): number {
        return Math.floor((Date.now() - new Date(date).getTime()) / (1000 * 60 * 60 * 24));
    }

    private interpolateMessage(template: string, lead: any, rule: AutomationRule): string {
        const days = String(this.daysSince(lead.updatedAt || new Date()));
        return template
            .replace(/\{\{name\}\}/g, lead.name || 'Client')
            .replace(/\{\{stage\}\}/g, lead.status || '')
            .replace(/\{\{days\}\}/g, days)
            .replace(/\{\{service\}\}/g, lead.serviceName || lead.serviceId || '')
            .replace(/\{\{email\}\}/g, lead.email || '')
            .replace(/\{\{phone\}\}/g, lead.phone || '')
            .replace(/\{\{id\}\}/g, lead.id || '');
    }

    // ═══════════════════════════════════════════════
    // STATISTIQUES
    // ═══════════════════════════════════════════════

    async getStats(): Promise<any> {
        await this.loadRules();
        const byType: Record<string, number> = {};
        let totalExec = 0;
        for (const rule of this.rules) {
            byType[rule.type] = (byType[rule.type] || 0) + 1;
            totalExec += rule.executionCount || 0;
        }
        return {
            totalRules: this.rules.length,
            activeRules: this.rules.filter(r => r.enabled).length,
            totalExecutions: totalExec,
            byType,
            recentLogs: this.executionLogs.slice(-20).reverse(),
        };
    }

    // ═══════════════════════════════════════════════
    // RÈGLES PAR DÉFAUT
    // ═══════════════════════════════════════════════

    private getDefaultRules(): AutomationRule[] {
        const now = new Date().toISOString();
        return [
            // ── NOTIFICATIONS SUR CHANGEMENT D'ÉTAPE ──
            {
                id: 'default_notif_paid', name: 'Notification paiement reçu',
                description: 'Envoie un WhatsApp au client quand son paiement est enregistré',
                type: 'NOTIFICATION', enabled: true, priority: 1,
                trigger: { event: 'STAGE_CHANGE', toStage: 'PAID' },
                action: { type: 'WHATSAPP', template: 'payment_confirmation', message: '✅ {{name}}, votre paiement a bien été reçu ! Votre dossier {{service}} est maintenant pris en charge. Prochaine étape : collecte de vos documents.', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_notif_hunting', name: 'Notification hunting RDV activé',
                description: 'Informe le client que la recherche de RDV préfecture est activée',
                type: 'NOTIFICATION', enabled: true, priority: 1,
                trigger: { event: 'STAGE_CHANGE', toStage: 'HUNTING' },
                action: { type: 'WHATSAPP', template: 'hunting_start', message: '⚡ {{name}}, la recherche de RDV est activée pour votre dossier {{service}} ! Nous surveillons les créneaux de préfecture 24h/24.', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_notif_booked', name: 'Notification RDV réservé',
                description: 'Informe le client que son RDV en préfecture est réservé',
                type: 'NOTIFICATION', enabled: true, priority: 1,
                trigger: { event: 'STAGE_CHANGE', toStage: 'BOOKED' },
                action: { type: 'WHATSAPP', template: 'booking_success', message: '🎉 {{name}}, votre RDV préfecture est RÉSERVÉ ! Consultez les détails dans votre espace client.', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_notif_ofii', name: 'Notification enquête OFII',
                description: 'Alerte le client sur l\'étape enquête OFII (logement + visite médicale)',
                type: 'NOTIFICATION', enabled: true, priority: 1,
                trigger: { event: 'STAGE_CHANGE', toStage: 'OFII_INVESTIGATION' },
                action: { type: 'WHATSAPP', template: 'coach_ofii_alert', message: '⚠️ {{name}}, étape importante : Enquête OFII. Préparez votre logement et planifiez la visite médicale. Guide : simulegal.fr/guide-ofii', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_notif_done', name: 'Notification dossier terminé',
                description: 'Félicite le client à la clôture de son dossier',
                type: 'NOTIFICATION', enabled: true, priority: 1,
                trigger: { event: 'STAGE_CHANGE', toStage: 'DONE' },
                action: { type: 'WHATSAPP', template: 'case_completed', message: '🎊 Félicitations {{name}} ! Votre dossier {{service}} est terminé avec succès. Merci de votre confiance !', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },

            // ── RELANCES AUTOMATIQUES ──
            {
                id: 'default_relance_docs_7j', name: 'Relance documents J+7',
                description: 'Relance le client après 7 jours en collecte de documents',
                type: 'RELANCE', enabled: true, priority: 2,
                trigger: { event: 'STALE_DAYS', stage: 'COLLECTING', staleDays: 7 },
                action: { type: 'WHATSAPP', message: '📋 {{name}}, votre dossier {{service}} attend vos documents depuis {{days}} jours. Envoyez-les via votre espace client pour avancer rapidement !', maxExecutions: 3, cooldownHours: 168 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_relance_docs_14j', name: 'Relance documents J+14 (urgente)',
                description: 'Relance urgente après 14 jours sans documents',
                type: 'RELANCE', enabled: true, priority: 2,
                trigger: { event: 'STALE_DAYS', stage: 'COLLECTING', staleDays: 14 },
                action: { type: 'WHATSAPP', message: '⚠️ {{name}}, cela fait {{days}} jours que nous attendons vos documents pour {{service}}. Sans action de votre part, votre dossier risque d\'être suspendu.', maxExecutions: 2, cooldownHours: 168 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_relance_nouveau_3j', name: 'Relance nouveau dossier J+3',
                description: 'Relance les dossiers "Nouveau" non payés après 3 jours',
                type: 'RELANCE', enabled: true, priority: 2,
                trigger: { event: 'STALE_DAYS', stage: 'NEW', staleDays: 3 },
                action: { type: 'WHATSAPP', message: '👋 {{name}}, vous avez un dossier {{service}} en attente de paiement. Finalisez votre commande pour démarrer votre procédure !', maxExecutions: 2, cooldownHours: 72 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },

            // ── ESCALADES ──
            {
                id: 'default_escalade_30j', name: 'Escalade dossier inactif 30 jours',
                description: 'Alerte le manager quand un dossier stagne depuis 30 jours',
                type: 'ESCALADE', enabled: true, priority: 3,
                trigger: { event: 'STALE_DAYS', stage: '*', staleDays: 30 },
                action: { type: 'INTERNAL_ALERT', alertLevel: 'WARNING', message: '⚠️ ESCALADE: Le dossier de {{name}} ({{service}}) stagne depuis {{days}} jours en étape "{{stage}}". Action requise.', maxExecutions: 1, cooldownHours: 720 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_escalade_decision_90j', name: 'Escalade attente décision 90 jours',
                description: 'Alerte critique quand l\'attente de décision dépasse 90 jours',
                type: 'ESCALADE', enabled: true, priority: 3,
                trigger: { event: 'STALE_DAYS', stage: 'DECISION_WAIT', staleDays: 90 },
                action: { type: 'INTERNAL_ALERT', alertLevel: 'CRITICAL', message: '🔴 CRITIQUE: {{name}} attend une décision depuis {{days}} jours pour {{service}}. Envisager un recours.', maxExecutions: 1, cooldownHours: 720 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },

            // ── TRANSITIONS AUTOMATIQUES ──
            {
                id: 'default_auto_transition_docs', name: 'Auto-transition : documents complets',
                description: 'Quand tous les documents sont reçus, passe à "Vérification juriste"',
                type: 'AUTO_TRANSITION', enabled: true, priority: 1,
                trigger: { event: 'DOCS_COMPLETE' },
                action: { type: 'MOVE_STAGE', targetStage: 'REVIEW', message: 'Documents complets → vérification juriste automatique', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
            {
                id: 'default_auto_transition_payment', name: 'Auto-transition : paiement reçu',
                description: 'Quand le paiement est confirmé, passe de "Nouveau" à "Payé"',
                type: 'AUTO_TRANSITION', enabled: true, priority: 1,
                trigger: { event: 'PAYMENT_RECEIVED' },
                action: { type: 'MOVE_STAGE', targetStage: 'PAID', message: 'Paiement reçu → passage automatique à "Payé"', maxExecutions: 1, cooldownHours: 0 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },

            // ── ALERTES INTERNES ──
            {
                id: 'default_alerte_rappel_qualification', name: 'Alerte qualification sans suite J+5',
                description: 'Alerte quand un lead "À rappeler" n\'a pas été recontacté en 5 jours',
                type: 'ALERTE_INTERNE', enabled: true, priority: 4,
                trigger: { event: 'STALE_DAYS', stage: 'CALLBACK', staleDays: 5 },
                action: { type: 'INTERNAL_ALERT', alertLevel: 'WARNING', message: '📞 Lead {{name}} en attente de rappel depuis {{days}} jours. Risque de perte.', maxExecutions: 1, cooldownHours: 120 },
                createdAt: now, updatedAt: now, executionCount: 0,
            },
        ];
    }
}
