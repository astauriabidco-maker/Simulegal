import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

/**
 * SalesTrackingService — Module Suivi Commercial
 *
 * Gère :
 * - Objectifs commerciaux (mensuels/trimestriels) par commercial
 * - Journal d'activité (appels, RDV, conversions)
 * - Commissions individuelles
 * - Classement / Leaderboard
 * - Récapitulatif de performance
 */
@Injectable()
export class SalesTrackingService {
    private readonly logger = new Logger(SalesTrackingService.name);

    constructor(private prisma: PrismaService) { }

    // ═══════════════════════════════════════════════
    // OBJECTIFS COMMERCIAUX
    // ═══════════════════════════════════════════════

    /**
     * Définir ou mettre à jour un objectif pour un commercial
     */
    async setObjective(data: {
        userId: string;
        agencyId?: string;
        period: string;
        periodType?: string;
        metric: string;
        targetValue: number;
    }) {
        return this.prisma.salesObjective.upsert({
            where: {
                userId_period_metric: {
                    userId: data.userId,
                    period: data.period,
                    metric: data.metric,
                },
            },
            create: {
                userId: data.userId,
                agencyId: data.agencyId,
                period: data.period,
                periodType: data.periodType || 'MONTHLY',
                metric: data.metric,
                targetValue: data.targetValue,
            },
            update: {
                targetValue: data.targetValue,
                agencyId: data.agencyId,
            },
        });
    }

    /**
     * Définir plusieurs objectifs d'un coup pour une équipe
     */
    async setBulkObjectives(objectives: {
        userId: string;
        agencyId?: string;
        period: string;
        periodType?: string;
        metric: string;
        targetValue: number;
    }[]) {
        const results = await Promise.allSettled(
            objectives.map(obj => this.setObjective(obj))
        );
        const succeeded = results.filter(r => r.status === 'fulfilled').length;
        const failed = results.filter(r => r.status === 'rejected').length;
        return { succeeded, failed, total: objectives.length };
    }

    /**
     * Recalculer la valeur actuelle d'un objectif à partir des données réelles
     */
    async recalculateObjective(objectiveId: string) {
        const obj = await this.prisma.salesObjective.findUnique({ where: { id: objectiveId } });
        if (!obj) return null;

        const [startDate, endDate] = this.getPeriodDates(obj.period, obj.periodType);
        let currentValue = 0;

        switch (obj.metric) {
            case 'CALLS':
                currentValue = await this.prisma.salesActivity.count({
                    where: {
                        userId: obj.userId,
                        activityType: 'CALL',
                        date: { gte: startDate, lte: endDate },
                    },
                });
                break;

            case 'MEETINGS_BOOKED':
                currentValue = await this.prisma.salesActivity.count({
                    where: {
                        userId: obj.userId,
                        activityType: 'MEETING',
                        date: { gte: startDate, lte: endDate },
                    },
                });
                break;

            case 'CONVERSIONS':
                currentValue = await this.prisma.salesActivity.count({
                    where: {
                        userId: obj.userId,
                        activityType: 'CONVERSION',
                        date: { gte: startDate, lte: endDate },
                    },
                });
                break;

            case 'QUALIFIED':
                currentValue = await this.prisma.salesActivity.count({
                    where: {
                        userId: obj.userId,
                        activityType: 'QUALIFICATION',
                        date: { gte: startDate, lte: endDate },
                    },
                });
                break;

            case 'REVENUE':
                const commissions = await this.prisma.salesCommission.findMany({
                    where: {
                        userId: obj.userId,
                        period: obj.period,
                        status: { in: ['APPROVED', 'PAID'] },
                    },
                });
                currentValue = Math.round(commissions.reduce((sum, c) => sum + c.baseAmount, 0));
                break;
        }

        return this.prisma.salesObjective.update({
            where: { id: objectiveId },
            data: { currentValue },
        });
    }

    /**
     * Recalculer tous les objectifs de la période en cours
     */
    async recalculateAllCurrentObjectives() {
        const currentPeriod = this.getCurrentPeriod();
        const objectives = await this.prisma.salesObjective.findMany({
            where: { period: currentPeriod },
        });

        const results = await Promise.allSettled(
            objectives.map(obj => this.recalculateObjective(obj.id))
        );

        this.logger.log(`[SalesTracking] Recalculated ${results.length} objectives for period ${currentPeriod}`);
        return { recalculated: results.length, period: currentPeriod };
    }

    /**
     * Récupérer les objectifs d'un commercial
     */
    async getObjectives(userId: string, period?: string) {
        return this.prisma.salesObjective.findMany({
            where: {
                userId,
                ...(period ? { period } : { period: this.getCurrentPeriod() }),
            },
            orderBy: { metric: 'asc' },
        });
    }

    /**
     * Récupérer les objectifs de toute une agence
     */
    async getAgencyObjectives(agencyId: string, period?: string) {
        return this.prisma.salesObjective.findMany({
            where: {
                agencyId,
                ...(period ? { period } : { period: this.getCurrentPeriod() }),
            },
            orderBy: [{ userId: 'asc' }, { metric: 'asc' }],
        });
    }

    // ═══════════════════════════════════════════════
    // JOURNAL D'ACTIVITÉ
    // ═══════════════════════════════════════════════

    /**
     * Enregistrer une activité commerciale
     */
    async logActivity(data: {
        userId: string;
        agencyId?: string;
        activityType: string;
        prospectId?: string;
        prospectName?: string;
        outcome?: string;
        duration?: number;
        notes?: string;
        metadata?: any;
    }) {
        const activity = await this.prisma.salesActivity.create({
            data: {
                userId: data.userId,
                agencyId: data.agencyId,
                activityType: data.activityType,
                prospectId: data.prospectId,
                prospectName: data.prospectName,
                outcome: data.outcome,
                duration: data.duration,
                notes: data.notes,
                metadata: data.metadata || undefined,
            },
        });

        // Auto-increment objectif correspondant
        try {
            const currentPeriod = this.getCurrentPeriod();
            const metricMap: Record<string, string> = {
                'CALL': 'CALLS',
                'MEETING': 'MEETINGS_BOOKED',
                'CONVERSION': 'CONVERSIONS',
                'QUALIFICATION': 'QUALIFIED',
            };
            const metric = metricMap[data.activityType];
            if (metric) {
                await this.prisma.salesObjective.updateMany({
                    where: {
                        userId: data.userId,
                        period: currentPeriod,
                        metric,
                    },
                    data: { currentValue: { increment: 1 } },
                });
            }
        } catch { /* pas d'objectif défini — on ignore */ }

        return activity;
    }

    /**
     * Récupérer le journal d'activité d'un commercial
     */
    async getActivities(userId: string, options?: {
        dateFrom?: string;
        dateTo?: string;
        activityType?: string;
        limit?: number;
    }) {
        return this.prisma.salesActivity.findMany({
            where: {
                userId,
                ...(options?.activityType ? { activityType: options.activityType } : {}),
                ...(options?.dateFrom || options?.dateTo ? {
                    date: {
                        ...(options?.dateFrom ? { gte: new Date(options.dateFrom) } : {}),
                        ...(options?.dateTo ? { lte: new Date(options.dateTo) } : {}),
                    },
                } : {}),
            },
            orderBy: { date: 'desc' },
            take: options?.limit || 100,
        });
    }

    /**
     * Résumé d'activité quotidien pour un commercial
     */
    async getDailySummary(userId: string, date?: string) {
        const targetDate = date ? new Date(date) : new Date();
        const startOfDay = new Date(targetDate);
        startOfDay.setHours(0, 0, 0, 0);
        const endOfDay = new Date(targetDate);
        endOfDay.setHours(23, 59, 59, 999);

        const activities = await this.prisma.salesActivity.findMany({
            where: {
                userId,
                date: { gte: startOfDay, lte: endOfDay },
            },
        });

        const summary: Record<string, number> = {};
        let totalDuration = 0;

        for (const a of activities) {
            summary[a.activityType] = (summary[a.activityType] || 0) + 1;
            if (a.duration) totalDuration += a.duration;
        }

        return {
            date: targetDate.toISOString().split('T')[0],
            userId,
            totalActivities: activities.length,
            breakdown: summary,
            totalCallDuration: totalDuration,
            activities,
        };
    }

    // ═══════════════════════════════════════════════
    // COMMISSIONS
    // ═══════════════════════════════════════════════

    /**
     * Créer une commission pour un commercial
     */
    async createCommission(data: {
        userId: string;
        agencyId?: string;
        prospectId?: string;
        prospectName?: string;
        serviceId?: string;
        serviceName?: string;
        baseAmount: number;
        rate: number;
    }) {
        const amountEuros = (data.baseAmount * data.rate) / 100;
        const period = this.getCurrentPeriod();

        return this.prisma.salesCommission.create({
            data: {
                userId: data.userId,
                agencyId: data.agencyId,
                prospectId: data.prospectId,
                prospectName: data.prospectName,
                serviceId: data.serviceId,
                serviceName: data.serviceName,
                baseAmount: data.baseAmount,
                rate: data.rate,
                amountEuros,
                period,
            },
        });
    }

    /**
     * Récupérer les commissions d'un commercial
     */
    async getCommissions(userId: string, options?: {
        period?: string;
        status?: string;
    }) {
        return this.prisma.salesCommission.findMany({
            where: {
                userId,
                ...(options?.period ? { period: options.period } : {}),
                ...(options?.status ? { status: options.status } : {}),
            },
            orderBy: { createdAt: 'desc' },
        });
    }

    /**
     * Approuver / Payer / Annuler une commission
     */
    async updateCommissionStatus(commissionId: string, status: 'APPROVED' | 'PAID' | 'CANCELLED') {
        return this.prisma.salesCommission.update({
            where: { id: commissionId },
            data: {
                status,
                ...(status === 'PAID' ? { paidAt: new Date() } : {}),
            },
        });
    }

    /**
     * Récapitulatif des commissions pour une période
     */
    async getCommissionSummary(options?: { period?: string; agencyId?: string }) {
        const period = options?.period || this.getCurrentPeriod();

        const commissions = await this.prisma.salesCommission.findMany({
            where: {
                period,
                ...(options?.agencyId ? { agencyId: options.agencyId } : {}),
            },
        });

        const byUser: Record<string, { total: number; pending: number; approved: number; paid: number; count: number }> = {};

        for (const c of commissions) {
            if (!byUser[c.userId]) {
                byUser[c.userId] = { total: 0, pending: 0, approved: 0, paid: 0, count: 0 };
            }
            byUser[c.userId].total += c.amountEuros;
            byUser[c.userId].count++;
            if (c.status === 'PENDING') byUser[c.userId].pending += c.amountEuros;
            if (c.status === 'APPROVED') byUser[c.userId].approved += c.amountEuros;
            if (c.status === 'PAID') byUser[c.userId].paid += c.amountEuros;
        }

        return {
            period,
            totalCommissions: commissions.reduce((s, c) => s + c.amountEuros, 0),
            totalPending: commissions.filter(c => c.status === 'PENDING').reduce((s, c) => s + c.amountEuros, 0),
            totalApproved: commissions.filter(c => c.status === 'APPROVED').reduce((s, c) => s + c.amountEuros, 0),
            totalPaid: commissions.filter(c => c.status === 'PAID').reduce((s, c) => s + c.amountEuros, 0),
            byUser,
        };
    }

    // ═══════════════════════════════════════════════
    // LEADERBOARD / CLASSEMENT
    // ═══════════════════════════════════════════════

    /**
     * Classement des commerciaux sur la période en cours
     */
    async getLeaderboard(options?: {
        period?: string;
        agencyId?: string;
        metric?: string; // CALLS, CONVERSIONS, REVENUE
    }) {
        const period = options?.period || this.getCurrentPeriod();
        const [startDate, endDate] = this.getPeriodDates(period, 'MONTHLY');

        // 1. Récupérer les users commerciaux
        const users = await this.prisma.user.findMany({
            where: {
                isActive: true,
                ...(options?.agencyId ? { agencyId: options.agencyId } : {}),
            },
            select: { id: true, name: true, email: true, agencyId: true },
        });

        // 2. Pour chaque commercial, calculer les métriques
        const leaderboard = await Promise.all(users.map(async (user) => {
            const [
                callCount,
                meetingCount,
                conversionCount,
                qualifiedCount,
                activities,
                commissions,
            ] = await Promise.all([
                this.prisma.salesActivity.count({
                    where: { userId: user.id, activityType: 'CALL', date: { gte: startDate, lte: endDate } },
                }),
                this.prisma.salesActivity.count({
                    where: { userId: user.id, activityType: 'MEETING', date: { gte: startDate, lte: endDate } },
                }),
                this.prisma.salesActivity.count({
                    where: { userId: user.id, activityType: 'CONVERSION', date: { gte: startDate, lte: endDate } },
                }),
                this.prisma.salesActivity.count({
                    where: { userId: user.id, activityType: 'QUALIFICATION', date: { gte: startDate, lte: endDate } },
                }),
                this.prisma.salesActivity.count({
                    where: { userId: user.id, date: { gte: startDate, lte: endDate } },
                }),
                this.prisma.salesCommission.findMany({
                    where: { userId: user.id, period, status: { in: ['APPROVED', 'PAID'] } },
                }),
            ]);

            const revenue = commissions.reduce((s, c) => s + c.baseAmount, 0);
            const commissionsEarned = commissions.reduce((s, c) => s + c.amountEuros, 0);

            // Score composite : pondéré par les conversions (x10) + RDV (x5) + appels (x1)
            const compositeScore = conversionCount * 10 + meetingCount * 5 + qualifiedCount * 3 + callCount;

            // Récupérer les objectifs
            const objectives = await this.prisma.salesObjective.findMany({
                where: { userId: user.id, period },
            });

            const objectiveCompletion = objectives.length > 0
                ? Math.round(objectives.reduce((s, o) => s + Math.min(100, (o.currentValue / Math.max(1, o.targetValue)) * 100), 0) / objectives.length)
                : null;

            return {
                user: { id: user.id, name: user.name, email: user.email, agencyId: user.agencyId },
                metrics: {
                    calls: callCount,
                    meetings: meetingCount,
                    conversions: conversionCount,
                    qualified: qualifiedCount,
                    totalActivities: activities,
                    revenue: Math.round(revenue),
                    commissionsEarned: Math.round(commissionsEarned * 100) / 100,
                },
                compositeScore,
                objectiveCompletion,
                objectives: objectives.map(o => ({
                    metric: o.metric,
                    target: o.targetValue,
                    current: o.currentValue,
                    progress: Math.round((o.currentValue / Math.max(1, o.targetValue)) * 100),
                })),
            };
        }));

        // 3. Trier selon la métrique demandée
        const sortKey = options?.metric === 'REVENUE' ? 'revenue'
            : options?.metric === 'CONVERSIONS' ? 'conversions'
                : options?.metric === 'CALLS' ? 'calls'
                    : 'compositeScore';

        if (sortKey === 'compositeScore') {
            leaderboard.sort((a, b) => b.compositeScore - a.compositeScore);
        } else {
            leaderboard.sort((a, b) => (b.metrics as any)[sortKey] - (a.metrics as any)[sortKey]);
        }

        // 4. Ajouter le rang
        return leaderboard.map((entry, i) => ({ rank: i + 1, ...entry }));
    }

    // ═══════════════════════════════════════════════
    // PERFORMANCE INDIVIDUELLE
    // ═══════════════════════════════════════════════

    /**
     * Dashboard de performance d'un commercial
     */
    async getUserPerformance(userId: string, period?: string) {
        const currentPeriod = period || this.getCurrentPeriod();
        const [startDate, endDate] = this.getPeriodDates(currentPeriod, 'MONTHLY');

        // Activités
        const activities = await this.prisma.salesActivity.findMany({
            where: { userId, date: { gte: startDate, lte: endDate } },
            orderBy: { date: 'desc' },
        });

        // Objectifs
        const objectives = await this.prisma.salesObjective.findMany({
            where: { userId, period: currentPeriod },
        });

        // Commissions
        const commissions = await this.prisma.salesCommission.findMany({
            where: { userId, period: currentPeriod },
        });

        // Agrégations
        const activityBreakdown: Record<string, number> = {};
        let totalCallDuration = 0;
        const dailyActivity: Record<string, number> = {};

        for (const a of activities) {
            activityBreakdown[a.activityType] = (activityBreakdown[a.activityType] || 0) + 1;
            if (a.duration) totalCallDuration += a.duration;
            const day = a.date.toISOString().split('T')[0];
            dailyActivity[day] = (dailyActivity[day] || 0) + 1;
        }

        // Prospects assignés
        const prospectCounts = await this.prisma.prospect.groupBy({
            by: ['status'],
            where: { assignedToSalesId: userId },
            _count: { status: true },
        });

        return {
            period: currentPeriod,
            userId,
            summary: {
                totalActivities: activities.length,
                totalCallDuration,
                avgCallDuration: activities.filter(a => a.activityType === 'CALL' && a.duration).length > 0
                    ? Math.round(totalCallDuration / activities.filter(a => a.activityType === 'CALL' && a.duration).length)
                    : 0,
                activityBreakdown,
            },
            objectives: objectives.map(o => ({
                metric: o.metric,
                target: o.targetValue,
                current: o.currentValue,
                progress: Math.round((o.currentValue / Math.max(1, o.targetValue)) * 100),
                onTrack: this.isOnTrack(o),
            })),
            commissions: {
                total: commissions.reduce((s, c) => s + c.amountEuros, 0),
                pending: commissions.filter(c => c.status === 'PENDING').reduce((s, c) => s + c.amountEuros, 0),
                approved: commissions.filter(c => c.status === 'APPROVED').reduce((s, c) => s + c.amountEuros, 0),
                paid: commissions.filter(c => c.status === 'PAID').reduce((s, c) => s + c.amountEuros, 0),
                details: commissions,
            },
            dailyActivity: Object.entries(dailyActivity).map(([d, count]) => ({ date: d, count })).sort((a, b) => a.date.localeCompare(b.date)),
            prospectPortfolio: prospectCounts.map(p => ({ status: p.status, count: p._count.status })),
            recentActivities: activities.slice(0, 20),
        };
    }

    // ═══════════════════════════════════════════════
    // HELPERS
    // ═══════════════════════════════════════════════

    private getCurrentPeriod(): string {
        const now = new Date();
        return `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}`;
    }

    private getPeriodDates(period: string, periodType: string): [Date, Date] {
        if (periodType === 'QUARTERLY') {
            const [year, quarter] = period.split('-Q');
            const q = parseInt(quarter);
            const startMonth = (q - 1) * 3;
            return [
                new Date(parseInt(year), startMonth, 1),
                new Date(parseInt(year), startMonth + 3, 0, 23, 59, 59),
            ];
        }

        if (periodType === 'YEARLY') {
            const year = parseInt(period);
            return [new Date(year, 0, 1), new Date(year, 11, 31, 23, 59, 59)];
        }

        // MONTHLY (default)
        const [year, month] = period.split('-').map(Number);
        return [
            new Date(year, month - 1, 1),
            new Date(year, month, 0, 23, 59, 59),
        ];
    }

    /**
     * Vérifie si un objectif est "on track" (en bonne voie)
     * basé sur la progression temporelle de la période
     */
    private isOnTrack(objective: any): boolean {
        const now = new Date();
        const [startDate, endDate] = this.getPeriodDates(objective.period, objective.periodType);
        const totalDays = (endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24);
        const elapsedDays = (now.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24);
        const expectedProgress = elapsedDays / totalDays;
        const actualProgress = objective.currentValue / Math.max(1, objective.targetValue);
        return actualProgress >= expectedProgress * 0.8; // 80% du rythme attendu = on track
    }
}
