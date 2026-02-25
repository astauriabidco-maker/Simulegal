import { Injectable, Logger } from '@nestjs/common';
import { Cron, CronExpression } from '@nestjs/schedule';
import { PrismaService } from '../prisma/prisma.service';
import { ProspectStatus } from '@prisma/client';

/**
 * ═══════════════════════════════════════════════════════════
 * ProspectPipelineService
 * ═══════════════════════════════════════════════════════════
 *
 * Gère les transitions automatiques du pipeline de qualification :
 *
 *   🟡 NEW → 🟣 CONTACTED → 📅 MEETING_BOOKED → ✅ SIGNED
 *                                       ↓
 *                                   🚫 NO_SHOW
 *                                       ↓
 *                                   ⚫ LOST
 *
 * Règles d'automatisation :
 *   1. CallLog créé           → NEW → CONTACTED
 *   2. Appointment créé       → * → MEETING_BOOKED
 *   3. RDV passé + 24h        → MEETING_BOOKED → NO_SHOW  (CRON)
 *   4. Nouveau RDV après NS   → NO_SHOW → MEETING_BOOKED
 *   5. 3ème no-show           → NO_SHOW → LOST            (CRON)
 *   6. 30j sans contact       → * → LOST                  (CRON)
 */

// ─── Transition Log ─────────────────────────────────────────
export interface PipelineTransitionLog {
    id: string;
    prospectId: string;
    prospectName: string;
    fromStatus: string;
    toStatus: string;
    trigger: string;
    automated: boolean;
    details?: string;
    createdAt: string;
}

@Injectable()
export class ProspectPipelineService {
    private readonly logger = new Logger('ProspectPipeline');

    constructor(private prisma: PrismaService) { }

    // ═══════════════════════════════════════════════════════════
    // RÈGLE 1 : CallLog créé → NEW → CONTACTED
    // ═══════════════════════════════════════════════════════════

    /**
     * Appelé quand un CallLog est créé pour un prospect.
     * Si le prospect est en NEW, le passe automatiquement en CONTACTED.
     */
    async onCallStarted(prospectId: string): Promise<ProspectStatus | null> {
        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        if (!prospect) return null;

        if (prospect.status === 'NEW') {
            await this.transitionTo(prospectId, 'CONTACTED', 'CALL_STARTED', `Premier appel passé à ${prospect.firstName} ${prospect.lastName}`);
            return 'CONTACTED';
        }

        // Met à jour lastContactAt dans tous les cas
        await this.prisma.prospect.update({
            where: { id: prospectId },
            data: { lastContactAt: new Date() },
        });

        return prospect.status;
    }

    // ===============================================================
    // (REMOVED) : L'étape QUALIFIED a été supprimée du pipeline.
    // La qualification se fait pendant l'appel (checklist cockpit).
    // Le prospect reste en CONTACTED jusqu'à la fixation d'un RDV.
    // ===============================================================

    /**
     * @deprecated L'étape QUALIFIED a été supprimée. Cette méthode est un no-op.
     */
    async checkQualification(prospectId: string): Promise<ProspectStatus | null> {
        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        return prospect?.status ?? null;
    }

    // ═══════════════════════════════════════════════════════════
    // RÈGLE 3 & 5 : Appointment créé → * → MEETING_BOOKED
    // ═══════════════════════════════════════════════════════════

    /**
     * Appelé quand un rendez-vous est fixé pour un prospect.
     * Passe automatiquement en MEETING_BOOKED depuis n'importe quel statut non-terminal.
     */
    async onAppointmentBooked(prospectId: string, appointmentDate: string): Promise<ProspectStatus | null> {
        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        if (!prospect) return null;

        // Ne pas toucher si déjà SIGNED
        if (prospect.status === 'SIGNED') return prospect.status;

        const previousStatus = prospect.status;

        // Règle 5 : NO_SHOW → MEETING_BOOKED (reprogrammation)
        if (previousStatus === 'NO_SHOW') {
            await this.transitionTo(
                prospectId,
                'MEETING_BOOKED',
                'APPOINTMENT_RESCHEDULED',
                `RDV reprogrammé pour le ${new Date(appointmentDate).toLocaleDateString('fr-FR')} (was NO_SHOW)`
            );
            return 'MEETING_BOOKED';
        }

        // Règle 3 : Tout statut non-terminal → MEETING_BOOKED
        if (['NEW', 'CONTACTED', 'QUALIFIED'].includes(previousStatus)) {
            await this.transitionTo(
                prospectId,
                'MEETING_BOOKED',
                'APPOINTMENT_BOOKED',
                `RDV fixé pour le ${new Date(appointmentDate).toLocaleDateString('fr-FR')}`
            );
            return 'MEETING_BOOKED';
        }

        return prospect.status;
    }

    // ═══════════════════════════════════════════════════════════
    // CRON : RÈGLES TEMPORELLES (4, 6, 7)
    // ═══════════════════════════════════════════════════════════

    /**
     * Exécuté toutes les heures.
     * Vérifie :
     *   - Règle 4 : RDV dépassé de +2h sans action → NO_SHOW
     *   - Règle 6 : 3ème no-show consécutif → LOST
     *   - Règle 7 : 30 jours sans contact → LOST
     */
    @Cron(CronExpression.EVERY_HOUR)
    async checkTimedTransitions(): Promise<{ noShows: number; abandoned: number; inactive: number }> {
        const now = new Date();
        let noShows = 0;
        let abandoned = 0;
        let inactive = 0;

        // ─── Règle 4 : Auto NO_SHOW ──────────────────────────
        // Prospects en MEETING_BOOKED depuis plus de 24h
        // (updatedAt sert de proxy — c'est la date de la transition vers MEETING_BOOKED)
        const twentyFourHoursAgo = new Date(now.getTime() - 24 * 60 * 60 * 1000);

        const meetingBookedProspects = await this.prisma.prospect.findMany({
            where: {
                status: 'MEETING_BOOKED',
                updatedAt: { lt: twentyFourHoursAgo },
            },
        });

        for (const prospect of meetingBookedProspects) {
            await this.transitionTo(
                prospect.id,
                'NO_SHOW',
                'AUTO_NO_SHOW',
                `Prospect en MEETING_BOOKED depuis +24h sans confirmation de présence (auto-détecté)`
            );

            // Envoyer SMS de relance
            if (prospect.phone) {
                this.logger.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, nous avons remarqué que vous n'avez pas pu venir. Souhaitez-vous reprogrammer ?"`);
            }

            noShows++;
        }

        // ─── Règle 6 : 3ème no-show → LOST ──────────────────
        const noShowProspects = await this.prisma.prospect.findMany({
            where: { status: 'NO_SHOW' },
        });

        for (const prospect of noShowProspects) {
            const noShowCount = await this.countNoShows(prospect.id);
            if (noShowCount >= 3) {
                await this.transitionTo(
                    prospect.id,
                    'LOST',
                    'AUTO_ABANDON_3_NO_SHOWS',
                    `${noShowCount} no-shows consécutifs → abandon automatique`
                );
                abandoned++;
            }
        }

        // ─── Règle 7 : 30 jours sans contact → LOST ─────────
        const thirtyDaysAgo = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);

        const staleProspects = await this.prisma.prospect.findMany({
            where: {
                status: { in: ['NEW', 'CONTACTED'] },
                lastContactAt: { lt: thirtyDaysAgo },
            },
        });

        // Also catch prospects who were never contacted
        const neverContactedStale = await this.prisma.prospect.findMany({
            where: {
                status: { in: ['NEW', 'CONTACTED'] },
                lastContactAt: null,
                createdAt: { lt: thirtyDaysAgo },
            },
        });

        const allStale = [...staleProspects, ...neverContactedStale];

        for (const prospect of allStale) {
            const lastDate = prospect.lastContactAt || prospect.createdAt;
            const daysSince = Math.floor((now.getTime() - new Date(lastDate).getTime()) / (1000 * 60 * 60 * 24));

            await this.transitionTo(
                prospect.id,
                'LOST',
                'AUTO_ABANDON_INACTIVE',
                `${daysSince} jours sans contact → abandon automatique`
            );
            inactive++;
        }

        // ─── Règle : 5 appels sans réponse consécutifs → LOST ────
        const tooManyNoAnswer = await this.prisma.prospect.findMany({
            where: {
                status: { in: ['NEW', 'CONTACTED'] },
                noAnswerCount: { gte: 5 },
            },
        });

        for (const prospect of tooManyNoAnswer) {
            await this.transitionTo(
                prospect.id,
                'LOST',
                'AUTO_ABANDON_NO_ANSWER',
                `${prospect.noAnswerCount} appels sans réponse consécutifs → abandon automatique`
            );
            inactive++;
        }

        // ─── Règle : 3 demandes de rappel sans suite → LOST ─────
        const tooManyCallbacks = await this.prisma.prospect.findMany({
            where: {
                status: { in: ['NEW', 'CONTACTED'] },
                callbackCount: { gte: 3 },
            },
        });

        for (const prospect of tooManyCallbacks) {
            await this.transitionTo(
                prospect.id,
                'LOST',
                'AUTO_ABANDON_CALLBACKS',
                `${prospect.callbackCount} demandes de rappel sans suite → abandon automatique`
            );
            inactive++;
        }

        // ─── Règle : Rappel demandé il y a +7j sans action → LOST ─
        const sevenDaysAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        const staleCallbacks = await this.prisma.prospect.findMany({
            where: {
                status: { in: ['NEW', 'CONTACTED'] },
                callbackRequestedAt: { lt: sevenDaysAgo },
                lastCallOutcome: 'CALLBACK',
            },
        });

        for (const prospect of staleCallbacks) {
            const daysSinceCallback = Math.floor((now.getTime() - new Date(prospect.callbackRequestedAt!).getTime()) / (1000 * 60 * 60 * 24));
            await this.transitionTo(
                prospect.id,
                'LOST',
                'AUTO_ABANDON_STALE_CALLBACK',
                `Rappel demandé il y a ${daysSinceCallback}j, aucune suite → abandon automatique`
            );
            inactive++;
        }

        if (noShows + abandoned + inactive > 0) {
            this.logger.log(
                `[CRON] Transitions auto: ${noShows} no-shows, ${abandoned} abandons (3x NS), ${inactive} inactifs/épuisés`
            );
        }

        return { noShows, abandoned, inactive };
    }

    // ═══════════════════════════════════════════════════════════
    // TRANSITION CORE
    // ═══════════════════════════════════════════════════════════

    /**
     * Effectue une transition de statut et enregistre un log.
     */
    private async transitionTo(
        prospectId: string,
        newStatus: ProspectStatus,
        trigger: string,
        details: string
    ): Promise<void> {
        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        if (!prospect) return;

        const oldStatus = prospect.status;
        if (oldStatus === newStatus) return; // No-op

        // Validate transition
        if (!this.isValidTransition(oldStatus, newStatus)) {
            this.logger.warn(
                `[BLOCKED] Invalid transition ${oldStatus} → ${newStatus} for ${prospect.firstName} ${prospect.lastName} (trigger: ${trigger})`
            );
            return;
        }

        // Execute transition
        await this.prisma.prospect.update({
            where: { id: prospectId },
            data: {
                status: newStatus,
                lastContactAt: new Date(),
            },
        });

        // Log the transition
        await this.logTransition({
            prospectId,
            prospectName: `${prospect.firstName} ${prospect.lastName}`,
            fromStatus: oldStatus,
            toStatus: newStatus,
            trigger,
            automated: true,
            details,
        });

        this.logger.log(
            `✅ ${prospect.firstName} ${prospect.lastName}: ${oldStatus} → ${newStatus} (${trigger})`
        );
    }

    /**
     * Vérifie si une transition est valide.
     */
    private isValidTransition(from: ProspectStatus, to: ProspectStatus): boolean {
        const VALID_TRANSITIONS: Record<string, string[]> = {
            'NEW': ['CONTACTED', 'MEETING_BOOKED', 'LOST'],
            'CONTACTED': ['QUALIFIED', 'MEETING_BOOKED', 'LOST'],
            'QUALIFIED': ['MEETING_BOOKED', 'LOST'],
            'MEETING_BOOKED': ['SIGNED', 'NO_SHOW', 'LOST'],
            'SIGNED': [], // Terminal — aucune transition sortante
            'NO_SHOW': ['MEETING_BOOKED', 'LOST', 'CONTACTED'],
            'LOST': ['NEW'], // Réactivation uniquement
        };

        return VALID_TRANSITIONS[from]?.includes(to) ?? false;
    }

    // ═══════════════════════════════════════════════════════════
    // HELPERS
    // ═══════════════════════════════════════════════════════════

    /**
     * Compte le nombre de transitions vers NO_SHOW pour un prospect.
     */
    private async countNoShows(prospectId: string): Promise<number> {
        const logs = await this.getTransitionLogs(prospectId);
        return logs.filter(l => l.toStatus === 'NO_SHOW').length;
    }

    /**
     * Log une transition dans les notes du prospect.
     */
    private async logTransition(log: Omit<PipelineTransitionLog, 'id' | 'createdAt'>): Promise<void> {
        // Store as a prospect note for audit trail
        await this.prisma.prospectNote.create({
            data: {
                prospectId: log.prospectId,
                authorId: 'SYSTEM',
                text: `[AUTO] ${log.fromStatus} → ${log.toStatus} | ${log.trigger} | ${log.details || ''}`,
            },
        });
    }

    /**
     * Récupère l'historique des transitions d'un prospect.
     */
    async getTransitionLogs(prospectId: string): Promise<PipelineTransitionLog[]> {
        const notes = await this.prisma.prospectNote.findMany({
            where: {
                prospectId,
                text: { startsWith: '[AUTO]' },
            },
            orderBy: { createdAt: 'desc' },
        });

        return notes.map(note => {
            const match = note.text.match(/\[AUTO\] (\w+) → (\w+) \| (\w+) \| (.+)/);
            return {
                id: note.id,
                prospectId: note.prospectId,
                prospectName: '',
                fromStatus: match?.[1] || '',
                toStatus: match?.[2] || '',
                trigger: match?.[3] || '',
                automated: true,
                details: match?.[4] || '',
                createdAt: note.createdAt.toISOString(),
            };
        });
    }

    // ═══════════════════════════════════════════════════════════
    // API : Statistiques
    // ═══════════════════════════════════════════════════════════

    async getAutomationStats(): Promise<any> {
        const allNotes = await this.prisma.prospectNote.findMany({
            where: { text: { startsWith: '[AUTO]' } },
            orderBy: { createdAt: 'desc' },
            take: 200,
        });

        // Count by trigger type
        const byTrigger: Record<string, number> = {};
        const byTransition: Record<string, number> = {};

        for (const note of allNotes) {
            const match = note.text.match(/\[AUTO\] (\w+) → (\w+) \| (\w+)/);
            if (match) {
                const trigger = match[3];
                const transition = `${match[1]}→${match[2]}`;
                byTrigger[trigger] = (byTrigger[trigger] || 0) + 1;
                byTransition[transition] = (byTransition[transition] || 0) + 1;
            }
        }

        // Current pipeline counts
        const pipelineCounts = await this.prisma.prospect.groupBy({
            by: ['status'],
            _count: { status: true },
        });

        return {
            totalAutoTransitions: allNotes.length,
            byTrigger,
            byTransition,
            pipeline: pipelineCounts.reduce((acc, g) => {
                acc[g.status] = g._count.status;
                return acc;
            }, {} as Record<string, number>),
            recentTransitions: allNotes.slice(0, 20).map(n => ({
                text: n.text,
                createdAt: n.createdAt,
                prospectId: n.prospectId,
            })),
        };
    }

    /**
     * Force un check manuel (pour test ou debug).
     */
    async forceCheck(): Promise<{ noShows: number; abandoned: number; inactive: number }> {
        return this.checkTimedTransitions();
    }
}
