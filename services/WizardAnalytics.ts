/**
 * WizardAnalytics — Client-side funnel tracking for the SimuLegal wizard.
 *
 * Persists events in localStorage under `simulegal_analytics`.
 * Each "session" tracks one user journey through the wizard, recording:
 *   - start time, completion, total duration
 *   - per-question timing & interaction count
 *   - drop-off point if abandoned
 *
 * The admin dashboard can read aggregated stats via `getStats()`.
 */

const ANALYTICS_KEY = 'simulegal_analytics';

/* ─── Types ─── */

export interface QuestionEvent {
    questionId: string;
    enteredAt: number;       // timestamp ms
    answeredAt?: number;     // timestamp ms
    durationMs?: number;
    interactions: number;    // how many times user changed answer before next
}

export interface WizardSession {
    id: string;
    serviceId: string;
    startedAt: number;
    completedAt?: number;
    durationMs?: number;
    totalQuestions: number;
    lastQuestionIndex: number;
    lastQuestionId: string;
    completed: boolean;
    resumed: boolean;         // did the user resume a previous session?
    questions: QuestionEvent[];
}

export interface WizardStats {
    totalSessions: number;
    completedSessions: number;
    completionRate: number;
    avgDurationMs: number;
    avgDurationFormatted: string;
    dropOffPoints: Record<string, number>;  // questionId → drop count
    avgTimePerQuestion: Record<string, number>; // questionId → avg ms
    topDropOff: { questionId: string; count: number } | null;
    sessionsLast7Days: number;
    sessionsLast30Days: number;
}

/* ─── Helpers ─── */

function generateId(): string {
    return `ws_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
}

function loadSessions(): WizardSession[] {
    try {
        const raw = localStorage.getItem(ANALYTICS_KEY);
        return raw ? JSON.parse(raw) : [];
    } catch {
        return [];
    }
}

function saveSessions(sessions: WizardSession[]) {
    try {
        // Keep max 200 sessions to avoid localStorage bloat
        const trimmed = sessions.slice(-200);
        localStorage.setItem(ANALYTICS_KEY, JSON.stringify(trimmed));
    } catch { /* quota exceeded */ }
}

/* ─── Public API ─── */

export const WizardAnalytics = {

    /**
     * Start a new wizard session. Returns the session ID.
     */
    startSession(serviceId: string, totalQuestions: number, resumed: boolean = false): string {
        const sessions = loadSessions();
        const session: WizardSession = {
            id: generateId(),
            serviceId,
            startedAt: Date.now(),
            totalQuestions,
            lastQuestionIndex: 0,
            lastQuestionId: '',
            completed: false,
            resumed,
            questions: [],
        };
        sessions.push(session);
        saveSessions(sessions);
        return session.id;
    },

    /**
     * Record that the user entered a question.
     */
    enterQuestion(sessionId: string, questionId: string, questionIndex: number) {
        const sessions = loadSessions();
        const session = sessions.find(s => s.id === sessionId);
        if (!session) return;

        // Finalize previous question timing
        const prev = session.questions[session.questions.length - 1];
        if (prev && !prev.answeredAt) {
            prev.answeredAt = Date.now();
            prev.durationMs = prev.answeredAt - prev.enteredAt;
        }

        session.lastQuestionIndex = questionIndex;
        session.lastQuestionId = questionId;
        session.questions.push({
            questionId,
            enteredAt: Date.now(),
            interactions: 0,
        });

        saveSessions(sessions);
    },

    /**
     * Record an interaction (answer change) on the current question.
     */
    recordInteraction(sessionId: string) {
        const sessions = loadSessions();
        const session = sessions.find(s => s.id === sessionId);
        if (!session) return;

        const current = session.questions[session.questions.length - 1];
        if (current) {
            current.interactions++;
        }
        saveSessions(sessions);
    },

    /**
     * Mark the session as completed (wizard finished → results shown).
     */
    completeSession(sessionId: string) {
        const sessions = loadSessions();
        const session = sessions.find(s => s.id === sessionId);
        if (!session) return;

        // Finalize last question
        const last = session.questions[session.questions.length - 1];
        if (last && !last.answeredAt) {
            last.answeredAt = Date.now();
            last.durationMs = last.answeredAt - last.enteredAt;
        }

        session.completed = true;
        session.completedAt = Date.now();
        session.durationMs = session.completedAt - session.startedAt;

        saveSessions(sessions);
    },

    /**
     * Compute aggregated stats from all recorded sessions.
     */
    getStats(): WizardStats {
        const sessions = loadSessions();
        const total = sessions.length;
        const completed = sessions.filter(s => s.completed);

        // Drop-off points (count which question was the last one for abandoned sessions)
        const dropOffPoints: Record<string, number> = {};
        sessions.filter(s => !s.completed).forEach(s => {
            const qid = s.lastQuestionId || 'unknown';
            dropOffPoints[qid] = (dropOffPoints[qid] || 0) + 1;
        });

        // Find top drop-off point
        let topDropOff: WizardStats['topDropOff'] = null;
        for (const [qid, count] of Object.entries(dropOffPoints)) {
            if (!topDropOff || count > topDropOff.count) {
                topDropOff = { questionId: qid, count };
            }
        }

        // Avg time per question across all sessions
        const questionTimes: Record<string, number[]> = {};
        sessions.forEach(s => {
            s.questions.forEach(q => {
                if (q.durationMs) {
                    if (!questionTimes[q.questionId]) questionTimes[q.questionId] = [];
                    questionTimes[q.questionId].push(q.durationMs);
                }
            });
        });
        const avgTimePerQuestion: Record<string, number> = {};
        for (const [qid, times] of Object.entries(questionTimes)) {
            avgTimePerQuestion[qid] = Math.round(times.reduce((a, b) => a + b, 0) / times.length);
        }

        // Avg total duration for completed sessions
        const completedDurations = completed.map(s => s.durationMs || 0).filter(d => d > 0);
        const avgDurationMs = completedDurations.length > 0
            ? Math.round(completedDurations.reduce((a, b) => a + b, 0) / completedDurations.length)
            : 0;

        // Format duration
        const avgSec = Math.round(avgDurationMs / 1000);
        const avgMin = Math.floor(avgSec / 60);
        const avgRemSec = avgSec % 60;
        const avgDurationFormatted = avgMin > 0 ? `${avgMin}m ${avgRemSec}s` : `${avgSec}s`;

        // Sessions in recent periods
        const now = Date.now();
        const d7 = 7 * 24 * 60 * 60 * 1000;
        const d30 = 30 * 24 * 60 * 60 * 1000;

        return {
            totalSessions: total,
            completedSessions: completed.length,
            completionRate: total > 0 ? Math.round((completed.length / total) * 100) : 0,
            avgDurationMs,
            avgDurationFormatted,
            dropOffPoints,
            avgTimePerQuestion,
            topDropOff,
            sessionsLast7Days: sessions.filter(s => now - s.startedAt < d7).length,
            sessionsLast30Days: sessions.filter(s => now - s.startedAt < d30).length,
        };
    },

    /**
     * Get raw sessions (for admin debugging).
     */
    getSessions(): WizardSession[] {
        return loadSessions();
    },

    /**
     * Clear all analytics data.
     */
    clear() {
        try { localStorage.removeItem(ANALYTICS_KEY); } catch { }
    },
};
