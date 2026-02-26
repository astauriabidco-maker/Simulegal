/**
 * Store pour la gestion des notes de Veille Juridique
 * Sync backend (DB) avec fallback localStorage
 */

export interface VeilleNote {
    id: string;
    date: string;           // ISO string (mapped from createdAt)
    title: string;
    summary: string;
    category: string;
    severity: 'high' | 'medium' | 'low';
    sourceUrl?: string;
    authorName?: string;
    applied: boolean;
    appliedAt?: string;
    linkedRuleIds?: string[];  // IDs des règles impactées
    isAutoDetected?: boolean;  // true if detected by RSS scanner
}

const STORAGE_KEY = 'v2_veille_juridique';
const API_URL = typeof window !== 'undefined'
    ? (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000')
    : 'http://localhost:4000';

// ─── Helpers ──────────────────────────────────────────────────────

function getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('admin_token') : null;
    return {
        'Content-Type': 'application/json',
        ...(token ? { 'Authorization': `Bearer ${token}` } : {}),
    };
}

function mapBackendToNote(item: any): VeilleNote {
    let linkedRuleIds: string[] = [];
    try {
        linkedRuleIds = item.linkedRuleIds ? JSON.parse(item.linkedRuleIds) : [];
    } catch { linkedRuleIds = []; }
    return {
        id: item.id,
        date: item.createdAt || item.date,
        title: item.title,
        summary: item.summary,
        category: item.category,
        severity: item.severity as 'high' | 'medium' | 'low',
        sourceUrl: item.sourceUrl || undefined,
        authorName: item.authorName || undefined,
        applied: !!item.applied,
        appliedAt: item.appliedAt || undefined,
        linkedRuleIds,
        isAutoDetected: item.authorName?.startsWith('🤖') || false,
    };
}

function loadFromLocalStorage(): VeilleNote[] {
    if (typeof window === 'undefined') return [];
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) {
        try { return JSON.parse(saved); } catch { return []; }
    }
    return [];
}

function saveToLocalStorage(notes: VeilleNote[]) {
    if (typeof window === 'undefined') return;
    localStorage.setItem(STORAGE_KEY, JSON.stringify(notes));
}

// ─── Store ────────────────────────────────────────────────────────

export const VeilleStore = {
    /**
     * Liste toutes les notes triées par date (plus récentes d'abord)
     */
    getAll: (): VeilleNote[] => {
        return loadFromLocalStorage().sort(
            (a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()
        );
    },

    /**
     * Sync depuis le backend — remplace localStorage
     */
    syncFromBackend: async (): Promise<VeilleNote[]> => {
        try {
            const res = await fetch(`${API_URL}/veille`, { headers: getAuthHeaders() });
            if (res.ok) {
                const data = await res.json();
                const notes = data.map(mapBackendToNote);
                saveToLocalStorage(notes);
                console.log(`[VEILLE] ✅ Synced ${notes.length} notes from backend`);
                return notes;
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend sync failed, using localStorage', err);
        }
        return loadFromLocalStorage();
    },

    /**
     * Déclenche un scan manuel des sources RSS
     */
    triggerScan: async (): Promise<{ created: number; errors: number; sourcesUsed: string[]; message: string }> => {
        try {
            const res = await fetch(`${API_URL}/veille/scan`, {
                method: 'POST',
                headers: getAuthHeaders(),
            });
            if (res.ok) {
                return await res.json();
            }
            const err = await res.json().catch(() => ({}));
            throw new Error(err.message || `Scan failed (${res.status})`);
        } catch (err) {
            console.error('[VEILLE] ⚠️ Scan failed:', err);
            throw err;
        }
    },

    /**
     * Ajoute une note — envoie au backend + fallback localStorage
     */
    add: async (note: Omit<VeilleNote, 'id' | 'date' | 'applied'>): Promise<VeilleNote> => {
        try {
            const res = await fetch(`${API_URL}/veille`, {
                method: 'POST',
                headers: getAuthHeaders(),
                body: JSON.stringify(note),
            });
            if (res.ok) {
                const created = await res.json();
                const mapped = mapBackendToNote(created);
                const notes = loadFromLocalStorage();
                notes.push(mapped);
                saveToLocalStorage(notes);
                return mapped;
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend create failed, using localStorage', err);
        }
        // Fallback localStorage
        const notes = loadFromLocalStorage();
        const newNote: VeilleNote = {
            ...note,
            id: `note_${Date.now()}`,
            date: new Date().toISOString(),
            applied: false,
        };
        notes.push(newNote);
        saveToLocalStorage(notes);
        return newNote;
    },

    /**
     * Met à jour une note existante
     */
    update: async (id: string, changes: Partial<VeilleNote>): Promise<VeilleNote | null> => {
        try {
            const res = await fetch(`${API_URL}/veille/${id}`, {
                method: 'PUT',
                headers: getAuthHeaders(),
                body: JSON.stringify(changes),
            });
            if (res.ok) {
                const updated = await res.json();
                const mapped = mapBackendToNote(updated);
                const notes = loadFromLocalStorage();
                const idx = notes.findIndex(n => n.id === id);
                if (idx !== -1) { notes[idx] = mapped; saveToLocalStorage(notes); }
                return mapped;
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend update failed', err);
        }
        const notes = loadFromLocalStorage();
        const index = notes.findIndex(n => n.id === id);
        if (index === -1) return null;
        notes[index] = { ...notes[index], ...changes };
        saveToLocalStorage(notes);
        return notes[index];
    },

    /**
     * Supprime une note
     */
    remove: async (id: string): Promise<boolean> => {
        try {
            await fetch(`${API_URL}/veille/${id}`, { method: 'DELETE', headers: getAuthHeaders() });
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend delete failed', err);
        }
        const notes = loadFromLocalStorage();
        const filtered = notes.filter(n => n.id !== id);
        if (filtered.length === notes.length) return false;
        saveToLocalStorage(filtered);
        return true;
    },

    /**
     * Marque une note comme appliquée
     */
    markAsApplied: async (id: string): Promise<VeilleNote | null> => {
        try {
            const res = await fetch(`${API_URL}/veille/${id}/apply`, {
                method: 'PUT',
                headers: getAuthHeaders(),
            });
            if (res.ok) {
                const updated = await res.json();
                const mapped = mapBackendToNote(updated);
                const notes = loadFromLocalStorage();
                const idx = notes.findIndex(n => n.id === id);
                if (idx !== -1) { notes[idx] = mapped; saveToLocalStorage(notes); }
                return mapped;
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend markAsApplied failed', err);
        }
        return VeilleStore.update(id, {
            applied: true,
            appliedAt: new Date().toISOString(),
        });
    },

    /**
     * Stats dynamiques — from backend with fallback
     */
    getStats: () => {
        const notes = loadFromLocalStorage();
        const sorted = [...notes].sort((a, b) => new Date(b.date).getTime() - new Date(a.date).getTime());
        const lastUpdate = sorted.length > 0 ? sorted[0].date : null;
        const pending = notes.filter(n => !n.applied).length;
        const applied = notes.filter(n => n.applied).length;
        const total = notes.length;
        const autoDetected = notes.filter(n => n.isAutoDetected).length;
        const pendingHigh = notes.filter(n => !n.applied && n.severity === 'high').length;

        let lastUpdateLabel = 'Aucune';
        if (lastUpdate) {
            const diffMs = Date.now() - new Date(lastUpdate).getTime();
            const diffHours = Math.floor(diffMs / (1000 * 60 * 60));
            const diffDays = Math.floor(diffHours / 24);
            if (diffDays > 0) {
                lastUpdateLabel = `Il y a ${diffDays} jour${diffDays > 1 ? 's' : ''}`;
            } else if (diffHours > 0) {
                lastUpdateLabel = `Il y a ${diffHours}h`;
            } else {
                lastUpdateLabel = 'À l\'instant';
            }
        }

        return {
            lastUpdateLabel,
            pendingCount: pending,
            appliedCount: applied,
            totalCount: total,
            autoDetectedCount: autoDetected,
            pendingHighCount: pendingHigh,
            conformityPercent: total > 0 ? Math.round((applied / total) * 100) : 100,
        };
    },

    /**
     * Stats backend enrichies
     */
    fetchBackendStats: async () => {
        try {
            const res = await fetch(`${API_URL}/veille/stats`, { headers: getAuthHeaders() });
            if (res.ok) return await res.json();
        } catch { /* fallback */ }
        return null;
    },

    CATEGORIES: [
        'Immigration Professionnelle',
        'Regroupement Familial',
        'VPF',
        'Naturalisation',
        'Droit d\'asile',
        'Accords Bilatéraux',
        'Réglementation Générale',
    ]
};

export default VeilleStore;
