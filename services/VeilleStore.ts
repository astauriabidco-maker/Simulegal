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
}

const STORAGE_KEY = 'v2_veille_juridique';
const API_URL = typeof window !== 'undefined'
    ? (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000')
    : 'http://localhost:5000';

const SEED_DATA: VeilleNote[] = [
    {
        id: 'seed_1',
        date: '2026-01-17T10:00:00Z',
        title: 'Nouveau décret sur le Passeport Talent (Salarié qualifié)',
        summary: 'Le seuil de rémunération annuelle a été relevé. Vérifiez la mise à jour des seuils financiers.',
        category: 'Immigration Professionnelle',
        severity: 'high',
        sourceUrl: 'https://www.legifrance.gouv.fr',
        applied: false,
    },
    {
        id: 'seed_2',
        date: '2026-01-15T09:00:00Z',
        title: 'Simplification du regroupement familial (Algériens)',
        summary: 'Assouplissement des critères de logement pour les dossiers déposés en IDF.',
        category: 'Regroupement Familial',
        severity: 'medium',
        sourceUrl: '',
        applied: false,
    },
    {
        id: 'seed_3',
        date: '2026-01-10T14:30:00Z',
        title: 'Jurisprudence : Notion de communauté de vie',
        summary: 'Nouvelle décision du Conseil d\'État sur la preuve de cohabitation continue.',
        category: 'VPF',
        severity: 'low',
        sourceUrl: '',
        applied: true,
        appliedAt: '2026-01-12T08:00:00Z',
    },
];

// ─── Helpers ──────────────────────────────────────────────────────

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
    };
}

function loadFromLocalStorage(): VeilleNote[] {
    if (typeof window === 'undefined') return SEED_DATA;
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) {
        try { return JSON.parse(saved); } catch { return SEED_DATA; }
    }
    localStorage.setItem(STORAGE_KEY, JSON.stringify(SEED_DATA));
    return SEED_DATA;
}

function saveToLocalStorage(notes: VeilleNote[]) {
    if (typeof window === 'undefined') return;
    localStorage.setItem(STORAGE_KEY, JSON.stringify(notes));
}

// ─── Store ────────────────────────────────────────────────────────

export const VeilleStore = {
    /**
     * Liste toutes les notes triées par date (plus récentes d'abord)
     * Tente le backend, fallback localStorage
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
            const res = await fetch(`${API_URL}/veille`);
            if (res.ok) {
                const data = await res.json();
                if (data && data.length > 0) {
                    const notes = data.map(mapBackendToNote);
                    saveToLocalStorage(notes);
                    console.log(`[VEILLE] ✅ Synced ${notes.length} notes from backend`);
                    return notes;
                }
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend sync failed, using localStorage', err);
        }
        return loadFromLocalStorage();
    },

    /**
     * Ajoute une note — envoie au backend + fallback localStorage
     */
    add: async (note: Omit<VeilleNote, 'id' | 'date' | 'applied'>): Promise<VeilleNote> => {
        try {
            const res = await fetch(`${API_URL}/veille`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(note),
            });
            if (res.ok) {
                const created = await res.json();
                const mapped = mapBackendToNote(created);
                // Update localStorage
                const notes = loadFromLocalStorage();
                notes.push(mapped);
                saveToLocalStorage(notes);
                console.log(`[VEILLE] ✅ Note créée (backend): "${mapped.title}"`);
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
        console.log(`[VEILLE] ✅ Note créée (local): "${newNote.title}"`);
        return newNote;
    },

    /**
     * Met à jour une note existante
     */
    update: async (id: string, changes: Partial<VeilleNote>): Promise<VeilleNote | null> => {
        try {
            const res = await fetch(`${API_URL}/veille/${id}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
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
        // Fallback
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
            await fetch(`${API_URL}/veille/${id}`, { method: 'DELETE' });
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
            const res = await fetch(`${API_URL}/veille/${id}/apply`, { method: 'PUT' });
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
     * Stats dynamiques
     */
    getStats: () => {
        const notes = loadFromLocalStorage();
        const sorted = [...notes].sort((a, b) => new Date(b.date).getTime() - new Date(a.date).getTime());
        const lastUpdate = sorted.length > 0 ? sorted[0].date : null;
        const pending = notes.filter(n => !n.applied).length;
        const applied = notes.filter(n => n.applied).length;
        const total = notes.length;

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
            conformityPercent: total > 0 ? Math.round((applied / total) * 100) : 100,
        };
    },

    /**
     * Seed le backend avec les données par défaut si la DB est vide
     */
    seedBackendIfEmpty: async () => {
        try {
            const res = await fetch(`${API_URL}/veille`);
            if (res.ok) {
                const data = await res.json();
                if (data.length === 0) {
                    console.log('[VEILLE] 🌱 Seeding backend with default notes...');
                    for (const note of SEED_DATA) {
                        await fetch(`${API_URL}/veille`, {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({
                                title: note.title,
                                summary: note.summary,
                                category: note.category,
                                severity: note.severity,
                                sourceUrl: note.sourceUrl,
                                authorName: note.authorName,
                            }),
                        });
                    }
                    console.log('[VEILLE] ✅ Backend seeded');
                }
            }
        } catch (err) {
            console.warn('[VEILLE] ⚠️ Backend seed failed', err);
        }
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
