/**
 * ═══════════════════════════════════════════════════════════════
 * Workflow Service — Dynamique (chargé depuis l'API backend)
 * ═══════════════════════════════════════════════════════════════
 *
 * Ce service charge les configurations de pipeline depuis le backend
 * (GET /settings/services) pour que le Kanban s'adapte automatiquement
 * aux étapes de chaque service.
 *
 * Fallback statique intégré si le backend ne répond pas.
 */

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

// ═══════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════

export type WorkflowStage =
    | 'NEW'
    | 'COLLECTING'
    | 'REVIEW'
    | 'HUNTING'
    | 'BOOKED'
    | 'ANTS_SUBMISSION'
    | 'WAITING_ORIGINAL'
    | 'DRAFTING'
    | 'SUBMITTED'
    | 'OFII_INVESTIGATION'
    | 'DECISION_WAIT'
    | 'INSTRUCTION'
    | 'TO_CONTACT'
    | 'QUALIFIED'
    | 'SCHEDULING'
    | 'PAID'
    | 'DONE'
    | 'ARCHIVED'
    | 'CLOSED';

export interface PipelineStage {
    key: string;
    label: string;
    color: string;
    description?: string;
    autoNotify?: boolean;
}

export interface ServiceConfig {
    id: string;
    name: string;
    shortName: string;
    category: string;
    description: string;
    pipelineTemplate: string;
    basePrice?: number;
    estimatedDuration?: string;
    requiredDocs: { docId: string; required: boolean }[];
}

export interface ServiceCatalogResponse {
    services: ServiceConfig[];
    byCategory: Record<string, ServiceConfig[]>;
    totalServices: number;
}

export interface PipelineResponse {
    service: ServiceConfig;
    pipeline: PipelineStage[];
    requiredDocs: any[];
}

// ═══════════════════════════════════════════════════
// LABELS, COULEURS, DESCRIPTIONS (fallback statique)
// ═══════════════════════════════════════════════════

const STAGE_LABELS: Record<string, string> = {
    NEW: '📋 Nouveau',
    PAID: '💳 Payé',
    COLLECTING: '📎 Collecte Documents',
    REVIEW: '🔍 Vérification',
    HUNTING: '🎯 Recherche RDV',
    BOOKED: '📅 RDV Réservé',
    ANTS_SUBMISSION: '📤 Envoi ANTS',
    WAITING_ORIGINAL: '📬 Attente Original',
    DRAFTING: '✍️ Rédaction',
    SUBMITTED: '📨 Soumis',
    OFII_INVESTIGATION: '🏠 Enquête OFII',
    DECISION_WAIT: '⏳ Attente Décision',
    INSTRUCTION: '⚙️ Instruction',
    TO_CONTACT: '📞 À Rappeler',
    QUALIFIED: '✅ Qualifié',
    SCHEDULING: '📅 Planification',
    DONE: '✅ Terminé',
    ARCHIVED: '📦 Archivé',
    CLOSED: '✅ Clôturé'
};

const STAGE_COLORS: Record<string, string> = {
    NEW: 'slate',
    PAID: 'emerald',
    COLLECTING: 'amber',
    REVIEW: 'blue',
    HUNTING: 'purple',
    BOOKED: 'indigo',
    ANTS_SUBMISSION: 'cyan',
    WAITING_ORIGINAL: 'orange',
    DRAFTING: 'teal',
    SUBMITTED: 'violet',
    OFII_INVESTIGATION: 'pink',
    DECISION_WAIT: 'yellow',
    INSTRUCTION: 'sky',
    TO_CONTACT: 'orange',
    QUALIFIED: 'emerald',
    SCHEDULING: 'indigo',
    DONE: 'emerald',
    ARCHIVED: 'slate',
    CLOSED: 'emerald'
};

const STAGE_DESCRIPTIONS: Record<string, string> = {
    NEW: 'Nouveau dossier créé',
    PAID: 'Paiement confirmé',
    COLLECTING: 'Le client téléverse ses documents',
    REVIEW: 'Vérification par le juriste',
    HUNTING: 'Recherche de créneaux préfecture',
    BOOKED: 'Rendez-vous confirmé',
    ANTS_SUBMISSION: 'Dossier envoyé à l\'ANTS',
    WAITING_ORIGINAL: 'Attente réception des originaux',
    DRAFTING: 'Rédaction du dossier administratif',
    SUBMITTED: 'Dossier déposé à l\'administration',
    OFII_INVESTIGATION: 'Enquête logement par l\'OFII',
    DECISION_WAIT: 'En attente de la décision finale',
    INSTRUCTION: 'Dossier en cours d\'instruction',
    TO_CONTACT: 'En attente de rappel par le siège',
    QUALIFIED: 'Dossier qualifié et prêt',
    SCHEDULING: 'Recherche de créneau',
    DONE: 'Dossier terminé',
    ARCHIVED: 'Dossier archivé',
    CLOSED: 'Dossier finalisé'
};

// ═══════════════════════════════════════════════════
// FALLBACK STATIQUE (si l'API est down)
// ═══════════════════════════════════════════════════

const FALLBACK_PIPELINES: Record<string, WorkflowStage[]> = {
    rappel_echeances: ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'],
    rdv_juriste: ['NEW', 'SCHEDULING', 'BOOKED', 'DONE'],
    consultation_juriste: ['NEW', 'PAID', 'SCHEDULING', 'BOOKED', 'DONE'],
    rdv_prefecture: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'HUNTING', 'BOOKED', 'CLOSED'],
    bilan_eligibilite: ['NEW', 'PAID', 'SCHEDULING', 'BOOKED', 'DONE'],
    permis_conduire: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'ANTS_SUBMISSION', 'INSTRUCTION', 'DONE'],
    regroupement_familial: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'OFII_INVESTIGATION', 'DECISION_WAIT', 'DONE'],
    naturalisation: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'INSTRUCTION', 'DECISION_WAIT', 'DONE'],
    // Formation
    cours_francais_a2: ['NEW', 'PAID', 'COLLECTING', 'SCHEDULING', 'BOOKED', 'REVIEW', 'DONE'],
    cours_francais_b1: ['NEW', 'PAID', 'COLLECTING', 'SCHEDULING', 'BOOKED', 'REVIEW', 'DONE'],
    examen_civique: ['NEW', 'PAID', 'COLLECTING', 'SCHEDULING', 'BOOKED', 'REVIEW', 'DONE'],
    // Qualification / Rappel
    demande_rappel: ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'],
    contact_simple: ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'],
    // ── Pôles Frontend (IDs agrégés utilisés par le simulateur) ──
    nat_accomp: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'INSTRUCTION', 'DECISION_WAIT', 'DONE'],
    sejour_accomp: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'HUNTING', 'BOOKED', 'ANTS_SUBMISSION', 'INSTRUCTION', 'DECISION_WAIT', 'DONE'],
    langue_a2b1: ['NEW', 'PAID', 'COLLECTING', 'SCHEDULING', 'BOOKED', 'REVIEW', 'DONE'],
    form_civique: ['NEW', 'PAID', 'COLLECTING', 'SCHEDULING', 'BOOKED', 'REVIEW', 'DONE'],
    // Defaults
    _default: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'HUNTING', 'BOOKED', 'ANTS_SUBMISSION', 'INSTRUCTION', 'DECISION_WAIT', 'DONE'],
    _all_production: ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'DECISION_WAIT', 'DONE'],
};

// ═══════════════════════════════════════════════════
// CACHE DES PIPELINES DYNAMIQUES
// ═══════════════════════════════════════════════════

let cachedServiceCatalog: ServiceCatalogResponse | null = null;
let cachedPipelines: Map<string, PipelineStage[]> = new Map();
let catalogLoadedAt: number = 0;
const CACHE_TTL = 5 * 60 * 1000; // 5 minutes

// ═══════════════════════════════════════════════════
// SERVICE PRINCIPAL
// ═══════════════════════════════════════════════════

export const WorkflowService = {

    /**
     * Charge le catalogue des services depuis le backend
     * Utilise un cache de 5 minutes
     */
    loadServiceCatalog: async (): Promise<ServiceCatalogResponse | null> => {
        if (cachedServiceCatalog && Date.now() - catalogLoadedAt < CACHE_TTL) {
            return cachedServiceCatalog;
        }

        try {
            const res = await fetch(`${API_URL}/settings/services`);
            if (!res.ok) throw new Error(`HTTP ${res.status}`);
            const data = await res.json();
            cachedServiceCatalog = data;
            catalogLoadedAt = Date.now();
            return data;
        } catch (error) {
            console.warn('[WorkflowService] Failed to load service catalog:', error);
            return cachedServiceCatalog; // Return stale cache if available
        }
    },

    /**
     * Charge le pipeline pour un service spécifique depuis le backend
     */
    loadServicePipeline: async (serviceId: string): Promise<PipelineStage[] | null> => {
        if (cachedPipelines.has(serviceId)) {
            return cachedPipelines.get(serviceId)!;
        }

        try {
            const res = await fetch(`${API_URL}/settings/services/${serviceId}/pipeline`);
            if (!res.ok) return null;
            const data: PipelineResponse = await res.json();
            if (data.pipeline) {
                cachedPipelines.set(serviceId, data.pipeline);
                return data.pipeline;
            }
            return null;
        } catch (error) {
            console.warn(`[WorkflowService] Failed to load pipeline for ${serviceId}:`, error);
            return null;
        }
    },

    /**
     * Retourne les étapes du workflow pour un service donné
     * Utilise le cache dynamique, puis le fallback statique
     */
    getStepsForService: (serviceId: string): WorkflowStage[] => {
        // 1. Chercher dans le cache dynamique
        const cached = cachedPipelines.get(serviceId);
        if (cached) {
            return cached.map(s => s.key) as WorkflowStage[];
        }

        // 2. Fallback statique par serviceId exact
        if (FALLBACK_PIPELINES[serviceId]) {
            return FALLBACK_PIPELINES[serviceId];
        }

        // 3. Pattern matching pour les services connus
        if (serviceId.startsWith('vpf_') || serviceId.startsWith('cs_') || serviceId.startsWith('passeport_talent_') || serviceId.startsWith('carte_resident_') || serviceId.startsWith('aes_') || serviceId.startsWith('cra_') || serviceId.startsWith('accord_')) {
            return FALLBACK_PIPELINES['_default'];
        }

        if (serviceId.startsWith('nat_') || serviceId.startsWith('naturalisation')) {
            return FALLBACK_PIPELINES['naturalisation'];
        }

        if (serviceId.startsWith('asile') || serviceId.startsWith('aps_')) {
            return ['NEW', 'PAID', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'INSTRUCTION', 'DECISION_WAIT', 'DONE'];
        }

        // 4. Fallback par défaut
        return FALLBACK_PIPELINES['_all_production'];
    },

    /**
     * Retourne les colonnes à afficher sur le Kanban
     * Gère les onglets Production/Qualification et le filtre service
     */
    getKanbanColumns: (serviceFilter: string, activeTab: 'PRODUCTION' | 'QUALIFICATION'): WorkflowStage[] => {
        if (activeTab === 'QUALIFICATION') {
            return ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'];
        }

        if (serviceFilter === 'all') {
            return FALLBACK_PIPELINES['_all_production'];
        }

        return WorkflowService.getStepsForService(serviceFilter);
    },

    /**
     * Retourne le label humain d'une étape
     * Utilise le cache dynamique si disponible (pour labels personnalisés)
     */
    getStageLabel: (stage: WorkflowStage, serviceId?: string): string => {
        // Priorité au label dynamique du pipeline chargé
        if (serviceId) {
            const cached = cachedPipelines.get(serviceId);
            if (cached) {
                const found = cached.find(s => s.key === stage);
                if (found) return found.label;
            }
        }
        return STAGE_LABELS[stage] || stage;
    },

    /**
     * Retourne la couleur CSS associée à une étape
     */
    getStageColor: (stage: WorkflowStage, serviceId?: string): string => {
        // Priorité aux couleurs dynamiques (format hex → on les convertit en classe tailwind)
        if (serviceId) {
            const cached = cachedPipelines.get(serviceId);
            if (cached) {
                const found = cached.find(s => s.key === stage);
                if (found?.color) return found.color;
            }
        }
        return STAGE_COLORS[stage] || 'slate';
    },

    /**
     * Retourne la description d'une étape
     */
    getStageDescription: (stage: WorkflowStage): string => {
        return STAGE_DESCRIPTIONS[stage] || '';
    },

    /**
     * Calcule le pourcentage de progression
     */
    getProgress: (serviceId: string, currentStage: WorkflowStage): number => {
        const steps = WorkflowService.getStepsForService(serviceId);
        const index = steps.indexOf(currentStage);
        if (index === -1) return 0;
        return Math.round((index / (steps.length - 1)) * 100);
    },

    /**
     * Vérifie si une transition est possible
     */
    canTransition: (serviceId: string, from: WorkflowStage, to: WorkflowStage): boolean => {
        const steps = WorkflowService.getStepsForService(serviceId);
        const fromIndex = steps.indexOf(from);
        const toIndex = steps.indexOf(to);
        return toIndex === fromIndex + 1 || toIndex < fromIndex;
    },

    /**
     * Retourne l'étape suivante
     */
    getNextStage: (serviceId: string, currentStage: WorkflowStage): WorkflowStage | null => {
        const steps = WorkflowService.getStepsForService(serviceId);
        const index = steps.indexOf(currentStage);
        if (index === -1 || index >= steps.length - 1) return null;
        return steps[index + 1];
    },

    /**
     * Retourne l'index d'une étape
     */
    getStageIndex: (serviceId: string, stage: WorkflowStage): number => {
        const steps = WorkflowService.getStepsForService(serviceId);
        return steps.indexOf(stage);
    },

    /**
     * Retourne les services groupés par catégorie
     * Utilise le cache ou retourne un fallback
     */
    getServicesByCategory: (): Record<string, { id: string; name: string; shortName: string }[]> => {
        if (cachedServiceCatalog?.byCategory) {
            const result: Record<string, { id: string; name: string; shortName: string }[]> = {};
            for (const [cat, services] of Object.entries(cachedServiceCatalog.byCategory)) {
                result[cat] = services.map(s => ({ id: s.id, name: s.name, shortName: s.shortName }));
            }
            return result;
        }

        // Fallback statique
        return {
            'TITRE_SEJOUR': [
                { id: 'vpf_conjoint_francais', name: 'VPF Conjoint FR', shortName: 'VPF Conjoint FR' },
                { id: 'cs_salarie', name: 'CS Salarié', shortName: 'CS Salarié' },
                { id: 'cs_etudiant', name: 'CS Étudiant', shortName: 'CS Étudiant' },
                { id: 'passeport_talent_carte_bleue_eu', name: 'PT Carte Bleue', shortName: 'PT Carte Bleue' },
            ],
            'NATURALISATION': [
                { id: 'naturalisation', name: 'Naturalisation', shortName: 'Naturalisation' },
                { id: 'naturalisation_mariage', name: 'Nat. Mariage', shortName: 'Nat. Mariage' },
            ],
            'REGROUPEMENT_FAMILIAL': [
                { id: 'regroupement_familial', name: 'Regroupement Familial', shortName: 'Regr. Familial' },
            ],
            'PERMIS_CONDUIRE': [
                { id: 'permis_conduire', name: 'Échange Permis', shortName: 'Permis' },
            ],
            'FORMATION': [
                { id: 'cours_francais_a2', name: 'Français A2', shortName: 'Français A2' },
                { id: 'cours_francais_b1', name: 'Français B1', shortName: 'Français B1' },
                { id: 'examen_civique', name: 'Examen Civique', shortName: 'Exam. Civique' },
            ],
            'CONSULTATION': [
                { id: 'consultation_juriste', name: 'RDV Juriste', shortName: 'RDV Juriste' },
                { id: 'rdv_prefecture', name: 'RDV Préfecture', shortName: 'RDV Préf.' },
                { id: 'bilan_eligibilite', name: 'Bilan Éligibilité', shortName: 'Bilan' },
            ],
            'QUALIFICATION': [
                { id: 'demande_rappel', name: 'Demande de Rappel', shortName: 'Rappel' },
                { id: 'contact_simple', name: 'Contact Simple', shortName: 'Contact' },
            ],
        };
    },

    /**
     * Invalide le cache pour forcer un rechargement
     */
    invalidateCache: () => {
        cachedServiceCatalog = null;
        cachedPipelines.clear();
        catalogLoadedAt = 0;
    },

    /**
     * Pré-charge tous les pipelines pour les services actuellement dans les leads
     */
    preloadPipelines: async (serviceIds: string[]) => {
        const unique = [...new Set(serviceIds)].filter(id => !cachedPipelines.has(id));
        await Promise.all(unique.map(id => WorkflowService.loadServicePipeline(id)));
    }
};

// ═══════════════════════════════════════════════════
// CONSTANTES POUR LES CATÉGORIES
// ═══════════════════════════════════════════════════

export const CATEGORY_LABELS: Record<string, string> = {
    'TITRE_SEJOUR': '📋 Titres de Séjour',
    'NATURALISATION': '🇫🇷 Naturalisation',
    'REGROUPEMENT_FAMILIAL': '👨‍👩‍👧 Regroupement Familial',
    'PERMIS_CONDUIRE': '🚗 Permis de Conduire',
    'FORMATION': '🎓 Formations & Examens',
    'CONSULTATION': '📞 Consultations & RDV',
    'QUALIFICATION': '📥 Qualification / Rappels',
    'AUTRE': '📎 Autres',
};

export const CATEGORY_COLORS: Record<string, string> = {
    'TITRE_SEJOUR': 'indigo',
    'NATURALISATION': 'blue',
    'REGROUPEMENT_FAMILIAL': 'purple',
    'PERMIS_CONDUIRE': 'amber',
    'FORMATION': 'teal',
    'CONSULTATION': 'emerald',
    'QUALIFICATION': 'orange',
    'AUTRE': 'slate',
};

export default WorkflowService;
