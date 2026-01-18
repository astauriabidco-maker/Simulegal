/**
 * Workflow Service
 * Définit les étapes du pipeline de traitement selon le type de service
 */

// Étapes du workflow - adaptées par type de service
export type WorkflowStage =
    | 'NEW'              // Nouveau dossier
    | 'COLLECTING'       // Collecte des documents
    | 'REVIEW'           // Révision par le siège
    | 'HUNTING'          // Recherche de créneau RDV
    | 'BOOKED'           // RDV réservé
    | 'ANTS_SUBMISSION'  // Envoi ANTS (permis)
    | 'WAITING_ORIGINAL' // Attente original (permis)
    | 'DRAFTING'         // Rédaction dossier
    | 'SUBMITTED'        // Soumis à l'administration
    | 'OFII_INVESTIGATION' // Enquête OFII (regroupement familial)
    | 'DECISION_WAIT'    // Attente décision
    | 'INSTRUCTION'      // En instruction
    | 'TO_CONTACT'        // 📞 À rappeler (File d'attente Siège)
    | 'QUALIFIED'         // ✅ Qualifié (Prêt à être envoyé en agence ou transformé)
    | 'SCHEDULING'        // 📅 En cours de planification (Juriste)
    | 'DONE'              // Terminé (RDV honoré)
    | 'ARCHIVED'          // Abandonné
    | 'CLOSED';           // Dossier clôturé

// Labels humains pour chaque étape
const STAGE_LABELS: Record<WorkflowStage, string> = {
    NEW: '📋 Nouveau',
    COLLECTING: '📎 Collecte Documents',
    REVIEW: '🔍 Vérification',
    HUNTING: '🎯 Recherche RDV',
    BOOKED: '📅 RDV Réservé',
    ANTS_SUBMISSION: '📤 Envoi ANTS',
    WAITING_ORIGINAL: '📬 Attente Original',
    DRAFTING: '✍️ Rédaction',
    SUBMITTED: '📨 Soumis',
    OFII_INVESTIGATION: '🏠 Enquête Logement/OFII',
    DECISION_WAIT: '⏳ Attente Décision',
    INSTRUCTION: '⚙️ En Instruction',
    TO_CONTACT: '📞 À Rappeler (Siège)',
    QUALIFIED: '✅ Qualifié / Transféré',
    SCHEDULING: '📅 Planification Agenda',
    DONE: 'Consultation Terminée',
    ARCHIVED: 'Sans suite / Injoignable',
    CLOSED: '✅ Clôturé'
};

// Couleurs pour l'affichage
const STAGE_COLORS: Record<WorkflowStage, string> = {
    NEW: 'slate',
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
    DONE: 'blue',
    ARCHIVED: 'slate',
    CLOSED: 'emerald'
};

// Descriptions détaillées
const STAGE_DESCRIPTIONS: Record<WorkflowStage, string> = {
    NEW: 'Nouveau dossier créé',
    COLLECTING: 'Le client téléverse ses documents',
    REVIEW: 'Vérification par le siège',
    HUNTING: 'Recherche de créneaux disponibles',
    BOOKED: 'Rendez-vous confirmé',
    ANTS_SUBMISSION: 'Dossier envoyé à l\'ANTS',
    WAITING_ORIGINAL: 'Attente réception du permis original',
    DRAFTING: 'Rédaction du dossier administratif',
    SUBMITTED: 'Dossier déposé à l\'administration',
    OFII_INVESTIGATION: 'Enquête logement par l\'OFII',
    DECISION_WAIT: 'En attente de la décision finale',
    INSTRUCTION: 'Dossier en cours d\'instruction',
    TO_CONTACT: 'Dossier en attente de rappel par le siège',
    QUALIFIED: 'Dossier qualifié et prêt pour la suite',
    SCHEDULING: 'Recherche de créneau pour le juriste',
    DONE: 'La consultation a eu lieu',
    ARCHIVED: 'Dossier abandonné ou injoignable',
    CLOSED: 'Dossier finalisé'
};

export const WorkflowService = {
    /**
     * Retourne les étapes du workflow pour un service donné
     */
    getStepsForService: (serviceId: string): WorkflowStage[] => {
        // 1. Demande de Rappel (Flux Call Center Siège)
        if (serviceId === 'rappel_echeances' || serviceId === 'contact_simple') {
            return ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'];
        }

        // 2. RDV Juriste (Flux Secrétariat Siège)
        if (serviceId === 'rdv_juriste') {
            return ['NEW', 'SCHEDULING', 'BOOKED', 'DONE'];
        }

        // 3. RDV Préfecture - Workflow court avec recherche de créneaux
        if (serviceId === 'rdv_prefecture') {
            return ['NEW', 'COLLECTING', 'REVIEW', 'HUNTING', 'BOOKED', 'CLOSED'];
        }

        // 4. Permis de conduire - Workflow avec ANTS et envoi postal
        if (['permis_conduire', 'changement_permis', 'echange_permis'].includes(serviceId)) {
            return ['NEW', 'COLLECTING', 'REVIEW', 'ANTS_SUBMISSION', 'WAITING_ORIGINAL', 'INSTRUCTION', 'CLOSED'];
        }

        // 5. Regroupement Familial - Workflow long avec enquête OFII
        if (serviceId === 'regroupement_familial') {
            return ['NEW', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'OFII_INVESTIGATION', 'DECISION_WAIT', 'CLOSED'];
        }

        // 6. Workflow Standard (Naturalisation, Titres de séjour, etc.)
        return ['NEW', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'DECISION_WAIT', 'CLOSED'];
    },

    /**
     * Retourne le label humain d'une étape
     */
    getStageLabel: (stage: WorkflowStage): string => {
        return STAGE_LABELS[stage] || stage;
    },

    /**
     * Retourne la couleur associée à une étape
     */
    getStageColor: (stage: WorkflowStage): string => {
        return STAGE_COLORS[stage] || 'slate';
    },

    /**
     * Retourne la description d'une étape
     */
    getStageDescription: (stage: WorkflowStage): string => {
        return STAGE_DESCRIPTIONS[stage] || '';
    },

    /**
     * Calcule le pourcentage de progression pour un service
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

        // Peut avancer d'une étape ou revenir en arrière
        return toIndex === fromIndex + 1 || toIndex < fromIndex;
    },

    /**
     * Retourne l'étape suivante possible
     */
    getNextStage: (serviceId: string, currentStage: WorkflowStage): WorkflowStage | null => {
        const steps = WorkflowService.getStepsForService(serviceId);
        const index = steps.indexOf(currentStage);
        if (index === -1 || index >= steps.length - 1) return null;
        return steps[index + 1];
    },

    /**
     * Retourne l'index d'une étape (pour l'affichage)
     */
    getStageIndex: (serviceId: string, stage: WorkflowStage): number => {
        const steps = WorkflowService.getStepsForService(serviceId);
        return steps.indexOf(stage);
    }
};

export default WorkflowService;
