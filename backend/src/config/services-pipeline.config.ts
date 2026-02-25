/**
 * ═══════════════════════════════════════════════════════════════
 * CONFIGURATION DES PIPELINES PAR SERVICE
 * ═══════════════════════════════════════════════════════════════
 *
 * Chaque service proposé par Simulegal a :
 *   - Un pipeline (liste ordonnée d'étapes)
 *   - Une liste de documents requis
 *   - Des métadonnées (catégorie, prix, description)
 *
 * Les services partagent des "templates de pipeline" pour éviter
 * la duplication. Un service comme "VPF Conjoint" utilise le
 * pipeline TITRE_SEJOUR_STANDARD sans le re-définir.
 *
 * Architecture :
 *   1. PIPELINE_TEMPLATES → Les types de parcours (5-6 templates)
 *   2. DOCUMENT_CATALOG   → Tous les documents possibles
 *   3. SERVICE_CATALOG     → Chaque service avec son pipeline + docs
 */

// ═══════════════════════════════════════════════════
// 1. TEMPLATES DE PIPELINE (étapes réutilisables)
// ═══════════════════════════════════════════════════

export interface PipelineStage {
    key: string;
    label: string;
    color: string;
    description?: string;
    autoNotify?: boolean; // Envoyer un WhatsApp automatique à cette étape
}

/**
 * Les templates de pipeline définissent les parcours-types.
 * Chaque service référence un template.
 */
export const PIPELINE_TEMPLATES: Record<string, PipelineStage[]> = {

    // ── Titre de séjour (parcours complet avec hunting RDV) ──
    TITRE_SEJOUR_COMPLET: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Collecte documents', color: '#f59e0b' },
        { key: 'REVIEW', label: 'Vérification juriste', color: '#3b82f6' },
        { key: 'HUNTING', label: 'Hunting RDV', color: '#ec4899', autoNotify: true, description: 'Recherche automatique de créneaux préfecture' },
        { key: 'BOOKED', label: 'RDV réservé', color: '#14b8a6', autoNotify: true },
        { key: 'ANTS_SUBMISSION', label: 'Dépôt ANTS', color: '#8b5cf6', description: 'Télé-procédure ANTS ou dépôt physique' },
        { key: 'WAITING_ORIGINAL', label: 'Originaux attendus', color: '#d97706', description: 'Envoi des originaux par courrier' },
        { key: 'OFII_INVESTIGATION', label: 'Enquête OFII', color: '#f97316', autoNotify: true, description: 'Visite médicale + enquête logement' },
        { key: 'INSTRUCTION', label: 'Instruction', color: '#0ea5e9' },
        { key: 'DECISION_WAIT', label: 'Attente décision', color: '#ef4444' },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── Titre de séjour simplifié (sans hunting, ex: renouvellement) ──
    TITRE_SEJOUR_SIMPLE: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Collecte documents', color: '#f59e0b' },
        { key: 'REVIEW', label: 'Vérification juriste', color: '#3b82f6' },
        { key: 'ANTS_SUBMISSION', label: 'Dépôt ANTS', color: '#8b5cf6' },
        { key: 'INSTRUCTION', label: 'Instruction', color: '#0ea5e9' },
        { key: 'DECISION_WAIT', label: 'Attente décision', color: '#ef4444' },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── Naturalisation ──
    NATURALISATION: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Collecte documents', color: '#f59e0b' },
        { key: 'REVIEW', label: 'Vérification juriste', color: '#3b82f6' },
        { key: 'DRAFTING', label: 'Rédaction CERFA', color: '#8b5cf6', description: 'Préparation du formulaire CERFA 12753*02' },
        { key: 'SUBMITTED', label: 'Déposé préfecture', color: '#06b6d4' },
        { key: 'INSTRUCTION', label: 'Instruction', color: '#0ea5e9', description: 'Entretien d\'assimilation + enquête' },
        { key: 'DECISION_WAIT', label: 'Attente décision', color: '#ef4444', description: 'Décision ministérielle (12-18 mois)' },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── Regroupement familial ──
    REGROUPEMENT_FAMILIAL: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Collecte documents', color: '#f59e0b' },
        { key: 'REVIEW', label: 'Vérification juriste', color: '#3b82f6' },
        { key: 'DRAFTING', label: 'Constitution dossier', color: '#8b5cf6', description: 'Formulaire OFII + pièces justificatives' },
        { key: 'SUBMITTED', label: 'Déposé OFII', color: '#06b6d4' },
        { key: 'OFII_INVESTIGATION', label: 'Enquête OFII', color: '#f97316', autoNotify: true, description: 'Enquête logement, ressources, conditions' },
        { key: 'DECISION_WAIT', label: 'Attente décision', color: '#ef4444', description: 'Réponse OFII/Préfecture (6-12 mois)' },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── Échange de permis de conduire ──
    PERMIS_CONDUIRE: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Collecte documents', color: '#f59e0b' },
        { key: 'REVIEW', label: 'Vérification juriste', color: '#3b82f6' },
        { key: 'ANTS_SUBMISSION', label: 'Demande ANTS', color: '#8b5cf6', description: 'Demande en ligne sur ANTS' },
        { key: 'INSTRUCTION', label: 'Instruction', color: '#0ea5e9', description: 'Vérification authenticité (2-6 mois)' },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── RDV / Consultation (service simple) ──
    CONSULTATION: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'SCHEDULING', label: 'Planification', color: '#f59e0b' },
        { key: 'BOOKED', label: 'RDV confirmé', color: '#14b8a6', autoNotify: true },
        { key: 'DONE', label: 'Terminé', color: '#10b981' },
    ],

    // ── Formation (Cours de français, Examen civique) ──
    FORMATION: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'PAID', label: 'Payé', color: '#22c55e', autoNotify: true },
        { key: 'COLLECTING', label: 'Pièces justificatives', color: '#f59e0b', description: 'Récupération du titre de séjour et convocation OFII' },
        { key: 'SCHEDULING', label: 'Planification sessions', color: '#3b82f6', description: 'Attribution du groupe et planning des cours' },
        { key: 'BOOKED', label: 'Inscrit / En cours', color: '#14b8a6', autoNotify: true, description: 'Formation en cours ou examen planifié' },
        { key: 'REVIEW', label: 'Évaluation / Résultat', color: '#8b5cf6', description: 'Correction en attente ou résultat examen' },
        { key: 'DONE', label: 'Terminé', color: '#10b981', description: 'Attestation / diplôme délivré' },
    ],

    // ── Qualification / Rappel (flux court du call center) ──
    QUALIFICATION: [
        { key: 'NEW', label: 'Nouveau', color: '#6366f1' },
        { key: 'TO_CONTACT', label: 'À rappeler', color: '#f59e0b', description: 'File d\'attente du siège' },
        { key: 'CONTACTED', label: 'Contacté', color: '#22c55e', description: 'Information donnée ou dossier ouvert' },
        { key: 'ARCHIVED', label: 'Sans suite', color: '#94a3b8', description: 'Injoignable ou non éligible' },
    ],
};


// ═══════════════════════════════════════════════════
// 2. CATALOGUE DE DOCUMENTS
// ═══════════════════════════════════════════════════

export interface DocumentItem {
    id: string;
    name: string;
    description: string;
    category: 'IDENTITY' | 'CIVIL' | 'RESIDENCE' | 'FINANCIAL' | 'PROFESSIONAL' | 'EDUCATION' | 'HEALTH' | 'OTHER';
}

export const DOCUMENT_CATALOG: Record<string, DocumentItem> = {
    // ── Identité ──
    passeport: { id: 'passeport', name: 'Passeport (toutes les pages)', description: 'Passeport en cours de validité, toutes les pages scannées', category: 'IDENTITY' },
    photos_identite: { id: 'photos_identite', name: 'Photos d\'identité (x3)', description: 'Format 35x45mm, fond blanc, récentes', category: 'IDENTITY' },
    titre_sejour_actuel: { id: 'titre_sejour_actuel', name: 'Titre de séjour actuel (recto-verso)', description: 'Carte de séjour en cours ou récépissé', category: 'IDENTITY' },
    cni_conjoint: { id: 'cni_conjoint', name: 'CNI du conjoint français', description: 'Carte nationale d\'identité recto-verso', category: 'IDENTITY' },
    permis_etranger: { id: 'permis_etranger', name: 'Permis de conduire étranger (original)', description: 'Document original du permis à échanger', category: 'IDENTITY' },

    // ── État civil ──
    acte_naissance: { id: 'acte_naissance', name: 'Acte de naissance (traduit)', description: 'Traduit par traducteur assermenté, apostillé si nécessaire', category: 'CIVIL' },
    acte_mariage: { id: 'acte_mariage', name: 'Acte de mariage', description: 'Moins de 3 mois, traduit si étranger', category: 'CIVIL' },
    livret_famille: { id: 'livret_famille', name: 'Livret de famille', description: 'Toutes les pages renseignées', category: 'CIVIL' },
    acte_naissance_enfants: { id: 'acte_naissance_enfants', name: 'Actes de naissance des enfants', description: 'Pour chaque enfant à charge', category: 'CIVIL' },
    casier_judiciaire: { id: 'casier_judiciaire', name: 'Casier judiciaire (pays d\'origine)', description: 'Bulletin n°3, traduit, moins de 3 mois', category: 'CIVIL' },
    jugement_divorce: { id: 'jugement_divorce', name: 'Jugement de divorce', description: 'Si précédemment marié, jugement définitif', category: 'CIVIL' },
    certificat_celibat: { id: 'certificat_celibat', name: 'Certificat de célibat / coutume', description: 'Certificat de coutume ou de célibat, traduit', category: 'CIVIL' },

    // ── Résidence ──
    justif_domicile: { id: 'justif_domicile', name: 'Justificatif de domicile (-3 mois)', description: 'Facture EDF/Gaz/Internet ou quittance de loyer', category: 'RESIDENCE' },
    justif_domicile_5ans: { id: 'justif_domicile_5ans', name: 'Justificatifs domicile (5 dernières années)', description: '1 justificatif par an sur les 5 dernières années', category: 'RESIDENCE' },
    bail_logement: { id: 'bail_logement', name: 'Bail/Contrat de location', description: 'Bail en cours, mentionnant la surface', category: 'RESIDENCE' },
    attestation_hebergement: { id: 'attestation_hebergement', name: 'Attestation d\'hébergement', description: 'Si hébergé par un tiers, avec pièce d\'identité', category: 'RESIDENCE' },

    // ── Finances ──
    bulletins_salaire: { id: 'bulletins_salaire', name: 'Bulletins de salaire (3 derniers mois)', description: 'Bulletins de paie des 3 derniers mois', category: 'FINANCIAL' },
    avis_imposition: { id: 'avis_imposition', name: 'Avis d\'imposition (N-1)', description: 'Dernier avis d\'imposition ou de non-imposition', category: 'FINANCIAL' },
    avis_imposition_3ans: { id: 'avis_imposition_3ans', name: 'Avis d\'imposition (3 dernières années)', description: '3 derniers avis d\'imposition', category: 'FINANCIAL' },
    contrat_travail: { id: 'contrat_travail', name: 'Contrat de travail', description: 'CDI/CDD en cours, signé par l\'employeur', category: 'FINANCIAL' },
    attestation_employeur: { id: 'attestation_employeur', name: 'Attestation employeur', description: 'Attestation de l\'employeur mentionnant le poste et salaire', category: 'FINANCIAL' },
    releve_bancaire: { id: 'releve_bancaire', name: 'Relevés bancaires (3 derniers mois)', description: 'Relevés des 3 derniers mois', category: 'FINANCIAL' },
    justif_ressources: { id: 'justif_ressources', name: 'Justificatif de ressources', description: 'Bourse, allocation, revenus, 615€/mois minimum', category: 'FINANCIAL' },

    // ── Professionnel ──
    autorisation_travail: { id: 'autorisation_travail', name: 'Autorisation de travail', description: 'Si applicable, délivrée par la DIRECCTE', category: 'PROFESSIONAL' },
    kbis: { id: 'kbis', name: 'Extrait K-bis (-3 mois)', description: 'Pour les entrepreneurs / profession libérale', category: 'PROFESSIONAL' },
    business_plan: { id: 'business_plan', name: 'Business plan', description: 'Plan d\'affaires détaillé pour création d\'entreprise', category: 'PROFESSIONAL' },

    // ── Éducation ──
    diplome_francais: { id: 'diplome_francais', name: 'Diplôme français (supérieur)', description: 'Licence, Master, ou Doctorat obtenu en France', category: 'EDUCATION' },
    certificat_scolarite: { id: 'certificat_scolarite', name: 'Certificat de scolarité', description: 'Inscription pour l\'année en cours', category: 'EDUCATION' },
    cert_francais_b1: { id: 'cert_francais_b1', name: 'Certificat de français B1', description: 'TCF, TEF, DELF B1 ou équivalent', category: 'EDUCATION' },
    cert_francais_b2: { id: 'cert_francais_b2', name: 'Certificat de français B2', description: 'TCF, TEF, DELF B2 ou équivalent', category: 'EDUCATION' },
    attestation_civique: { id: 'attestation_civique', name: 'Attestation formation civique', description: 'Attestation de suivi du parcours civique OFII', category: 'EDUCATION' },
    traduction_assermentee: { id: 'traduction_assermentee', name: 'Traduction assermentée du permis', description: 'Traduction par traducteur agréé près d\'un tribunal', category: 'EDUCATION' },

    // ── Santé ──
    certificat_medical: { id: 'certificat_medical', name: 'Certificat médical OFII', description: 'Visite médicale OFII ou médecin agréé', category: 'HEALTH' },
    rapport_medical: { id: 'rapport_medical', name: 'Rapport médical (étranger malade)', description: 'Rapport du médecin attestant des soins nécessaires', category: 'HEALTH' },
    assurance_maladie: { id: 'assurance_maladie', name: 'Attestation d\'assurance maladie', description: 'Carte vitale ou attestation de droits', category: 'HEALTH' },

    // ── Autres ──
    ordonnance_protection: { id: 'ordonnance_protection', name: 'Ordonnance de protection', description: 'Ordonnance du JAF en cas de violence conjugale', category: 'OTHER' },
    visa_long_sejour: { id: 'visa_long_sejour', name: 'Visa long séjour (VLS-TS)', description: 'Visa validé ou attestation OFII de validation', category: 'OTHER' },
    timbre_fiscal: { id: 'timbre_fiscal', name: 'Timbre fiscal (225€ / 75€)', description: 'Timbre OFII à acheter en ligne sur timbres.impots.gouv.fr', category: 'OTHER' },
    formulaire_cerfa: { id: 'formulaire_cerfa', name: 'Formulaire CERFA', description: 'Formulaire pré-rempli par le juriste', category: 'OTHER' },
};


// ═══════════════════════════════════════════════════
// 3. CATALOGUE DES SERVICES
// ═══════════════════════════════════════════════════

export type ServiceCategory =
    | 'TITRE_SEJOUR'
    | 'NATURALISATION'
    | 'REGROUPEMENT_FAMILIAL'
    | 'PERMIS_CONDUIRE'
    | 'CONSULTATION'
    | 'FORMATION'
    | 'QUALIFICATION'
    | 'AUTRE';

export interface ServiceConfig {
    id: string;
    name: string;
    shortName: string;
    category: ServiceCategory;
    description: string;
    pipelineTemplate: keyof typeof PIPELINE_TEMPLATES;
    requiredDocs: { docId: string; required: boolean }[];
    basePrice?: number; // en centimes
    estimatedDuration?: string; // ex: "3-6 mois"
    eligibilityRule?: string; // id de la règle dans rules_*.json
}

export const SERVICE_CATALOG: ServiceConfig[] = [

    // ═══════════════════════════════════════════════════════
    // TITRE DE SÉJOUR (61 procédures)
    // ═══════════════════════════════════════════════════════

    {
        id: 'carte_resident_refugie',
        name: 'Carte de Résident – Réfugié (10 ans, de plein droit)',
        shortName: 'Réfugié (10 ans, de plein',
        category: 'TITRE_SEJOUR',
        description: 'Carte de Résident – Réfugié (10 ans, de plein droit)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_refugie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_pluriannuelle_apatride',
        name: 'Carte de séjour pluriannuelle – Apatride (4 ans)',
        shortName: '– Apatride (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour pluriannuelle – Apatride (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_pluriannuelle_apatride',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_apatride_apres_4ans',
        name: 'Carte de Résident – Apatride (10 ans, après 4 ans de séjour)',
        shortName: 'Apatride (10 ans, après 4',
        category: 'TITRE_SEJOUR',
        description: 'Carte de Résident – Apatride (10 ans, après 4 ans de séjour)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_apatride_apres_4ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_longue_duree_ue',
        name: 'Carte de résident de longue durée - UE (10 ans)',
        shortName: 'UE (10 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident de longue durée - UE (10 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_longue_duree_ue',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_conjoint_francais',
        name: 'Carte de résident - Conjoint de Français (10 ans)',
        shortName: 'Conjoint de Français (10',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident - Conjoint de Français (10 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_conjoint_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_regroupement_familial',
        name: 'Carte de résident - Regroupement Familial (10 ans)',
        shortName: 'Regroupement Familial (10',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident - Regroupement Familial (10 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_regroupement_familial',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_carte_bleue_eu',
        name: 'Passeport Talent - Carte Bleue Européenne (4 ans)',
        shortName: 'Carte Bleue Européenne (4',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Carte Bleue Européenne (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_carte_bleue_eu',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_investisseur',
        name: 'Passeport Talent - Investisseur (4 ans)',
        shortName: 'Investisseur (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Investisseur (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_investisseur',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_salarie_qualifie',
        name: 'Passeport Talent - Salarié Qualifié (4 ans)',
        shortName: 'Salarié Qualifié (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Salarié Qualifié (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_salarie_qualifie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_entreprise_innovante',
        name: 'Passeport Talent - Entreprise Innovante (4 ans)',
        shortName: 'Entreprise Innovante (4 a',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Entreprise Innovante (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_entreprise_innovante',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'kbis', required: true },
            { docId: 'business_plan', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'releve_bancaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_mandataire',
        name: 'Passeport Talent - Mandataire Social (4 ans)',
        shortName: 'Mandataire Social (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Mandataire Social (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_mandataire',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_creation',
        name: 'Passeport Talent - Création d\'entreprise (4 ans)',
        shortName: 'Création d\'entreprise (4',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent - Création d\'entreprise (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_creation',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'kbis', required: true },
            { docId: 'business_plan', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'releve_bancaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_famille',
        name: 'Passeport Talent (Famille accompagnante)',
        shortName: 'Passeport Talent (Famille',
        category: 'TITRE_SEJOUR',
        description: 'Passeport Talent (Famille accompagnante)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_famille',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_conjoint_francais',
        name: 'Carte de séjour - VPF - Conjoint de Français',
        shortName: 'Conjoint de Français',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - VPF - Conjoint de Français',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_conjoint_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_parent_enfant_francais',
        name: 'Carte de séjour - VPF - Parent d’enfant français',
        shortName: 'Parent d’enfant français',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - VPF - Parent d’enfant français',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_parent_enfant_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance_enfants', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'livret_famille', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_pacs_francais',
        name: 'Carte de séjour - VPF - PACS avec Français',
        shortName: 'PAavec Français',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - VPF - PACS avec Français',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_pacs_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_jeune_entre_mineur',
        name: 'Carte de séjour - VPF - Liens personnels (Jeune entré mineur)',
        shortName: 'Liens personnels (Jeune e',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - VPF - Liens personnels (Jeune entré mineur)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_jeune_entre_mineur',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_humanitaire_violence',
        name: 'Carte de séjour - VPF - Protection (Violence conjugale)',
        shortName: 'Protection (Violence conj',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - VPF - Protection (Violence conjugale)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 0,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_humanitaire_violence',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'ordonnance_protection', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aps_enfant_malade',
        name: 'APS - Parent d’enfant malade',
        shortName: 'Parent d’enfant malade',
        category: 'TITRE_SEJOUR',
        description: 'APS - Parent d’enfant malade',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 15000,
        estimatedDuration: '1-3 mois',
        eligibilityRule: 'aps_enfant_malade',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_salarie',
        name: 'Carte de séjour - Salarié (CDI)',
        shortName: 'Salarié (CDI)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Salarié (CDI)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_salarie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_travailleur_temporaire',
        name: 'Carte de séjour - Travailleur temporaire (CDD)',
        shortName: 'Travailleur temporaire (C',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Travailleur temporaire (CDD)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_travailleur_temporaire',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_entrepreneur_liberale',
        name: 'Carte de séjour - Entrepreneur / Profession Libérale',
        shortName: 'Entrepreneur / Profession',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Entrepreneur / Profession Libérale',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_entrepreneur_liberale',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'kbis', required: true },
            { docId: 'business_plan', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'releve_bancaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_etudiant',
        name: 'Carte de séjour - Étudiant',
        shortName: 'Étudiant',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Étudiant',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 15000,
        estimatedDuration: '1-3 mois',
        eligibilityRule: 'cs_etudiant',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'certificat_scolarite', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rece_post_master',
        name: 'Carte RECE (Recherche d\'emploi / Création d\'entreprise)',
        shortName: '(Recherche d\'emploi / Cré',
        category: 'TITRE_SEJOUR',
        description: 'Carte RECE (Recherche d\'emploi / Création d\'entreprise)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 20000,
        estimatedDuration: '1-3 mois',
        eligibilityRule: 'rece_post_master',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'certificat_scolarite', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_visiteur',
        name: 'Carte de séjour - Visiteur',
        shortName: 'Visiteur',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Visiteur',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_visiteur',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_saisonnier',
        name: 'Carte de séjour - Travailleur saisonnier',
        shortName: 'Travailleur saisonnier',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Travailleur saisonnier',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_saisonnier',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_ict_detache',
        name: 'Carte de séjour - Salarié détaché ICT',
        shortName: 'Salarié détaché ICT',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Salarié détaché ICT',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_ict_detache',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aes_metiers_tension',
        name: 'Admission Exceptionnelle au Séjour (AES) - Métiers en tension',
        shortName: 'Métiers en tension',
        category: 'TITRE_SEJOUR',
        description: 'Admission Exceptionnelle au Séjour (AES) - Métiers en tension',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'aes_metiers_tension',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cra_algerien_resident_10ans',
        name: 'Certificat de Résidence Algérien - 10 ans',
        shortName: '10 ans',
        category: 'TITRE_SEJOUR',
        description: 'Certificat de Résidence Algérien - 10 ans',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cra_algerien_resident_10ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cra_algerien_conjoint_francais',
        name: 'Certificat de Résidence Algérien - 1 an (VPF)',
        shortName: '1 an (VPF)',
        category: 'TITRE_SEJOUR',
        description: 'Certificat de Résidence Algérien - 1 an (VPF)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cra_algerien_conjoint_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cra_algerien_activite_liberale',
        name: 'Certificat de Résidence Algérien - Profession non salariée',
        shortName: 'Profession non salariée',
        category: 'TITRE_SEJOUR',
        description: 'Certificat de Résidence Algérien - Profession non salariée',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cra_algerien_activite_liberale',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_citoyen_ue_inactif_ou_actif',
        name: 'Carte de séjour - Citoyen UE / EEE / Suisse',
        shortName: 'Citoyen UE / EEE / Suisse',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Citoyen UE / EEE / Suisse',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_citoyen_ue_inactif_ou_actif',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_membre_famille_ue',
        name: 'Carte de séjour - Membre de famille d\'un citoyen UE',
        shortName: 'Membre de famille d\'un ci',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour - Membre de famille d\'un citoyen UE',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_membre_famille_ue',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_10ans_general',
        name: 'Carte de résident 10 ans (cas général)',
        shortName: '10 ans (cas général)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident 10 ans (cas général)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_10ans_general',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_retraite',
        name: 'Carte de résident « retraité »',
        shortName: '« retraité »',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident « retraité »',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_retraite',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'carte_resident_permanent',
        name: 'Carte de résident permanent',
        shortName: 'permanent',
        category: 'TITRE_SEJOUR',
        description: 'Carte de résident permanent',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '4-8 mois',
        eligibilityRule: 'carte_resident_permanent',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_chercheur',
        name: 'Passeport talent – Chercheur',
        shortName: 'Chercheur',
        category: 'TITRE_SEJOUR',
        description: 'Passeport talent – Chercheur',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_chercheur',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_artiste',
        name: 'Passeport talent – Artiste / Interprète',
        shortName: 'Artiste / Interprète',
        category: 'TITRE_SEJOUR',
        description: 'Passeport talent – Artiste / Interprète',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_artiste',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'passeport_talent_sportif',
        name: 'Passeport talent – Sportif de haut niveau',
        shortName: 'Sportif de haut niveau',
        category: 'TITRE_SEJOUR',
        description: 'Passeport talent – Sportif de haut niveau',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'passeport_talent_sportif',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_liens_personnels',
        name: 'VPF – Liens personnels et familiaux intenses',
        shortName: 'Liens personnels et famil',
        category: 'TITRE_SEJOUR',
        description: 'VPF – Liens personnels et familiaux intenses',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_liens_personnels',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_victime_traite',
        name: 'VPF – Victime de traite / proxénétisme',
        shortName: 'Victime de traite / proxé',
        category: 'TITRE_SEJOUR',
        description: 'VPF – Victime de traite / proxénétisme',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 0,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_victime_traite',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'ordonnance_protection', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_rente_accident_travail',
        name: 'VPF – Rente accident du travail (≥20%)',
        shortName: 'Rente accident du travail',
        category: 'TITRE_SEJOUR',
        description: 'VPF – Rente accident du travail (≥20%)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_rente_accident_travail',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'vpf_etranger_malade',
        name: 'VPF – Étranger malade (soins indisponibles)',
        shortName: 'Étranger malade (soins in',
        category: 'TITRE_SEJOUR',
        description: 'VPF – Étranger malade (soins indisponibles)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'vpf_etranger_malade',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'rapport_medical', required: true },
            { docId: 'certificat_medical', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_salarie_mission',
        name: 'CS Salarié en mission (hors ICT)',
        shortName: 'Salarié en mission (hors',
        category: 'TITRE_SEJOUR',
        description: 'CS Salarié en mission (hors ICT)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_salarie_mission',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_stagiaire',
        name: 'CS Stagiaire',
        shortName: 'Stagiaire',
        category: 'TITRE_SEJOUR',
        description: 'CS Stagiaire',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_stagiaire',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_jeune_au_pair',
        name: 'CS Jeune au pair',
        shortName: 'Jeune au pair',
        category: 'TITRE_SEJOUR',
        description: 'CS Jeune au pair',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_jeune_au_pair',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'cs_volontaire',
        name: 'CS Volontaire associatif / Service civique',
        shortName: 'Volontaire associatif / S',
        category: 'TITRE_SEJOUR',
        description: 'CS Volontaire associatif / Service civique',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'cs_volontaire',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'visa_long_sejour', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aps_parent_etranger_malade',
        name: 'APS Parent accompagnant enfant malade',
        shortName: 'Parent accompagnant enfan',
        category: 'TITRE_SEJOUR',
        description: 'APS Parent accompagnant enfant malade',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 15000,
        estimatedDuration: '1-3 mois',
        eligibilityRule: 'aps_parent_etranger_malade',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aps_victime_violences_conjugales',
        name: 'APS Victime de violences conjugales',
        shortName: 'Victime de violences conj',
        category: 'TITRE_SEJOUR',
        description: 'APS Victime de violences conjugales',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 15000,
        estimatedDuration: '1-3 mois',
        eligibilityRule: 'aps_victime_violences_conjugales',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'ordonnance_protection', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aes_vie_privee_familiale',
        name: 'AES – Vie privée et familiale (10 ans de présence)',
        shortName: 'Vie privée et familiale (',
        category: 'TITRE_SEJOUR',
        description: 'AES – Vie privée et familiale (10 ans de présence)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'aes_vie_privee_familiale',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aes_parent_enfant_scolarise',
        name: 'AES – Parent d\'enfant scolarisé (3 ans)',
        shortName: 'Parent d\'enfant scolarisé',
        category: 'TITRE_SEJOUR',
        description: 'AES – Parent d\'enfant scolarisé (3 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'aes_parent_enfant_scolarise',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'aes_talent_exceptionnel',
        name: 'AES – Talent exceptionnel / Renommée',
        shortName: 'Talent exceptionnel / Ren',
        category: 'TITRE_SEJOUR',
        description: 'AES – Talent exceptionnel / Renommée',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 45000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'aes_talent_exceptionnel',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'accord_tunisien_salarie',
        name: 'Certificat de résidence tunisien – Salarié',
        shortName: 'Salarié',
        category: 'TITRE_SEJOUR',
        description: 'Certificat de résidence tunisien – Salarié',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'accord_tunisien_salarie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'accord_tunisien_conjoint_francais',
        name: 'Certificat de résidence tunisien – Conjoint de Français',
        shortName: 'Conjoint de Français',
        category: 'TITRE_SEJOUR',
        description: 'Certificat de résidence tunisien – Conjoint de Français',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'accord_tunisien_conjoint_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'accord_marocain_salarie',
        name: 'Titre de séjour marocain – Salarié',
        shortName: 'Salarié',
        category: 'TITRE_SEJOUR',
        description: 'Titre de séjour marocain – Salarié',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'accord_marocain_salarie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'accord_marocain_conjoint_francais',
        name: 'Titre de séjour marocain – Conjoint de Français',
        shortName: 'Conjoint de Français',
        category: 'TITRE_SEJOUR',
        description: 'Titre de séjour marocain – Conjoint de Français',
        pipelineTemplate: 'TITRE_SEJOUR_COMPLET',
        basePrice: 39000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'accord_marocain_conjoint_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'csp_salarie_4ans',
        name: 'Carte de séjour pluriannuelle « salarié » (4 ans)',
        shortName: '« salarié » (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour pluriannuelle « salarié » (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'csp_salarie_4ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'attestation_employeur', required: true },
            { docId: 'autorisation_travail', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'csp_vpf_conjoint_4ans',
        name: 'Carte de séjour pluriannuelle « VPF conjoint » (4 ans)',
        shortName: '« VPF conjoint » (4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour pluriannuelle « VPF conjoint » (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'csp_vpf_conjoint_4ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: false },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'assurance_maladie', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'csp_vpf_parent_4ans',
        name: 'Carte de séjour pluriannuelle « VPF parent d\'enfant français » (4 ans)',
        shortName: '« VPF parent d\'enfant fra',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour pluriannuelle « VPF parent d\'enfant français » (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'csp_vpf_parent_4ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance_enfants', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'livret_famille', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'csp_etudiant_pluriannuelle',
        name: 'Carte de séjour pluriannuelle « étudiant » (2-4 ans)',
        shortName: '« étudiant » (2-4 ans)',
        category: 'TITRE_SEJOUR',
        description: 'Carte de séjour pluriannuelle « étudiant » (2-4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 35000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'csp_etudiant_pluriannuelle',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'certificat_scolarite', required: true },
            { docId: 'justif_ressources', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'csp_passeport_talent_renouvellement',
        name: 'Renouvellement du Passeport Talent (4 ans)',
        shortName: 'Renouvellement du Passepo',
        category: 'TITRE_SEJOUR',
        description: 'Renouvellement du Passeport Talent (4 ans)',
        pipelineTemplate: 'TITRE_SEJOUR_SIMPLE',
        basePrice: 59000,
        estimatedDuration: '2-4 mois',
        eligibilityRule: 'csp_passeport_talent_renouvellement',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'diplome_francais', required: true },
            { docId: 'bulletins_salaire', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════
    // NATURALISATION (12 procédures)
    // ═══════════════════════════════════════════════════════

    {
        id: 'nat_droit_du_sol_18ans',
        name: 'Acquisition automatique à la majorité (Né en France)',
        shortName: 'Acquisition automatique à',
        category: 'NATURALISATION',
        description: 'Acquisition automatique à la majorité (Né en France)',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 29000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'nat_droit_du_sol_18ans',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_droit_du_sol_anticipe_13_16',
        name: 'Acquisition anticipée (13-16 ans) - Né en France',
        shortName: 'Acquisition anticipée (13',
        category: 'NATURALISATION',
        description: 'Acquisition anticipée (13-16 ans) - Né en France',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 29000,
        estimatedDuration: '3-6 mois',
        eligibilityRule: 'nat_droit_du_sol_anticipe_13_16',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_declaration_mariage',
        name: 'Déclaration de nationalité par mariage',
        shortName: 'Déclaration de nationalit',
        category: 'NATURALISATION',
        description: 'Déclaration de nationalité par mariage',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 39000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'nat_declaration_mariage',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'acte_mariage', required: true },
            { docId: 'cni_conjoint', required: true },
            { docId: 'livret_famille', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_declaration_fratrie',
        name: 'Déclaration de nationalité - Frère/Sœur de Français',
        shortName: 'Déclaration de nationalit',
        category: 'NATURALISATION',
        description: 'Déclaration de nationalité - Frère/Sœur de Français',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 35000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'nat_declaration_fratrie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_declaration_ascendant',
        name: 'Déclaration de nationalité - Ascendant de Français',
        shortName: 'Déclaration de nationalit',
        category: 'NATURALISATION',
        description: 'Déclaration de nationalité - Ascendant de Français',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 35000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'nat_declaration_ascendant',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_decret_etudes_sup',
        name: 'Naturalisation par décret (Parcours Études Supérieures)',
        shortName: 'Naturalisation par décret',
        category: 'NATURALISATION',
        description: 'Naturalisation par décret (Parcours Études Supérieures)',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_decret_etudes_sup',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_decret_refugie',
        name: 'Naturalisation par décret - Réfugié / Apatride',
        shortName: 'Naturalisation par décret',
        category: 'NATURALISATION',
        description: 'Naturalisation par décret - Réfugié / Apatride',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_decret_refugie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_decret_standard',
        name: 'Naturalisation par décret (Cas général)',
        shortName: 'Naturalisation par décret',
        category: 'NATURALISATION',
        description: 'Naturalisation par décret (Cas général)',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_decret_standard',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_reintegration_decret',
        name: 'Réintégration dans la nationalité (Décret)',
        shortName: 'Réintégration dans la nat',
        category: 'NATURALISATION',
        description: 'Réintégration dans la nationalité (Décret)',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_reintegration_decret',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_decret_service_militaire',
        name: 'Naturalisation – Service dans l\'armée française',
        shortName: 'Naturalisation – Service',
        category: 'NATURALISATION',
        description: 'Naturalisation – Service dans l\'armée française',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_decret_service_militaire',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_decret_legion_honneur',
        name: 'Naturalisation – Légion d\'honneur / Services rendus',
        shortName: 'Naturalisation – Légion d',
        category: 'NATURALISATION',
        description: 'Naturalisation – Légion d\'honneur / Services rendus',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 49000,
        estimatedDuration: '12-18 mois',
        eligibilityRule: 'nat_decret_legion_honneur',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'casier_judiciaire', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'avis_imposition_3ans', required: true },
            { docId: 'cert_francais_b1', required: true },
            { docId: 'attestation_civique', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'nat_declaration_possession_etat',
        name: 'Déclaration – Possession d\'état de Français',
        shortName: 'Déclaration – Possession',
        category: 'NATURALISATION',
        description: 'Déclaration – Possession d\'état de Français',
        pipelineTemplate: 'NATURALISATION',
        basePrice: 35000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'nat_declaration_possession_etat',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'justif_domicile_5ans', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════
    // REGROUPEMENT FAMILIAL (13 procédures)
    // ═══════════════════════════════════════════════════════

    {
        id: 'family_reunification_eligibility',
        name: 'Éligibilité Regroupement Familial (Droit commun)',
        shortName: 'Éligibilité Regroupement',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Éligibilité Regroupement Familial (Droit commun)',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'family_reunification_eligibility',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'family_reunification_algerian',
        name: 'Regroupement Familial — Accord franco-algérien',
        shortName: 'Regroupement Familial — A',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial — Accord franco-algérien',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'family_reunification_algerian',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'family_reunification_housing_surface',
        name: 'Condition de surface du logement',
        shortName: 'Condition de surface du l',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Condition de surface du logement',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'family_reunification_housing_surface',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_enfants_seuls',
        name: 'Regroupement Familial Partiel – Enfants seuls',
        shortName: 'Regroupement Familial Par',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial Partiel – Enfants seuls',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_enfants_seuls',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_conjoint_seul',
        name: 'Regroupement Familial Partiel – Conjoint seul',
        shortName: 'Regroupement Familial Par',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial Partiel – Conjoint seul',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_conjoint_seul',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_sur_place',
        name: 'Regroupement Familial Sur Place',
        shortName: 'Regroupement Familial Sur',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial Sur Place',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_sur_place',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_ascendant_charge_francais',
        name: 'VPF – Ascendant à charge d\'un Français',
        shortName: 'Ascendant à charge d\'un F',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'VPF – Ascendant à charge d\'un Français',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 45000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_ascendant_charge_francais',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_tunisien_conjoint',
        name: 'Regroupement Familial – Accord Tunisien (Conjoint)',
        shortName: 'Regroupement Familial – A',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial – Accord Tunisien (Conjoint)',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_tunisien_conjoint',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_marocain_conjoint',
        name: 'Regroupement Familial – Accord Marocain (Conjoint)',
        shortName: 'Regroupement Familial – A',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Regroupement Familial – Accord Marocain (Conjoint)',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_marocain_conjoint',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_famille_refugie',
        name: 'Réunification Familiale – Famille de Réfugié',
        shortName: 'Réunification Familiale –',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Réunification Familiale – Famille de Réfugié',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_famille_refugie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_famille_apatride',
        name: 'Réunification Familiale – Famille d\'Apatride',
        shortName: 'Réunification Familiale –',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Réunification Familiale – Famille d\'Apatride',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 55000,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_famille_apatride',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_mna_prise_charge',
        name: 'Prise en charge d\'un Mineur Non Accompagné (MNA)',
        shortName: 'Prise en charge d\'un Mine',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Prise en charge d\'un Mineur Non Accompagné (MNA)',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 0,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_mna_prise_charge',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'rf_enfant_confie_ase',
        name: 'Titre de séjour – Enfant confié à l\'ASE (16-18 ans)',
        shortName: 'Titre de séjour – Enfant',
        category: 'REGROUPEMENT_FAMILIAL',
        description: 'Titre de séjour – Enfant confié à l\'ASE (16-18 ans)',
        pipelineTemplate: 'REGROUPEMENT_FAMILIAL',
        basePrice: 0,
        estimatedDuration: '6-12 mois',
        eligibilityRule: 'rf_enfant_confie_ase',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'acte_naissance', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'bail_logement', required: true },
            { docId: 'avis_imposition', required: true },
            { docId: 'contrat_travail', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════
    // ÉCHANGE PERMIS DE CONDUIRE (3 procédures)
    // ═══════════════════════════════════════════════════════

    {
        id: 'echange_permis_standard',
        name: 'Échange de Permis - Cas Standard',
        shortName: 'Échange de Permis - Cas S',
        category: 'PERMIS_CONDUIRE',
        description: 'Échange de Permis - Cas Standard',
        pipelineTemplate: 'PERMIS_CONDUIRE',
        basePrice: 9000,
        estimatedDuration: '2-6 mois',
        eligibilityRule: 'echange_permis_standard',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'permis_etranger', required: true },
            { docId: 'traduction_assermentee', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'echange_permis_etudiant',
        name: 'Conduite sous statut Étudiant',
        shortName: 'Conduite sous statut Étud',
        category: 'PERMIS_CONDUIRE',
        description: 'Conduite sous statut Étudiant',
        pipelineTemplate: 'PERMIS_CONDUIRE',
        basePrice: 9000,
        estimatedDuration: '2-6 mois',
        eligibilityRule: 'echange_permis_etudiant',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'permis_etranger', required: true },
            { docId: 'traduction_assermentee', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },
    {
        id: 'echange_permis_refugie',
        name: 'Échange de Permis - Réfugié / BPI',
        shortName: 'Échange de Permis - Réfug',
        category: 'PERMIS_CONDUIRE',
        description: 'Échange de Permis - Réfugié / BPI',
        pipelineTemplate: 'PERMIS_CONDUIRE',
        basePrice: 9000,
        estimatedDuration: '2-6 mois',
        eligibilityRule: 'echange_permis_refugie',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'photos_identite', required: true },
            { docId: 'justif_domicile', required: true },
            { docId: 'timbre_fiscal', required: true },
            { docId: 'permis_etranger', required: true },
            { docId: 'traduction_assermentee', required: true },
            { docId: 'titre_sejour_actuel', required: true },
            { docId: 'formulaire_cerfa', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════════
    // CONSULTATION & RDV PRÉFECTURE (3 services)
    // ═══════════════════════════════════════════════════════════

    {
        id: 'rdv_prefecture',
        name: 'Recherche RDV Préfecture',
        shortName: 'RDV Préfecture',
        category: 'CONSULTATION',
        description: 'Recherche et prise de rendez-vous en préfecture',
        pipelineTemplate: 'CONSULTATION',
        basePrice: 9900,
        estimatedDuration: '1-4 semaines',
        requiredDocs: [
            { docId: 'titre_sejour_actuel', required: true },
        ]
    },
    {
        id: 'bilan_eligibilite',
        name: 'Bilan d\'éligibilité complet',
        shortName: 'Bilan',
        category: 'CONSULTATION',
        description: 'Analyse complète de votre situation avec recommandations personnalisées',
        pipelineTemplate: 'CONSULTATION',
        basePrice: 4900,
        estimatedDuration: '1-2 jours',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'titre_sejour_actuel', required: false },
        ]
    },
    {
        id: 'consultation_juridique',
        name: 'Consultation Juridique',
        shortName: 'Consultation',
        category: 'CONSULTATION',
        description: 'Consultation juridique personnalisée avec un expert en droit des étrangers',
        pipelineTemplate: 'CONSULTATION',
        basePrice: 7500,
        estimatedDuration: '1-2 jours',
        requiredDocs: [
            { docId: 'passeport', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════════
    // FORMATION (3 services)
    // ═══════════════════════════════════════════════════════════

    {
        id: 'cours_francais_a2',
        name: 'Cours de Français — Niveau A2',
        shortName: 'Français A2',
        category: 'FORMATION',
        description: 'Préparation au niveau A2 (DELF A2 / TCF)',
        pipelineTemplate: 'FORMATION',
        basePrice: 29000,
        estimatedDuration: '2-3 mois',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'titre_sejour_actuel', required: true },
        ]
    },
    {
        id: 'cours_francais_b1',
        name: 'Cours de Français — Niveau B1',
        shortName: 'Français B1',
        category: 'FORMATION',
        description: 'Préparation au niveau B1 (DELF B1 / TCF)',
        pipelineTemplate: 'FORMATION',
        basePrice: 39000,
        estimatedDuration: '3-4 mois',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'titre_sejour_actuel', required: true },
        ]
    },
    {
        id: 'examen_civique',
        name: 'Examen Civique — Valeurs de la République',
        shortName: 'Examen Civique',
        category: 'FORMATION',
        description: 'Formation obligatoire pour carte de résident et naturalisation',
        pipelineTemplate: 'FORMATION',
        basePrice: 19000,
        estimatedDuration: '1-2 semaines',
        requiredDocs: [
            { docId: 'passeport', required: true },
            { docId: 'titre_sejour_actuel', required: true },
        ]
    },

    // ═══════════════════════════════════════════════════════════
    // QUALIFICATION / RAPPEL (2 services)
    // ═══════════════════════════════════════════════════════════

    {
        id: 'demande_rappel',
        name: 'Demande de Rappel',
        shortName: 'Rappel',
        category: 'QUALIFICATION',
        description: 'Le prospect demande à être recontacté',
        pipelineTemplate: 'QUALIFICATION',
        basePrice: 0,
        estimatedDuration: '24-48h',
        requiredDocs: []
    },
    {
        id: 'contact_simple',
        name: 'Contact Simple / Demande d\'information',
        shortName: 'Contact',
        category: 'QUALIFICATION',
        description: 'Demande d\'information générale',
        pipelineTemplate: 'QUALIFICATION',
        basePrice: 0,
        estimatedDuration: '24-48h',
        requiredDocs: []
    },

];




// ═══════════════════════════════════════════════════
// 4. FONCTIONS UTILITAIRES
// ═══════════════════════════════════════════════════

/**
 * Retrouve la config d'un service par son ID
 */
export function getServiceConfig(serviceId: string): ServiceConfig | undefined {
    return SERVICE_CATALOG.find(s => s.id === serviceId);
}

/**
 * Retourne les étapes du pipeline pour un service donné
 */
export function getServicePipeline(serviceId: string): PipelineStage[] {
    const service = getServiceConfig(serviceId);
    if (!service) return PIPELINE_TEMPLATES['TITRE_SEJOUR_COMPLET']; // fallback
    return PIPELINE_TEMPLATES[service.pipelineTemplate] || PIPELINE_TEMPLATES['TITRE_SEJOUR_COMPLET'];
}

/**
 * Retourne les documents requis pour un service, enrichis avec les infos du catalogue
 */
export function getServiceRequiredDocs(serviceId: string): (DocumentItem & { required: boolean })[] {
    const service = getServiceConfig(serviceId);
    if (!service) return [];
    return service.requiredDocs
        .map(ref => {
            const doc = DOCUMENT_CATALOG[ref.docId];
            if (!doc) return null;
            return { ...doc, required: ref.required };
        })
        .filter(Boolean) as (DocumentItem & { required: boolean })[];
}

/**
 * Retourne tous les services groupés par catégorie
 */
export function getServicesByCategory(): Record<ServiceCategory, ServiceConfig[]> {
    return SERVICE_CATALOG.reduce((acc, service) => {
        if (!acc[service.category]) acc[service.category] = [];
        acc[service.category].push(service);
        return acc;
    }, {} as Record<ServiceCategory, ServiceConfig[]>);
}

/**
 * Vérifie si une étape est valide pour un service donné
 */
export function isValidStageForService(serviceId: string, stage: string): boolean {
    const pipeline = getServicePipeline(serviceId);
    return pipeline.some(s => s.key === stage);
}

/**
 * Retourne l'étape suivante dans le pipeline d'un service
 */
export function getNextStage(serviceId: string, currentStage: string): string | null {
    const pipeline = getServicePipeline(serviceId);
    const currentIndex = pipeline.findIndex(s => s.key === currentStage);
    if (currentIndex === -1 || currentIndex >= pipeline.length - 1) return null;
    return pipeline[currentIndex + 1].key;
}

/**
 * Retourne la liste des templates de pipeline disponibles
 */
export function getPipelineTemplateNames(): string[] {
    return Object.keys(PIPELINE_TEMPLATES);
}
