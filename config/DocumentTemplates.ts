/**
 * Référentiel des Documents - Configuration Maître
 * Définit dynamiquement quels documents sont demandés pour chaque service
 */

// ============================================
// A. TYPES DE DOCUMENTS
// ============================================

export type DocumentCategory = 'IDENTITY' | 'FINANCIAL' | 'RESIDENCE' | 'CIVIL' | 'PROFESSIONAL' | 'EDUCATION' | 'OTHER';
export type OCRType = 'PASSPORT' | 'ID_CARD' | 'TAX_NOTICE' | 'RESIDENCE_PROOF' | 'BIRTH_CERTIFICATE' | 'PHOTO' | 'DRIVING_LICENSE' | 'GENERIC';

export interface DocumentRequirement {
    id: string;
    label: string;
    description: string;
    category: DocumentCategory;
    ocrType?: OCRType;
    required: boolean;
    acceptedFormats?: string[]; // ex: ['image/*', 'application/pdf']
    maxSizeMB?: number;
}

// ============================================
// B. CATALOGUE COMPLET DES DOCUMENTS
// ============================================

export const DOC_CATALOG: Record<string, DocumentRequirement> = {
    // === IDENTITÉ ===
    'PASSPORT': {
        id: 'PASSPORT',
        label: 'Passeport (Validité)',
        description: 'La page avec votre photo et signature. Le document doit être en cours de validité.',
        category: 'IDENTITY',
        ocrType: 'PASSPORT',
        required: true,
        acceptedFormats: ['image/*', 'application/pdf'],
        maxSizeMB: 10
    },
    'PASSPORT_ALL_PAGES': {
        id: 'PASSPORT_ALL_PAGES',
        label: 'Passeport (Toutes les pages)',
        description: 'Scannez toutes les pages de votre passeport, y compris les pages vierges.',
        category: 'IDENTITY',
        ocrType: 'PASSPORT',
        required: true
    },
    'ID_CARD': {
        id: 'ID_CARD',
        label: 'Carte d\'identité',
        description: 'Recto et verso de votre carte d\'identité en cours de validité.',
        category: 'IDENTITY',
        ocrType: 'ID_CARD',
        required: true
    },
    'PHOTOS': {
        id: 'PHOTOS',
        label: 'E-Photos (ANTS)',
        description: 'Planche de photos avec code numérique obtenu sur un site agréé ANTS.',
        category: 'IDENTITY',
        ocrType: 'PHOTO',
        required: true
    },
    'PHOTOS_35x45': {
        id: 'PHOTOS_35x45',
        label: 'Photos d\'identité (35x45mm)',
        description: '3 photos d\'identité récentes aux normes officielles (fond blanc, sans lunettes).',
        category: 'IDENTITY',
        ocrType: 'PHOTO',
        required: true
    },
    'TITRE_SEJOUR_ACTUEL': {
        id: 'TITRE_SEJOUR_ACTUEL',
        label: 'Titre de séjour en cours',
        description: 'Recto-verso de votre titre de séjour actuel (renouvellement).',
        category: 'IDENTITY',
        required: true
    },

    // === RÉSIDENCE ===
    'RESIDENCE_PROOF': {
        id: 'RESIDENCE_PROOF',
        label: 'Justificatif de Domicile',
        description: 'Facture EDF, Loyer, Internet ou Téléphone de moins de 3 mois.',
        category: 'RESIDENCE',
        ocrType: 'RESIDENCE_PROOF',
        required: true
    },
    'RESIDENCE_PROOF_5Y': {
        id: 'RESIDENCE_PROOF_5Y',
        label: 'Justificatifs de Domicile (5 ans)',
        description: 'Un justificatif par année des 5 dernières années (quittances, factures).',
        category: 'RESIDENCE',
        ocrType: 'RESIDENCE_PROOF',
        required: true
    },
    'ATTESTATION_HEBERGEMENT': {
        id: 'ATTESTATION_HEBERGEMENT',
        label: 'Attestation d\'hébergement',
        description: 'Attestation sur l\'honneur + pièce d\'identité de l\'hébergeant + justificatif de domicile.',
        category: 'RESIDENCE',
        required: true
    },

    // === FINANCES ===
    'TAX_NOTICE': {
        id: 'TAX_NOTICE',
        label: 'Avis d\'Imposition',
        description: 'Dernier avis d\'imposition disponible (N-1).',
        category: 'FINANCIAL',
        ocrType: 'TAX_NOTICE',
        required: true
    },
    'TAX_NOTICE_3Y': {
        id: 'TAX_NOTICE_3Y',
        label: 'Avis d\'Imposition (3 dernières années)',
        description: 'Les 3 derniers avis d\'imposition (N-1, N-2, N-3).',
        category: 'FINANCIAL',
        ocrType: 'TAX_NOTICE',
        required: true
    },
    'BANK_STATEMENTS': {
        id: 'BANK_STATEMENTS',
        label: 'Relevés bancaires (3 mois)',
        description: 'Les 3 derniers relevés de compte bancaire.',
        category: 'FINANCIAL',
        required: true
    },
    'FISCAL_STAMP': {
        id: 'FISCAL_STAMP',
        label: 'Timbre fiscal',
        description: 'Timbre fiscal acheté en ligne ou en bureau de tabac.',
        category: 'FINANCIAL',
        required: true
    },
    'FISCAL_STAMP_225': {
        id: 'FISCAL_STAMP_225',
        label: 'Timbre fiscal (225€)',
        description: 'Timbre fiscal de 225€ pour titre de séjour.',
        category: 'FINANCIAL',
        required: true
    },
    'FISCAL_STAMP_55': {
        id: 'FISCAL_STAMP_55',
        label: 'Timbre fiscal (55€)',
        description: 'Timbre fiscal de 55€ pour demande de naturalisation.',
        category: 'FINANCIAL',
        required: true
    },
    'INVESTMENT_PROOF': {
        id: 'INVESTMENT_PROOF',
        label: 'Justificatif d\'investissement',
        description: 'Preuve d\'investissement dans une entreprise française (relevé, attestation banque).',
        category: 'FINANCIAL',
        required: true
    },
    'RESOURCES_PROOF': {
        id: 'RESOURCES_PROOF',
        label: 'Justificatif de ressources',
        description: 'Preuve de ressources suffisantes (relevés, attestation bancaire...).',
        category: 'FINANCIAL',
        required: true
    },

    // === ÉTAT CIVIL ===
    'BIRTH_CERTIFICATE': {
        id: 'BIRTH_CERTIFICATE',
        label: 'Acte de Naissance',
        description: 'Copie intégrale avec filiation. Traduction assermentée si document étranger.',
        category: 'CIVIL',
        ocrType: 'BIRTH_CERTIFICATE',
        required: true
    },
    'MARRIAGE_CERTIFICATE': {
        id: 'MARRIAGE_CERTIFICATE',
        label: 'Acte de Mariage',
        description: 'Copie intégrale de l\'acte de mariage. Traduction assermentée si étranger.',
        category: 'CIVIL',
        required: true
    },
    'CRIMINAL_RECORD': {
        id: 'CRIMINAL_RECORD',
        label: 'Casier Judiciaire',
        description: 'Extrait de casier judiciaire du pays d\'origine (bulletin n°3).',
        category: 'CIVIL',
        required: true
    },
    'SPOUSE_ID': {
        id: 'SPOUSE_ID',
        label: 'Pièce d\'identité du conjoint',
        description: 'Carte d\'identité ou passeport du conjoint français.',
        category: 'CIVIL',
        required: true
    },
    'COMMUNITY_PROOF': {
        id: 'COMMUNITY_PROOF',
        label: 'Preuves de vie commune',
        description: 'Attestations, factures communes, bail commun prouvant la cohabitation.',
        category: 'CIVIL',
        required: true
    },
    'CHILD_BIRTH_CERT': {
        id: 'CHILD_BIRTH_CERT',
        label: 'Acte de naissance de l\'enfant',
        description: 'Acte de naissance de l\'enfant français.',
        category: 'CIVIL',
        required: true
    },

    // === PROFESSIONNEL ===
    'WORK_CONTRACT': {
        id: 'WORK_CONTRACT',
        label: 'Contrat de travail',
        description: 'Contrat de travail en cours ou promesse d\'embauche.',
        category: 'PROFESSIONAL',
        required: true
    },
    'WORK_CONTRACT_CDI': {
        id: 'WORK_CONTRACT_CDI',
        label: 'Contrat de travail CDI',
        description: 'Contrat à durée indéterminée.',
        category: 'PROFESSIONAL',
        required: true
    },
    'WORK_CONTRACT_CDD': {
        id: 'WORK_CONTRACT_CDD',
        label: 'Contrat de travail CDD',
        description: 'Contrat à durée déterminée.',
        category: 'PROFESSIONAL',
        required: true
    },
    'EMPLOYER_CERTIFICATE': {
        id: 'EMPLOYER_CERTIFICATE',
        label: 'Attestation employeur',
        description: 'Attestation de l\'employeur mentionnant le poste et la rémunération.',
        category: 'PROFESSIONAL',
        required: true
    },
    'PAYSLIPS': {
        id: 'PAYSLIPS',
        label: 'Bulletins de salaire (3 mois)',
        description: 'Les 3 derniers bulletins de salaire.',
        category: 'PROFESSIONAL',
        required: true
    },
    'WORK_AUTHORIZATION': {
        id: 'WORK_AUTHORIZATION',
        label: 'Autorisation de travail',
        description: 'Autorisation de travail délivrée par la DIRECCTE.',
        category: 'PROFESSIONAL',
        required: true
    },
    'BUSINESS_PLAN': {
        id: 'BUSINESS_PLAN',
        label: 'Business Plan',
        description: 'Plan d\'affaires détaillé pour création d\'entreprise.',
        category: 'PROFESSIONAL',
        required: true
    },
    'KBIS': {
        id: 'KBIS',
        label: 'Extrait Kbis',
        description: 'Extrait Kbis de la société (moins de 3 mois).',
        category: 'PROFESSIONAL',
        required: true
    },
    'JEI_CERTIFICATE': {
        id: 'JEI_CERTIFICATE',
        label: 'Attestation JEI',
        description: 'Attestation de statut Jeune Entreprise Innovante.',
        category: 'PROFESSIONAL',
        required: true
    },

    // === ÉDUCATION ===
    'STUDENT_CARD': {
        id: 'STUDENT_CARD',
        label: 'Carte d\'étudiant',
        description: 'Carte d\'étudiant de l\'année en cours.',
        category: 'EDUCATION',
        required: true
    },
    'ENROLLMENT_PROOF': {
        id: 'ENROLLMENT_PROOF',
        label: 'Inscription universitaire',
        description: 'Certificat de scolarité ou d\'inscription dans l\'établissement.',
        category: 'EDUCATION',
        required: true
    },
    'DIPLOMAS': {
        id: 'DIPLOMAS',
        label: 'Diplômes',
        description: 'Copies des diplômes obtenus (Master, Licence Pro, etc.).',
        category: 'EDUCATION',
        required: true
    },
    'FRENCH_B1': {
        id: 'FRENCH_B1',
        label: 'Diplôme de français niveau B1',
        description: 'TCF, TEF, DELF B1 ou équivalent.',
        category: 'OTHER',
        required: true
    },
    'FRENCH_B2': {
        id: 'FRENCH_B2',
        label: 'Diplôme de français niveau B2',
        description: 'TCF, TEF, DELF B2 ou tout diplôme attestant du niveau B2.',
        category: 'OTHER',
        required: true
    },
    'CIVIC_CERTIFICATE': {
        id: 'CIVIC_CERTIFICATE',
        label: 'Certificat de formation civique',
        description: 'Attestation de suivi de la journée de formation civique OFII.',
        category: 'OTHER',
        required: true
    },

    // === PERMIS DE CONDUIRE ===
    'FOREIGN_LICENSE': {
        id: 'FOREIGN_LICENSE',
        label: 'Permis de conduire étranger',
        description: 'Recto et verso de votre permis de conduire étranger.',
        category: 'IDENTITY',
        ocrType: 'DRIVING_LICENSE',
        required: true
    },
    'LICENSE_TRANSLATION': {
        id: 'LICENSE_TRANSLATION',
        label: 'Traduction assermentée du permis',
        description: 'Traduction certifiée conforme par un traducteur assermenté.',
        category: 'OTHER',
        required: true
    },

    // === PROTECTION / SANTÉ ===
    'PROTECTION_ORDER': {
        id: 'PROTECTION_ORDER',
        label: 'Ordonnance de protection',
        description: 'Ordonnance de protection délivrée par le juge.',
        category: 'OTHER',
        required: true
    },
    'HEALTH_CERTIFICATE': {
        id: 'HEALTH_CERTIFICATE',
        label: 'Certificat médical',
        description: 'Certificat médical attestant de l\'état de santé.',
        category: 'OTHER',
        required: true
    },
    'HEALTH_INSURANCE': {
        id: 'HEALTH_INSURANCE',
        label: 'Attestation d\'assurance maladie',
        description: 'Attestation de couverture maladie (Sécurité sociale ou privée).',
        category: 'OTHER',
        required: true
    }
};

// ============================================
// C. BASES COMMUNES (HÉRITAGE)
// ============================================

// Documents COMMUNS à tous les titres de séjour
const TITRE_SEJOUR_BASE = [
    'PASSPORT_ALL_PAGES',
    'RESIDENCE_PROOF',
    'PHOTOS',
    'FISCAL_STAMP_225'
];

// Documents COMMUNS pour la naturalisation
const NATURALISATION_BASE = [
    'PASSPORT_ALL_PAGES',
    'BIRTH_CERTIFICATE',
    'RESIDENCE_PROOF_5Y',
    'TAX_NOTICE_3Y',
    'FRENCH_B2',
    'CRIMINAL_RECORD',
    'PHOTOS_35x45',
    'FISCAL_STAMP_55'
];

// Documents COMMUNS VPF (Vie Privée et Familiale)
const VPF_BASE = [
    ...TITRE_SEJOUR_BASE,
    'BIRTH_CERTIFICATE'
];

// Documents COMMUNS Passeport Talent
const PASSEPORT_TALENT_BASE = [
    ...TITRE_SEJOUR_BASE,
    'DIPLOMAS'
];

// ============================================
// D. MODÈLES PAR SERVICE AVEC HÉRITAGE
// ============================================

export const SERVICE_TEMPLATES: Record<string, string[]> = {
    // ========================================
    // NATURALISATION - SOUS-TYPES
    // ========================================
    'naturalisation': NATURALISATION_BASE,

    // Par mariage avec un français
    'nat_declaration_mariage': [
        ...NATURALISATION_BASE,
        'MARRIAGE_CERTIFICATE',
        'SPOUSE_ID',
        'COMMUNITY_PROOF'
    ],

    // Par droit du sol (né en France)
    'nat_droit_du_sol_18ans': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'RESIDENCE_PROOF_5Y',
        'CRIMINAL_RECORD',
        'PHOTOS_35x45'
    ],

    // Naturalisation études supérieures
    'nat_decret_etudes_sup': [
        ...NATURALISATION_BASE,
        'DIPLOMAS',
        'ENROLLMENT_PROOF'
    ],

    // Naturalisation réfugié/apatride
    'nat_decret_refugie': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'RESIDENCE_PROOF',
        'FRENCH_B1',
        'PHOTOS_35x45',
        'FISCAL_STAMP_55'
    ],

    // ========================================
    // CARTE DE RÉSIDENT (10 ans)
    // ========================================
    'carte_resident_longue_duree_ue': [
        ...TITRE_SEJOUR_BASE,
        'TITRE_SEJOUR_ACTUEL',
        'TAX_NOTICE_3Y',
        'FRENCH_B1',
        'HEALTH_INSURANCE'
    ],

    'carte_resident_conjoint_francais': [
        ...VPF_BASE,
        'MARRIAGE_CERTIFICATE',
        'SPOUSE_ID',
        'COMMUNITY_PROOF',
        'FRENCH_B1'
    ],

    // ========================================
    // TITRES DE SÉJOUR - VPF (VIE PRIVÉE FAMILIALE)
    // ========================================
    'titre_sejour': TITRE_SEJOUR_BASE,

    'vpf_conjoint_francais': [
        ...VPF_BASE,
        'MARRIAGE_CERTIFICATE',
        'SPOUSE_ID',
        'COMMUNITY_PROOF'
    ],

    'vpf_parent_enfant_francais': [
        ...VPF_BASE,
        'CHILD_BIRTH_CERT',
        'COMMUNITY_PROOF'
    ],

    'vpf_pacs_francais': [
        ...VPF_BASE,
        'COMMUNITY_PROOF',
        'SPOUSE_ID'
    ],

    'vpf_humanitaire_violence': [
        ...VPF_BASE,
        'PROTECTION_ORDER'
    ],

    // ========================================
    // TITRES DE SÉJOUR - TRAVAIL
    // ========================================
    'cs_salarie': [
        ...TITRE_SEJOUR_BASE,
        'WORK_CONTRACT_CDI',
        'EMPLOYER_CERTIFICATE',
        'PAYSLIPS',
        'WORK_AUTHORIZATION'
    ],

    'cs_travailleur_temporaire': [
        ...TITRE_SEJOUR_BASE,
        'WORK_CONTRACT_CDD',
        'EMPLOYER_CERTIFICATE',
        'PAYSLIPS',
        'WORK_AUTHORIZATION'
    ],

    'cs_entrepreneur_liberale': [
        ...TITRE_SEJOUR_BASE,
        'BUSINESS_PLAN',
        'KBIS',
        'TAX_NOTICE',
        'BANK_STATEMENTS'
    ],

    // ========================================
    // TITRES DE SÉJOUR - ÉTUDIANTS
    // ========================================
    'cs_etudiant': [
        ...TITRE_SEJOUR_BASE,
        'STUDENT_CARD',
        'ENROLLMENT_PROOF',
        'RESOURCES_PROOF'
    ],

    'rece_post_master': [
        ...TITRE_SEJOUR_BASE,
        'DIPLOMAS',
        'TITRE_SEJOUR_ACTUEL'
    ],

    // ========================================
    // PASSEPORT TALENT
    // ========================================
    'passeport_talent_carte_bleue_eu': [
        ...PASSEPORT_TALENT_BASE,
        'WORK_CONTRACT_CDI',
        'PAYSLIPS',
        'EMPLOYER_CERTIFICATE'
    ],

    'passeport_talent_salarie_qualifie': [
        ...PASSEPORT_TALENT_BASE,
        'WORK_CONTRACT',
        'PAYSLIPS'
    ],

    'passeport_talent_investisseur': [
        ...PASSEPORT_TALENT_BASE,
        'INVESTMENT_PROOF',
        'BANK_STATEMENTS',
        'BUSINESS_PLAN'
    ],

    'passeport_talent_creation': [
        ...PASSEPORT_TALENT_BASE,
        'BUSINESS_PLAN',
        'INVESTMENT_PROOF'
    ],

    'passeport_talent_entreprise_innovante': [
        ...PASSEPORT_TALENT_BASE,
        'WORK_CONTRACT',
        'JEI_CERTIFICATE'
    ],

    'passeport_talent_mandataire': [
        ...PASSEPORT_TALENT_BASE,
        'KBIS',
        'PAYSLIPS'
    ],

    // ========================================
    // REGROUPEMENT FAMILIAL
    // ========================================
    'regroupement_familial': [
        'PASSPORT',
        'RESIDENCE_PROOF',
        'TAX_NOTICE',
        'MARRIAGE_CERTIFICATE',
        'BIRTH_CERTIFICATE',
        'BANK_STATEMENTS',
        'TITRE_SEJOUR_ACTUEL'
    ],

    'carte_resident_regroupement_familial': [
        ...VPF_BASE,
        'MARRIAGE_CERTIFICATE',
        'TITRE_SEJOUR_ACTUEL',
        'TAX_NOTICE'
    ],

    // ========================================
    // AUTRES
    // ========================================
    'cs_visiteur': [
        ...TITRE_SEJOUR_BASE,
        'RESOURCES_PROOF',
        'HEALTH_INSURANCE'
    ],

    // Échange permis de conduire
    'permis_conduire': [
        'PASSPORT',
        'FOREIGN_LICENSE',
        'LICENSE_TRANSLATION',
        'RESIDENCE_PROOF',
        'PHOTOS_35x45'
    ],

    'driving_exchange': [
        'PASSPORT',
        'FOREIGN_LICENSE',
        'LICENSE_TRANSLATION',
        'RESIDENCE_PROOF',
        'PHOTOS_35x45'
    ],

    // ========================================
    // NATURALISATION - SUITE
    // ========================================
    'nat_declaration_fratrie': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'RESIDENCE_PROOF_5Y',
        'CRIMINAL_RECORD',
        'PHOTOS_35x45',
        'ENROLLMENT_PROOF'
    ],

    'nat_declaration_ascendant': [
        ...NATURALISATION_BASE,
        'BIRTH_CERTIFICATE',
        'COMMUNITY_PROOF'
    ],

    'nat_reintegration_decret': [
        ...NATURALISATION_BASE
    ],

    // ========================================
    // CARTE DE RÉSIDENT - SUITE
    // ========================================
    'carte_resident_refugie_apatride': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'RESIDENCE_PROOF',
        'PHOTOS_35x45',
        'PROTECTION_ORDER'
    ],

    // ========================================
    // PASSEPORT TALENT - SUITE
    // ========================================
    'passeport_talent_famille': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'MARRIAGE_CERTIFICATE',
        'RESIDENCE_PROOF',
        'PHOTOS_35x45'
    ],

    // ========================================
    // AUTRES TITRES
    // ========================================
    'vpf_jeune_entre_mineur': [
        ...VPF_BASE,
        'ENROLLMENT_PROOF'
    ],

    'aps_enfant_malade': [
        ...VPF_BASE,
        'HEALTH_CERTIFICATE',
        'CHILD_BIRTH_CERT'
    ],

    'cs_saisonnier': [
        'PASSPORT',
        'WORK_CONTRACT_CDD',
        'RESIDENCE_PROOF',
        'PHOTOS_35x45',
        'WORK_AUTHORIZATION'
    ],

    // ========================================
    // PROCÉDURES SPÉCIFIQUE (ALGÉRIENS / CRA)
    // ========================================
    'cra_algerien_resident_10ans': [
        ...TITRE_SEJOUR_BASE,
        'TITRE_SEJOUR_ACTUEL',
        'FRENCH_B1'
    ],
    'cra_algerien_conjoint_francais': [
        ...VPF_BASE,
        'MARRIAGE_CERTIFICATE',
        'SPOUSE_ID',
        'COMMUNITY_PROOF'
    ],
    'cra_algerien_activite_liberale': [
        ...TITRE_SEJOUR_BASE,
        'INVESTMENT_PROOF',
        'TAX_NOTICE'
    ],

    // ========================================
    // CITOYENS UE / MEMBRES FAMILLE
    // ========================================
    'cs_citoyen_ue_inactif_ou_actif': [
        'PASSPORT',
        'RESIDENCE_PROOF',
        'HEALTH_INSURANCE',
        'FINANCIAL_RESOURCES'
    ],
    'cs_membre_famille_ue': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'MARRIAGE_CERTIFICATE',
        'RESIDENCE_PROOF',
        'UE_CITIZEN_ID'
    ],
    'cs_ict_detache': [
        'PASSPORT',
        'WORK_CONTRACT_CDI',
        'DETACHMENT_CERT',
        'RESIDENCE_PROOF'
    ],

    // ========================================
    // ADMISSION EXCEPTIONNELLE (AES)
    // ========================================
    'aes_metiers_tension': [
        'PASSPORT',
        'RESIDENCE_PROOF',
        'PAYSLIPS_LAST_8',
        'WORK_CONTRACT_CDI',
        'WORK_AUTHORIZATION'
    ],

    // ========================================
    // NATURALISATION ANTICIPÉE
    // ========================================
    'nat_droit_du_sol_anticipe_13_16': [
        'PASSPORT',
        'BIRTH_CERTIFICATE',
        'RESIDENCE_PROOF_5Y',
        'ENROLLMENT_PROOF',
        'PARENTAL_AUTH'
    ],
    'nat_decret_standard': [
        ...NATURALISATION_BASE
    ],

    // Par défaut (services généraux)
    'default': [
        'PASSPORT',
        'RESIDENCE_PROOF'
    ]
};




// ============================================
// D. FONCTIONS UTILITAIRES
// ============================================

/**
 * Récupère la liste des documents requis pour un service donné
 */
export const getRequirementsForService = (serviceId: string): DocumentRequirement[] => {
    const normalizedId = serviceId.toLowerCase().replace(/[^a-z_]/g, '_');
    const template = SERVICE_TEMPLATES[normalizedId] || SERVICE_TEMPLATES['default'];

    return template
        .map(docId => DOC_CATALOG[docId])
        .filter(Boolean); // Filtre les undefined si un docId n'existe pas
};

/**
 * Récupère un document du catalogue par son ID
 */
export const getDocumentById = (docId: string): DocumentRequirement | undefined => {
    return DOC_CATALOG[docId];
};

/**
 * Récupère tous les documents d'une catégorie
 */
export const getDocumentsByCategory = (category: DocumentCategory): DocumentRequirement[] => {
    return Object.values(DOC_CATALOG).filter(doc => doc.category === category);
};

/**
 * Vérifie si tous les documents obligatoires sont présents dans une liste
 */
export const validateRequiredDocuments = (
    serviceId: string,
    uploadedDocIds: string[]
): { complete: boolean; missing: DocumentRequirement[] } => {
    const requirements = getRequirementsForService(serviceId);
    const requiredDocs = requirements.filter(doc => doc.required);
    const missing = requiredDocs.filter(doc => !uploadedDocIds.includes(doc.id));

    return {
        complete: missing.length === 0,
        missing
    };
};

export default {
    DOC_CATALOG,
    SERVICE_TEMPLATES,
    getRequirementsForService,
    getDocumentById,
    getDocumentsByCategory,
    validateRequiredDocuments
};
