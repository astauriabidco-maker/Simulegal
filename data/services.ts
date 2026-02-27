/**
 * CATALOGUE DES SERVICES — Source Unique de Vérité
 * 
 * ═══════════════════════════════════════════════════════════════
 *  Pour ajouter un nouveau service :
 *  1. Ajouter une entrée dans SERVICES_CATALOG ci-dessous
 *  2. C'est tout ! La landing page, le back-office, le pipeline
 *     commercial, le kiosque et le calendrier se mettent à jour
 *     automatiquement.
 * 
 *  Pour utiliser une nouvelle icône :
 *    → Vérifier la liste dans lib/icon-resolver.ts
 *    → Si l'icône n'existe pas, l'ajouter dans ICON_REGISTRY
 * 
 *  Les prix sont gérés dans : Paramètres → Tarifs Services
 * ═══════════════════════════════════════════════════════════════
 */

export type ServicePole = 'PROCEDURES' | 'INTEGRATION' | 'EXPERTISE';
export type ServiceType = 'SIMULATION' | 'CONSULTATION' | 'DOCUMENT' | 'FORMATION' | 'CALLBACK';

export interface Service {
    /** Identifiant unique (snake_case) */
    id: string;
    /** Titre affiché partout */
    title: string;
    /** Description courte (landing page, kiosque) */
    description: string;
    /** Pôle de rattachement */
    pole: ServicePole;
    /** Type de service */
    type: ServiceType;
    /** Peut lancer le simulateur */
    isSimulatable: boolean;
    /** Nom de l'icône lucide-react (voir lib/icon-resolver.ts) */
    iconName: string;
    /** Badge affiché en coin (ex: 'SIMUL', 'GRATUIT', 'NOUVEAU') */
    badge?: string;
    /** Service actif (visible sur la plateforme) */
    isActive?: boolean;
    /** Ordre d'affichage au sein du pôle (plus petit = en premier) */
    sortOrder?: number;
    /** Mots-clés pour la recherche / correspondance IA */
    keywords?: string[];
}

/** Définition des pôles avec métadonnées d'affichage */
export const SERVICE_POLES: { id: ServicePole; label: string; emoji: string; color: string; bgColor: string }[] = [
    { id: 'PROCEDURES', label: 'Pôle Procédures', emoji: '🚀', color: 'text-blue-600', bgColor: 'bg-blue-50' },
    { id: 'INTEGRATION', label: 'Pôle Intégration', emoji: '🎓', color: 'text-indigo-600', bgColor: 'bg-indigo-50' },
    { id: 'EXPERTISE', label: 'Pôle Expertise', emoji: '⚖️', color: 'text-purple-600', bgColor: 'bg-purple-50' },
];

/** Définition des types de services */
export const SERVICE_TYPES: { id: ServiceType; label: string; emoji: string }[] = [
    { id: 'SIMULATION', label: 'Simulation', emoji: '🧪' },
    { id: 'CONSULTATION', label: 'Consultation', emoji: '💬' },
    { id: 'DOCUMENT', label: 'Document', emoji: '📄' },
    { id: 'FORMATION', label: 'Formation', emoji: '🎓' },
    { id: 'CALLBACK', label: 'Rappel', emoji: '📞' },
];

export const SERVICES_CATALOG: Service[] = [
    // ═══════════════════════════════════════
    //  PÔLE PROCÉDURES (Avec Simulation)
    // ═══════════════════════════════════════
    {
        id: 'nat_accomp',
        title: 'Accompagnement Nationalité',
        description: 'Accompagnement complet pour votre demande de nationalité française.',
        pole: 'PROCEDURES',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'Flag',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 10,
        keywords: ['naturalisation', 'nationalité', 'français', 'citoyenneté'],
    },
    {
        id: 'sejour_accomp',
        title: 'Accompagnement Titre Séjour',
        description: 'Aide à la préparation et au dépôt de votre dossier de titre de séjour.',
        pole: 'PROCEDURES',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'FileText',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 20,
        keywords: ['titre séjour', 'carte séjour', 'visa', 'résidence'],
    },
    {
        id: 'regroupement_familial',
        title: 'Regroupement Familial',
        description: 'Procédure pour faire venir votre famille en France en toute sécurité.',
        pole: 'PROCEDURES',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'Users',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 30,
        keywords: ['famille', 'regroupement', 'conjoint', 'enfants'],
    },

    // ═══════════════════════════════════════
    //  PÔLE EXPERTISE (Sans Simulation ou Partielle)
    // ═══════════════════════════════════════
    {
        id: 'permis_conduire',
        title: 'Changement Permis Conduire',
        description: 'Aide à l\'obtention ou à l\'échange de votre permis de conduire étranger.',
        pole: 'EXPERTISE',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'Car',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 10,
        keywords: ['permis', 'conduire', 'échange', 'automobile'],
    },
    {
        id: 'rdv_juriste',
        title: 'Rendez-vous Juriste',
        description: 'Conseil juridique personnalisé avec un expert en droit des étrangers.',
        pole: 'EXPERTISE',
        type: 'CONSULTATION',
        isSimulatable: true,
        iconName: 'Gavel',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 20,
        keywords: ['juriste', 'avocat', 'conseil', 'juridique'],
    },
    {
        id: 'rdv_prefecture',
        title: 'Rendez-vous Préfecture',
        description: 'Assistance à la prise de rendez-vous et préparation du passage en préfecture.',
        pole: 'EXPERTISE',
        type: 'CONSULTATION',
        isSimulatable: true,
        iconName: 'Calendar',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 30,
        keywords: ['préfecture', 'rendez-vous', 'rdv', 'guichet'],
    },

    // ═══════════════════════════════════════
    //  PÔLE INTÉGRATION
    // ═══════════════════════════════════════
    {
        id: 'langue_a2b1',
        title: 'Cours de langues A2/B1',
        description: 'Cours adaptés pour vos examens et votre intégration en France.',
        pole: 'INTEGRATION',
        type: 'FORMATION',
        isSimulatable: true,
        iconName: 'Languages',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 10,
        keywords: ['langue', 'français', 'cours', 'A2', 'B1', 'TCF', 'DELF'],
    },
    {
        id: 'form_civique',
        title: 'Formation Civique',
        description: 'Sessions de formation obligatoire sur les valeurs de la République.',
        pole: 'INTEGRATION',
        type: 'FORMATION',
        isSimulatable: true,
        iconName: 'GraduationCap',
        badge: 'SIMUL',
        isActive: true,
        sortOrder: 20,
        keywords: ['civique', 'formation', 'valeurs', 'république', 'intégration'],
    },
    {
        id: 'rappel_echeances',
        title: 'Être Rappelé',
        description: 'Service gratuit pour être contacté par un conseiller SimuLegal.',
        pole: 'INTEGRATION',
        type: 'CALLBACK',
        isSimulatable: true,
        iconName: 'Phone',
        badge: 'GRATUIT',
        isActive: true,
        sortOrder: 99,
        keywords: ['rappel', 'callback', 'téléphone', 'contact'],
    },
];

// ═══════════════════════════════════════
//  HELPERS — Utilisés par tous les composants
// ═══════════════════════════════════════

/** Retourne uniquement les services actifs, triés par sortOrder */
export function getActiveServices(): Service[] {
    return SERVICES_CATALOG
        .filter(s => s.isActive !== false)
        .sort((a, b) => (a.sortOrder ?? 50) - (b.sortOrder ?? 50));
}

/** Retourne les services actifs d'un pôle donné */
export function getServicesByPole(pole: ServicePole): Service[] {
    return getActiveServices().filter(s => s.pole === pole);
}

/** Trouve un service par son ID */
export function getServiceById(id: string): Service | undefined {
    return SERVICES_CATALOG.find(s => s.id === id);
}

/** Retourne les services simulables actifs */
export function getSimulatableServices(): Service[] {
    return getActiveServices().filter(s => s.isSimulatable);
}
