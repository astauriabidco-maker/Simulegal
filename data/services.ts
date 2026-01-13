export type ServicePole = 'PROCEDURES' | 'INTEGRATION' | 'EXPERTISE';

export interface Service {
    id: string;
    title: string;
    description: string;
    pole: ServicePole;
    isSimulatable: boolean;
    iconName: string;
    badge?: string;
}

export const SERVICES_CATALOG: Service[] = [
    // PÔLE PROCÉDURES
    {
        id: 'residence_permit',
        title: 'Accompagnement Titre Séjour',
        description: 'Aide à la préparation et au dépôt de votre dossier de titre de séjour.',
        pole: 'PROCEDURES',
        isSimulatable: true,
        iconName: 'FileText',
        badge: 'SIMUL'
    },
    {
        id: 'naturalization',
        title: 'Accompagnement Naturalisation',
        description: 'Accompagnement complet pour votre demande de nationalité française.',
        pole: 'PROCEDURES',
        isSimulatable: true,
        iconName: 'Flag',
        badge: 'SIMUL'
    },
    {
        id: 'family_reunification',
        title: 'Regroupement familial',
        description: 'Procédure pour faire venir votre famille en France en toute sécurité.',
        pole: 'PROCEDURES',
        isSimulatable: true,
        iconName: 'Users',
        badge: 'SIMUL'
    },

    // PÔLE INTÉGRATION
    {
        id: 'french_course',
        title: 'Cours de français A2/B1/B2',
        description: 'Cours adaptés pour vos examens et votre intégration en France.',
        pole: 'INTEGRATION',
        isSimulatable: false,
        iconName: 'Languages'
    },
    {
        id: 'integration_support',
        title: 'Support Intégration',
        description: 'Aide à l\'installation et à la compréhension de la culture française.',
        pole: 'INTEGRATION',
        isSimulatable: false,
        iconName: 'GraduationCap'
    },
    {
        id: 'administrative_help',
        title: 'Aide Administrative',
        description: 'Support pour vos démarches quotidiennes (CAF, Ameli, impôts).',
        pole: 'INTEGRATION',
        isSimulatable: false,
        iconName: 'BookOpen'
    },

    // PÔLE EXPERTISE
    {
        id: 'driving_license',
        title: 'Changement Permis Conduire',
        description: 'Aide à l\'obtention ou à l\'échange de votre permis de conduire étranger.',
        pole: 'EXPERTISE',
        isSimulatable: false,
        iconName: 'Car'
    },
    {
        id: 'prefecture_assistance',
        title: 'Assistance RDV préfecture',
        description: 'Aide à la prise de rendez-vous et préparation du passage en préfecture.',
        pole: 'EXPERTISE',
        isSimulatable: false,
        iconName: 'Calendar'
    },
    {
        id: 'oqtf_appeal',
        title: 'Recours OQTF',
        description: 'Assistance juridique d\'urgence en cas d\'obligation de quitter le territoire.',
        pole: 'EXPERTISE',
        isSimulatable: false,
        iconName: 'Gavel'
    }
];
