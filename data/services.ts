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
        isSimulatable: true,
        iconName: 'Languages',
        badge: 'LOCAL'
    },
    {
        id: 'examen_civique',
        title: 'Examen Civique',
        description: 'Entraînement intensif au QCM des valeurs républicaines (Obligatoire 2026).',
        pole: 'INTEGRATION',
        isSimulatable: true,
        iconName: 'GraduationCap',
        badge: 'LOCAL'
    },
    {
        id: 'rappel_echeances',
        title: 'Je veux être rappelé.e',
        description: 'Service de rappel pour ne jamais rater vos échéances de renouvellement.',
        pole: 'INTEGRATION',
        isSimulatable: true,
        iconName: 'BookOpen',
        badge: 'LOCAL'
    },

    // PÔLE EXPERTISE
    {
        id: 'permis_conduire',
        title: 'Changement Permis Conduire',
        description: 'Aide à l\'obtention ou à l\'échange de votre permis de conduire étranger.',
        pole: 'EXPERTISE',
        isSimulatable: true,
        iconName: 'Car',
        badge: 'SIMUL'
    },
    {
        id: 'rdv_prefecture',
        title: 'Assistance RDV préfecture',
        description: 'Aide à la prise de rendez-vous et préparation du passage en préfecture.',
        pole: 'EXPERTISE',
        isSimulatable: true,
        iconName: 'Calendar',
        badge: 'SIMUL'
    },
    {
        id: 'rdv_juriste',
        title: 'Rendez-vous Juriste',
        description: 'Conseil juridique personnalisé avec un expert en droit des étrangers.',
        pole: 'EXPERTISE',
        isSimulatable: true,
        iconName: 'Gavel',
        badge: 'SIMUL'
    }
];
