export type ServicePole = 'PROCEDURES' | 'INTEGRATION' | 'EXPERTISE';
export type ServiceType = 'SIMULATION' | 'CONSULTATION' | 'DOCUMENT' | 'FORMATION' | 'CALLBACK';

export interface Service {
    id: string;
    title: string;
    description: string;
    pole: ServicePole;
    type: ServiceType;
    isSimulatable: boolean;
    iconName: string;
    badge?: string;
    price?: number; // Prix indicatif en centimes
}

export const SERVICES_CATALOG: Service[] = [
    // PÔLE PROCÉDURES (Avec Simulation)
    {
        id: 'nat_accomp',
        title: 'Accompagnement Nationalité',
        description: 'Accompagnement complet pour votre demande de nationalité française.',
        pole: 'PROCEDURES',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'Flag',
        badge: 'SIMUL',
        price: 49000 // 490€
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
        price: 35000 // 350€
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
        price: 39000 // 390€
    },

    // PÔLE EXPERTISE (Sans Simulation ou Partielle)
    {
        id: 'permis_conduire',
        title: 'Changement Permis Conduire',
        description: 'Aide à l\'obtention ou à l\'échange de votre permis de conduire étranger.',
        pole: 'EXPERTISE',
        type: 'SIMULATION',
        isSimulatable: true,
        iconName: 'Car',
        badge: 'SIMUL',
        price: 15000 // 150€
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
        price: 8000 // 80€
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
        price: 5000 // 50€
    },

    // PÔLE INTÉGRATION
    {
        id: 'langue_a2b1',
        title: 'Cours de langues A2/B1',
        description: 'Cours adaptés pour vos examens et votre intégration en France.',
        pole: 'INTEGRATION',
        type: 'FORMATION',
        isSimulatable: true,
        iconName: 'Languages',
        badge: 'SIMUL',
        price: 25000 // 250€
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
        price: 12000 // 120€
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
        price: 0
    }
];
