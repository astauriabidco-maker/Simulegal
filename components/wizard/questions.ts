import { UserProfile } from '@/types';

/* ═══════════════════════════════════════════════════════════════
   WIZARD QUESTION DEFINITIONS
   Each question is one screen in the wizard.
   `condition` determines when it's shown.
   `fields` maps to UserProfile sections.
   ═══════════════════════════════════════════════════════════════ */

export type QuestionType = 'SELECT' | 'NUMBER' | 'TOGGLE' | 'GRID' | 'DATE' | 'MULTI_CHECK' | 'COMPOSITE';

export interface QuestionOption {
    id: string;
    label: string;
    icon?: string;
    description?: string;
}

export interface QuestionField {
    key: string;           // e.g. 'nationality_group'
    section: keyof UserProfile; // e.g. 'identity'
    label?: string;
    type: 'select' | 'number' | 'checkbox' | 'date';
    options?: QuestionOption[];
    placeholder?: string;
    suffix?: string;
}

export interface WizardQuestion {
    id: string;
    icon: string;
    title: string;
    subtitle?: string;
    type: QuestionType;
    fields: QuestionField[];
    options?: QuestionOption[];     // for SELECT / GRID
    gridCols?: number;             // for GRID layout
    autoAdvance?: boolean;         // auto-next on single selection
    condition: (p: UserProfile) => boolean;
    // For composite questions: sub-fields shown conditionally
    subQuestions?: {
        condition: (p: UserProfile) => boolean;
        fields: QuestionField[];
    }[];
}

/* ─────────── Helpers ─────────── */
const always = () => true;
const notEU = (p: UserProfile) => p.identity.nationality_group !== 'EU';
const isNaturalization = (p: UserProfile) =>
    p.project.target_goal === 'NATURALIZATION' || p.project.target_goal === 'BOTH';
const hasTitre = (p: UserProfile) =>
    p.admin.current_visa_type !== 'NONE' && !!p.admin.current_visa_type;
const sansPapiers = (p: UserProfile) =>
    p.admin.current_visa_type === 'NONE' && p.identity.nationality_group !== 'EU';
const isWorker = (p: UserProfile) =>
    hasTitre(p) && p.work.main_situation === 'WORKER';
const isStudent = (p: UserProfile) =>
    hasTitre(p) && p.work.main_situation === 'STUDENT';
const isEntrepreneur = (p: UserProfile) =>
    hasTitre(p) && p.work.main_situation === 'ENTREPRENEUR';
const isWorkerOrOther = (p: UserProfile) =>
    hasTitre(p) && (p.work.main_situation === 'WORKER' || p.work.main_situation === 'OTHER');
const visaImpliesSituation = (p: UserProfile) =>
    ['STUDENT', 'WORKER', 'PASSEPORT_TALENT'].includes(p.admin.current_visa_type || '');
const isCouple = (p: UserProfile) =>
    p.family.spouse_nationality !== 'NONE';
const hasFrenchSpouse = (p: UserProfile) =>
    p.family.spouse_nationality === 'FRENCH';
const notFrenchSpouse = (p: UserProfile) =>
    !hasFrenchSpouse(p);
const bornOrYoung = (p: UserProfile) =>
    p.identity.born_in_france || (p.identity.age - p.timeline.years_continuous_residence) < 16;

/* ═══════════════════════════════════════════════════════════════
   THE QUESTIONS — ordered by flow
   ═══════════════════════════════════════════════════════════════ */

export const WIZARD_QUESTIONS: WizardQuestion[] = [

    /* ──── 1. OBJECTIF ──── */
    {
        id: 'objective',
        icon: '🎯',
        title: 'Quel est votre objectif ?',
        subtitle: 'Cela détermine les procédures que nous analysons pour vous.',
        type: 'GRID',
        gridCols: 3,
        autoAdvance: true,
        condition: always,
        fields: [{ key: 'target_goal', section: 'project', type: 'select' }],
        options: [
            { id: 'RESIDENCE_PERMIT', label: 'Titre de Séjour', icon: '🛂', description: 'Obtenir ou renouveler un titre' },
            { id: 'NATURALIZATION', label: 'Naturalisation', icon: '🇫🇷', description: 'Devenir Français(e)' },
            { id: 'BOTH', label: 'Les deux', icon: '✨', description: 'Explorer toutes les options' },
        ],
    },

    /* ──── 2. NATIONALITÉ ──── */
    {
        id: 'nationality',
        icon: '🌍',
        title: 'Quelle est votre nationalité ?',
        type: 'SELECT',
        autoAdvance: true,
        condition: always,
        fields: [{ key: 'nationality_group', section: 'identity', type: 'select' }],
        options: [
            { id: 'TUNISIAN', label: '🇹🇳 Tunisienne' },
            { id: 'ALGERIAN', label: '🇩🇿 Algérienne' },
            { id: 'MOROCCAN', label: '🇲🇦 Marocaine' },
            { id: 'EU', label: '🇪🇺 UE / EEE / Suisse' },
            { id: 'REFUGEE', label: '🛡️ Réfugié / Protection Subsidiaire' },
            { id: 'STATELESS', label: '📄 Apatride' },
            { id: 'NON_EU', label: '🌐 Autre nationalité (hors UE)' },
        ],
    },

    /* ──── 3. VISA ACTUEL ──── */
    {
        id: 'visa',
        icon: '📋',
        title: 'Quel est votre titre de séjour actuel ?',
        subtitle: 'Si vous n\'avez pas de titre, sélectionnez "Aucun".',
        type: 'SELECT',
        autoAdvance: true,
        condition: notEU,
        fields: [{ key: 'current_visa_type', section: 'admin', type: 'select' }],
        options: [
            { id: 'VLS-TS', label: '📝 Visa long séjour valant titre (VLS-TS)' },
            { id: 'STUDENT', label: '🎓 Titre étudiant' },
            { id: 'WORKER', label: '💼 Titre salarié / Travailleur' },
            { id: 'VPF', label: '💍 Vie Privée et Familiale (VPF)' },
            { id: 'VISITOR', label: '🏖️ Visiteur' },
            { id: 'PASSEPORT_TALENT', label: '⭐ Passeport Talent' },
            { id: 'RESIDENT_CARD', label: '🏠 Carte de Résident (10 ans)' },
            { id: 'RECEIPISSE', label: '📄 Récépissé' },
            { id: 'NONE', label: '🚫 Aucun titre (sans papiers)' },
        ],
    },

    /* ──── 4. ÂGE ──── */
    {
        id: 'age',
        icon: '🎂',
        title: 'Quel est votre âge ?',
        type: 'NUMBER',
        condition: always,
        fields: [{
            key: 'age', section: 'identity', type: 'number',
            placeholder: 'Ex: 28', suffix: 'ans'
        }],
    },

    /* ──── 5. NÉ EN FRANCE ──── */
    {
        id: 'born_france',
        icon: '🏥',
        title: 'Êtes-vous né(e) en France ?',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: always,
        fields: [{ key: 'born_in_france', section: 'identity', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui', icon: '✅' },
            { id: 'false', label: 'Non', icon: '❌' },
        ],
    },

    /* ──── 6. DATE D'ENTRÉE ──── */
    {
        id: 'entry_date',
        icon: '📅',
        title: 'Quand êtes-vous arrivé(e) en France ?',
        subtitle: 'La date de votre première entrée régulière sur le territoire.',
        type: 'DATE',
        condition: (p) => !p.identity.born_in_france,
        fields: [{ key: 'entry_date', section: 'timeline', type: 'date' }],
    },

    /* ──── 6b. ENTRÉE RÉGULIÈRE ──── */
    {
        id: 'entered_legally',
        icon: '🛂',
        title: 'Êtes-vous entré(e) légalement en France ?',
        subtitle: 'Avec un visa ou dispense de visa. Requis pour les titres Vie Privée et Familiale (Art. L. 423-1 CESEDA).',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) => sansPapiers(p) && (hasFrenchSpouse(p) || p.family.has_french_child),
        fields: [{ key: 'entered_legally', section: 'admin', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui, entrée régulière', icon: '✅' },
            { id: 'false', label: 'Non / Irrégulière', icon: '❌' },
        ],
    },


    /* ──── 7. SITUATION PRINCIPALE (si avec titre) ──── */
    {
        id: 'main_situation',
        icon: '💼',
        title: 'Quelle est votre situation principale ?',
        type: 'GRID',
        gridCols: 4,
        autoAdvance: true,
        condition: (p) => (hasTitre(p) && !visaImpliesSituation(p)) || isNaturalization(p),
        fields: [{ key: 'main_situation', section: 'work', type: 'select' }],
        options: [
            { id: 'STUDENT', label: 'Étudiant', icon: '🎓' },
            { id: 'WORKER', label: 'Salarié', icon: '💼' },
            { id: 'ENTREPRENEUR', label: 'Indépendant', icon: '🚀' },
            { id: 'OTHER', label: 'Autre', icon: '✨' },
        ],
    },

    /* ──── 7b. CONTRAT + SALAIRE (Worker) ──── */
    {
        id: 'work_details',
        icon: '📊',
        title: 'Vos conditions d\'emploi',
        type: 'COMPOSITE',
        condition: isWorker,
        fields: [
            {
                key: 'contract_type', section: 'work', type: 'select', label: 'Type de contrat',
                options: [
                    { id: 'NONE', label: 'Pas de contrat' },
                    { id: 'CDI', label: 'CDI' },
                    { id: 'CDD', label: 'CDD' },
                    { id: 'SEASONAL', label: 'Saisonnier' },
                ]
            },
            {
                key: 'salary_monthly_gross', section: 'work', type: 'number',
                label: 'Salaire mensuel brut (€)', placeholder: 'Ex: 2200', suffix: '€/mois'
            },
            {
                key: 'contract_duration_months', section: 'work', type: 'number',
                label: 'Durée du contrat (mois)', placeholder: 'Ex: 12', suffix: 'mois'
            },
            { key: 'has_work_authorization', section: 'work', type: 'checkbox', label: 'Je dispose d\'une autorisation de travail' },
        ],
    },

    /* ──── 7b-SP. CONTRAT + SALAIRE (Sans papiers) ──── */
    {
        id: 'work_sans_papiers',
        icon: '📊',
        title: 'Votre situation professionnelle',
        subtitle: 'Même sans titre, vos preuves de travail comptent.',
        type: 'COMPOSITE',
        condition: sansPapiers,
        fields: [
            {
                key: 'contract_type', section: 'work', type: 'select', label: 'Type de contrat ou promesse',
                options: [
                    { id: 'NONE', label: 'Aucun contrat' },
                    { id: 'CDI', label: 'CDI / Promesse de CDI' },
                    { id: 'CDD', label: 'CDD / Promesse de CDD' },
                ]
            },
            {
                key: 'salary_monthly_gross', section: 'work', type: 'number',
                label: 'Revenus mensuels (€)', placeholder: 'Ex: 1500', suffix: '€/mois'
            },
            { key: 'has_payslips', section: 'work', type: 'checkbox', label: 'Je dispose de fiches de paie (preuve d\'ancienneté)' },
        ],
    },

    /* ──── 7c. DIPLÔME (Student) ──── */
    {
        id: 'education',
        icon: '🎓',
        title: 'Votre parcours d\'études',
        type: 'COMPOSITE',
        condition: isStudent,
        fields: [
            {
                key: 'diploma_level', section: 'education', type: 'select', label: 'Niveau de diplôme',
                options: [
                    { id: 'NONE', label: 'Aucun / Baccalauréat' },
                    { id: 'LICENCE', label: 'Licence / Bachelor (Bac+3)' },
                    { id: 'LICENCE_PRO', label: 'Licence Pro' },
                    { id: 'MASTER', label: 'Master / Ingénieur (Bac+5)' },
                    { id: 'SPECIALIZED_MASTER', label: 'Mastère Spécialisé / MSc (CGE)' },
                    { id: 'PHD', label: 'Doctorat (PhD)' },
                ]
            },
            { key: 'is_enrolled_higher_ed', section: 'education', type: 'checkbox', label: 'Actuellement inscrit dans l\'enseignement supérieur' },
            { key: 'has_french_higher_education_diploma', section: 'education', type: 'checkbox', label: 'Diplôme obtenu en France' },
        ],
    },

    /* ──── 7d. INVESTISSEMENT (Entrepreneur) ──── */
    {
        id: 'investment',
        icon: '🚀',
        title: 'Votre projet entrepreneurial',
        type: 'COMPOSITE',
        condition: isEntrepreneur,
        fields: [
            {
                key: 'amount', section: 'investment', type: 'number',
                label: 'Montant d\'investissement (€)', placeholder: 'Ex: 30000', suffix: '€'
            },
            { key: 'business_project_viable', section: 'work', type: 'checkbox', label: 'Projet jugé réel et sérieux / viable' },
            { key: 'creates_jobs', section: 'investment', type: 'checkbox', label: 'Mon projet crée ou préserve des emplois en France' },
            {
                key: 'resources_monthly_average', section: 'financial', type: 'number',
                label: 'Revenus mensuels totaux (€)', placeholder: 'Ex: 2000', suffix: '€/mois'
            },
        ],
    },

    /* ──── 8. SITUATION SPÉCIFIQUE (Worker/Other) ──── */
    {
        id: 'specific_situation',
        icon: '🎯',
        title: 'Avez-vous un profil spécifique ?',
        subtitle: 'Si aucun ne correspond, restez sur « Salarié classique ». Des questions complémentaires s\'afficheront selon votre choix.',
        type: 'GRID',
        gridCols: 5,
        autoAdvance: false,
        condition: isWorkerOrOther,
        fields: [{ key: '_specific_situation', section: 'work', type: 'select' }],
        options: [
            { id: 'CLASSIC', label: 'Classique', icon: '💼' },
            { id: 'RESEARCHER', label: 'Chercheur', icon: '🔬' },
            { id: 'ARTIST', label: 'Artiste', icon: '🎨' },
            { id: 'SPORTIF', label: 'Sportif', icon: '🏅' },
            { id: 'INTERN', label: 'Stagiaire', icon: '📋' },
            { id: 'AU_PAIR', label: 'Au pair', icon: '🏠' },
            { id: 'VOLUNTEER', label: 'Volontaire', icon: '🤝' },
            { id: 'MISSION', label: 'En mission', icon: '🌍' },
            { id: 'ICT', label: 'Intra-groupe', icon: '🏢' },
            { id: 'MANAGER', label: 'Manager', icon: '👔' },
        ],
        subQuestions: [
            {
                condition: (p) => p.work.is_researcher === true,
                fields: [{ key: 'has_hosting_agreement', section: 'work', type: 'checkbox', label: 'J\'ai une convention d\'accueil d\'un organisme de recherche' }],
            },
            {
                condition: (p) => p.work.is_manager_or_expert === true || p.work.is_ict_transfer === true,
                fields: [
                    {
                        key: 'company_role', section: 'work', type: 'select', label: 'Rôle dans l\'entreprise',
                        options: [
                            { id: 'MANDATAIRE', label: 'Mandataire social / Dirigeant' },
                            { id: 'EMPLOYEE', label: 'Cadre / Expert' },
                            { id: 'FOUNDER', label: 'Fondateur' },
                        ]
                    },
                    {
                        key: 'group_seniority_months', section: 'work', type: 'number',
                        label: 'Ancienneté dans le groupe (mois)', placeholder: 'Ex: 6', suffix: 'mois'
                    },
                ],
            },
            {
                condition: (p) => p.work.main_situation === 'WORKER',
                fields: [
                    { key: 'is_innovative_company', section: 'work', type: 'checkbox', label: 'Mon employeur est une Jeune Entreprise Innovante (JEI)' },
                    {
                        key: 'years_experience_comparable', section: 'work', type: 'number',
                        label: 'Années d\'expérience professionnelle comparable', placeholder: 'Ex: 5', suffix: 'ans'
                    },
                ],
            },
            {
                condition: (p) => p.work.is_innovative_company === true,
                fields: [
                    { key: 'job_related_to_rd', section: 'work', type: 'checkbox', label: 'Mon poste est lié à la R&D' },
                ],
            },
        ],
    },

    /* ──── 9. MÉTIER EN TENSION ──── */
    {
        id: 'job_tension',
        icon: '📋',
        title: 'Votre métier est-il en tension ?',
        subtitle: 'Les métiers en tension facilitent l\'obtention d\'un titre de travail.',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) => sansPapiers(p) && (p.work.contract_type === 'CDI' || p.work.contract_type === 'CDD'),
        fields: [{ key: 'job_in_tension_list', section: 'work', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui, métier en tension', icon: '✅' },
            { id: 'false', label: 'Non / Je ne sais pas', icon: '❓' },
        ],
    },

    /* ──── 10. FRANÇAIS ──── */
    {
        id: 'french_level',
        icon: '🗣️',
        title: 'Quel est votre niveau de français ?',
        type: 'SELECT',
        autoAdvance: true,
        condition: notEU,
        fields: [{ key: 'french_level', section: 'integration', type: 'select' }],
        options: [
            { id: 'A1', label: 'A1 — Débutant' },
            { id: 'A2', label: 'A2 — Élémentaire (Titre pluriannuel)' },
            { id: 'B1', label: 'B1 — Intermédiaire (Carte résident)' },
            { id: 'B2', label: 'B2 — Avancé (Naturalisation)' },
            { id: 'C1', label: 'C1/C2 — Expert' },
        ],
    },

    /* ──── 10b. EXAMEN CIVIQUE ──── */
    {
        id: 'civic_exam',
        icon: '🏛️',
        title: 'Avez-vous réussi l\'examen civique ?',
        subtitle: 'Obligatoire depuis la réforme 2026 (sauf réfugiés/apatrides et +65 ans).',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) =>
            isNaturalization(p) &&
            !(p.identity.nationality_group === 'REFUGEE' || p.identity.nationality_group === 'STATELESS') &&
            p.identity.age <= 65,
        fields: [{ key: 'civic_exam_passed', section: 'integration', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui, réussi', icon: '✅' },
            { id: 'false', label: 'Pas encore', icon: '⏳' },
        ],
    },

    /* ──── 11. MATRIMONIAL ──── */
    {
        id: 'marital',
        icon: '💍',
        title: 'Votre situation matrimoniale',
        type: 'GRID',
        gridCols: 3,
        autoAdvance: true,
        condition: notEU,
        fields: [{ key: '_marital_status', section: 'family', type: 'select' }],
        options: [
            { id: 'MARRIED', label: 'Marié(e)', icon: '💍' },
            { id: 'PACS', label: 'Pacsé(e)', icon: '🤝' },
            { id: 'SINGLE', label: 'Célibataire', icon: '👤' },
        ],
    },

    /* ──── 11b. DÉTAILS CONJOINT ──── */
    {
        id: 'spouse_details',
        icon: '👫',
        title: 'Votre conjoint(e)',
        type: 'COMPOSITE',
        condition: isCouple,
        fields: [
            {
                key: 'spouse_nationality', section: 'family', type: 'select', label: 'Nationalité du conjoint(e)',
                options: [
                    { id: 'FRENCH', label: 'Française' },
                    { id: 'EU', label: 'Union Européenne' },
                    { id: 'NON_EU', label: 'Autre (Hors UE)' },
                ]
            },
            {
                key: 'marriage_duration_years', section: 'family', type: 'number',
                label: 'Durée de l\'union (années)', placeholder: 'Ex: 4', suffix: 'ans'
            },
            { key: 'community_of_life', section: 'family', type: 'checkbox', label: 'Communauté de vie effective (vous vivez ensemble)' },
            { key: 'is_polygamous', section: 'family', type: 'checkbox', label: 'ATTENTION : Je suis en situation de polygamie' },
        ],
        subQuestions: [
            {
                condition: hasFrenchSpouse,
                fields: [{ key: 'spouse_kept_nationality', section: 'family', type: 'checkbox', label: 'Votre conjoint(e) a conservé la nationalité française' }],
            },
            {
                condition: (p) => !hasFrenchSpouse(p) && isCouple(p),
                fields: [{ key: 'spouse_has_passport_talent', section: 'family', type: 'checkbox', label: 'Mon conjoint(e) est titulaire d\'un Passeport Talent' }],
            },
            {
                condition: (p) => p.family.is_pacsed_with_french === true,
                fields: [
                    {
                        key: 'cohabitation_duration_years', section: 'family', type: 'number',
                        label: 'Durée de cohabitation (années)', placeholder: 'Ex: 2', suffix: 'ans'
                    },
                ],
            },
        ],
    },

    /* ──── 12. ENFANT FRANÇAIS ──── */
    {
        id: 'french_child',
        icon: '👶',
        title: 'Avez-vous un enfant français mineur ?',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: false,
        condition: notEU,
        fields: [{ key: 'has_french_child', section: 'family', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui', icon: '✅' },
            { id: 'false', label: 'Non', icon: '❌' },
        ],
        subQuestions: [
            {
                condition: (p) => p.family.has_french_child,
                fields: [
                    { key: 'contributes_to_education', section: 'family', type: 'checkbox', label: 'Je contribue à son éducation et entretien' },
                    { key: 'child_residence_france', section: 'family', type: 'checkbox', label: 'Mon enfant réside en France' },
                ],
            },
        ],
    },

    /* ──── 13. LIENS FAMILIAUX (si pas marié à un Français) ──── */
    {
        id: 'family_links',
        icon: '🔗',
        title: 'Autres liens familiaux avec la France',
        subtitle: 'Ces liens peuvent ouvrir des droits supplémentaires.',
        type: 'MULTI_CHECK',
        condition: (p) => notFrenchSpouse(p) && notEU(p),
        fields: [
            { key: 'has_french_sibling', section: 'family', type: 'checkbox', label: 'Frère ou sœur de nationalité française' },
            { key: 'is_ascendant_of_french', section: 'family', type: 'checkbox', label: 'Parent/Ascendant d\'un Français majeur' },
        ],
    },

    /* ──── 13b. MODE D'ENTRÉE (condition: marié, pas sans-papiers) ──── */
    {
        id: 'entry_mode',
        icon: '🛬',
        title: 'Comment êtes-vous entré(e) en France ?',
        subtitle: 'Le mode d\'entrée détermine certaines procédures familiales.',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) => notEU(p) && hasTitre(p) && isCouple(p),
        fields: [{ key: 'entry_mode', section: 'admin', type: 'select' }],
        options: [
            { id: 'FAMILY_REUNIFICATION', label: 'Regroupement familial', icon: '👨‍👩‍👧' },
            { id: 'STANDARD', label: 'Autre voie', icon: '✈️' },
        ],
    },

    /* ──── 13c. RÉSIDENCE HORS FRANCE (saisonnier/retraité) ──── */
    {
        id: 'residence_abroad',
        icon: '🏡',
        title: 'Maintenez-vous un domicile à l\'étranger ?',
        subtitle: 'Important pour les travailleurs saisonniers et retraités.',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) => notEU(p) && (p.work.contract_type === 'SEASONAL' || p.identity.age >= 62),
        fields: [{ key: 'maintains_home_abroad', section: 'residence', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui', icon: '✅' },
            { id: 'false', label: 'Non', icon: '❌' },
        ],
    },

    /* ──── 13d. INTENTION DE TRAVAILLER (visiteur) ──── */
    {
        id: 'work_intention',
        icon: '💼',
        title: 'Souhaitez-vous travailler en France ?',
        subtitle: 'Le titre Visiteur interdit l\'exercice d\'une activité professionnelle.',
        type: 'GRID',
        gridCols: 2,
        autoAdvance: true,
        condition: (p) => p.admin.current_visa_type === 'VISITOR',
        fields: [{ key: 'wants_to_work', section: 'work', type: 'checkbox' }],
        options: [
            { id: 'true', label: 'Oui', icon: '✅' },
            { id: 'false', label: 'Non, visiteur uniquement', icon: '🏖️' },
        ],
    },

    /* ──── 14. CASIER & ÉLOIGNEMENT ──── */
    {
        id: 'civic',
        icon: '⚖️',
        title: 'Votre situation judiciaire et civique',
        type: 'COMPOSITE',
        condition: notEU,
        fields: [
            { key: 'clean_criminal_record', section: 'civic', type: 'checkbox', label: 'Mon casier judiciaire est vierge' },
            { key: 'no_expulsion_order', section: 'civic', type: 'checkbox', label: 'Je n\'ai aucune mesure d\'éloignement (OQTF, ITF…)' },
            { key: 'adheres_to_republican_values', section: 'integration', type: 'checkbox', label: 'J\'adhère aux principes et valeurs de la République' },
            { key: 'health_insurance', section: 'admin', type: 'checkbox', label: 'Je dispose d\'une couverture maladie (Sécurité Sociale, AME, CMU…)' },
        ],
    },

    /* ──── 15. VULNÉRABILITÉ ──── */
    {
        id: 'vulnerability',
        icon: '🩺',
        title: 'Êtes-vous concerné(e) par une situation de santé ou de violence ?',
        subtitle: 'Des questions complémentaires apparaîtront si vous cochez certains éléments.',
        type: 'MULTI_CHECK',
        condition: notEU,
        fields: [
            { key: 'personal_needs_treatment', section: 'health', type: 'checkbox', label: 'J\'ai besoin de soins médicaux indisponibles dans mon pays' },
            { key: 'child_needs_care', section: 'health', type: 'checkbox', label: 'Mon enfant nécessite des soins médicaux en France' },
            { key: 'has_work_accident_pension', section: 'work', type: 'checkbox', label: 'Rente d\'accident du travail (taux ≥ 20%)' },
            { key: 'is_victim_trafficking', section: 'vulnerability', type: 'checkbox', label: 'Victime de traite des êtres humains' },
            { key: 'is_victim_domestic_violence', section: 'vulnerability', type: 'checkbox', label: 'Victime de violences conjugales' },
        ],
        subQuestions: [
            {
                condition: (p) => p.vulnerability.is_victim_domestic_violence === true,
                fields: [
                    { key: 'has_protection_order_violence', section: 'vulnerability', type: 'checkbox', label: 'Ordonnance de protection ou plainte déposée' },
                ],
            },
            {
                condition: (p) => p.health.personal_needs_treatment === true || p.health.child_needs_care === true,
                fields: [
                    { key: 'treatment_unavailable_in_origin', section: 'health', type: 'checkbox', label: 'Les soins ne sont pas disponibles dans mon pays d\'origine' },
                ],
            },
            {
                condition: (p) => p.work.has_work_accident_pension === true,
                fields: [
                    {
                        key: 'work_accident_rate', section: 'work', type: 'number',
                        label: 'Taux d\'incapacité (%)', placeholder: 'Ex: 25', suffix: '%'
                    },
                ],
            },
        ],
    },

    /* ──── 16. PARCOURS SCOLAIRE (si né en France ou arrivé jeune) ──── */
    {
        id: 'schooling',
        icon: '🎓',
        title: 'Votre parcours scolaire en France',
        subtitle: 'Avoir été scolarisé en France peut faciliter certaines procédures.',
        type: 'COMPOSITE',
        condition: bornOrYoung,
        fields: [
            {
                key: 'years_schooling_france', section: 'education', type: 'number',
                label: 'Années de scolarité en France', placeholder: 'Ex: 8', suffix: 'ans'
            },
            {
                key: 'years_higher_education', section: 'education', type: 'number',
                label: 'Années d\'études supérieures en France', placeholder: 'Ex: 3', suffix: 'ans'
            },
            { key: 'schooling_in_france_age_6_to_16', section: 'education', type: 'checkbox', label: 'Scolarisé(e) en France entre 6 et 16 ans (scolarité obligatoire)' },
        ],
    },

    /* ──── 17. DISTINCTIONS (si naturalisation) ──── */
    {
        id: 'distinctions',
        icon: '🏅',
        title: 'Distinctions & Services à la France',
        subtitle: 'Ces éléments ouvrent des voies de naturalisation accélérée.',
        type: 'MULTI_CHECK',
        condition: isNaturalization,
        fields: [
            { key: 'served_french_military', section: 'work', type: 'checkbox', label: 'J\'ai servi dans l\'armée française' },
            { key: 'has_legion_honneur', section: 'work', type: 'checkbox', label: 'Décoré(e) de la Légion d\'honneur' },
            { key: 'possession_etat_francais', section: 'nationality_extra', type: 'checkbox', label: 'Possession d\'état de Français (traité comme Français depuis 10+ ans)' },
            { key: 'lost_french_nationality', section: 'identity', type: 'checkbox', label: 'J\'ai perdu la nationalité française (réintégration)' },
        ],
    },

    /* ──── 18. RÉGULARISATION (si sans papiers) ──── */
    {
        id: 'regularisation',
        icon: '📋',
        title: 'Votre situation de régularisation',
        subtitle: 'Pour les personnes sans titre, nous analysons vos chances d\'Admission Exceptionnelle au Séjour.',
        type: 'COMPOSITE',
        condition: (p) => sansPapiers(p) && notEU(p),
        fields: [
            { key: 'has_children_schooled_3y', section: 'regularisation', type: 'checkbox', label: 'Enfants scolarisés en France depuis au moins 3 ans' },
            { key: 'has_exceptional_talent', section: 'regularisation', type: 'checkbox', label: 'Talent exceptionnel ou renommée nationale/internationale' },
        ],
    },


];
