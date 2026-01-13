export const FRENCH_QUESTIONS = [
    {
        id: 'french_goal',
        label: "Pour quel objectif avez-vous besoin du français ?",
        type: 'card-select',
        options: [
            { label: "🇫🇷 Naturalisation (Niveau B1)", value: 'NATURALIZATION' },
            { label: "🆔 Carte de Résident 10 ans (Niveau A2)", value: 'RESIDENCE' },
            { label: "🎓 Études / Pro (Niveau B2/C1)", value: 'PROFESSIONAL' }
        ],
        required: true
    },
    {
        id: 'current_level',
        label: "Quel est votre niveau actuel (estimation) ?",
        type: 'radio',
        options: [
            { label: "Débutant complet", value: 'A1' },
            { label: "Intermédiaire (Je me débrouille)", value: 'A2_B1' },
            { label: "Avancé", value: 'B2' }
        ],
        required: true
    },
    {
        id: 'location_zip',
        label: "Quel est votre Code Postal ?",
        type: 'text',
        placeholder: "Ex: 93200",
        required: true,
        description: "Pour trouver le centre de formation agréé le plus proche."
    }
];
