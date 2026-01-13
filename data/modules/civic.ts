export const CIVIC_QUESTIONS = [
    {
        id: 'civic_goal',
        label: "Dans quel cadre devez-vous passer cet examen ?",
        type: 'card-select',
        options: [
            {
                label: "🆔 Carte de Résident (10 ans) / Pluriannuelle",
                value: 'RESIDENCE',
                description: "Préparation au nouveau QCM obligatoire (Réforme)."
            },
            {
                label: "🇫🇷 Naturalisation (Entretien)",
                value: 'NATURALIZATION',
                description: "Préparation aux questions de culture et histoire pour l'entretien."
            },
            {
                label: "⚠️ J'ai échoué et je dois le repasser",
                value: 'RETAKE',
                description: "Coaching intensif pour réussir la 2ème chance."
            }
        ],
        required: true
    },
    {
        id: 'knowledge_level',
        label: "Connaissez-vous l'Histoire et les valeurs de la France ?",
        type: 'radio',
        options: [
            { label: "Non, je pars de zéro", value: 'BEGINNER' },
            { label: "J'ai quelques notions (Liberté, Égalité...)", value: 'INTERMEDIATE' },
            { label: "Je connais bien, je veux juste m'entraîner", value: 'ADVANCED' }
        ],
        required: true
    },
    {
        id: 'location_zip',
        label: "Quel est votre Code Postal ?",
        type: 'text',
        placeholder: "Ex: 69003",
        required: true,
        description: "Pour vous orienter vers une association ou un formateur local."
    }
];
