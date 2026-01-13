export const CALLBACK_QUESTIONS = [
    {
        id: 'callback_subject',
        label: "Quel est le sujet de votre demande ?",
        type: 'select',
        options: [
            { label: "ℹ️ Information générale", value: 'INFO' },
            { label: "🛑 Je suis bloqué dans une démarche", value: 'BLOCKED' },
            { label: "🚨 Urgence (OQTF / Police / Retenue)", value: 'URGENT_LEGAL' },
            { label: "📅 Suivi de mon dossier en cours", value: 'FOLLOW_UP' }
        ],
        required: true
    },
    {
        id: 'callback_urgency',
        label: "Quand souhaitez-vous être rappelé ?",
        type: 'radio',
        options: [
            { label: "Immédiatement (Si disponible)", value: 'ASAP' },
            { label: "Dans la journée", value: 'TODAY' },
            { label: "Sur créneau planifié", value: 'PLANNED' }
        ],
        required: true
    },
    {
        id: 'location_zip',
        label: "Code Postal de résidence ?",
        type: 'text',
        placeholder: "Ex: 13001",
        required: true,
        description: "Pour diriger votre demande vers l'agence la plus proche."
    }
];
