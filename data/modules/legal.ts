export const LEGAL_QUESTIONS = [
  {
    id: 'legal_subject',
    label: "Sur quel sujet souhaitez-vous consulter notre expert ?",
    type: 'select',
    options: [
      { label: "OQTF / Contentieux (Urgent)", value: 'oqtf_contentieux' },
      { label: "Refus de titre / Recours", value: 'refus_recours' },
      { label: "Conseil dossier (Mariage, Naturalisation)", value: 'conseil_dossier' },
      { label: "Vérification dossier avant dépôt", value: 'verification_dossier' },
      { label: "Autre question juridique", value: 'autre' }
    ],
    required: true
  },
  {
    id: 'consultation_type',
    label: "Quel type de rendez-vous préférez-vous ?",
    type: 'radio', // ou 'card-select' si disponible
    options: [
      {
        label: "🌍 Consultation à Distance (Visio/Tél)",
        value: 'remote',
        description: "Disponibilité rapide sous 24h. Idéal pour valider un point précis."
      },
      {
        label: "🏢 Consultation en Agence (Paris)",
        value: 'physical',
        description: "Rencontre expert et analyse complète des pièces originales."
      }
    ],
    required: true
  }
];
