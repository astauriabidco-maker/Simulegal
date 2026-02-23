import { Prospect, ProspectStatus } from './SalesStore';

export const MarketingAutomation = {
    /**
     * Déclenche des actions automatiques basées sur le changement de statut
     */
    triggerAutomation: async (prospect: Prospect, newStatus: ProspectStatus): Promise<void> => {
        console.log(`[AUTOMATION] Analysing trigger for ${prospect.firstName} -> ${newStatus}`);

        // Simulation délais api
        await new Promise(resolve => setTimeout(resolve, 800));

        switch (newStatus) {
            case 'TO_CALL':
                // Send welcome only if it's a fresh lead (logic handled in caller or assumed here for simplicity)
                // In a real app we would check if previous status was 'NEW' or undefined.
                // For now, let's assume this triggers on creation.
                MarketingAutomation.sendWelcomeSMS(prospect);
                break;

            case 'MEETING_BOOKED':
                MarketingAutomation.sendMeetingConfirmation(prospect);
                break;

            case 'LOST':
                MarketingAutomation.addToRetargetingAudience(prospect);
                break;

            case 'SIGNED':
                MarketingAutomation.sendWelcomePackEmail(prospect);
                break;

            case 'APPOINTMENT_DONE':
                MarketingAutomation.sendSimulationLink(prospect);
                break;
        }
    },

    sendWelcomeSMS: (prospect: Prospect) => {
        if (!prospect.phone) return;
        console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, merci de votre intérêt pour Simulegal. Un expert va vous rappeler ds les 2h."`);
    },

    sendMeetingConfirmation: (prospect: Prospect) => {
        console.log(`[EMAIL] 📧 To ${prospect.email || 'NO_EMAIL'}: "Votre RDV Simulegal est confirmé. Lien visio: https://meet.google.com/abc-defg-hij"`);
        if (prospect.phone) {
            console.log(`[SMS] 📤 To ${prospect.phone}: "Rappel: RDV confirmé avec votre juriste Simulegal/."`);
        }
    },

    addToRetargetingAudience: (prospect: Prospect) => {
        console.log(`[META_API] 🎯 Adding ${prospect.email} to Custom Audience 'Lead_Lost_Retargeting'`);
    },

    sendWelcomePackEmail: (prospect: Prospect) => {
        console.log(`[EMAIL] 📧 To ${prospect.email}: "Bienvenue chez Simulegal ! Voici votre espace personnel sécurisé."`);
    },

    sendSimulationLink: (prospect: Prospect) => {
        if (!prospect.phone) return;
        console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, voici votre lien sécurisé pour finaliser votre dossier Simulegal: https://simulegal.fr/pay/${prospect.id}"`);
    }
};
