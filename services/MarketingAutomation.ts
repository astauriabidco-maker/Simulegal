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
            case 'NEW':
                MarketingAutomation.sendWelcomeSMS(prospect);
                break;

            case 'CONTACTED':
                // No automation on contacted — manual follow-up
                break;

            case 'QUALIFIED':
                MarketingAutomation.sendSimulationLink(prospect);
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

            case 'NO_SHOW':
                // Send a reminder / re-engagement message
                if (prospect.phone) {
                    console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, nous avons remarqué que vous n'avez pas pu venir. Souhaitez-vous reprogrammer ?"`);
                }
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
