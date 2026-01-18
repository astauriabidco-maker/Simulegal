import { Lead } from './crmStore';
import { WorkflowStage } from './WorkflowService';

export const NotificationService = {
    /**
     * Simule l'envoi WhatsApp Business API
     * En production, cela appellerait une API comme Twilio ou Meta
     */
    sendWhatsApp: (phone: string, template: string, params: any) => {
        console.log(`[WhatsApp Business] 🟢 Message envoyé à ${phone}`);
        console.log(`[WhatsApp] Template: ${template}`, params);

        // Notification visuelle système (pour simulation front)
        if (typeof window !== 'undefined') {
            const event = new CustomEvent('simulegal_notification', {
                detail: {
                    type: 'WHATSAPP',
                    phone,
                    message: params.message,
                    timestamp: new Date().toISOString()
                }
            });
            window.dispatchEvent(event);
        }
    },

    /**
     * TRIGGER AUTOMATIQUE
     * Appelé lors de chaque changement d'étape dans le workflow
     */
    onStageChange: (lead: Lead, oldStage: WorkflowStage, newStage: WorkflowStage) => {
        console.log(`[NotificationTrigger] Changement d'étape pour ${lead.name}: ${oldStage} -> ${newStage}`);

        // Cas 1 : Passage en Enquête OFII (Regroupement Familial)
        if (newStage === 'OFII_INVESTIGATION') {
            NotificationService.sendWhatsApp(lead.phone, 'coach_ofii_alert', {
                name: lead.name,
                message: `⚠️ Important : Votre dossier est à l'étape Enquête Logement/OFII. Préparez votre logement. Cliquez ici pour voir la checklist visite : simulegal.fr/guide-ofii`
            });
        }

        // Cas 2 : Passage en Recherche Créneau (RDV Préfecture)
        if (newStage === 'HUNTING') {
            NotificationService.sendWhatsApp(lead.phone, 'hunting_start', {
                name: lead.name,
                message: `⚡️ Recherche activée. Votre robot SimuLegal vient de commencer la chasse aux rdv. Gardez votre téléphone près de vous !`
            });
        }

        // Cas 3 : RDV Réservé
        if (newStage === 'BOOKED') {
            NotificationService.sendWhatsApp(lead.phone, 'booking_success', {
                name: lead.name,
                message: `✅ RDV RÉSERVÉ ! Nous avons trouvé un créneau. Rendez-vous dans votre espace client pour voir les détails et télécharger votre convocation.`
            });
        }

        // Cas 4 : Dossier Clôturé (Succès)
        if (newStage === 'CLOSED') {
            NotificationService.sendWhatsApp(lead.phone, 'dossier_closed', {
                name: lead.name,
                message: `🎉 Félicitations ${lead.name} ! Votre dossier est maintenant terminé. Merci d'avoir fait confiance à SimuLegal.`
            });
        }
    }
};
