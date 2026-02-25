import { Lead } from './crmStore';
import { WorkflowStage } from './WorkflowService';

export const NotificationService = {
    /**
     * Simule l'envoi WhatsApp Business API via le Backend
     */
    sendWhatsApp: async (phone: string, template: string, params: any) => {
        const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

        try {
            await fetch(`${API_URL}/notifications/send-whatsapp`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                    // Note: This API might need auth if called from admin, 
                    // but for leads it might be public or use a different key.
                    // For now, assume it's protected or tracked.
                },
                body: JSON.stringify({ phone, template, params })
            });

            console.log(`[WhatsApp Business] 🟢 Message envoyé à ${phone}`);

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
        } catch (e) {
            console.warn('[WhatsApp] Erreur API:', e);
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
