import { Injectable } from '@nestjs/common';

@Injectable()
export class NotificationsService {
    /**
     * Simule l'envoi WhatsApp Business API côté Backend
     */
    async sendWhatsApp(phone: string, template: string, params: any) {
        console.log(`[BACKEND-WhatsApp] 🟢 Envoi à ${phone} | Template: ${template}`);
        console.log(`[BACKEND-WhatsApp] Message: ${params.message}`);

        // Ici on appellerait l'API Meta/Twilio en prod
        return { success: true, messageId: `msg_${Math.random().toString(36).substr(2, 9)}` };
    }

    /**
     * Logique de trigger sur changement d'étape
     */
    async onStageChange(lead: any, oldStage: string, newStage: string) {
        console.log(`[Backend-NotificationTrigger] Dossier ${lead.id} (${lead.name}): ${oldStage} -> ${newStage}`);

        if (newStage === 'OFII_INVESTIGATION') {
            await this.sendWhatsApp(lead.phone, 'coach_ofii_alert', {
                name: lead.name,
                message: `⚠️ Important : Votre dossier est à l'étape Enquête Logement/OFII. Préparez votre logement. Checklist : simulegal.fr/guide-ofii`
            });
        }

        if (newStage === 'HUNTING') {
            await this.sendWhatsApp(lead.phone, 'hunting_start', {
                name: lead.name,
                message: `⚡️ Recherche activée. Nous surveillons les créneaux de RDV pour vous.`
            });
        }

        if (newStage === 'BOOKED') {
            await this.sendWhatsApp(lead.phone, 'booking_success', {
                name: lead.name,
                message: `✅ RDV RÉSERVÉ ! Détails disponibles dans votre espace client.`
            });
        }
    }
}
