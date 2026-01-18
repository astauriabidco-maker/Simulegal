"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.NotificationsService = void 0;
const common_1 = require("@nestjs/common");
let NotificationsService = class NotificationsService {
    async sendWhatsApp(phone, template, params) {
        console.log(`[BACKEND-WhatsApp] 🟢 Envoi à ${phone} | Template: ${template}`);
        console.log(`[BACKEND-WhatsApp] Message: ${params.message}`);
        return { success: true, messageId: `msg_${Math.random().toString(36).substr(2, 9)}` };
    }
    async onStageChange(lead, oldStage, newStage) {
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
};
exports.NotificationsService = NotificationsService;
exports.NotificationsService = NotificationsService = __decorate([
    (0, common_1.Injectable)()
], NotificationsService);
//# sourceMappingURL=notifications.service.js.map