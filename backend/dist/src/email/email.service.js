"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var EmailService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.EmailService = void 0;
const common_1 = require("@nestjs/common");
let EmailService = EmailService_1 = class EmailService {
    logger = new common_1.Logger(EmailService_1.name);
    async sendOrderConfirmation(to, clientName, serviceName, amount, transactionRef) {
        const subject = `Confirmation de votre commande Simulegal #${transactionRef}`;
        const textContent = `
        Bonjour ${clientName},

        Nous confirmons la bonne réception de votre commande pour le service "${serviceName}".
        
        Référence transaction : ${transactionRef}
        Montant réglé : ${amount} € TTC
        
        Votre dossier a été ouvert sous le numéro ${transactionRef}.
        
        PROCHAINES ÉTAPES :
        1. Connectez-vous à votre espace client (lien ci-dessous)
        2. Téléchargez la liste des pièces justificatives personnalisée
        3. Déposez vos scan de documents
        
        [Lien Espace Client]
        
        Un juriste va prendre connaissance de votre dossier sous 24h ouvrées.
        
        Cordialement,
        L'équipe Simulegal
        `;
        this.logger.log(`📧 [MOCK EMAIL] To: ${to} | Subject: ${subject}`);
        this.logger.log(`Content:\n${textContent}`);
        return true;
    }
    async sendMandateCopy(to, clientName) {
        const subject = `Votre copie du Mandat de Représentation - Simulegal`;
        this.logger.log(`📧 [MOCK EMAIL] To: ${to} | Subject: ${subject}`);
        this.logger.log(`[Pièce jointe simulée: mandat_signe.pdf]`);
        return true;
    }
};
exports.EmailService = EmailService;
exports.EmailService = EmailService = EmailService_1 = __decorate([
    (0, common_1.Injectable)()
], EmailService);
//# sourceMappingURL=email.service.js.map