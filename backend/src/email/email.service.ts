import { Injectable, Logger } from '@nestjs/common';

@Injectable()
export class EmailService {
    private readonly logger = new Logger(EmailService.name);

    async sendOrderConfirmation(to: string, clientName: string, serviceName: string, amount: number, transactionRef: string) {
        // En vrai (TODO: Configurer SMTP/SendGrid ici)
        // await transporter.sendMail(...)

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

    async sendMandateCopy(to: string, clientName: string) {
        const subject = `Votre copie du Mandat de Représentation - Simulegal`;
        this.logger.log(`📧 [MOCK EMAIL] To: ${to} | Subject: ${subject}`);
        this.logger.log(`[Pièce jointe simulée: mandat_signe.pdf]`);
        return true;
    }
}
