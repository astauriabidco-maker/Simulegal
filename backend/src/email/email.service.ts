import { Injectable, Logger } from '@nestjs/common';

@Injectable()
export class EmailService {
    private readonly logger = new Logger(EmailService.name);
    private lastEmailSent: any = null; // Debugging purpose

    async sendOrderConfirmation(to: string, clientName: string, serviceName: string, amount: number, transactionRef: string, requiredDocs?: any[], clientSpaceUrl?: string) {
        // En vrai (TODO: Configurer SMTP/SendGrid ici)
        // await transporter.sendMail(...)

        const subject = `Confirmation de votre commande Simulegal #${transactionRef}`;

        let checklistText = '';
        if (requiredDocs && requiredDocs.length > 0) {
            checklistText = `\nVoici la liste des pièces justificatives à nous fournir :\n`;
            requiredDocs.forEach(doc => {
                checklistText += `- ${doc.name}\n`;
            });
            checklistText += `\nMerci de vous connecter à votre espace client pour les déposer.\n`;
        } else {
            checklistText = `\n2. Téléchargez la liste des pièces justificatives personnalisée
3. Déposez vos scan de documents\n`;
        }

        const clientLink = clientSpaceUrl || 'https://simulegal.fr/client';

        const textContent = `
        Bonjour ${clientName},

        Nous confirmons la bonne réception de votre commande pour le service "${serviceName}".
        
        Référence transaction : ${transactionRef}
        Montant réglé : ${amount} € TTC
        
        Votre dossier a été ouvert sous le numéro ${transactionRef}.
        
        PROCHAINES ÉTAPES :
        1. Accédez à votre espace client via le lien ci-dessous
        ${checklistText}
        
        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        🔗 ACCÉDER À VOTRE ESPACE CLIENT :
        ${clientLink}
        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        
        Ce lien est personnel et sécurisé. Il vous permet de :
        ✅ Déposer vos documents
        ✅ Suivre l'avancement de votre dossier
        ✅ Consulter les pièces validées ou à corriger
        
        Un juriste va prendre connaissance de votre dossier sous 24h ouvrées.
        
        Cordialement,
        L'équipe Simulegal
        `;

        this.logger.log(`📧 [MOCK EMAIL] To: ${to} | Subject: ${subject}`);
        this.logger.log(`Content:\n${textContent}`);

        // Debug storage
        this.lastEmailSent = {
            to,
            subject,
            content: textContent,
            type: 'OrderConfirmation'
        };

        return true;
    }

    async sendMandateCopy(to: string, clientName: string) {
        const subject = `Votre copie du Mandat de Représentation - Simulegal`;
        this.logger.log(`📧 [MOCK EMAIL] To: ${to} | Subject: ${subject}`);
        this.logger.log(`[Pièce jointe simulée: mandat_signe.pdf]`);

        // Debug storage - only if we don't have a confirmation stored (or if this is the only action)
        if (!this.lastEmailSent || this.lastEmailSent.type !== 'OrderConfirmation') {
            this.lastEmailSent = {
                to,
                subject,
                content: "[Pièce jointe simulée: mandat_signe.pdf]",
                type: 'MandateCopy'
            };
        }

        return true;
    }

    getLastEmail() {
        return this.lastEmailSent;
    }
}
