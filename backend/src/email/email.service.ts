import { Injectable, Logger, Inject, forwardRef } from '@nestjs/common';
import { NotificationsService } from '../notifications/notifications.service';

@Injectable()
export class EmailService {
    private readonly logger = new Logger(EmailService.name);
    private lastEmailSent: any = null; // Debugging purpose

    constructor(
        @Inject(forwardRef(() => NotificationsService))
        private notifications: NotificationsService
    ) { }

    async sendOrderConfirmation(to: string, clientName: string, serviceName: string, amount: number, transactionRef: string, requiredDocs?: any[], clientSpaceUrl?: string) {
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

        // Route vers le vrai SMTP via NotificationsService
        await this.notifications.sendEmail(to, subject, textContent);

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
        const body = `Bonjour ${clientName},\n\nVeuillez trouver ci-joint votre copie du Mandat de Représentation signé.\n\nCordialement,\nL'équipe Simulegal`;

        // Route vers le vrai SMTP
        await this.notifications.sendEmail(to, subject, body);

        // Debug storage
        this.lastEmailSent = {
            to,
            subject,
            content: body,
            type: 'MandateCopy'
        };

        return true;
    }

    getLastEmail() {
        return this.lastEmailSent;
    }
}
