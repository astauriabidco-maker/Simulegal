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

    async sendOrderConfirmation(
        to: string,
        clientName: string,
        serviceName: string,
        amount: number,
        transactionRef: string,
        requiredDocs?: any[],
        clientSpaceUrl?: string,
        invoicePdf?: Buffer,
        invoiceFilename?: string,
        checklistPdf?: Buffer,
        checklistFilename?: string,
    ) {
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

        // Build PJ description text
        const pjLines: string[] = [];
        if (invoicePdf) pjLines.push('📎 Votre facture');
        if (checklistPdf) pjLines.push('📎 Votre checklist de documents personnalisée');
        const pjText = pjLines.length > 0 ? pjLines.join(' et ') + ' sont jointes à cet email au format PDF.\n        ' : '';

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
        
        ${pjText}Un juriste va prendre connaissance de votre dossier sous 24h ouvrées.
        
        Cordialement,
        L'équipe Simulegal
        `;

        // Build attachments array
        const attachments: { filename: string; content: Buffer; contentType?: string }[] = [];
        if (invoicePdf) {
            attachments.push({
                filename: invoiceFilename || `facture-simulegal.pdf`,
                content: invoicePdf,
                contentType: 'application/pdf',
            });
        }
        if (checklistPdf) {
            attachments.push({
                filename: checklistFilename || `checklist-documents.pdf`,
                content: checklistPdf,
                contentType: 'application/pdf',
            });
        }

        // Route vers le vrai SMTP via NotificationsService (with attachments)
        await this.notifications.sendEmail(to, subject, textContent, undefined, attachments.length > 0 ? attachments : undefined);

        // Debug storage
        this.lastEmailSent = {
            to,
            subject,
            content: textContent,
            type: 'OrderConfirmation',
            hasInvoicePdf: !!invoicePdf,
            hasChecklistPdf: !!checklistPdf,
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
