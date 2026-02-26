import { Injectable } from '@nestjs/common';

@Injectable()
export class EmailTemplatesService {
    private readonly brandColor = '#2563eb';
    private readonly bgColor = '#f8fafc';
    private readonly textColor = '#1e293b';

    /**
     * Base HTML wrapper with header, footer, and inline styles
     */
    getBaseTemplate(content: string, previewText: string = ''): string {
        return `
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>SimuLegal</title>
    <!--[if mso]>
    <style type="text/css">
        table { border-collapse: collapse; }
        .content { width: 600px !important; }
    </style>
    <![endif]-->
</head>
<body style="margin: 0; padding: 0; background-color: ${this.bgColor}; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;">
    <!-- Preview text -->
    <div style="display: none; max-height: 0; overflow: hidden;">
        ${previewText}
    </div>
    
    <table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background-color: ${this.bgColor};">
        <tr>
            <td align="center" style="padding: 40px 20px;">
                <table role="presentation" class="content" width="600" cellspacing="0" cellpadding="0" style="background-color: #ffffff; border-radius: 12px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);">
                    <!-- Header -->
                    <tr>
                        <td style="background: linear-gradient(135deg, ${this.brandColor} 0%, #1d4ed8 100%); padding: 30px 40px; border-radius: 12px 12px 0 0;">
                            <h1 style="margin: 0; color: #ffffff; font-size: 28px; font-weight: 700; letter-spacing: -0.5px;">
                                SIMULEGAL
                            </h1>
                            <p style="margin: 5px 0 0; color: rgba(255,255,255,0.8); font-size: 14px;">
                                Votre partenaire juridique
                            </p>
                        </td>
                    </tr>
                    
                    <!-- Content -->
                    <tr>
                        <td style="padding: 40px;">
                            ${content}
                        </td>
                    </tr>
                    
                    <!-- Footer -->
                    <tr>
                        <td style="padding: 20px 40px 30px; border-top: 1px solid #e2e8f0;">
                            <p style="margin: 0; color: #64748b; font-size: 12px; text-align: center;">
                                © ${new Date().getFullYear()} SimuLegal. Tous droits réservés.<br>
                                <a href="https://simulegal.fr" style="color: ${this.brandColor}; text-decoration: none;">simulegal.fr</a>
                            </p>
                        </td>
                    </tr>
                </table>
            </td>
        </tr>
    </table>
</body>
</html>`;
    }

    /**
     * Welcome email with temporary credentials
     */
    renderWelcome(name: string, email: string, tempPassword: string): { subject: string; html: string } {
        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Bienvenue ${name} ! 🎉
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Votre compte SimuLegal a été créé avec succès. Voici vos accès pour vous connecter :
            </p>
            
            <div style="background-color: #f1f5f9; border-radius: 8px; padding: 20px; margin: 25px 0;">
                <table role="presentation" width="100%" cellspacing="0" cellpadding="0">
                    <tr>
                        <td style="padding: 8px 0;">
                            <span style="color: #64748b; font-size: 14px;">Email :</span><br>
                            <strong style="color: ${this.textColor}; font-size: 16px;">${email}</strong>
                        </td>
                    </tr>
                    <tr>
                        <td style="padding: 8px 0;">
                            <span style="color: #64748b; font-size: 14px;">Mot de passe temporaire :</span><br>
                            <strong style="color: ${this.brandColor}; font-size: 18px; font-family: monospace;">${tempPassword}</strong>
                        </td>
                    </tr>
                </table>
            </div>
            
            <p style="margin: 0 0 25px; color: #64748b; font-size: 14px;">
                ⚠️ Pour votre sécurité, changez votre mot de passe dès votre première connexion.
            </p>
            
            <a href="https://simulegal.fr/connexion" 
               style="display: inline-block; background-color: ${this.brandColor}; color: #ffffff; text-decoration: none; padding: 14px 28px; border-radius: 8px; font-weight: 600; font-size: 16px;">
                Se connecter →
            </a>
        `;

        return {
            subject: 'Bienvenue chez SimuLegal - Vos accès',
            html: this.getBaseTemplate(content, `Bienvenue ${name} ! Vos accès SimuLegal sont prêts.`)
        };
    }

    /**
     * Password reset email with new temporary credentials
     */
    renderPasswordReset(name: string, email: string, tempPassword: string): { subject: string; html: string } {
        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Mot de passe réinitialisé 🔑
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Bonjour ${name}, votre mot de passe a été réinitialisé par un administrateur. Voici vos nouveaux accès :
            </p>
            
            <div style="background-color: #f1f5f9; border-radius: 8px; padding: 20px; margin: 25px 0;">
                <table role="presentation" width="100%" cellspacing="0" cellpadding="0">
                    <tr>
                        <td style="padding: 8px 0;">
                            <span style="color: #64748b; font-size: 14px;">Email :</span><br>
                            <strong style="color: ${this.textColor}; font-size: 16px;">${email}</strong>
                        </td>
                    </tr>
                    <tr>
                        <td style="padding: 8px 0;">
                            <span style="color: #64748b; font-size: 14px;">Nouveau mot de passe :</span><br>
                            <strong style="color: ${this.brandColor}; font-size: 18px; font-family: monospace;">${tempPassword}</strong>
                        </td>
                    </tr>
                </table>
            </div>
            
            <p style="margin: 0 0 25px; color: #64748b; font-size: 14px;">
                ⚠️ Pour votre sécurité, changez votre mot de passe dès votre prochaine connexion.
            </p>
            
            <a href="https://simulegal.fr/connexion" 
               style="display: inline-block; background-color: ${this.brandColor}; color: #ffffff; text-decoration: none; padding: 14px 28px; border-radius: 8px; font-weight: 600; font-size: 16px;">
                Se connecter →
            </a>
        `;

        return {
            subject: 'SimuLegal - Mot de passe réinitialisé',
            html: this.getBaseTemplate(content, `${name}, votre mot de passe SimuLegal a été réinitialisé.`)
        };
    }

    /**
     * Diagnostic invitation email with magic link
     */
    renderDiagnosticInvitation(name: string, magicLink: string): { subject: string; html: string } {
        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Bonjour ${name},
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Vous avez été invité(e) à passer votre diagnostic d'éligibilité SimuLegal. Ce test rapide nous permettra de mieux comprendre votre situation.
            </p>
            
            <div style="background: linear-gradient(135deg, #dbeafe 0%, #e0e7ff 100%); border-radius: 8px; padding: 25px; margin: 25px 0; text-align: center;">
                <p style="margin: 0 0 15px; color: ${this.textColor}; font-size: 14px;">
                    🕐 Durée estimée : <strong>5-10 minutes</strong>
                </p>
                <a href="${magicLink}" 
                   style="display: inline-block; background-color: ${this.brandColor}; color: #ffffff; text-decoration: none; padding: 16px 32px; border-radius: 8px; font-weight: 600; font-size: 18px;">
                    Commencer le diagnostic
                </a>
            </div>
            
            <p style="margin: 0; color: #64748b; font-size: 14px;">
                Ce lien est personnel et expire dans 7 jours. Si vous n'êtes pas à l'origine de cette demande, ignorez cet email.
            </p>
        `;

        return {
            subject: 'Votre diagnostic SimuLegal vous attend',
            html: this.getBaseTemplate(content, `${name}, passez votre diagnostic d'éligibilité SimuLegal.`)
        };
    }

    /**
     * Appointment confirmation email
     */
    renderAppointmentConfirmation(
        name: string,
        date: Date,
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY',
        meetingLink?: string,
        agencyAddress?: string
    ): { subject: string; html: string } {
        const dateStr = date.toLocaleDateString('fr-FR', {
            weekday: 'long',
            day: 'numeric',
            month: 'long',
            year: 'numeric',
            hour: '2-digit',
            minute: '2-digit'
        });

        const locationBlock = type === 'VISIO_JURISTE'
            ? `
                <tr>
                    <td style="padding: 10px 0;">
                        <span style="color: #64748b; font-size: 14px;">📹 Lien visio :</span><br>
                        <a href="${meetingLink}" style="color: ${this.brandColor}; font-size: 16px; word-break: break-all;">${meetingLink}</a>
                    </td>
                </tr>`
            : `
                <tr>
                    <td style="padding: 10px 0;">
                        <span style="color: #64748b; font-size: 14px;">📍 Adresse :</span><br>
                        <strong style="color: ${this.textColor}; font-size: 16px;">${agencyAddress || 'Siège SimuLegal'}</strong>
                    </td>
                </tr>`;

        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Rendez-vous confirmé ! ✅
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Bonjour ${name}, votre rendez-vous avec un expert SimuLegal est confirmé.
            </p>
            
            <div style="background-color: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 20px; margin: 25px 0;">
                <table role="presentation" width="100%" cellspacing="0" cellpadding="0">
                    <tr>
                        <td style="padding: 10px 0;">
                            <span style="color: #64748b; font-size: 14px;">📅 Date et heure :</span><br>
                            <strong style="color: ${this.textColor}; font-size: 18px;">${dateStr}</strong>
                        </td>
                    </tr>
                    <tr>
                        <td style="padding: 10px 0;">
                            <span style="color: #64748b; font-size: 14px;">Type :</span><br>
                            <strong style="color: ${this.textColor}; font-size: 16px;">${type === 'VISIO_JURISTE' ? 'Visioconférence' : 'Rendez-vous en agence'}</strong>
                        </td>
                    </tr>
                    ${locationBlock}
                </table>
            </div>
            
            <p style="margin: 0; color: #64748b; font-size: 14px;">
                💡 Conseil : Préparez vos documents d'identité et tout justificatif utile à votre dossier.
            </p>
        `;

        return {
            subject: `RDV SimuLegal confirmé - ${dateStr}`,
            html: this.getBaseTemplate(content, `Votre RDV SimuLegal est confirmé pour le ${dateStr}.`)
        };
    }

    /**
     * Payment confirmation with refund code (B2C)
     */
    renderPaymentConfirmation(name: string, amount: number, refundCode: string): { subject: string; html: string } {
        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Paiement reçu ! 💳
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Merci ${name}, votre paiement de <strong>${amount.toFixed(2)} €</strong> a bien été reçu.
            </p>
            
            <div style="background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%); border-radius: 8px; padding: 25px; margin: 25px 0; text-align: center;">
                <p style="margin: 0 0 10px; color: #92400e; font-size: 14px; font-weight: 600;">
                    🎁 VOTRE CODE DE REMBOURSEMENT
                </p>
                <p style="margin: 0; color: #78350f; font-size: 28px; font-weight: 700; font-family: monospace; letter-spacing: 2px;">
                    ${refundCode}
                </p>
            </div>
            
            <p style="margin: 0 0 15px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Ce code vous permet d'être remboursé si vous vous inscrivez chez l'un de nos partenaires agréés. Présentez-le lors de votre inscription.
            </p>
            
            <div style="background-color: #f1f5f9; border-radius: 8px; padding: 15px; margin: 20px 0;">
                <p style="margin: 0; color: #64748b; font-size: 13px;">
                    ✅ Votre accès au diagnostic est maintenant actif.<br>
                    Connectez-vous pour commencer votre évaluation.
                </p>
            </div>
            
            <a href="https://simulegal.fr/mon-espace" 
               style="display: inline-block; background-color: ${this.brandColor}; color: #ffffff; text-decoration: none; padding: 14px 28px; border-radius: 8px; font-weight: 600; font-size: 16px;">
                Accéder à mon espace →
            </a>
        `;

        return {
            subject: 'Paiement confirmé - SimuLegal',
            html: this.getBaseTemplate(content, `Merci ${name} ! Votre code de remboursement : ${refundCode}`)
        };
    }

    /**
     * Appointment reminder (24h before)
     */
    renderAppointmentReminder(
        name: string,
        date: Date,
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY',
        meetingLink?: string
    ): { subject: string; html: string } {
        const dateStr = date.toLocaleDateString('fr-FR', {
            weekday: 'long',
            day: 'numeric',
            month: 'long',
            hour: '2-digit',
            minute: '2-digit'
        });

        const content = `
            <h2 style="margin: 0 0 20px; color: ${this.textColor}; font-size: 22px;">
                Rappel : RDV demain ! ⏰
            </h2>
            <p style="margin: 0 0 20px; color: ${this.textColor}; font-size: 16px; line-height: 1.6;">
                Bonjour ${name}, nous vous rappelons votre rendez-vous prévu <strong>demain</strong>.
            </p>
            
            <div style="background-color: #fef3c7; border: 1px solid #fcd34d; border-radius: 8px; padding: 20px; margin: 25px 0; text-align: center;">
                <p style="margin: 0; color: #92400e; font-size: 18px; font-weight: 600;">
                    📅 ${dateStr}
                </p>
                ${type === 'VISIO_JURISTE' && meetingLink ? `
                    <a href="${meetingLink}" 
                       style="display: inline-block; margin-top: 15px; background-color: ${this.brandColor}; color: #ffffff; text-decoration: none; padding: 12px 24px; border-radius: 8px; font-weight: 600;">
                        Rejoindre la visio
                    </a>
                ` : ''}
            </div>
            
            <p style="margin: 0; color: #64748b; font-size: 14px;">
                En cas d'empêchement, merci de nous prévenir au plus vite.
            </p>
        `;

        return {
            subject: `Rappel : RDV SimuLegal demain à ${date.toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}`,
            html: this.getBaseTemplate(content, `Rappel : votre RDV SimuLegal est prévu demain.`)
        };
    }
}
