import { Controller, Post, Body, UseGuards } from '@nestjs/common';
import { NotificationsService } from './notifications.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { ConfigService } from '@nestjs/config';

@Controller('notifications')
export class NotificationsController {
    constructor(
        private readonly notificationsService: NotificationsService,
        private readonly configService: ConfigService
    ) { }

    @Post('send-whatsapp')
    @UseGuards(JwtAuthGuard)
    async sendWhatsApp(@Body() data: { phone: string, template: string, params: any }) {
        return this.notificationsService.sendWhatsApp(data.phone, data.template, data.params);
    }

    @Post('send-sms')
    @UseGuards(JwtAuthGuard)
    async sendSms(@Body() data: { phone: string, message: string }) {
        return this.notificationsService.sendSMS(data.phone, data.message);
    }

    @Post('send-prospect-link')
    @UseGuards(JwtAuthGuard)
    async sendProspectLink(@Body() data: {
        prospectId: string,
        prospectPhone: string,
        prospectFirstName: string,
        channel: 'SMS' | 'WHATSAPP'
    }) {
        const baseUrl = this.configService.get<string>('FRONTEND_URL') || 'https://app.simulegal.fr';
        const link = `${baseUrl}/simulation?ref=${data.prospectId}`;

        const message = `Bonjour ${data.prospectFirstName}, voici votre lien de simulation personnalisé SimuLegal :\n${link}\n\nCe lien est valable 48h.`;

        if (data.channel === 'WHATSAPP') {
            return this.notificationsService.sendWhatsApp(data.prospectPhone, 'simulation_link', { message });
        } else {
            return this.notificationsService.sendSMS(data.prospectPhone, message);
        }
    }

    @Post('send-appointment-confirmation')
    @UseGuards(JwtAuthGuard)
    async sendAppointmentConfirmation(@Body() data: {
        prospectId: string,
        prospectPhone: string,
        prospectFirstName: string,
        appointment: any,
        channel: 'SMS' | 'WHATSAPP'
    }) {
        const appointmentDate = new Date(data.appointment.date).toLocaleString('fr-FR', {
            weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', hour: '2-digit', minute: '2-digit'
        });
        const message = `Bonjour ${data.prospectFirstName},\n\nVotre RDV SimuLegal est confirmé le ${appointmentDate} à l'agence ${data.appointment.agencyName}.\n\nPour toute modification, répondez à ce message.\n\nÀ bientôt !`;

        if (data.channel === 'WHATSAPP') {
            return this.notificationsService.sendWhatsApp(data.prospectPhone, 'appointment_confirmation', { message });
        } else {
            return this.notificationsService.sendSMS(data.prospectPhone, message);
        }
    }

    @Post('trigger-stage-change')
    @UseGuards(JwtAuthGuard)
    async triggerStageChange(@Body() data: { lead: any, oldStage: string, newStage: string }) {
        return this.notificationsService.onStageChange(data.lead, data.oldStage, data.newStage);
    }

    /**
     * Test endpoint for transactional emails (DEV only)
     */
    @Post('test-email')
    async testEmail(@Body() data: { to: string; template: 'welcome' | 'diagnostic' | 'appointment' | 'payment' | 'reminder' }) {
        const testUser = { name: 'Jean Dupont', email: data.to };
        const testAppointment = {
            start: new Date(Date.now() + 24 * 60 * 60 * 1000), // Tomorrow
            type: 'VISIO_JURISTE' as const,
            meetingLink: 'https://meet.google.com/abc-defg-hij'
        };

        switch (data.template) {
            case 'welcome':
                return this.notificationsService.sendWelcomeEmail(testUser, 'TempPass123!');
            case 'diagnostic':
                return this.notificationsService.sendDiagnosticInvitation(testUser, 'https://simulegal.fr/diagnostic?token=abc123');
            case 'appointment':
                return this.notificationsService.sendAppointmentConfirmationEmail(testUser, testAppointment);
            case 'payment':
                return this.notificationsService.sendPaymentConfirmation(testUser, 9.90, 'REFUND-ABC123');
            case 'reminder':
                return this.notificationsService.sendAppointmentReminder(testUser, testAppointment);
            default:
                return { error: 'Invalid template. Use: welcome, diagnostic, appointment, payment, reminder' };
        }
    }

    /**
     * Refresh SMTP configuration cache (call after updating settings)
     */
    @Post('refresh-smtp')
    @UseGuards(JwtAuthGuard)
    async refreshSmtp() {
        await this.notificationsService.refreshSmtpConfig();
        return { success: true, message: 'SMTP configuration cache refreshed' };
    }

    /**
     * Test SMTP connection with current settings
     */
    @Post('test-smtp')
    async testSmtp(@Body() data: { to?: string }) {
        const testEmail = data.to || 'test@example.com';
        const result = await this.notificationsService.sendEmail(
            testEmail,
            'Test SMTP - SimuLegal',
            'Ceci est un email de test pour vérifier la configuration SMTP.',
            '<h1>✅ Configuration SMTP fonctionnelle</h1><p>Cet email confirme que votre serveur SMTP est correctement configuré.</p>'
        );
        return {
            success: result.success,
            messageId: result.messageId,
            message: result.success
                ? `Email de test envoyé à ${testEmail}`
                : 'Échec de l\'envoi (voir logs)'
        };
    }
}
