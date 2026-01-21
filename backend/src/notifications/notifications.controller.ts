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

    @Post('trigger-stage-change')
    @UseGuards(JwtAuthGuard)
    async triggerStageChange(@Body() data: { lead: any, oldStage: string, newStage: string }) {
        return this.notificationsService.onStageChange(data.lead, data.oldStage, data.newStage);
    }
}
