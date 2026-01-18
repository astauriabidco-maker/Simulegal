import { Controller, Post, Body, UseGuards } from '@nestjs/common';
import { NotificationsService } from './notifications.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';

@Controller('notifications')
export class NotificationsController {
    constructor(private readonly notificationsService: NotificationsService) { }

    @Post('send-whatsapp')
    @UseGuards(JwtAuthGuard)
    async sendWhatsApp(@Body() data: { phone: string, template: string, params: any }) {
        return this.notificationsService.sendWhatsApp(data.phone, data.template, data.params);
    }

    @Post('trigger-stage-change')
    @UseGuards(JwtAuthGuard)
    async triggerStageChange(@Body() data: { lead: any, oldStage: string, newStage: string }) {
        return this.notificationsService.onStageChange(data.lead, data.oldStage, data.newStage);
    }
}
