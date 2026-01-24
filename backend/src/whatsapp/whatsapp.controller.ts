import { Controller, Post, Body, Logger, Get, Query, Param, UseGuards } from '@nestjs/common';
import { WhatsappService } from './whatsapp.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';

@Controller('whatsapp')
export class WhatsappController {
    private readonly logger = new Logger(WhatsappController.name);

    constructor(private readonly whatsappService: WhatsappService) { }

    @Get('webhook')
    healthCheck() {
        return { status: 'WhatsApp Webhook Listener Active' };
    }

    @Post('webhook')
    async handleWebhook(@Body() body: any) {
        this.logger.log(`Incoming Twilio Webhook: ${JSON.stringify(body)}`);

        await this.whatsappService.handleIncoming({
            from: body.From,
            body: body.Body,
            messageSid: body.MessageSid
        });

        return '<?xml version="1.0" encoding="UTF-8"?><Response></Response>';
    }

    @UseGuards(JwtAuthGuard)
    @Get('conversations')
    async getConversations() {
        return this.whatsappService.getConversations();
    }

    @UseGuards(JwtAuthGuard)
    @Get('messages/:type/:id')
    async getMessages(
        @Param('type') type: 'LEAD' | 'PROSPECT',
        @Param('id') id: string
    ) {
        return this.whatsappService.getMessages(type, id);
    }

    @UseGuards(JwtAuthGuard)
    @Post('send')
    async sendMessage(
        @Body() body: { type: 'LEAD' | 'PROSPECT'; id: string; content: string }
    ) {
        return this.whatsappService.sendMessage(body.type, body.id, body.content);
    }
}
