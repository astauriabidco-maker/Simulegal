import { Controller, Post, Body, Logger, Get, Param, UseGuards, StreamableFile, Header, NotFoundException } from '@nestjs/common';
import { WhatsappService } from './whatsapp.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { TwilioWebhookGuard } from './twilio-webhook.guard';
import * as path from 'path';
import * as fs from 'fs';

@Controller('whatsapp')
export class WhatsappController {
    private readonly logger = new Logger(WhatsappController.name);

    constructor(private readonly whatsappService: WhatsappService) { }

    @Get('webhook')
    healthCheck() {
        return { status: 'WhatsApp Webhook Listener Active' };
    }

    /**
     * Twilio Webhook - Reçoit les messages entrants WhatsApp
     * Protégé par la validation de signature Twilio (X-Twilio-Signature)
     * Twilio envoie : From, Body, MessageSid, NumMedia, MediaUrl0, MediaContentType0, etc.
     */
    @UseGuards(TwilioWebhookGuard)
    @Post('webhook')
    async handleWebhook(@Body() body: any) {
        this.logger.log(`📩 Incoming Twilio Webhook: From=${body.From}, NumMedia=${body.NumMedia || 0}`);

        // Extraire les médias envoyés par le client
        const numMedia = parseInt(body.NumMedia || '0', 10);
        const mediaItems: { url: string; contentType: string }[] = [];

        for (let i = 0; i < numMedia; i++) {
            const url = body[`MediaUrl${i}`];
            const contentType = body[`MediaContentType${i}`];
            if (url && contentType) {
                mediaItems.push({ url, contentType });
                this.logger.log(`  📎 Media ${i}: ${contentType} -> ${url}`);
            }
        }

        await this.whatsappService.handleIncoming({
            from: body.From,
            body: body.Body || '',
            messageSid: body.MessageSid,
            numMedia,
            mediaItems
        });

        // Réponse TwiML vide (pas de réponse automatique)
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

    /**
     * Servir les fichiers médias uploadés (images, PDFs reçus par WhatsApp)
     * Accessible via GET /whatsapp/media/:filename
     */
    @UseGuards(JwtAuthGuard)
    @Get('media/:filename')
    serveMedia(
        @Param('filename') filename: string,
    ): StreamableFile {
        // Sécurité : empêcher le path traversal
        const sanitizedFilename = path.basename(filename);
        const filePath = path.resolve(__dirname, '..', '..', 'uploads', 'whatsapp', sanitizedFilename);

        if (!fs.existsSync(filePath)) {
            throw new NotFoundException('Fichier non trouvé');
        }

        const fileStream = fs.createReadStream(filePath);
        return new StreamableFile(fileStream);
    }
}
