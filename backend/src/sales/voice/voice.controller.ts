import { Controller, Get, Post, Body, Req, Res, UseGuards } from '@nestjs/common';
import { VoiceService } from './voice.service';
import { JwtAuthGuard } from '../../auth/jwt-auth.guard'; // Assuming you have this
import type { Request, Response } from 'express';

@Controller('voice')
export class VoiceController {
    constructor(private readonly voiceService: VoiceService) { }

    @Get('token')
    // @UseGuards(JwtAuthGuard) // Enable in production
    getToken(@Req() req: Request) {
        // In real app, use req.user.id
        const identity = 'agent-' + Math.random().toString(36).substring(7);
        return this.voiceService.generateToken(identity);
    }

    @Post('twiml')
    handleVoiceWebhook(@Body() body: any, @Res() res: Response) {
        // Twilio hits this when a call is made from the browser
        const To = body.To;
        const twiml = this.voiceService.handleOutgoingCall(To);

        res.type('text/xml');
        res.send(twiml);
    }
}
