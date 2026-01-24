import { Controller, Post, Body, Headers, Request as Req, BadRequestException } from '@nestjs/common';
import type { RawBodyRequest } from '@nestjs/common';
import type { Request } from 'express';
import { PaymentsService } from './payments.service';

@Controller('payments')
export class PaymentsController {
    constructor(private readonly paymentsService: PaymentsService) { }

    @Post('create-session')
    async createSession(@Body() data: { leadId: string, successUrl: string, cancelUrl: string }) {
        if (!data.leadId) throw new BadRequestException('leadId is required');
        return this.paymentsService.createCheckoutSession(data.leadId, data.successUrl, data.cancelUrl);
    }

    @Post('webhook')
    async stripeWebhook(
        @Headers('stripe-signature') signature: string,
        @Req() req: RawBodyRequest<Request>
    ) {
        if (!signature) throw new BadRequestException('Missing stripe-signature');

        // Note: req.rawBody must be enabled in main.ts
        const payload = (req as any).rawBody;
        if (!payload) {
            console.error('[Payments] rawBody is missing! Webhook cannot be verified.');
            throw new BadRequestException('rawBody is missing');
        }

        return this.paymentsService.handleWebhook(signature, payload);
    }
}
