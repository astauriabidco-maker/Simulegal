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

    /**
     * Créer une session de paiement pour un prospect (avant conversion en Lead).
     * Le webhook Stripe déclenchera automatiquement : SIGNED + création Lead CRM.
     */
    @Post('prospect-checkout')
    async createProspectCheckout(@Body() data: {
        prospectId: string;
        amount: number;
        serviceId: string;
        serviceName: string;
        installments?: 1 | 3;
        successUrl: string;
        cancelUrl: string;
    }) {
        if (!data.prospectId) throw new BadRequestException('prospectId is required');
        if (!data.amount || data.amount <= 0) throw new BadRequestException('amount must be positive');
        if (!data.serviceId) throw new BadRequestException('serviceId is required');

        return this.paymentsService.createProspectCheckoutSession(data.prospectId, {
            amount: data.amount,
            serviceId: data.serviceId,
            serviceName: data.serviceName || data.serviceId,
            installments: data.installments || 1,
            successUrl: data.successUrl,
            cancelUrl: data.cancelUrl,
        });
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
