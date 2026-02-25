import { Controller, Post, Get, Body, Headers, Param, Request as Req, BadRequestException, Res, UseGuards } from '@nestjs/common';
import type { RawBodyRequest } from '@nestjs/common';
import type { Request, Response } from 'express';
import { PaymentsService } from './payments.service';
import { InvoicePdfService } from './invoice-pdf.service';
import { SettingsService } from '../settings/settings.service';
import { SERVICE_CATALOG } from '../config/services-pipeline.config';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';

@Controller('payments')
export class PaymentsController {
    constructor(
        private readonly paymentsService: PaymentsService,
        private readonly invoicePdfService: InvoicePdfService,
        private readonly settingsService: SettingsService,
    ) { }

    /**
     * GET /payments/resolve-price/:serviceId
     * Retourne le prix officiel résolu pour un service (promo > override > catalogue > mapping frontend > fallback)
     */
    @Get('resolve-price/:serviceId')
    async resolvePrice(@Param('serviceId') serviceId: string) {
        const overrides = await this.settingsService.getServicePricing();
        const catalogEntry = SERVICE_CATALOG.find(s => s.id === serviceId);

        // Mapping des IDs frontend (pôles de service)
        const FRONTEND_MAP: Record<string, { defaultPrice: number; label: string }> = {
            'nat_accomp': { defaultPrice: 49000, label: 'Accompagnement Nationalité' },
            'sejour_accomp': { defaultPrice: 35000, label: 'Accompagnement Titre Séjour' },
            'regroupement_familial': { defaultPrice: 39000, label: 'Regroupement Familial' },
            'permis_conduire': { defaultPrice: 15000, label: 'Changement Permis Conduire' },
            'rdv_juriste': { defaultPrice: 8000, label: 'Rendez-vous Juriste' },
            'rdv_prefecture': { defaultPrice: 5000, label: 'Rendez-vous Préfecture' },
            'langue_a2b1': { defaultPrice: 25000, label: 'Cours de langues A2/B1' },
            'form_civique': { defaultPrice: 12000, label: 'Formation Civique' },
            'rappel_echeances': { defaultPrice: 0, label: 'Être Rappelé (gratuit)' },
        };

        const frontendEntry = FRONTEND_MAP[serviceId];
        const basePriceCents = catalogEntry?.basePrice || frontendEntry?.defaultPrice || 0;
        const label = catalogEntry?.name || frontendEntry?.label || serviceId;

        const override = overrides[serviceId];
        let priceCents = basePriceCents;
        let source = catalogEntry ? 'CATALOG_DEFAULT' : (frontendEntry ? 'FRONTEND_MAP' : 'UNKNOWN');
        let promoActive = false;

        if (override) {
            if (override.promoPrice && override.promoUntil && new Date(override.promoUntil) > new Date()) {
                priceCents = override.promoPrice;
                source = 'PROMO';
                promoActive = true;
            } else if (override.price) {
                priceCents = override.price;
                source = 'ADMIN_OVERRIDE';
            }
        }

        if (priceCents === 0 && !catalogEntry && !frontendEntry) {
            priceCents = 10000;
            source = 'FALLBACK';
        }

        return {
            serviceId,
            serviceName: label,
            priceCents,
            priceEuros: priceCents / 100,
            pricePer3: Math.ceil(priceCents / 3) / 100,
            source,
            promoActive,
            promoUntil: promoActive ? override?.promoUntil : null,
            defaultPriceCents: basePriceCents,
        };
    }

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

    /**
     * Exporte la facture d'un lead au format PDF
     */
    @Get(':leadId/invoice')
    // @UseGuards(JwtAuthGuard) // Protection optionnelle en fonction du contexte
    async downloadInvoice(@Param('leadId') leadId: string, @Res() res: Response) {
        try {
            const pdfBuffer = await this.invoicePdfService.generateInvoicePdf(leadId);

            res.set({
                'Content-Type': 'application/pdf',
                'Content-Disposition': `attachment; filename="facture-${leadId.substring(0, 6)}.pdf"`,
                'Content-Length': pdfBuffer.length,
            });

            res.end(pdfBuffer);
        } catch (error: any) {
            res.status(404).json({ message: error.message || 'Invoice generation failed' });
        }
    }
}

