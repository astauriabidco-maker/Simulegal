import { Injectable, Logger, OnModuleInit } from '@nestjs/common';
import { SettingsService } from '../settings/settings.service';
import Stripe from 'stripe';
import { LeadsService } from '../leads/leads.service';

@Injectable()
export class PaymentsService implements OnModuleInit {
    private readonly logger = new Logger(PaymentsService.name);
    private stripe: Stripe;

    constructor(
        private settingsService: SettingsService,
        private leadsService: LeadsService
    ) { }

    async onModuleInit() {
        const settings = await this.settingsService.getSettings();
        const stripeKey = settings.payment.secretKey;

        if (stripeKey && stripeKey !== 'sk_test_sample') {
            this.stripe = new Stripe(stripeKey, {
                apiVersion: '2025-01-27.acacia' as any,
            });
            this.logger.log('Stripe SDK initialized');
        } else {
            this.logger.warn('Stripe Secret Key is missing or default. Payments will be disabled.');
        }
    }

    async createCheckoutSession(leadId: string, successUrl: string, cancelUrl: string) {
        if (!this.stripe) throw new Error('Stripe is not configured');

        const lead = await this.leadsService.findOne(leadId);
        if (!lead) throw new Error('Lead not found');

        const session = await this.stripe.checkout.sessions.create({
            payment_method_types: ['card'],
            line_items: [
                {
                    price_data: {
                        currency: 'eur',
                        product_data: {
                            name: `Prestation SimuLegal - ${lead.serviceName}`,
                            description: `Dossier #${lead.id}`,
                        },
                        unit_amount: lead.amountPaid || 10000, // Fallback if amount is missing, in cents
                    },
                    quantity: 1,
                },
            ],
            mode: 'payment',
            success_url: successUrl,
            cancel_url: cancelUrl,
            client_reference_id: lead.id,
            metadata: {
                leadId: lead.id,
            },
        });

        return { url: session.url };
    }

    async handleWebhook(signature: string, payload: Buffer) {
        if (!this.stripe) return;

        const settings = await this.settingsService.getSettings();
        const webhookSecret = settings.payment.webhookSecret; // Note: Need to verify if this exists in settings

        let event: Stripe.Event;

        try {
            event = this.stripe.webhooks.constructEvent(
                payload,
                signature,
                webhookSecret || ''
            );
        } catch (err) {
            this.logger.error(`Webhook signature verification failed: ${err.message}`);
            throw new Error(`Webhook Error: ${err.message}`);
        }

        if (event.type === 'checkout.session.completed') {
            const session = event.data.object as Stripe.Checkout.Session;
            const leadId = session.client_reference_id;

            if (leadId) {
                this.logger.log(`Payment confirmed for Lead ${leadId}`);
                await this.leadsService.recordPayment(leadId, {
                    amount: session.amount_total || 0,
                    method: 'STRIPE',
                    reference: session.payment_intent as string
                });
            }
        }
    }
}
