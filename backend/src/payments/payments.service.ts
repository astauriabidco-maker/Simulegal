import { Injectable, Logger, OnModuleInit, Inject, forwardRef } from '@nestjs/common';
import { SettingsService } from '../settings/settings.service';
import Stripe from 'stripe';
import { LeadsService } from '../leads/leads.service';
import { EmailService } from '../email/email.service';
import { NotificationsService } from '../notifications/notifications.service';
import { PipelineAutomationService } from '../pipeline-automation/pipeline-automation.service';

@Injectable()
export class PaymentsService implements OnModuleInit {
    private readonly logger = new Logger(PaymentsService.name);
    private stripe: Stripe;

    constructor(
        private settingsService: SettingsService,
        @Inject(forwardRef(() => LeadsService)) private leadsService: LeadsService,
        private emailService: EmailService,
        private notificationsService: NotificationsService,
        private pipelineAutomation: PipelineAutomationService,
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
    isStripeConfigured(): boolean {
        return !!this.stripe;
    }

    async createCheckoutSession(leadId: string, successUrl: string, cancelUrl: string, amount?: number, label?: string) {
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
                            name: label || `Prestation SimuLegal - ${lead.serviceName}`,
                            description: `Dossier #${lead.id}`,
                        },
                        unit_amount: amount ? amount * 100 : (lead.amountPaid || 10000), // En centimes
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
                label: label || lead.serviceName,
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

                // Trigger pipeline automations on payment
                const leadForAutomation = await this.leadsService.findOne(leadId);
                if (leadForAutomation) {
                    await this.pipelineAutomation.onPaymentReceived(leadForAutomation);
                }

                // Envoi des emails de facturation / de notification
                try {
                    const leadInfo = await this.leadsService.findOne(leadId);
                    if (leadInfo) {
                        const amountStr = session.amount_total ? session.amount_total / 100 : 0;
                        const serviceLabel = session.metadata?.label || leadInfo.serviceName;

                        if (leadInfo.email) {
                            const clientSpaceUrlForEmail = this.leadsService.generateClientSpaceUrl(leadInfo.id);
                            await this.emailService.sendOrderConfirmation(
                                leadInfo.email,
                                leadInfo.name,
                                serviceLabel,
                                amountStr,
                                session.id,
                                leadInfo.requiredDocs,
                                clientSpaceUrlForEmail
                            );
                            await this.emailService.sendMandateCopy(leadInfo.email, leadInfo.name);
                        }

                        // Envoi de la checklist WhatsApp avec Boutons Interactifs + Espace Client
                        if (leadInfo.phone && leadInfo.requiredDocs && leadInfo.requiredDocs.length > 0) {
                            const uploadLinks = this.leadsService.generateDocumentUploadLinks(leadInfo.id, leadInfo.requiredDocs);
                            const clientSpaceUrl = this.leadsService.generateClientSpaceUrl(leadInfo.id);
                            const { message, buttons } = this.leadsService.buildWhatsAppChecklistMessage(serviceLabel, clientSpaceUrl, uploadLinks);

                            await this.notificationsService.sendWhatsApp(
                                leadInfo.phone,
                                'order_checklist',
                                { message },
                                { leadId: leadInfo.id },
                                buttons
                            );
                        }
                    }
                } catch (e) {
                    this.logger.error(`Failed to send payment confirmation email or whatsapp: ${e.message}`);
                }
            }
        }
    }
}
