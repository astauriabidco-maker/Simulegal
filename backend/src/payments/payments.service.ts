import { Injectable, Logger, OnModuleInit, Inject, forwardRef } from '@nestjs/common';
import { SettingsService } from '../settings/settings.service';
import Stripe from 'stripe';
import { LeadsService } from '../leads/leads.service';
import { EmailService } from '../email/email.service';
import { NotificationsService } from '../notifications/notifications.service';
import { PipelineAutomationService } from '../pipeline-automation/pipeline-automation.service';
import { SalesService } from '../sales/sales.service';
import { PrismaService } from '../prisma/prisma.service';
import { InvoicePdfService } from './invoice-pdf.service';
import { ChecklistPdfService } from './checklist-pdf.service';
import { SERVICE_CATALOG } from '../config/services-pipeline.config';

import { EventEmitter2 } from '@nestjs/event-emitter';

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
        @Inject(forwardRef(() => SalesService)) private salesService: SalesService,
        private prisma: PrismaService,
        private eventEmitter: EventEmitter2,
        private invoicePdfService: InvoicePdfService,
        private checklistPdfService: ChecklistPdfService,
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

    // ═════════════════════════════════════════════════
    // RÉSOLUTION DU PRIX OFFICIEL (source unique de vérité)
    // ═════════════════════════════════════════════════

    /**
     * Mapping des IDs du catalogue frontend (data/services.ts)
     * vers les prix par défaut en centimes.
     * Ces IDs sont des « pôles de service » utilisés quand le simulateur
     * n'a pas encore renvoyé une procédure spécifique.
     */
    private static readonly FRONTEND_SERVICE_MAP: Record<string, { defaultPrice: number; label: string }> = {
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

    /**
     * Résout le prix officiel d'un service en centimes.
     * Priorité : 1) Override admin (par serviceId exact) → 2) Prix promo actif → 3) SERVICE_CATALOG → 4) FRONTEND_SERVICE_MAP → 5) Fallback
     */
    private async resolveServicePrice(serviceId: string): Promise<{ priceCents: number; label: string; source: string }> {
        const overrides = await this.settingsService.getServicePricing();

        // 1. Chercher dans le catalogue backend (procédures granulaires)
        const catalogEntry = SERVICE_CATALOG.find(s => s.id === serviceId);

        // 2. Chercher dans le mapping frontend (pôles de service)
        const frontendEntry = PaymentsService.FRONTEND_SERVICE_MAP[serviceId];

        const basePriceCents = catalogEntry?.basePrice || frontendEntry?.defaultPrice || 0;
        const label = catalogEntry?.name || frontendEntry?.label || serviceId;

        // 3. Vérifier les overrides admin
        const override = overrides[serviceId];
        if (override) {
            if (override.promoPrice && override.promoUntil) {
                const promoEnd = new Date(override.promoUntil);
                if (promoEnd > new Date()) {
                    this.logger.log(`[💰] Prix promo actif pour ${serviceId}: ${override.promoPrice}c (jusqu'au ${override.promoUntil})`);
                    return { priceCents: override.promoPrice, label, source: 'PROMO' };
                }
            }
            if (override.price) {
                return { priceCents: override.price, label, source: 'ADMIN_OVERRIDE' };
            }
        }

        // 4. Prix par défaut (catalogue backend ou mapping frontend)
        if (basePriceCents > 0) {
            return { priceCents: basePriceCents, label, source: catalogEntry ? 'CATALOG_DEFAULT' : 'FRONTEND_MAP' };
        }

        // 5. Fallback absolu
        this.logger.warn(`[⚠️] Aucun prix trouvé pour ${serviceId}, fallback 100€`);
        return { priceCents: 10000, label, source: 'FALLBACK' };
    }

    // ═════════════════════════════════════════════════
    // CHECKOUT POUR PROSPECTS (paiement avant création Lead)
    // ═════════════════════════════════════════════════

    async createProspectCheckoutSession(
        prospectId: string,
        options: {
            amount: number;        // Montant envoyé par le frontend (informatif, NON utilisé)
            serviceId: string;     // Service sélectionné
            serviceName: string;   // Label du service
            installments?: 1 | 3;  // Paiement en 1x ou 3x
            successUrl: string;
            cancelUrl: string;
        }
    ) {
        // ── Résoudre le prix officiel depuis la DB (source unique de vérité) ──
        const resolved = await this.resolveServicePrice(options.serviceId);
        const installments = options.installments || 1;
        const unitAmountCents = installments === 3 ? Math.ceil(resolved.priceCents / 3) : resolved.priceCents;
        const totalAmountCents = resolved.priceCents;

        // Log si le frontend envoie un montant différent
        const frontendCents = options.amount * 100;
        if (Math.abs(frontendCents - totalAmountCents) > 100) { // tolérance 1€
            this.logger.warn(
                `[⚠️ Prix mismatch] Frontend: ${options.amount}€ vs Backend: ${totalAmountCents / 100}€ ` +
                `(source: ${resolved.source}) — Backend fait foi`
            );
        }

        if (!this.stripe) {
            this.logger.warn('Stripe is not configured, mocking payment and auto-converting prospect');
            try {
                const result = await this.salesService.convertToLead(prospectId, options.serviceId);
                if (result) {
                    await this.leadsService.recordPayment(result.leadId, {
                        amount: totalAmountCents,
                        method: 'MOCK_STRIPE',
                        reference: 'mock_payment_' + Date.now(),
                    });
                }
            } catch (err) {
                this.logger.error(`[❌] Failed to mock auto-convert prospect ${prospectId}: ${err.message}`);
            }
            return { url: options.successUrl, sessionId: 'mock_session_' + Date.now(), resolvedPrice: totalAmountCents / 100 };
        }

        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        if (!prospect) throw new Error('Prospect not found');

        // Vérifier que la simulation d'éligibilité a été réalisée
        if (!(prospect as any).eligibilityResult) {
            throw new Error('Simulation d\'éligibilité requise avant le paiement. Veuillez d\'abord réaliser la simulation en agence.');
        }

        // ── Construire la session Stripe avec le prix officiel ──
        const serviceName = resolved.label || options.serviceName;
        const sessionConfig: Stripe.Checkout.SessionCreateParams = {
            payment_method_types: ['card'],
            line_items: [
                {
                    price_data: {
                        currency: 'eur',
                        product_data: {
                            name: `SimuLegal — ${serviceName}`,
                            description: installments === 3
                                ? `Paiement 1/3 pour ${prospect.firstName} ${prospect.lastName} (total: ${totalAmountCents / 100}€)`
                                : `Prestation pour ${prospect.firstName} ${prospect.lastName}`,
                        },
                        unit_amount: unitAmountCents,
                    },
                    quantity: 1,
                },
            ],
            mode: 'payment',
            success_url: options.successUrl,
            cancel_url: options.cancelUrl,
            customer_email: prospect.email || undefined,
            client_reference_id: prospectId,
            metadata: {
                type: 'PROSPECT_CONVERSION',
                prospectId,
                serviceId: options.serviceId,
                serviceName,
                installments: String(installments),
                priceSource: resolved.source,
                totalPriceCents: String(totalAmountCents),
            },
        };

        // Paiement en 3x via Stripe (payment_intent_data)
        if (installments === 3) {
            sessionConfig.payment_intent_data = {
                metadata: {
                    type: 'PROSPECT_CONVERSION',
                    prospectId,
                    installments: '3',
                    installment_note: `Paiement 1/3 de ${unitAmountCents / 100}€ (total: ${totalAmountCents / 100}€)`,
                },
            };
        }

        const session = await this.stripe.checkout.sessions.create(sessionConfig);

        this.logger.log(
            `[💳 Prospect Checkout] ${prospect.firstName} ${prospect.lastName} — ${unitAmountCents / 100}€ ` +
            `(${installments}x) — Service: ${serviceName} — Source prix: ${resolved.source}`
        );

        return { url: session.url, sessionId: session.id, resolvedPrice: totalAmountCents / 100 };
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
            const metadata = session.metadata || {};

            // ─── PROSPECT PAYMENT : Auto-conversion ────────────
            if (metadata.type === 'PROSPECT_CONVERSION' && metadata.prospectId) {
                const prospectId = metadata.prospectId;
                this.logger.log(`[💳] Payment confirmed for Prospect ${prospectId} — auto-converting to SIGNED + Lead`);

                try {
                    // Auto-conversion : Prospect → SIGNED + Lead CRM (status PAID, requiredDocs injectés)
                    const result = await this.salesService.convertToLead(prospectId, metadata.serviceId);

                    if (result) {
                        // Enregistrer le paiement sur le Lead créé
                        await this.leadsService.recordPayment(result.leadId, {
                            amount: session.amount_total || 0,
                            method: 'STRIPE',
                            reference: session.payment_intent as string,
                        });

                        // Charger le Lead complet pour les notifications
                        const leadInfo = await this.leadsService.findOne(result.leadId);
                        const amountStr = session.amount_total ? session.amount_total / 100 : 0;
                        const serviceLabel = metadata.serviceName || leadInfo?.serviceName || 'Prestation SimuLegal';

                        // Trigger pipeline automations (relances, assignation automatique, etc.)
                        if (leadInfo) {
                            await this.pipelineAutomation.onPaymentReceived(leadInfo);
                        }

                        // ── Génération automatique de la facture PDF ──
                        let invoicePdfBuffer: Buffer | undefined;
                        let invoiceFilename: string | undefined;
                        try {
                            invoicePdfBuffer = await this.invoicePdfService.generateInvoicePdf(result.leadId);
                            const invoiceNum = leadInfo?.invoiceNumber || `FAC-${new Date().getFullYear()}-${result.leadId.substring(0, 6).toUpperCase()}`;
                            invoiceFilename = `facture-${invoiceNum}.pdf`;
                            this.logger.log(`[📄] Facture PDF générée : ${invoiceFilename} (${invoicePdfBuffer.length} bytes)`);
                        } catch (pdfErr: any) {
                            this.logger.error(`[❌] Échec génération facture PDF pour ${result.leadId}: ${pdfErr.message}`);
                        }

                        // ── Génération automatique de la checklist PDF ──
                        let checklistPdfBuffer: Buffer | undefined;
                        let checklistFilename: string | undefined;
                        try {
                            if (leadInfo?.requiredDocs && leadInfo.requiredDocs.length > 0) {
                                checklistPdfBuffer = await this.checklistPdfService.generateChecklistPdf(result.leadId);
                                checklistFilename = `checklist-documents-${result.leadId.substring(0, 6)}.pdf`;
                                this.logger.log(`[📋] Checklist PDF générée : ${checklistFilename} (${checklistPdfBuffer.length} bytes)`);
                            }
                        } catch (clErr: any) {
                            this.logger.error(`[❌] Échec génération checklist PDF pour ${result.leadId}: ${clErr.message}`);
                        }

                        // ── Envoi email de confirmation + facture PDF + checklist PDF + mandat ──
                        if (leadInfo?.email) {
                            try {
                                const clientSpaceUrlForEmail = this.leadsService.generateClientSpaceUrl(result.leadId);
                                await this.emailService.sendOrderConfirmation(
                                    leadInfo.email,
                                    leadInfo.name,
                                    serviceLabel,
                                    amountStr,
                                    session.id,
                                    leadInfo.requiredDocs,
                                    clientSpaceUrlForEmail,
                                    invoicePdfBuffer,
                                    invoiceFilename,
                                    checklistPdfBuffer,
                                    checklistFilename,
                                );
                                await this.emailService.sendMandateCopy(leadInfo.email, leadInfo.name);
                                this.logger.log(`[📧] Email confirmation + facture + checklist + mandat envoyé à ${leadInfo.email}`);
                            } catch (emailErr) {
                                this.logger.error(`[❌] Failed to send confirmation email: ${emailErr.message}`);
                            }
                        }

                        // ── Envoi WhatsApp checklist avec magic links + lien facture ──
                        const invoiceDownloadUrl = `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000'}/payments/${result.leadId}/invoice`;
                        if (leadInfo?.phone && leadInfo?.requiredDocs && leadInfo.requiredDocs.length > 0) {
                            try {
                                const uploadLinks = this.leadsService.generateDocumentUploadLinks(result.leadId, leadInfo.requiredDocs);
                                const clientSpaceUrl = this.leadsService.generateClientSpaceUrl(result.leadId);
                                const { message, buttons } = this.leadsService.buildWhatsAppChecklistMessage(serviceLabel, clientSpaceUrl, uploadLinks);

                                // Ajouter le bouton de téléchargement de la facture
                                buttons.push({ title: '📄 Télécharger ma facture', url: invoiceDownloadUrl });

                                await this.notificationsService.sendWhatsApp(
                                    leadInfo.phone,
                                    'order_checklist',
                                    { message },
                                    { leadId: result.leadId },
                                    buttons
                                );
                                this.logger.log(`[📲] WhatsApp checklist + lien facture envoyé au ${leadInfo.phone} (${uploadLinks.length} docs)`);
                            } catch (waErr) {
                                this.logger.error(`[❌] Failed to send WhatsApp checklist: ${waErr.message}`);
                            }
                        } else if (leadInfo?.phone) {
                            // Fallback : notification simple si pas de requiredDocs
                            await this.notificationsService.sendWhatsApp(
                                leadInfo.phone,
                                'payment_confirmation',
                                {
                                    name: leadInfo.name,
                                    message: `✅ ${leadInfo.name}, votre paiement de ${amountStr}€ a été confirmé ! ` +
                                        `Votre dossier ${serviceLabel} est maintenant ouvert. Nous vous recontacterons sous 24h.\n\n` +
                                        `📄 Téléchargez votre facture : ${invoiceDownloadUrl}`,
                                },
                            );
                        }

                        this.logger.log(`[✅] Prospect ${prospectId} → Lead ${result.leadId} (auto-converted on payment)`);
                    }
                } catch (err) {
                    this.logger.error(`[❌] Failed to auto-convert prospect ${prospectId}: ${err.message}`);
                }
                return;
            }

            // ─── LEAD PAYMENT (existing flow) ─────────────────
            const leadId = session.client_reference_id;

            if (leadId && metadata.type !== 'PROSPECT_CONVERSION') {
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
                    // 🤖 Déclencher l'Agent de Supervision (Veille Fraude Paiement)
                    this.eventEmitter.emit('lead.payment.received', {
                        leadId,
                        amount: session.amount_total,
                        sessionId: session.id,
                        customerEmail: session.customer_details?.email,
                        customerName: session.customer_details?.name
                    });
                }

                // Envoi des emails de facturation / de notification
                try {
                    const leadInfo = await this.leadsService.findOne(leadId);
                    if (leadInfo) {
                        const amountStr = session.amount_total ? session.amount_total / 100 : 0;
                        const serviceLabel = session.metadata?.label || leadInfo.serviceName;

                        // ── Génération automatique de la facture PDF ──
                        let invoicePdfBuffer: Buffer | undefined;
                        let invoiceFilename: string | undefined;
                        try {
                            invoicePdfBuffer = await this.invoicePdfService.generateInvoicePdf(leadId);
                            const invoiceNum = leadInfo.invoiceNumber || `FAC-${new Date().getFullYear()}-${leadId.substring(0, 6).toUpperCase()}`;
                            invoiceFilename = `facture-${invoiceNum}.pdf`;
                            this.logger.log(`[📄] Facture PDF générée : ${invoiceFilename} (${invoicePdfBuffer.length} bytes)`);
                        } catch (pdfErr: any) {
                            this.logger.error(`[❌] Échec génération facture PDF pour ${leadId}: ${pdfErr.message}`);
                        }

                        // ── Génération automatique de la checklist PDF ──
                        let checklistPdfBuffer: Buffer | undefined;
                        let checklistFilename: string | undefined;
                        try {
                            if (leadInfo.requiredDocs && leadInfo.requiredDocs.length > 0) {
                                checklistPdfBuffer = await this.checklistPdfService.generateChecklistPdf(leadId);
                                checklistFilename = `checklist-documents-${leadId.substring(0, 6)}.pdf`;
                                this.logger.log(`[📋] Checklist PDF générée : ${checklistFilename} (${checklistPdfBuffer.length} bytes)`);
                            }
                        } catch (clErr: any) {
                            this.logger.error(`[❌] Échec génération checklist PDF pour ${leadId}: ${clErr.message}`);
                        }

                        if (leadInfo.email) {
                            const clientSpaceUrlForEmail = this.leadsService.generateClientSpaceUrl(leadInfo.id);
                            await this.emailService.sendOrderConfirmation(
                                leadInfo.email,
                                leadInfo.name,
                                serviceLabel,
                                amountStr,
                                session.id,
                                leadInfo.requiredDocs,
                                clientSpaceUrlForEmail,
                                invoicePdfBuffer,
                                invoiceFilename,
                                checklistPdfBuffer,
                                checklistFilename,
                            );
                            await this.emailService.sendMandateCopy(leadInfo.email, leadInfo.name);
                            this.logger.log(`[📧] Email confirmation + facture + checklist + mandat envoyé à ${leadInfo.email}`);
                        }

                        // Envoi de la checklist WhatsApp avec Boutons Interactifs + lien facture
                        const invoiceDownloadUrl = `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000'}/payments/${leadInfo.id}/invoice`;
                        if (leadInfo.phone && leadInfo.requiredDocs && leadInfo.requiredDocs.length > 0) {
                            const uploadLinks = this.leadsService.generateDocumentUploadLinks(leadInfo.id, leadInfo.requiredDocs);
                            const clientSpaceUrl = this.leadsService.generateClientSpaceUrl(leadInfo.id);
                            const { message, buttons } = this.leadsService.buildWhatsAppChecklistMessage(serviceLabel, clientSpaceUrl, uploadLinks);

                            // Ajouter le bouton de téléchargement de la facture
                            buttons.push({ title: '📄 Télécharger ma facture', url: invoiceDownloadUrl });

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
