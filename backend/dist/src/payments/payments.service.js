"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
var PaymentsService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.PaymentsService = void 0;
const common_1 = require("@nestjs/common");
const settings_service_1 = require("../settings/settings.service");
const stripe_1 = __importDefault(require("stripe"));
const leads_service_1 = require("../leads/leads.service");
let PaymentsService = PaymentsService_1 = class PaymentsService {
    settingsService;
    leadsService;
    logger = new common_1.Logger(PaymentsService_1.name);
    stripe;
    constructor(settingsService, leadsService) {
        this.settingsService = settingsService;
        this.leadsService = leadsService;
    }
    async onModuleInit() {
        const settings = await this.settingsService.getSettings();
        const stripeKey = settings.payment.secretKey;
        if (stripeKey && stripeKey !== 'sk_test_sample') {
            this.stripe = new stripe_1.default(stripeKey, {
                apiVersion: '2025-01-27.acacia',
            });
            this.logger.log('Stripe SDK initialized');
        }
        else {
            this.logger.warn('Stripe Secret Key is missing or default. Payments will be disabled.');
        }
    }
    async createCheckoutSession(leadId, successUrl, cancelUrl) {
        if (!this.stripe)
            throw new Error('Stripe is not configured');
        const lead = await this.leadsService.findOne(leadId);
        if (!lead)
            throw new Error('Lead not found');
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
                        unit_amount: lead.amountPaid || 10000,
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
    async handleWebhook(signature, payload) {
        if (!this.stripe)
            return;
        const settings = await this.settingsService.getSettings();
        const webhookSecret = settings.payment.webhookSecret;
        let event;
        try {
            event = this.stripe.webhooks.constructEvent(payload, signature, webhookSecret || '');
        }
        catch (err) {
            this.logger.error(`Webhook signature verification failed: ${err.message}`);
            throw new Error(`Webhook Error: ${err.message}`);
        }
        if (event.type === 'checkout.session.completed') {
            const session = event.data.object;
            const leadId = session.client_reference_id;
            if (leadId) {
                this.logger.log(`Payment confirmed for Lead ${leadId}`);
                await this.leadsService.recordPayment(leadId, {
                    amount: session.amount_total || 0,
                    method: 'STRIPE',
                    reference: session.payment_intent
                });
            }
        }
    }
};
exports.PaymentsService = PaymentsService;
exports.PaymentsService = PaymentsService = PaymentsService_1 = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [settings_service_1.SettingsService,
        leads_service_1.LeadsService])
], PaymentsService);
//# sourceMappingURL=payments.service.js.map