import { Controller, Post, Body, Get, Param, Query, BadRequestException } from '@nestjs/common';
import { LeadsService } from './leads.service';
import { EmailService } from '../email/email.service';

@Controller('public/leads')
export class PublicLeadsController {
    constructor(
        private readonly leadsService: LeadsService,
        private readonly emailService: EmailService
    ) { }

    @Post()
    async create(@Body() leadData: any) {
        // Validation basique pour éviter le spam, on vérifie au moins un nom et un email/tel
        if (!leadData.name || (!leadData.email && !leadData.phone)) {
            throw new BadRequestException('Contact information missing');
        }

        // On force le statut NEW pour les leads publics
        const secureData = {
            ...leadData,
            status: 'NEW',
            originAgencyId: leadData.forceAgencyId || undefined,
            id: leadData.id || undefined // Permet de passer un ID généré par le front si besoin, sinon service en crée un
        };

        return this.leadsService.create(secureData);
    }

    @Post('checkout')
    async createCheckoutSession(@Body() body: { leadId: string, amount: number, label: string, successUrl: string, cancelUrl: string }) {
        const { leadId, amount, label, successUrl, cancelUrl } = body;

        // VERIFICATION: Le lead existe-t-il ?
        const lead = await this.leadsService.findOne(leadId);
        if (!lead) throw new BadRequestException('Lead not found');

        // MOCK STRIPE: Ici on retournera une URL de paiement fictive qui auto-confirme
        // Dans une vraie implémentation, on appellerait Stripe.checkout.sessions.create()

        // Simule un délai réseau
        await new Promise(resolve => setTimeout(resolve, 800));

        // URL de simulation de succès (pour test local sans webhook)
        // En prod, Stripe redirige vers successUrl, et appelle un webhook.
        // Ici, on triche : on redirige vers successUrl avec un paramètre session_id mockup
        const mockSessionId = `cs_test_${leadId}_${Date.now()}`;

        // SIMULATION ENVOI EMAIL TRANSACTIONNEL (Puisque paiement validé instantanément)
        if (lead.email) {
            // 1. Email de confirmation de commande + Espcae Client
            await this.emailService.sendOrderConfirmation(lead.email, lead.name, label, amount, mockSessionId);
            // 2. Email copie du mandat signé (simulé)
            await this.emailService.sendMandateCopy(lead.email, lead.name);
        } else {
            console.warn(`[Checkout] Lead ${leadId} has no email, skipping confirmation email.`);
        }

        return {
            url: `${successUrl}?session_id=${mockSessionId}&mock_payment=true`,
            sessionId: mockSessionId
        };
    }
}
