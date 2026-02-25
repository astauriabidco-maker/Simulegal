import { Controller, Get, Patch, Body, UseGuards, Param, Query } from '@nestjs/common';
import { SettingsService } from './settings.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';
import { getTemplatesStatus, setDbContentSids } from '../notifications/whatsapp-templates.config';
import { NotificationsService } from '../notifications/notifications.service';
import {
    SERVICE_CATALOG, PIPELINE_TEMPLATES, DOCUMENT_CATALOG,
    getServiceConfig, getServicePipeline, getServiceRequiredDocs,
    getServicesByCategory
} from '../config/services-pipeline.config';

@Controller('settings')
export class SettingsController {
    constructor(
        private readonly settingsService: SettingsService,
        private readonly notificationsService: NotificationsService,
    ) { }

    @Get()
    async getSettings() {
        return this.settingsService.getSettings();
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch(':section')
    async updateSection(
        @Param('section') section: string,
        @Body() data: any
    ) {
        return this.settingsService.updateSection(section, data);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch('update/:section')
    async update(@Body() data: any, @Param('section') section: string) {
        return this.settingsService.updateSection(section, data);
    }

    // ═══════════════════════════════════════════════════
    // WHATSAPP TEMPLATES & PROVIDER
    // ═══════════════════════════════════════════════════

    /**
     * Retourne le provider WhatsApp actif et sa configuration
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Get('whatsapp-provider')
    async getWhatsAppProvider() {
        const provider = this.notificationsService.getWhatsAppProvider();
        return {
            provider,
            description: provider === 'META' ? 'WhatsApp Cloud API (Meta Direct)'
                : provider === 'TWILIO' ? 'Twilio (Proxy)'
                    : 'Non configuré — Messages en mode log uniquement',
            envVars: provider === 'META'
                ? ['META_WHATSAPP_PHONE_ID', 'META_WHATSAPP_TOKEN', 'META_WHATSAPP_BUSINESS_ID']
                : ['TWILIO_ACCOUNT_SID', 'TWILIO_AUTH_TOKEN', 'TWILIO_WHATSAPP_NUMBER']
        };
    }

    /**
     * Liste tous les Content Templates avec leur statut de configuration
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Get('twilio-templates')
    async getTwilioTemplates() {
        await this.settingsService.loadTwilioTemplatesFromDb();
        const provider = this.notificationsService.getWhatsAppProvider();
        return {
            provider,
            templates: getTemplatesStatus()
        };
    }

    /**
     * Met à jour les contentSid / template IDs (sauvegarde en DB)
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch('twilio-templates')
    async updateTwilioTemplates(@Body('templates') templates: Record<string, string>) {
        await this.settingsService.saveTwilioTemplates(templates);
        setDbContentSids(templates);
        const provider = this.notificationsService.getWhatsAppProvider();
        return {
            provider,
            templates: getTemplatesStatus()
        };
    }

    // ═══════════════════════════════════════════════════
    // CATALOGUE DES SERVICES & PIPELINES
    // ═══════════════════════════════════════════════════

    /**
     * GET /settings/services
     * Retourne le catalogue complet des services groupés par catégorie
     */
    @Get('services')
    async getServiceCatalog() {
        return {
            services: SERVICE_CATALOG,
            byCategory: getServicesByCategory(),
            totalServices: SERVICE_CATALOG.length,
        };
    }

    /**
     * GET /settings/services/:serviceId/pipeline
     * Retourne le pipeline + documents requis pour un service spécifique
     */
    @Get('services/:serviceId/pipeline')
    async getServicePipelineConfig(@Param('serviceId') serviceId: string) {
        const config = getServiceConfig(serviceId);
        if (!config) {
            return { error: `Service '${serviceId}' not found`, available: SERVICE_CATALOG.map(s => s.id) };
        }
        return {
            service: config,
            pipeline: getServicePipeline(serviceId),
            requiredDocs: getServiceRequiredDocs(serviceId),
        };
    }

    /**
     * GET /settings/pipeline-templates
     * Retourne tous les templates de pipeline disponibles
     */
    @Get('pipeline-templates')
    async getPipelineTemplates() {
        return {
            templates: PIPELINE_TEMPLATES,
            documentCatalog: DOCUMENT_CATALOG,
        };
    }

    // ═══════════════════════════════════════════════════
    // TARIFICATION DYNAMIQUE DES SERVICES
    // ═══════════════════════════════════════════════════

    /**
     * GET /settings/service-pricing
     * Retourne les prix (défauts du code + overrides DB)
     */
    @Get('service-pricing')
    async getServicePricing() {
        const overrides = await this.settingsService.getServicePricing();

        // Merge : prix par défaut du code + overrides DB
        const merged = SERVICE_CATALOG.map(service => {
            const override = overrides[service.id] || {};
            return {
                id: service.id,
                name: service.name,
                shortName: service.shortName,
                category: service.category,
                defaultPrice: service.basePrice || 0,
                currentPrice: override.price ?? service.basePrice ?? 0,
                promoPrice: override.promoPrice || null,
                promoUntil: override.promoUntil || null,
                notes: override.notes || '',
                isOverridden: !!override.price,
                estimatedDuration: service.estimatedDuration,
                updatedAt: override.updatedAt || null,
            };
        });

        return { services: merged, overrides };
    }

    /**
     * PATCH /settings/service-pricing
     * Met à jour le prix d'un ou plusieurs services
     * Body: { pricing: { serviceId: { price: number, promoPrice?: number, promoUntil?: string, notes?: string } } }
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch('service-pricing')
    async updateServicePricing(@Body('pricing') pricing: Record<string, any>) {
        const updated = await this.settingsService.updateServicePricing(pricing);
        return { success: true, overrides: updated };
    }

    /**
     * PATCH /settings/service-pricing/reset/:serviceId
     * Supprime l'override de prix d'un service (revient au prix par défaut)
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch('service-pricing/reset/:serviceId')
    async resetServicePrice(@Param('serviceId') serviceId: string) {
        const updated = await this.settingsService.resetServicePrice(serviceId);
        return { success: true, overrides: updated };
    }

    /**
     * GET /settings/service-pricing/history
     * Retourne l'historique des modifications de prix (audit trail)
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Get('service-pricing/history')
    async getServicePricingHistory(@Query('serviceId') serviceId?: string, @Query('limit') limit?: string) {
        const history = await this.settingsService.getServicePricingHistory();
        let filtered = history;
        if (serviceId) {
            filtered = history.filter((h: any) => h.serviceId === serviceId);
        }
        const max = parseInt(limit || '50', 10);
        return { history: filtered.slice(0, max), total: filtered.length };
    }

    // ═══════════════════════════════════════════════════
    // DOCUMENTS JURIDIQUES (CGV, Contrats, Mentions)
    // ═══════════════════════════════════════════════════

    /**
     * GET /settings/legal-documents
     * Retourne tous les documents juridiques (CGV, contrat, mentions)
     */
    @Get('legal-documents')
    async getLegalDocuments() {
        return this.settingsService.getLegalDocuments();
    }

    /**
     * PATCH /settings/legal-documents/:docType
     * Met à jour un document juridique (cgv, contrat_representation, mentions_legales)
     * Body: { content: string, version?: string }
     */
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'SUPERADMIN')
    @Patch('legal-documents/:docType')
    async updateLegalDocument(
        @Param('docType') docType: string,
        @Body() data: { content: string; version?: string }
    ) {
        const allowed = ['cgv', 'contrat_representation', 'mentions_legales'];
        if (!allowed.includes(docType)) {
            return { error: `Invalid document type. Allowed: ${allowed.join(', ')}` };
        }
        const updated = await this.settingsService.updateLegalDocument(docType, data);
        return { success: true, documents: updated };
    }

    /**
     * GET /settings/legal-documents/public/:docType
     * Endpoint PUBLIC — retourne le document pour le client (pas besoin d'auth)
     */
    @Get('legal-documents/public/:docType')
    async getPublicLegalDocument(@Param('docType') docType: string) {
        const doc = await this.settingsService.getPublicLegalDocument(docType);
        if (!doc) return { error: 'Document not found' };
        return doc;
    }
}
