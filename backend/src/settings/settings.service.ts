import { Injectable, OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { setDbContentSids } from '../notifications/whatsapp-templates.config';

@Injectable()
export class SettingsService implements OnModuleInit {
    constructor(private prisma: PrismaService) { }

    async onModuleInit() {
        // Ensure the settings row exists
        const settings = await this.prisma.systemSettings.findUnique({
            where: { id: 'GLOBAL' }
        });

        if (!settings) {
            await this.prisma.systemSettings.create({
                data: {
                    id: 'GLOBAL',
                    company: JSON.stringify({
                        name: "Simulegal SAS",
                        address: "123 Avenue de la Justice",
                        zipCode: "75000",
                        city: "Paris",
                        siret: "123 456 789 00012",
                        tvaNumber: "FR 12 123456789",
                        supportEmail: "support@simulegal.fr",
                        supportPhone: "+33 1 23 45 67 89"
                    }),
                    payment: JSON.stringify({
                        provider: 'STRIPE',
                        mode: 'TEST',
                        publicKey: 'pk_test_sample',
                        secretKey: 'sk_test_sample',
                        currency: 'EUR'
                    }),
                    notifications: JSON.stringify({
                        smtpHost: 'smtp.gmail.com',
                        smtpPort: 587,
                        smtpUser: 'notifications@simulegal.fr',
                        smtpPass: '******',
                        smsProvider: 'TWILIO',
                        smsSid: 'AC_sample',
                        smsToken: 'token_sample',
                        whatsappEnabled: false,
                        twilioTemplates: {}
                    }),
                    integrations: JSON.stringify({
                        ocrProvider: 'GOOGLE_VISION',
                        ocrApiKey: 'key_sample',
                        mapsApiKey: 'key_sample'
                    }),
                    storage: JSON.stringify({
                        provider: 'LOCAL',
                        bucketName: 'simulegal-docs',
                        region: 'eu-west-3',
                        accessKey: 'key_sample',
                        secretKey: 'secret_sample'
                    })
                }
            });
            console.log('[Settings] ⚙️ Global settings initialized in DB');
        }

        // Charger les Content Templates Twilio depuis la DB au démarrage
        await this.loadTwilioTemplatesFromDb();
    }

    async getSettings() {
        let settings = await this.prisma.systemSettings.findUnique({
            where: { id: 'GLOBAL' }
        });

        if (!settings) {
            await this.onModuleInit();
            settings = await this.prisma.systemSettings.findUnique({
                where: { id: 'GLOBAL' }
            });
        }

        if (!settings) {
            throw new Error('System settings not initialized');
        }

        return {
            company: JSON.parse(settings.company),
            payment: JSON.parse(settings.payment),
            notifications: JSON.parse(settings.notifications),
            integrations: JSON.parse(settings.integrations),
            storage: JSON.parse(settings.storage),
            updatedAt: settings.updatedAt
        };
    }

    async updateSection(section: string, data: any) {
        const current = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });

        const updateData: any = {};
        updateData[section] = JSON.stringify(data);

        return this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: updateData
        });
    }

    // ═══════════════════════════════════════════════════════
    // TWILIO CONTENT TEMPLATES — Persistance DB
    // ═══════════════════════════════════════════════════════

    /**
     * Charge les contentSid des templates depuis la DB et met à jour le cache mémoire.
     * Appelé au démarrage et lors de l'accès à l'interface d'admin.
     */
    async loadTwilioTemplatesFromDb() {
        try {
            const settings = await this.prisma.systemSettings.findUnique({
                where: { id: 'GLOBAL' }
            });

            if (settings) {
                const notif = JSON.parse(settings.notifications);
                const templates = notif.twilioTemplates || {};
                setDbContentSids(templates);
                const configured = Object.values(templates).filter(v => v).length;
                if (configured > 0) {
                    console.log(`[Settings] 📱 ${configured} Twilio Content Template(s) loaded from DB`);
                }
            }
        } catch (e) {
            console.warn('[Settings] Failed to load Twilio templates:', e.message);
        }
    }

    /**
     * Sauvegarde les contentSid des templates en DB (dans notifications.twilioTemplates).
     */
    async saveTwilioTemplates(templates: Record<string, string>) {
        const settings = await this.prisma.systemSettings.findUnique({
            where: { id: 'GLOBAL' }
        });

        if (!settings) throw new Error('Settings not initialized');

        const notif = JSON.parse(settings.notifications);
        notif.twilioTemplates = { ...(notif.twilioTemplates || {}), ...templates };

        await this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: { notifications: JSON.stringify(notif) }
        });

        // Mettre à jour le cache mémoire
        setDbContentSids(notif.twilioTemplates);

        console.log(`[Settings] 📱 Twilio templates saved:`, Object.keys(templates).join(', '));
    }

    // ═══════════════════════════════════════════════════════
    // TARIFS DES SERVICES (prix dynamiques)
    // ═══════════════════════════════════════════════════════

    /**
     * Retourne les prix dynamiques.
     * Format: { serviceId: { price: number, promoPrice?: number, promoUntil?: string, notes?: string } }
     */
    async getServicePricing(): Promise<Record<string, any>> {
        const settings = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });
        if (!settings) return {};
        try {
            return JSON.parse(settings.servicePricing || '{}');
        } catch {
            return {};
        }
    }

    /**
     * Met à jour le prix d'un ou plusieurs services.
     * Merge avec les prix existants + enregistre l'historique (audit trail).
     */
    async updateServicePricing(pricing: Record<string, any>): Promise<Record<string, any>> {
        const current = await this.getServicePricing();
        const history = await this.getServicePricingHistory();

        // Merge : on garde les anciens + on écrase avec les nouveaux
        const merged = { ...current };
        const now = new Date().toISOString();

        for (const [serviceId, data] of Object.entries(pricing)) {
            const prev = current[serviceId] || {};
            const changeTypes: string[] = [];

            // Détecter ce qui a changé
            if (data.price !== undefined && data.price !== prev.price) changeTypes.push('PRICE');
            if (data.promoPrice !== undefined && data.promoPrice !== prev.promoPrice) changeTypes.push('PROMO');
            if (data.promoUntil !== undefined && data.promoUntil !== prev.promoUntil) changeTypes.push('PROMO_DATE');
            if (data.notes !== undefined && data.notes !== prev.notes) changeTypes.push('NOTES');

            // Enregistrer dans l'historique si quelque chose a changé
            if (changeTypes.length > 0) {
                history.unshift({
                    serviceId,
                    timestamp: now,
                    changeTypes,
                    previousPrice: prev.price || null,
                    newPrice: data.price ?? prev.price ?? null,
                    previousPromoPrice: prev.promoPrice || null,
                    newPromoPrice: data.promoPrice ?? prev.promoPrice ?? null,
                    previousPromoUntil: prev.promoUntil || null,
                    newPromoUntil: data.promoUntil ?? prev.promoUntil ?? null,
                    notes: data.notes || prev.notes || '',
                });
            }

            merged[serviceId] = { ...prev, ...data, updatedAt: now };
        }

        // Limiter l'historique à 500 entrées
        const trimmedHistory = history.slice(0, 500);

        await this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: {
                servicePricing: JSON.stringify(merged),
                servicePricingHistory: JSON.stringify(trimmedHistory),
            }
        });

        console.log(`[Settings] 💰 Service pricing updated:`, Object.keys(pricing).join(', '));
        return merged;
    }

    /**
     * Retourne l'historique des modifications de prix (audit trail).
     */
    async getServicePricingHistory(): Promise<any[]> {
        const settings = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });
        if (!settings) return [];
        try {
            return JSON.parse((settings as any).servicePricingHistory || '[]');
        } catch {
            return [];
        }
    }

    /**
     * Supprime l'override de prix d'un service (revient au prix par défaut du code)
     * Enregistre l'action dans l'historique.
     */
    async resetServicePrice(serviceId: string): Promise<Record<string, any>> {
        const current = await this.getServicePricing();
        const history = await this.getServicePricingHistory();
        const prev = current[serviceId];

        if (prev) {
            history.unshift({
                serviceId,
                timestamp: new Date().toISOString(),
                changeTypes: ['RESET'],
                previousPrice: prev.price || null,
                newPrice: null,
                previousPromoPrice: prev.promoPrice || null,
                newPromoPrice: null,
                previousPromoUntil: prev.promoUntil || null,
                newPromoUntil: null,
                notes: 'Remise au prix par défaut',
            });
        }

        delete current[serviceId];

        await this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: {
                servicePricing: JSON.stringify(current),
                servicePricingHistory: JSON.stringify(history.slice(0, 500)),
            }
        });

        console.log(`[Settings] 💰 Service price reset to default:`, serviceId);
        return current;
    }

    // ═══════════════════════════════════════════════════════
    // DOCUMENTS JURIDIQUES (CGV, Contrats de représentation)
    // ═══════════════════════════════════════════════════════

    /**
     * Retourne tous les documents juridiques.
     * Format: { cgv: { content: string, version: string, updatedAt: string },
     *            contrat_representation: { content: string, version: string, updatedAt: string },
     *            mentions_legales: { ... } }
     */
    async getLegalDocuments(): Promise<Record<string, any>> {
        const settings = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });
        if (!settings) return this.getDefaultLegalDocuments();
        try {
            const docs = JSON.parse(settings.legalDocuments || '{}');
            // Merge avec les défauts pour les docs manquants
            return { ...this.getDefaultLegalDocuments(), ...docs };
        } catch {
            return this.getDefaultLegalDocuments();
        }
    }

    /**
     * Met à jour un document juridique spécifique
     */
    async updateLegalDocument(docType: string, data: { content: string; version?: string }): Promise<Record<string, any>> {
        const current = await this.getLegalDocuments();

        current[docType] = {
            content: data.content,
            version: data.version || this.generateVersion(current[docType]?.version),
            updatedAt: new Date().toISOString(),
            updatedBy: 'admin', // TODO: récupérer du JWT
        };

        await this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: { legalDocuments: JSON.stringify(current) }
        });

        console.log(`[Settings] 📜 Legal document '${docType}' updated (v${current[docType].version})`);
        return current;
    }

    /**
     * Retourne les CGV et contrats publiés côté client (endpoint public)
     */
    async getPublicLegalDocument(docType: string): Promise<{ content: string; version: string; updatedAt: string } | null> {
        const docs = await this.getLegalDocuments();
        return docs[docType] || null;
    }

    private getDefaultLegalDocuments(): Record<string, any> {
        return {
            cgv: {
                content: `# Conditions Générales de Vente — Simulegal

## Article 1 — Objet
Les présentes Conditions Générales de Vente (CGV) régissent les relations contractuelles entre **Simulegal SAS** et ses clients dans le cadre de ses prestations d'accompagnement juridique en droit des étrangers.

## Article 2 — Services proposés
Simulegal propose les services suivants :
- Accompagnement pour les demandes de titre de séjour
- Accompagnement pour les demandes de naturalisation
- Cours de français (niveaux A2 / B1)
- Formation civique (valeurs de la République)
- Rendez-vous préfecture (recherche de créneaux)
- Échange de permis de conduire étranger
- Consultations juridiques

## Article 3 — Tarification
Les tarifs sont indiqués en euros TTC. Simulegal se réserve le droit de modifier ses tarifs. Toute modification sera notifiée aux clients potentiels. Les tarifs applicables sont ceux en vigueur au moment de la commande.

## Article 4 — Modalités de paiement
Le paiement s'effectue en ligne par carte bancaire via notre plateforme sécurisée Stripe. Le paiement est exigible à la commande.

## Article 5 — Droit de rétractation
Conformément à l'article L.221-28 du Code de la consommation, le droit de rétractation ne s'applique pas aux services pleinement exécutés avant la fin du délai de rétractation.

## Article 6 — Obligations du client
Le client s'engage à fournir des documents authentiques et des informations exactes. Toute fausse déclaration entraîne la résiliation immédiate du contrat.

## Article 7 — Protection des données
Simulegal s'engage à traiter les données personnelles conformément au RGPD. Les données sont conservées le temps nécessaire au traitement du dossier.

## Article 8 — Litige
En cas de litige, les parties tenteront une résolution amiable. À défaut, les tribunaux de Paris seront compétents.

*Dernière mise à jour : ${new Date().toLocaleDateString('fr-FR')}*`,
                version: '1.0',
                updatedAt: new Date().toISOString(),
            },
            contrat_representation: {
                content: `# Contrat de Représentation — Simulegal

## Article 1 — Parties
**LE MANDANT** : Le client, ci-après désigné « le Client »

**LE MANDATAIRE** : Simulegal SAS, société par actions simplifiée, ci-après désigné « Simulegal »

## Article 2 — Objet du mandat
Le Client donne mandat à Simulegal pour le représenter et l'accompagner dans ses démarches administratives auprès des autorités compétentes (préfectures, sous-préfectures, OFII, ANTS) dans le cadre de la procédure suivante :

**[TYPE_PROCEDURE]** — **[NOM_SERVICE]**

## Article 3 — Étendue du mandat
Le mandataire est autorisé à :
- Constituer le dossier administratif au nom du Client
- Déposer le dossier auprès des autorités compétentes
- Suivre l'avancement de la procédure
- Réceptionner les correspondances administratives liées au dossier
- Prendre rendez-vous en préfecture au nom du Client

## Article 4 — Obligations de Simulegal
Simulegal s'engage à :
- Agir avec diligence au nom du Client
- Tenir le Client informé de l'avancement de son dossier
- Restituer tous les documents originaux au terme du mandat
- Respecter le secret professionnel

## Article 5 — Obligations du Client
Le Client s'engage à :
- Fournir des documents authentiques et à jour
- Répondre dans les meilleurs délais aux demandes d'information
- Signaler tout changement de situation (adresse, emploi, état civil)
- Régler les honoraires convenus

## Article 6 — Honoraires
Les honoraires sont fixés conformément à la grille tarifaire en vigueur. Ils couvrent l'ensemble des prestations décrites à l'article 3.

**Montant : [MONTANT]€ TTC**

## Article 7 — Durée
Le présent mandat prend effet à la date de signature et reste valable jusqu'à la décision finale de l'administration concernée, ou jusqu'à résiliation.

## Article 8 — Résiliation
Chaque partie peut résilier le mandat par lettre recommandée avec un préavis de 15 jours. Les honoraires restent acquis au prorata des prestations effectuées.

*Fait en deux exemplaires, le [DATE]*

| Le Client | Simulegal SAS |
|-----------|---------------|
| Signature : ________ | Signature : ________ |`,
                version: '1.0',
                updatedAt: new Date().toISOString(),
            },
            mentions_legales: {
                content: `# Mentions Légales — Simulegal

**Raison sociale** : Simulegal SAS
**Siège social** : 123 Avenue de la Justice, 75000 Paris
**SIRET** : 123 456 789 00012
**N° TVA** : FR 12 123456789
**Email** : support@simulegal.fr
**Téléphone** : +33 1 23 45 67 89

**Directeur de publication** : [NOM DIRECTEUR]
**Hébergeur** : [NOM HÉBERGEUR]

## Protection des données personnelles
Conformément au RGPD, vous disposez d'un droit d'accès, de rectification et de suppression de vos données. Contact : dpo@simulegal.fr`,
                version: '1.0',
                updatedAt: new Date().toISOString(),
            }
        };
    }

    private generateVersion(currentVersion?: string): string {
        if (!currentVersion) return '1.0';
        const parts = currentVersion.split('.');
        const minor = parseInt(parts[1] || '0', 10) + 1;
        return `${parts[0]}.${minor}`;
    }

    // ═══════════════════════════════════════════════════════
    // SURCHARGES DU CATALOGUE (enable/disable services, etc.)
    // ═══════════════════════════════════════════════════════

    async getCatalogOverrides(): Promise<Record<string, any>> {
        const settings = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });
        if (!settings) return {};
        try {
            return JSON.parse(settings.catalogOverrides || '{}');
        } catch {
            return {};
        }
    }

    async updateCatalogOverrides(overrides: Record<string, any>): Promise<Record<string, any>> {
        const current = await this.getCatalogOverrides();
        const merged = { ...current, ...overrides };

        await this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: { catalogOverrides: JSON.stringify(merged) }
        });

        console.log(`[Settings] ⚙️ Catalog overrides updated`);
        return merged;
    }
}

