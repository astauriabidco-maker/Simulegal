/**
 * ═══════════════════════════════════════════════════════════════
 * META WHATSAPP CLOUD API — Service d'envoi
 * ═══════════════════════════════════════════════════════════════
 * 
 * Gère l'envoi de messages WhatsApp via l'API directe de Meta.
 * Plus simple et moins cher que Twilio (pas d'intermédiaire).
 * 
 * PRÉREQUIS :
 * - Un compte Meta Business vérifié
 * - Une app WhatsApp Business dans Meta Developer
 * - Un Phone Number ID (META_WHATSAPP_PHONE_ID)
 * - Un token d'accès permanent (META_WHATSAPP_TOKEN)
 * 
 * DOCS : https://developers.facebook.com/docs/whatsapp/cloud-api
 */

export interface MetaWhatsAppConfig {
    phoneNumberId: string;
    accessToken: string;
    apiVersion: string;
    businessAccountId?: string;
}

export interface MetaMessageResult {
    success: boolean;
    messageId?: string;
    error?: any;
}

/**
 * Service d'envoi via Meta WhatsApp Cloud API.
 * Simple, sans SDK — juste des appels REST fetch.
 */
export class MetaWhatsAppService {
    private readonly baseUrl: string;
    private readonly config: MetaWhatsAppConfig;

    constructor(config: MetaWhatsAppConfig) {
        this.config = config;
        this.baseUrl = `https://graph.facebook.com/${config.apiVersion || 'v21.0'}/${config.phoneNumberId}`;
    }

    /**
     * Vérifie que la configuration est valide
     */
    isConfigured(): boolean {
        return !!(this.config.phoneNumberId && this.config.accessToken);
    }

    /**
     * Envoie un message texte simple
     */
    async sendText(to: string, body: string): Promise<MetaMessageResult> {
        return this.sendRequest({
            messaging_product: 'whatsapp',
            recipient_type: 'individual',
            to: this.formatPhone(to),
            type: 'text',
            text: { body }
        });
    }

    /**
     * Envoie un message via un template approuvé
     * C'est le mode recommandé pour les notifications proactives (hors fenêtre 24h)
     * 
     * @param to - Numéro de téléphone
     * @param templateName - Nom du template (ex: 'simulegal_order_checklist')
     * @param language - Code langue (ex: 'fr')
     * @param bodyParameters - Variables du body [{type: 'text', text: 'Marie'}]
     * @param buttonParameters - Variables des boutons URL [{index: 0, sub_type: 'url', parameters: [{type: 'text', text: 'token123'}]}]
     */
    async sendTemplate(
        to: string,
        templateName: string,
        language: string = 'fr',
        bodyParameters?: { type: string; text: string }[],
        buttonParameters?: { index: number; sub_type: string; parameters: { type: string; text: string }[] }[]
    ): Promise<MetaMessageResult> {
        const components: any[] = [];

        if (bodyParameters && bodyParameters.length > 0) {
            components.push({
                type: 'body',
                parameters: bodyParameters
            });
        }

        if (buttonParameters && buttonParameters.length > 0) {
            buttonParameters.forEach(btn => {
                components.push({
                    type: 'button',
                    sub_type: btn.sub_type,
                    index: btn.index,
                    parameters: btn.parameters
                });
            });
        }

        const payload: any = {
            messaging_product: 'whatsapp',
            recipient_type: 'individual',
            to: this.formatPhone(to),
            type: 'template',
            template: {
                name: templateName,
                language: { code: language },
            }
        };

        if (components.length > 0) {
            payload.template.components = components;
        }

        return this.sendRequest(payload);
    }

    /**
     * Envoie un message interactif avec boutons CTA
     * Utilisé pour les réponses dans la fenêtre de 24h (pas besoin de template approuvé)
     */
    async sendInteractive(
        to: string,
        bodyText: string,
        buttons: { id: string; title: string; url?: string }[],
        headerText?: string,
        footerText?: string
    ): Promise<MetaMessageResult> {
        // Pour les boutons URL (CTA), utiliser le type 'cta_url'
        const ctaButtons = buttons.filter(b => b.url).map(b => ({
            type: 'cta_url',
            title: b.title.substring(0, 20), // WhatsApp limite à 20 chars
            url: b.url
        }));

        // Pour les boutons reply (sans URL)
        const replyButtons = buttons.filter(b => !b.url).map(b => ({
            type: 'reply',
            reply: {
                id: b.id,
                title: b.title.substring(0, 20)
            }
        }));

        const action: any = {};

        if (ctaButtons.length > 0) {
            action.buttons = ctaButtons;
        } else if (replyButtons.length > 0) {
            action.buttons = replyButtons;
        }

        const interactive: any = {
            type: ctaButtons.length > 0 ? 'cta_url' : 'button',
            body: { text: bodyText },
            action
        };

        if (headerText) {
            interactive.header = { type: 'text', text: headerText };
        }

        if (footerText) {
            interactive.footer = { text: footerText };
        }

        return this.sendRequest({
            messaging_product: 'whatsapp',
            recipient_type: 'individual',
            to: this.formatPhone(to),
            type: 'interactive',
            interactive
        });
    }

    /**
     * Vérifie la connexion en récupérant les infos du numéro
     */
    async testConnection(): Promise<{ success: boolean; message: string; phoneNumber?: string }> {
        try {
            const response = await fetch(this.baseUrl, {
                headers: {
                    'Authorization': `Bearer ${this.config.accessToken}`
                }
            });

            if (!response.ok) {
                const error = await response.json();
                return {
                    success: false,
                    message: `Erreur API (${response.status}): ${error.error?.message || 'Unknown'}`
                };
            }

            const data = await response.json();
            return {
                success: true,
                message: `Connecté ✅ — ${data.verified_name || data.display_phone_number || 'OK'}`,
                phoneNumber: data.display_phone_number
            };
        } catch (error: any) {
            return {
                success: false,
                message: `Erreur de connexion: ${error.message}`
            };
        }
    }

    /**
     * Liste les templates disponibles sur le compte Meta Business
     */
    async listTemplates(businessId?: string): Promise<{ success: boolean; templates?: any[]; error?: string }> {
        const wabaId = businessId || this.config.businessAccountId;
        if (!wabaId) {
            return { success: false, error: 'Business Account ID requis pour lister les templates' };
        }

        try {
            const url = `https://graph.facebook.com/${this.config.apiVersion || 'v21.0'}/${wabaId}/message_templates`;
            const response = await fetch(url, {
                headers: {
                    'Authorization': `Bearer ${this.config.accessToken}`
                }
            });

            if (!response.ok) {
                const error = await response.json();
                return { success: false, error: error.error?.message || 'Failed to list templates' };
            }

            const data = await response.json();
            return { success: true, templates: data.data || [] };
        } catch (error: any) {
            return { success: false, error: error.message };
        }
    }

    // ═══════════════════════════════════════════════════════════
    // PRIVATE METHODS
    // ═══════════════════════════════════════════════════════════

    /**
     * Envoie une requête à l'API Meta WhatsApp
     */
    private async sendRequest(payload: any): Promise<MetaMessageResult> {
        try {
            const url = `${this.baseUrl}/messages`;
            console.log(`[Meta WhatsApp] 📤 Sending to ${payload.to}...`);

            const response = await fetch(url, {
                method: 'POST',
                headers: {
                    'Authorization': `Bearer ${this.config.accessToken}`,
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(payload)
            });

            const data = await response.json();

            if (!response.ok) {
                console.error(`[Meta WhatsApp] ❌ Error (${response.status}):`, data.error?.message || data);
                return {
                    success: false,
                    error: data.error || data
                };
            }

            const messageId = data.messages?.[0]?.id || 'unknown';
            console.log(`[Meta WhatsApp] 🟢 Message sent: ${messageId}`);

            return {
                success: true,
                messageId
            };
        } catch (error: any) {
            console.error(`[Meta WhatsApp] ❌ Network error:`, error.message);
            return {
                success: false,
                error: error.message
            };
        }
    }

    /**
     * Formate un numéro de téléphone au format international (sans +)
     * Ex: 0612345678 → 33612345678
     * Ex: +33612345678 → 33612345678
     */
    private formatPhone(phone: string): string {
        let cleaned = phone.replace(/[\s\-\(\)]/g, '');

        // Retirer le + initial
        if (cleaned.startsWith('+')) {
            cleaned = cleaned.substring(1);
        }

        // Convertir les numéros français (0...) en international
        if (cleaned.startsWith('0')) {
            cleaned = '33' + cleaned.substring(1);
        }

        return cleaned;
    }
}
