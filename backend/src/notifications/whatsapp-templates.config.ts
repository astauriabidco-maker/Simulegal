/**
 * ═══════════════════════════════════════════════════════════════
 * WHATSAPP TEMPLATES — Configuration (Provider-agnostic)
 * ═══════════════════════════════════════════════════════════════
 * 
 * Ce fichier définit tous les templates WhatsApp utilisés pour
 * envoyer des messages interactifs (avec boutons CTA).
 * 
 * Supporte 2 providers :
 * - TWILIO  : Via Twilio Content Templates API
 * - META    : Via WhatsApp Cloud API (Meta Business)
 * 
 * Les templates sont définis de manière agnostique et mappés
 * vers le format spécifique de chaque provider au runtime.
 */

export type WhatsAppProvider = 'TWILIO' | 'META' | 'NONE';

export interface WhatsAppTemplate {
    /** Nom unique du template dans notre système */
    key: string;
    /** Nom affiché (utilisé pour l'enregistrement chez le provider) */
    friendlyName: string;
    /** Langue du template */
    language: string;
    /** Catégorie WhatsApp Business */
    category: 'UTILITY' | 'MARKETING' | 'AUTHENTICATION';
    /** Corps du message (variables: {{1}}, {{2}}, etc.) */
    body: string;
    /** Boutons CTA (max 3) */
    actions: {
        type: 'URL' | 'PHONE_NUMBER';
        title: string;
        /** URL statique ou avec suffix dynamique {{1}} */
        url?: string;
    }[];
    /** Clé de la variable d'environnement pour le contentSid / template ID */
    envKey: string;
}

// ═══════════════════════════════════════════════════════════════
// TEMPLATES (identiques pour Twilio et Meta)
// ═══════════════════════════════════════════════════════════════

export const WHATSAPP_TEMPLATES: WhatsAppTemplate[] = [
    // ─── Confirmation de paiement + checklist documents ─────────
    {
        key: 'order_checklist',
        friendlyName: 'simulegal_order_checklist',
        language: 'fr',
        category: 'UTILITY',
        body: '✅ *Paiement confirmé pour {{1}} !*\n\n' +
            'Bonjour, votre dossier a été enregistré avec succès.\n\n' +
            '📋 *Pièces à fournir :*\n{{2}}\n\n' +
            'Cliquez sur le bouton ci-dessous pour accéder à votre espace sécurisé.\n\n' +
            '🔒 _Liens sécurisés, valables 30 jours._',
        actions: [
            { type: 'URL', title: '📂 Mon espace client', url: 'https://simulegal.fr/client/{{1}}' },
            { type: 'URL', title: '📤 Déposer un document', url: 'https://simulegal.fr/upload/{{1}}' },
        ],
        envKey: 'TWILIO_CONTENT_ORDER_CHECKLIST',
    },

    // ─── Document refusé ────────────────────────────────────────
    {
        key: 'document_rejected',
        friendlyName: 'simulegal_document_rejected',
        language: 'fr',
        category: 'UTILITY',
        body: '⚠️ *Document refusé*\n\n' +
            'Bonjour {{1}},\n' +
            'Votre document « *{{2}}* » n\'a pas pu être validé.\n\n' +
            '💬 Motif : _{{3}}_\n\n' +
            'Merci de renvoyer ce document via le bouton ci-dessous.',
        actions: [
            { type: 'URL', title: '📤 Renvoyer ce document', url: 'https://simulegal.fr/upload/{{1}}' },
            { type: 'URL', title: '📂 Mon espace client', url: 'https://simulegal.fr/client/{{1}}' },
        ],
        envKey: 'TWILIO_CONTENT_DOC_REJECTED',
    },

    // ─── Document validé ────────────────────────────────────────
    {
        key: 'document_validated',
        friendlyName: 'simulegal_document_validated',
        language: 'fr',
        category: 'UTILITY',
        body: '✅ *Document validé*\n\n' +
            'Bonjour {{1}},\n' +
            'Votre document « *{{2}}* » a été validé par notre équipe.\n\n' +
            'Suivez l\'avancement de votre dossier dans votre espace client.',
        actions: [
            { type: 'URL', title: '📂 Voir mon dossier', url: 'https://simulegal.fr/client/{{1}}' },
        ],
        envKey: 'TWILIO_CONTENT_DOC_VALIDATED',
    },

    // ─── Dossier complet ────────────────────────────────────────
    {
        key: 'all_documents_validated',
        friendlyName: 'simulegal_dossier_complet',
        language: 'fr',
        category: 'UTILITY',
        body: '🎉 *Dossier complet !*\n\n' +
            'Bonjour {{1}},\n' +
            'Tous vos documents ont été vérifiés et validés. ' +
            'Votre dossier est maintenant *en cours de traitement* par notre équipe juridique.\n\n' +
            'Merci de votre confiance ! 🙏',
        actions: [
            { type: 'URL', title: '📂 Suivre mon dossier', url: 'https://simulegal.fr/client/{{1}}' },
        ],
        envKey: 'TWILIO_CONTENT_DOSSIER_COMPLET',
    },

    // ─── Juriste assigné ────────────────────────────────────────
    {
        key: 'jurist_assigned',
        friendlyName: 'simulegal_jurist_assigned',
        language: 'fr',
        category: 'UTILITY',
        body: '💼 *Juriste assigné*\n\n' +
            'Bonjour {{1}},\n' +
            'Votre dossier est maintenant pris en charge par *{{2}}*.\n\n' +
            'Suivez l\'avancement dans votre espace client.',
        actions: [
            { type: 'URL', title: '📂 Mon espace client', url: 'https://simulegal.fr/client/{{1}}' },
        ],
        envKey: 'TWILIO_CONTENT_JURIST_ASSIGNED',
    },
];

// Backward compatibility alias
export const TWILIO_TEMPLATES = WHATSAPP_TEMPLATES;
export type TwilioTemplate = WhatsAppTemplate;

// ═══════════════════════════════════════════════════════════════
// CACHE & RÉSOLUTION DES TEMPLATE IDs
// ═══════════════════════════════════════════════════════════════

/**
 * Cache mémoire des contentSid / template IDs chargés depuis la DB.
 */
let _dbContentSids: Record<string, string> = {};

/**
 * Met à jour le cache mémoire depuis la DB.
 */
export function setDbContentSids(sids: Record<string, string>) {
    _dbContentSids = sids;
}

/**
 * Retourne le contentSid / template ID d'un template.
 * Priorité : DB (via cache) > .env > null (mode fallback texte)
 */
export function getContentSid(templateKey: string): string | null {
    const template = WHATSAPP_TEMPLATES.find(t => t.key === templateKey);
    if (!template) return null;

    // 1. Chercher dans le cache DB
    if (_dbContentSids[templateKey]) {
        return _dbContentSids[templateKey];
    }

    // 2. Fallback .env
    const sid = process.env[template.envKey];
    if (!sid || sid === '' || sid === 'undefined') return null;

    return sid;
}

/**
 * Retourne tous les templates avec leur statut de configuration.
 */
export function getTemplatesStatus() {
    return WHATSAPP_TEMPLATES.map(t => {
        const sid = getContentSid(t.key);
        return {
            key: t.key,
            friendlyName: t.friendlyName,
            envKey: t.envKey,
            body: t.body,
            actions: t.actions,
            contentSid: sid,
            configured: !!sid
        };
    });
}
