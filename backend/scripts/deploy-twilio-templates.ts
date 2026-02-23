#!/usr/bin/env node
/**
 * ═══════════════════════════════════════════════════════════════
 * SCRIPT DE DÉPLOIEMENT — Twilio Content Templates
 * ═══════════════════════════════════════════════════════════════
 * 
 * Usage:
 *   npx ts-node scripts/deploy-twilio-templates.ts
 *   ou
 *   npm run twilio:templates
 * 
 * Prérequis:
 *   - TWILIO_ACCOUNT_SID dans le .env
 *   - TWILIO_AUTH_TOKEN dans le .env
 * 
 * NOTE: Si vous utilisez Meta WhatsApp Cloud API (recommandé),
 * créez vos templates directement dans Meta Business Suite.
 * Ce script est uniquement pour le provider Twilio.
 */

import 'dotenv/config';
import { WHATSAPP_TEMPLATES } from '../src/notifications/whatsapp-templates.config';

const ACCOUNT_SID = process.env.TWILIO_ACCOUNT_SID;
const AUTH_TOKEN = process.env.TWILIO_AUTH_TOKEN;
const MESSAGING_SERVICE_SID = process.env.TWILIO_MESSAGING_SERVICE_SID;

if (!ACCOUNT_SID || !AUTH_TOKEN) {
    console.error('❌ TWILIO_ACCOUNT_SID et TWILIO_AUTH_TOKEN sont requis dans le .env');
    console.log('\n💡 Si vous utilisez Meta WhatsApp Cloud API, créez vos templates');
    console.log('   directement dans Meta Business Suite > WhatsApp > Message Templates');
    process.exit(1);
}

const BASE_URL = `https://content.twilio.com/v1/Content`;
const AUTH = Buffer.from(`${ACCOUNT_SID}:${AUTH_TOKEN}`).toString('base64');

async function createTemplate(template: typeof WHATSAPP_TEMPLATES[0]) {
    const payload = {
        friendly_name: template.friendlyName,
        language: template.language,
        variables: {},
        types: {
            'twilio/call-to-action': {
                body: template.body,
                actions: template.actions.map(action => {
                    if (action.type === 'URL') {
                        return { type: 'URL', title: action.title, url: action.url };
                    }
                    return action;
                })
            }
        }
    };

    try {
        const response = await fetch(BASE_URL, {
            method: 'POST',
            headers: {
                'Authorization': `Basic ${AUTH}`,
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(payload),
        });

        const data = await response.json();

        if (response.ok) {
            console.log(`✅ ${template.key}: Créé avec contentSid = ${data.sid}`);
            return { key: template.key, sid: data.sid, envKey: template.envKey };
        } else {
            console.error(`❌ ${template.key}: Erreur —`, data.message || JSON.stringify(data));
            return null;
        }
    } catch (e) {
        console.error(`❌ ${template.key}: Exception —`, e);
        return null;
    }
}

async function submitForApproval(contentSid: string, templateName: string) {
    const approvalUrl = `${BASE_URL}/${contentSid}/ApprovalRequests/whatsapp`;

    try {
        const payload: any = {
            name: templateName,
            category: 'UTILITY',
        };

        if (MESSAGING_SERVICE_SID) {
            payload.messaging_service_sid = MESSAGING_SERVICE_SID;
        }

        const response = await fetch(approvalUrl, {
            method: 'POST',
            headers: {
                'Authorization': `Basic ${AUTH}`,
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(payload),
        });

        if (response.ok) {
            console.log(`   📨 Soumis à l'approbation WhatsApp`);
        } else {
            const data = await response.json();
            console.warn(`   ⚠️ Approbation: ${data.message || 'erreur'}`);
        }
    } catch (e) {
        console.warn(`   ⚠️ Approbation: exception —`, e);
    }
}

async function main() {
    console.log('═══════════════════════════════════════════════════');
    console.log(' SIMULEGAL — Déploiement des Content Templates Twilio');
    console.log('═══════════════════════════════════════════════════\n');

    const results: { key: string; sid: string; envKey: string }[] = [];

    for (const template of WHATSAPP_TEMPLATES) {
        const result = await createTemplate(template);
        if (result) {
            results.push(result);
            await submitForApproval(result.sid, template.friendlyName);
        }
        console.log('');
    }

    console.log('\n═══════════════════════════════════════════════════');
    console.log(' AJOUTEZ CES LIGNES À VOTRE .env :');
    console.log('═══════════════════════════════════════════════════\n');

    for (const r of results) {
        console.log(`${r.envKey}=${r.sid}`);
    }

    console.log('\n═══════════════════════════════════════════════════');
    console.log(` ${results.length}/${WHATSAPP_TEMPLATES.length} templates créés.`);
    console.log(' Les templates seront actifs après approbation WhatsApp (24-48h).');
    console.log('═══════════════════════════════════════════════════\n');
}

main().catch(console.error);
