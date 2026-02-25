import { Controller, Post, Body, BadRequestException, Get } from '@nestjs/common';
import { SalesService } from './sales.service';

/**
 * Formulaire public de capture de leads
 * ⚠️ PAS de AuthGuard — accessible sans authentification
 *
 * Utilisé par :
 * - Le formulaire public sur le site (/contact, /landing)
 * - Les widgets embed sur des sites partenaires
 * - Les QR codes en agence
 */
@Controller('lead-capture')
export class LeadCaptureController {
    constructor(private readonly salesService: SalesService) { }

    /**
     * POST /lead-capture
     * Crée un nouveau prospect depuis un formulaire public
     *
     * Protections :
     * - Validation des champs obligatoires
     * - Détection de doublons (via SalesService)
     * - Honeypot anti-bot (champ hidden `website`)
     * - Rate limiting basique (à ajouter via middleware)
     */
    @Post()
    async capturePublicLead(@Body() data: {
        firstName: string;
        lastName: string;
        phone: string;
        email?: string;
        city?: string;
        zipCode?: string;
        interestServiceId?: string;
        source?: string;
        campaignName?: string;
        agencyId?: string;
        // Honeypot anti-spam (doit être vide)
        website?: string;
        // UTM tracking
        utmSource?: string;
        utmMedium?: string;
        utmCampaign?: string;
    }) {
        // ── Honeypot anti-bot ──
        if (data.website && data.website.trim().length > 0) {
            // Bot détecté — on retourne un faux succès
            return { success: true, message: 'Merci ! Nous vous contacterons rapidement.' };
        }

        // ── Validation ──
        if (!data.firstName || !data.lastName || !data.phone) {
            throw new BadRequestException('Nom, prénom et téléphone sont obligatoires.');
        }

        if (data.phone.replace(/\s/g, '').length < 8) {
            throw new BadRequestException('Numéro de téléphone invalide.');
        }

        if (data.email && !data.email.includes('@')) {
            throw new BadRequestException('Adresse email invalide.');
        }

        // ── Construction des données prospect ──
        const prospectData: any = {
            firstName: data.firstName.trim(),
            lastName: data.lastName.trim(),
            phone: data.phone.replace(/\s/g, ''),
            email: data.email?.trim() || undefined,
            city: data.city?.trim() || undefined,
            zipCode: data.zipCode?.trim() || undefined,
            interestServiceId: data.interestServiceId || undefined,
            source: data.source || 'WEBSITE',
            campaignName: data.campaignName || undefined,
            agencyId: data.agencyId || 'HQ-001',
        };

        // Enrichir la source avec les UTM
        if (data.utmSource) {
            prospectData.campaignName = [data.utmSource, data.utmMedium, data.utmCampaign]
                .filter(Boolean)
                .join(' / ');
        }

        try {
            const prospect = await this.salesService.create(prospectData);
            return {
                success: true,
                message: 'Merci ! Un conseiller vous contactera dans les plus brefs délais.',
                prospectId: prospect.id,
            };
        } catch (err: any) {
            // Doublon détecté — on retourne un message user-friendly
            if (err.status === 400 && err.response?.existingProspectId) {
                return {
                    success: true,
                    message: 'Votre demande a bien été enregistrée. Un conseiller vous contactera rapidement.',
                    alreadyExists: true,
                };
            }
            throw err;
        }
    }

    /**
     * GET /lead-capture/services
     * Retourne la liste des services disponibles pour le formulaire public
     */
    @Get('services')
    async getAvailableServices() {
        return [
            { id: 'nat_accomp', label: 'Accompagnement Nationalité', pole: 'PROCEDURES' },
            { id: 'sejour_accomp', label: 'Accompagnement Titre Séjour', pole: 'PROCEDURES' },
            { id: 'regroupement_familial', label: 'Regroupement Familial', pole: 'PROCEDURES' },
            { id: 'permis_conduire', label: 'Changement Permis Conduire', pole: 'EXPERTISE' },
            { id: 'rdv_juriste', label: 'Rendez-vous Juriste', pole: 'EXPERTISE' },
            { id: 'rdv_prefecture', label: 'Rendez-vous Préfecture', pole: 'EXPERTISE' },
            { id: 'langue_a2b1', label: 'Cours de langues A2/B1', pole: 'INTEGRATION' },
            { id: 'rappel_echeances', label: 'Être rappelé', pole: 'GENERAL' },
        ];
    }
}
