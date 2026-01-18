import { AgencyExt } from './AgencyStore';

export const LeadRouter = {
    /**
     * Détermine l'agence d'origine (ou le Siège) pour un nouveau lead
     * @param serviceId ID du service demandé
     * @param zipCode Code postal saisi par l'utilisateur (optionnel)
     * @param partnerId ID du partenaire si borne/kiosk (optionnel)
     * @param agencies Liste des agences pour le matching (optionnel)
     */
    getOriginAgency: (
        serviceId: string,
        zipCode?: string,
        partnerId?: string | null,
        agencies: AgencyExt[] = []
    ): string | null => {

        // 1. SERVICES CENTRALISÉS (Flux Call Center / Secrétariat Siège)
        if (['rappel_echeances', 'contact_simple', 'rdv_juriste'].includes(serviceId)) {
            console.log(`[LeadRouter] 🏢 Routage vers Siège (HQ) pour le service: ${serviceId}`);
            return 'HQ';
        }

        // 2. MODE BORNE / KIOSK
        if (partnerId) {
            console.log(`[LeadRouter] 🤖 Routage vers Borne/Partenaire: ${partnerId}`);
            return partnerId;
        }

        // 3. ROUTAGE GÉOGRAPHIQUE (Agences Physiques)
        if (zipCode && agencies.length > 0) {
            const agencyId = LeadRouter.findAgencyByZipCode(zipCode, agencies);
            if (agencyId) {
                console.log(`[LeadRouter] 📍 Routage vers Agence Locale: ${agencyId} (CP: ${zipCode})`);
                return agencyId;
            }
        }

        // Par défaut, si rien ne correspond -> Siège
        return 'HQ';
    },

    /**
     * Logique de matching par code postal sur les données réelles
     */
    findAgencyByZipCode: (zipCode: string, agencies: AgencyExt[]): string | null => {
        // On cherche une agence dont la liste des zipCodes (séparés par virgules) contient le zipCode
        const match = agencies.find(a => {
            if (!a.zipCodes) return false;
            const codes = a.zipCodes.split(',').map(c => c.trim());
            return codes.some(c => zipCode.startsWith(c));
        });

        return match ? match.id : null;
    }
};

export default LeadRouter;
