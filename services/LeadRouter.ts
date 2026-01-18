/**
 * LeadRouter Service
 * Décide de l'assignation initiale des dossiers (Agence vs Siège)
 */

export const LeadRouter = {
    /**
     * Détermine l'agence d'origine (ou le Siège) pour un nouveau lead
     * @param serviceId ID du service demandé
     * @param zipCode Code postal saisi par l'utilisateur (optionnel)
     * @param partnerId ID du partenaire si borne/kiosk (optionnel)
     */
    getOriginAgency: (
        serviceId: string,
        zipCode?: string,
        partnerId?: string | null
    ): string | null => {

        // 1. SERVICES CENTRALISÉS (Flux Call Center / Secrétariat Siège)
        // Ces flux arrivent tous au Siège pour qualification
        if (['rappel_echeances', 'contact_simple', 'rdv_juriste'].includes(serviceId)) {
            console.log(`[LeadRouter] 🏢 Routage vers Siège (HQ) pour le service: ${serviceId}`);
            return 'HQ'; // ou null selon la convention de la DB
        }

        // 2. MODE BORNE / KIOSK
        // Si on a un partnerId, le dossier appartient à la borne propriétaire
        if (partnerId) {
            console.log(`[LeadRouter] 🤖 Routage vers Borne/Partenaire: ${partnerId}`);
            return partnerId;
        }

        // 3. ROUTAGE GÉOGRAPHIQUE (Agences Physiques)
        // Logique par défaut : on tente de matcher le CP avec une agence
        if (zipCode) {
            const agencyId = LeadRouter.findAgencyByZipCode(zipCode);
            if (agencyId) {
                console.log(`[LeadRouter] 📍 Routage vers Agence Locale: ${agencyId} (CP: ${zipCode})`);
                return agencyId;
            }
        }

        // Par défaut, si rien ne correspond -> Siège
        return 'HQ';
    },

    /**
     * Logique simplifiée de matching par code postal
     * (En prod, cela interrogerait une base d'agences avec leurs zones de chalandise)
     */
    findAgencyByZipCode: (zipCode: string): string | null => {
        // Simulation de zones
        if (zipCode.startsWith('75')) return 'AGENCY-PARIS';
        if (zipCode.startsWith('69')) return 'AGENCY-LYON';
        if (zipCode.startsWith('13')) return 'AGENCY-MARSEILLE';
        if (zipCode.startsWith('33')) return 'AGENCY-BORDEAUX';

        return null; // Pas d'agence locale trouvée
    }
};

export default LeadRouter;
