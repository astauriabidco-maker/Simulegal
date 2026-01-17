import { Lead } from '../services/crmStore';
import { Agency, BackOfficeLead, MOCK_AGENCIES } from '../types/backoffice';

// Services de production qui vont toujours au HQ
const PRODUCTION_SERVICES = [
    'titre_sejour',
    'naturalisation',
    'regroupement_familial',
    'driving_exchange',
    'rdv_prefecture'
];

// Services commerciaux pour aiguillage agence
const COMMERCIAL_SERVICES = [
    'rappel_echeances',
    'rdv_juriste'
];

interface DispatchResult {
    lead: BackOfficeLead;
    assignedAgency: Agency;
    dispatchType: 'PRODUCTION_HQ' | 'COMMERCIAL_AGENCY' | 'COMMERCIAL_HQ_FALLBACK';
    reason: string;
}

/**
 * Moteur d'aiguillage des leads
 * - Production (titre_sejour, naturalisation...) → HQ toujours
 * - Commercial (rappel_echeances...) → Agence locale ou HQ si non couvert
 */
export function dispatchLead(
    lead: Lead,
    agencies: Agency[] = MOCK_AGENCIES,
    leadZipCode?: string
): DispatchResult {
    const hqAgency = agencies.find(a => a.type === 'HQ') || agencies[0];

    // === CAS 1: Service de PRODUCTION → Toujours au HQ ===
    if (PRODUCTION_SERVICES.includes(lead.serviceId)) {
        const backOfficeLead: BackOfficeLead = {
            ...lead,
            originAgencyId: hqAgency.id, // Par défaut HQ si pas d'origine spécifique
            currentStage: 'NEW',
            notes: []
        };

        return {
            lead: backOfficeLead,
            assignedAgency: hqAgency,
            dispatchType: 'PRODUCTION_HQ',
            reason: `Dossier de production "${lead.serviceId}" assigné au siège pour traitement juridique.`
        };
    }

    // === CAS 2: Service COMMERCIAL → Cherche agence locale ===
    if (COMMERCIAL_SERVICES.includes(lead.serviceId) && leadZipCode) {
        // Cherche une agence qui couvre le code postal
        const matchingAgency = agencies.find(agency =>
            agency.type !== 'HQ' && agency.zipCodes.includes(leadZipCode)
        );

        if (matchingAgency) {
            const backOfficeLead: BackOfficeLead = {
                ...lead,
                originAgencyId: matchingAgency.id,
                currentStage: 'NEW',
                notes: []
            };

            return {
                lead: backOfficeLead,
                assignedAgency: matchingAgency,
                dispatchType: 'COMMERCIAL_AGENCY',
                reason: `Lead commercial assigné à "${matchingAgency.name}" (couvre ${leadZipCode}).`
            };
        }
    }

    // === CAS 3: Fallback → HQ prend en charge ===
    const backOfficeLead: BackOfficeLead = {
        ...lead,
        originAgencyId: hqAgency.id,
        currentStage: 'NEW',
        notes: []
    };

    return {
        lead: backOfficeLead,
        assignedAgency: hqAgency,
        dispatchType: 'COMMERCIAL_HQ_FALLBACK',
        reason: `Aucune agence ne couvre la zone. Assigné au siège (HQ).`
    };
}

/**
 * Trouve l'agence par code postal
 */
export function findAgencyByZipCode(zipCode: string, agencies: Agency[] = MOCK_AGENCIES): Agency | null {
    return agencies.find(agency => agency.zipCodes.includes(zipCode)) || null;
}

/**
 * Vérifie si un service est de type production (traité par HQ)
 */
export function isProductionService(serviceId: string): boolean {
    return PRODUCTION_SERVICES.includes(serviceId);
}
