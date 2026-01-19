import { AuthStore } from './authStore';
import { LeadScoring } from './LeadScoring';
import { MarketingAutomation } from './MarketingAutomation';

export type ProspectSource = 'MANUAL' | 'CSV_IMPORT' | 'META_ADS' | 'GOOGLE_ADS' | 'TIKTOK_ADS' | 'PARTNER_API';
export type ProspectStatus = 'TO_CALL' | 'IN_DISCUSSION' | 'MEETING_BOOKED' | 'LINK_SENT' | 'CONVERTED' | 'LOST';

export interface ProspectNote {
    authorId: string;
    text: string;
    date: string;
}

export interface Prospect {
    id: string;
    firstName: string;
    lastName: string;
    phone: string;
    email?: string;

    // Marketing
    source: ProspectSource;
    campaignName?: string;
    interestServiceId?: string;
    score: number; // 0 à 100

    // Logistique
    agencyId: string;
    assignedToSalesId?: string;

    // Pipeline
    status: ProspectStatus;

    // Historique
    notes: ProspectNote[];
    createdAt: string;
    lastContactAt?: string;
}

// Mock database
let prospectsDB: Prospect[] = [
    {
        id: 'PROSPECT-001',
        firstName: 'Jean',
        lastName: 'Dupont',
        phone: '0612345678',
        email: 'jean.dupont@email.com',
        source: 'GOOGLE_ADS',
        campaignName: 'Search_Naturalisation_Paris',
        interestServiceId: 'naturalisation',
        score: 75,
        agencyId: 'HQ-001',
        status: 'TO_CALL',
        notes: [],
        createdAt: new Date(Date.now() - 86400000).toISOString() // Hier
    },
    {
        id: 'PROSPECT-002',
        firstName: 'Sarah',
        lastName: 'Connor',
        phone: '0799887766',
        email: 'sarah.connor@skynet.com',
        source: 'TIKTOK_ADS',
        campaignName: 'Video_Permis_Humour',
        interestServiceId: 'permis_conduire',
        score: 40,
        agencyId: 'OWN-001',
        status: 'TO_CALL',
        notes: [],
        createdAt: new Date().toISOString()
    }
];

const API_URL = 'http://localhost:3001';

export const SalesStore = {
    /**
     * Helper to get common headers
     */
    getHeaders: () => {
        const token = AuthStore.getToken();
        return {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${token}`
        };
    },

    /**
     * Récupère les prospects
     */
    getProspects: async (): Promise<Prospect[]> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects`, {
                headers: SalesStore.getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch prospects');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching prospects:', error);
            return [];
        }
    },

    /**
     * Crée un nouveau prospect
     */
    addProspect: async (prospectData: Partial<Prospect>): Promise<Prospect> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(prospectData)
            });
            if (!response.ok) throw new Error('Failed to create prospect');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error adding prospect:', error);
            throw error;
        }
    },

    /**
     * Met à jour un prospect
     */
    updateProspect: async (id: string, updates: Partial<Prospect>): Promise<Prospect | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${id}`, {
                method: 'PATCH',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(updates)
            });
            if (!response.ok) throw new Error('Failed to update prospect');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error updating prospect:', error);
            return null;
        }
    },

    /**
     * Ajoute une note à un prospect
     */
    addNote: async (prospectId: string, text: string): Promise<ProspectNote | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/notes`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ text })
            });
            if (!response.ok) throw new Error('Failed to add note');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error adding note:', error);
            return null;
        }
    },

    /**
     * Simulation Round-Robin: Now handled on backend
     */
    assignLeadToSales: async (prospect: Prospect, agencyId: string): Promise<string> => {
        console.warn('[SalesStore] assignLeadToSales is deprecated on frontend. Use backend logic.');
        return '';
    },

    /**
     * Simulation Webhook Receiver: Now handled on backend
     */
    handleExternalLeadInjection: async (provider: string, payload: any, agencyId: string) => {
        console.warn('[SalesStore] handleExternalLeadInjection is deprecated on frontend. Use backend logic.');
        return null;
    },

    /**
     * CSV Import Parser (Mock still used for local parsing before sending to backend)
     */
    importProspectsFromCSV: async (file: File): Promise<number> => {
        // Simulation parsing local simple
        await new Promise(resolve => setTimeout(resolve, 500));

        // En prod, on lirait le CSV et on appellerait addProspect pour chaque ligne
        // Ici on mock l'envoi de 5 prospects
        for (let i = 0; i < 5; i++) {
            await SalesStore.addProspect({
                firstName: `Imported${i}`,
                lastName: `User`,
                phone: `060000000${i}`,
                source: 'CSV_IMPORT'
            });
        }
        return 5;
    }
};
