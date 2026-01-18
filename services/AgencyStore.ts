import { AuthStore } from './authStore';

export type AgencyEntityType = 'OWNED' | 'FRANCHISE' | 'CORNER' | 'HQ';
export type AgencyStatus = 'ACTIVE' | 'INACTIVE';

export interface AgencyStats {
    totalCA: number;
    totalCommission: number;
    dealsCount: number;
}

export interface AgencyExt {
    id: string;
    name: string;
    type: AgencyEntityType;
    status: AgencyStatus;
    contactEmail: string;
    kioskUrl: string;
    commissionRate: number;
    zipCodes: string;
}

const API_URL = 'http://localhost:3001/agencies';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const AgencyStore = {
    getAllAgencies: async (): Promise<AgencyExt[]> => {
        try {
            const response = await fetch(API_URL, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement agences');
            return response.json();
        } catch (error) {
            console.error('[Network] ❌ Erreur API:', error);
            return [];
        }
    },

    addAgency: async (data: any) => {
        try {
            const response = await fetch(API_URL, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Erreur création agence');
            return response.json();
        } catch (error) {
            console.error('[Network] ❌ Erreur API création:', error);
            return null;
        }
    },

    updateAgency: async (id: string, data: any) => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Erreur mise à jour agence');
            return response.json();
        } catch (error) {
            console.error('[Network] ❌ Erreur API mise à jour:', error);
            return null;
        }
    },

    getAgencyStats: async (id: string): Promise<AgencyStats> => {
        try {
            // Pour l'instant on utilise l'API balance et on pourrait étendre l'API stats
            const response = await fetch(`http://localhost:3001/finance/balance?agencyId=${id}`, {
                headers: getHeaders()
            });
            const data = await response.json();
            return {
                totalCA: data.totalEarned || 0, // À ajuster selon le besoin exact du front
                totalCommission: data.totalEarned || 0,
                dealsCount: 0 // Nécessite une API stats plus complète
            };
        } catch {
            return { totalCA: 0, totalCommission: 0, dealsCount: 0 };
        }
    }
};
