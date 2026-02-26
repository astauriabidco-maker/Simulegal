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
    zipCodes: string[];
    region?: string;
}

const API_URL = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000') + '/agencies';

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
                method: 'POST', headers: getHeaders(), body: JSON.stringify(data)
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
                method: 'PATCH', headers: getHeaders(), body: JSON.stringify(data)
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
            const baseUrl = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000');
            const response = await fetch(`${baseUrl}/finance/balance?agencyId=${id}`, { headers: getHeaders() });
            const data = await response.json();
            return { totalCA: data.totalEarned || 0, totalCommission: data.totalEarned || 0, dealsCount: 0 };
        } catch {
            return { totalCA: 0, totalCommission: 0, dealsCount: 0 };
        }
    },

    checkTerritoryAvailability: async (zipCode: string): Promise<{ available: boolean, agencyId?: string, agencyName?: string }> => {
        try {
            const response = await fetch(`${API_URL}/check-availability/${zipCode}`, { headers: getHeaders() });
            return response.json();
        } catch {
            return { available: true };
        }
    },

    getPerformanceTrends: async (agencyId: string): Promise<any[]> => {
        try {
            const baseUrl = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000');
            const response = await fetch(`${baseUrl}/finance/performance-trends?agencyId=${agencyId}`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur');
            return response.json();
        } catch { return []; }
    },

    deleteAgency: async (id: string): Promise<boolean> => {
        try {
            const response = await fetch(`${API_URL}/${id}`, { method: 'DELETE', headers: getHeaders() });
            return response.ok;
        } catch { return false; }
    },

    // ── NEW: Network Analytics ──
    getNetworkAnalytics: async (): Promise<any | null> => {
        try {
            const response = await fetch(`${API_URL}/analytics/network`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Failed');
            return response.json();
        } catch { return null; }
    },

    // ── NEW: Map Data ──
    getMapData: async (): Promise<any[] | null> => {
        try {
            const response = await fetch(`${API_URL}/analytics/map`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Failed');
            return response.json();
        } catch { return null; }
    },

    // ── NEW: Agency Performance ──
    getAgencyPerformance: async (id: string): Promise<any | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/performance`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Failed');
            return response.json();
        } catch { return null; }
    },

    downloadCSV: () => {
        window.open(`${API_URL}/export/csv`, '_blank');
    }
};

