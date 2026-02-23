
export type FranchiseLeadStatus = 'NEW' | 'CONTACTED' | 'MEETING' | 'VALIDATED' | 'DIP_SENT' | 'CONTRACT_SENT' | 'SIGNED' | 'REJECTED';

export interface FranchiseLead {
    id: string;
    name: string;
    email: string;
    phone: string;
    targetCity: string;
    region: string;
    companyName?: string;
    siret?: string;
    legalForm?: string;
    status: FranchiseLeadStatus;
    contractDetails: string; // JSON string
    contractHistory?: string; // JSON string
    documents?: string; // JSON string
    rejectionReason?: string;
    convertedAgencyId?: string;
    // Loi Doubin fields
    dipSentAt?: string;
    coolingPeriodRemaining?: number | null;
    entryFee?: number;
    royaltyRate?: number;
    advertisingFee?: number;
    contractDuration?: number;
    renewalTerms?: string;
    terminationNotice?: number;
    nonCompeteDuration?: number;
    exclusiveTerritory?: boolean;
    exclusiveRadius?: number;
    createdAt: string;
    updatedAt: string;
    notes?: Array<{
        id: string;
        content: string;
        type: string;
        author: string;
        createdAt: string;
    }>;
}

const API_URL = 'http://localhost:5000/franchise-leads';

import { AuthStore } from './authStore';

const getHeaders = () => {
    const token = AuthStore.getToken();
    const headers: HeadersInit = {
        'Content-Type': 'application/json'
    };
    if (token) {
        headers['Authorization'] = `Bearer ${token}`;
    }
    return headers;
};

export const FranchiseLeadStore = {
    getAll: async (): Promise<FranchiseLead[]> => {
        try {
            const response = await fetch(API_URL, {
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch franchise leads');
            return await response.json();
        } catch (error) {
            console.error('Error fetching franchise leads:', error);
            return [];
        }
    },

    getById: async (id: string): Promise<FranchiseLead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                headers: getHeaders()
            });
            if (!response.ok) return null;
            return await response.json();
        } catch (error) {
            console.error('Error fetching franchise lead:', error);
            return null;
        }
    },

    create: async (data: Partial<FranchiseLead>): Promise<FranchiseLead | null> => {
        try {
            // Note: Create endpoint is public for Landing Page, but we can send auth if available
            const response = await fetch(API_URL, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Failed to create franchise lead');
            return await response.json();
        } catch (error) {
            console.error('Error creating franchise lead:', error);
            return null;
        }
    },

    update: async (id: string, data: Partial<FranchiseLead>): Promise<FranchiseLead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Failed to update franchise lead');
            return await response.json();
        } catch (error) {
            console.error('Error updating franchise lead:', error);
            return null;
        }
    },

    updateDocuments: async (id: string, documents: any[]) => {
        return FranchiseLeadStore.update(id, { documents: JSON.stringify(documents) } as any);
    },

    logContractHistory: async (id: string, version: any, currentHistory: string = '[]') => {
        const history = JSON.parse(currentHistory || '[]');
        history.push({
            ...version,
            timestamp: new Date().toISOString()
        });
        return FranchiseLeadStore.update(id, { contractHistory: JSON.stringify(history) } as any);
    },

    addNote: async (id: string, note: { content: string, author: string, type: 'NOTE' | 'CALL' | 'EMAIL' }) => {
        try {
            const response = await fetch(`${API_URL}/${id}/notes`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(note)
            });
            if (!response.ok) throw new Error('Failed to add note');
            return await response.json();
        } catch (error) {
            console.error('Error adding note:', error);
            return null;
        }
    },

    signContract: async (id: string): Promise<any | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/sign`, {
                method: 'POST',
                headers: getHeaders()
            });
            if (!response.ok) {
                const err = await response.json().catch(() => ({}));
                throw new Error(err.message || 'Failed to sign contract');
            }
            return await response.json();
        } catch (error: any) {
            console.error('Error signing contract:', error);
            alert(error.message || 'Erreur lors de la signature');
            return null;
        }
    },

    sendDIP: async (id: string): Promise<FranchiseLead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/dip/send`, {
                method: 'POST',
                headers: getHeaders()
            });
            if (!response.ok) {
                const err = await response.json().catch(() => ({}));
                throw new Error(err.message || 'Failed to send DIP');
            }
            return await response.json();
        } catch (error: any) {
            console.error('Error sending DIP:', error);
            alert(error.message || 'Erreur lors de l\'envoi du DIP');
            return null;
        }
    },

    downloadDIP: (id: string) => {
        const token = AuthStore.getToken();
        window.open(`${API_URL}/${id}/dip?token=${token}`, '_blank');
    },

    downloadOpeningKit: (id: string) => {
        const token = AuthStore.getToken();
        window.open(`${API_URL}/${id}/opening-kit?token=${token}`, '_blank');
    },

    validateSiret: async (siret: string): Promise<{ valid: boolean; name?: string; address?: string; error?: string } | null> => {
        try {
            const response = await fetch(`${API_URL}/siret/validate`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify({ siret })
            });
            if (!response.ok) throw new Error('Validation failed');
            return await response.json();
        } catch (error) {
            console.error('Error validating SIRET:', error);
            return null;
        }
    },

    getCoolingStatus: async (id: string): Promise<{ daysElapsed: number; daysRemaining: number; canProceed: boolean; expiresAt: string | null } | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/cooling-status`, {
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Failed to get cooling status');
            return await response.json();
        } catch (error) {
            console.error('Error fetching cooling status:', error);
            return null;
        }
    },

    updateStatus: async (id: string, status: FranchiseLeadStatus): Promise<FranchiseLead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify({ status })
            });
            if (!response.ok) throw new Error('Failed to update status');
            return await response.json();
        } catch (error) {
            console.error('Error updating status:', error);
            return null;
        }
    },

    getAnalytics: async (): Promise<{
        total: number;
        statusCounts: Record<string, number>;
        regionCounts: Record<string, number>;
        conversionRate: number;
        monthlyTrend: { month: string; count: number; signed: number }[];
    } | null> => {
        try {
            const response = await fetch(`${API_URL}/analytics/dashboard`, {
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch analytics');
            return await response.json();
        } catch (error) {
            console.error('Error fetching analytics:', error);
            return null;
        }
    },

    downloadCSV: () => {
        const token = AuthStore.getToken();
        window.open(`${API_URL}/export/csv?token=${token}`, '_blank');
    }
};
