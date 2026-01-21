
export type FranchiseLeadStatus = 'NEW' | 'CONTACTED' | 'MEETING' | 'CONTRACT_SENT' | 'SIGNED' | 'REJECTED';

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
    status: 'NEW' | 'CONTACTED' | 'MEETING' | 'VALIDATED' | 'CONTRACT_SENT' | 'SIGNED' | 'REJECTED';
    contractDetails: string; // JSON string
    contractHistory?: string; // JSON string
    documents?: string; // JSON string
    rejectionReason?: string;
    convertedAgencyId?: string;
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

const API_URL = 'http://localhost:3001/franchise-leads';

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
        return (FranchiseLeadStore as any).updateLead(id, { documents: JSON.stringify(documents) });
    },

    logContractHistory: async (id: string, version: any, currentHistory: string = '[]') => {
        const history = JSON.parse(currentHistory || '[]');
        history.push({
            ...version,
            timestamp: new Date().toISOString()
        });
        return (FranchiseLeadStore as any).updateLead(id, { contractHistory: JSON.stringify(history) });
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
            if (!response.ok) throw new Error('Failed to sign contract');
            return await response.json();
        } catch (error) {
            console.error('Error signing contract:', error);
            return null;
        }
    }
};
