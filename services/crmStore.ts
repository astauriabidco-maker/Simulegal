import { DocumentRequirement } from '../config/DocumentTemplates';
import { WorkflowStage } from './WorkflowService';

// Types pour les notes internes
export interface LeadNote {
    id: string;
    author: 'HQ' | 'AGENCY';
    authorName: string;
    content: string;
    createdAt: string;
}

// Types pour la vérification des documents
export type DocumentStatus = 'EMPTY' | 'ANALYZING' | 'VALID' | 'REJECTED';

export interface LeadDocument {
    id: string;           // Type de doc (ex: PASSPORT)
    docType: string;
    fileName?: string;
    fileUrl?: string;
    uploadedAt?: string;
    status: DocumentStatus;
    aiConfidence?: number;
    rejectionReason?: string;
    validatedBy?: 'AI' | 'HUMAN';
    validatedAt?: string;
}

export type PaymentStatus = 'PAID' | 'REFUNDED';

export interface Lead {
    id: string;

    // Identité
    name: string;
    email: string;
    phone: string;

    // Service & Business
    serviceId: string;       // ex: 'regroupement_familial'
    serviceName: string;
    status: string; // From backend LeadStatus

    // Billing
    amountPaid: number;
    paymentStatus: PaymentStatus;
    paymentMethod?: string;
    paymentDate?: string;
    paymentRef?: string;
    invoiceNumber?: string;

    // Logistique Réseau
    originAgencyId?: string; // ID de l'agence/borne (pour commissions)
    assignedUser?: string;   // Juriste du siège assigné

    // Opérations - Workflow
    currentStage?: WorkflowStage;  // L'étape actuelle du pipeline
    documents?: LeadDocument[];    // Documents avec statut de vérification

    // Legacy compatibility
    notes?: LeadNote[];
    requiredDocuments?: DocumentRequirement[];

    // Juridique (Preuve)
    contract?: {
        signedAt: string;
        ipAddress: string;    // IP de signature
        consentVersion: string; // ex: "v1.0"
        isSigned: boolean;
    };

    createdAt: string;
    updatedAt: string;
}



import { AuthStore } from './authStore';

const API_URL = 'http://localhost:3001/leads';
const AUTH_URL = 'http://localhost:3001/auth';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const CRM = {
    /**
     * Demo login - calls real backend endpoint for *@demo.fr emails
     * Returns lead info and stores JWT token
     */
    demoLogin: async (email: string): Promise<Lead | null> => {
        try {
            const response = await fetch(`${AUTH_URL}/demo-login`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ email })
            });

            if (!response.ok) {
                console.error('[CRM] ❌ Demo login failed');
                return null;
            }

            const data = await response.json();

            // Store JWT token for future API calls
            AuthStore.setToken(data.access_token);
            AuthStore.setUser({
                id: data.lead.id,
                email: data.lead.email,
                name: data.lead.name,
                role: 'CLIENT'
            });

            console.log(`[CRM] 🟢 Demo login successful for ${email}`);

            // Return lead with contract info (for compatibility)
            return {
                ...data.lead,
                createdAt: new Date().toISOString(),
                amountPaid: 0,
                phone: '',
                contract: {
                    signedAt: new Date().toISOString(),
                    ipAddress: '127.0.0.1',
                    consentVersion: 'v1.0',
                    isSigned: true
                }
            };
        } catch (error) {
            console.error('[CRM] ❌ Demo login error:', error);
            return null;
        }
    },

    saveLead: async (leadData: any) => {
        // ... (API call unchanged)
        try {
            const response = await fetch(API_URL, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(leadData)
            });
            if (!response.ok) throw new Error('Erreur sauvegarde lead');
            const data = await response.json();
            console.log(`[CRM] 🟢 NOUVEAU CLIENT SIGNÉ via API: ${data.id}`);
            return data;
        } catch (error) {
            console.error('[CRM] ❌ Erreur sauvegarde lead:', error);
            return null;
        }
    },

    getAllLeads: async (agencyId?: string): Promise<Lead[]> => {
        try {
            const url = agencyId ? `${API_URL}?agencyId=${agencyId}` : API_URL;
            const response = await fetch(url, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement leads');
            const data = await response.json();
            return data;
        } catch (error) {
            console.error('[CRM] ❌ Erreur chargement leads:', error);
            return [];
        }
    },

    getLeadById: async (leadId: string): Promise<Lead | null> => {
        try {
            const response = await fetch(`${API_URL}/${leadId}`, { headers: getHeaders() });
            if (!response.ok) return null;
            return response.json();
        } catch {
            return null;
        }
    },


    addNote: async (leadId: string, note: { content: string, author: string, authorName?: string }): Promise<Lead | null> => {
        if (leadId.startsWith('DEMO-')) return CRM.getLeadById(leadId);
        try {
            const response = await fetch(`${API_URL}/${leadId}/notes`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(note)
            });
            if (!response.ok) throw new Error('Erreur ajout note');
            return response.json();
        } catch (error) {
            console.error('[CRM] ❌ Erreur ajout note:', error);
            return null;
        }
    },

    updateLeadStatus: async (leadId: string, status: string): Promise<Lead | null> => {
        if (leadId.startsWith('DEMO-')) return CRM.getLeadById(leadId);
        try {
            const response = await fetch(`${API_URL}/${leadId}/status`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify({ status })
            });
            if (!response.ok) throw new Error('Erreur mise à jour statut');
            return response.json();
        } catch (error) {
            console.error('[CRM] ❌ Erreur mise à jour statut:', error);
            return null;
        }
    },

    getLeadByEmail: async (email: string): Promise<Lead | null> => {
        // Pour l'instant, on récupère tout et on filtre, ou on pourrait ajouter un endpoint backend
        const all = await CRM.getAllLeads();
        return all.find(l => l.email.toLowerCase() === email.toLowerCase()) || null;
    },


    getAllLeadsByEmail: async (email: string): Promise<Lead[]> => {
        const all = await CRM.getAllLeads();
        return all.filter(l => l.email.toLowerCase() === email.toLowerCase());
    }

};
