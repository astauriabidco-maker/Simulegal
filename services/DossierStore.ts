import { CRM, Lead, LeadDocument } from './crmStore';
import { AuthStore } from './authStore';
import { WorkflowStage } from './WorkflowService';

const API_URL = 'http://localhost:5000/leads';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const DossierStore = {
    /**
     * Récupère tous les dossiers (filtre par agence automatique via le backend)
     */
    getAll: async (agencyId?: string): Promise<Lead[]> => {
        const leads = await CRM.getAllLeads(agencyId);
        return leads.map(mapLead);
    },

    /**
     * Récupère un dossier par ID
     */
    getById: async (id: string): Promise<Lead | null> => {
        const lead = await CRM.getLeadById(id);
        return lead ? mapLead(lead) : null;
    },

    /**
     * Met à jour le statut (étape du workflow)
     */
    updateStatus: async (id: string, stage: WorkflowStage): Promise<Lead | null> => {
        const updated = await CRM.updateLeadStatus(id, stage);
        return updated ? mapLead(updated) : null;
    },

    /**
     * Assigne un juriste à un dossier
     */
    assignJurist: async (id: string, userId: string): Promise<Lead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/assign`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify({ userId })
            });
            if (!response.ok) throw new Error('Erreur assignation');
            const updated = await response.json();
            return mapLead(updated);
        } catch (error) {
            console.error('[DossierStore] Erreur assignation:', error);
            return null;
        }
    },

    /**
     * Met à jour les documents d'un dossier
     */
    updateDocuments: async (id: string, documents: LeadDocument[]): Promise<Lead | null> => {
        try {
            const response = await fetch(`${API_URL}/${id}/documents`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify({ documents })
            });
            if (!response.ok) throw new Error('Erreur validation documents');
            const updated = await response.json();
            return mapLead(updated);
        } catch (error) {
            console.error('[DossierStore] Erreur validation documents:', error);
            return null;
        }
    },

    /**
     * Ajoute une note au dossier
     */
    addNote: async (id: string, content: string): Promise<Lead | null> => {
        const currentUser = AuthStore.getCurrentUser();
        const updated = await CRM.addNote(id, {
            content,
            author: 'HQ', // Par défaut pour le staff
            authorName: currentUser?.name || 'Staff'
        });
        return updated ? mapLead(updated) : null;
    }
};

/**
 * Mappe un Lead du backend vers le modèle Frontend (gestion des types JSON et currentStage)
 */
function mapLead(lead: any): Lead {
    return {
        ...lead,
        currentStage: lead.status as WorkflowStage,
        documents: typeof lead.documents === 'string' ? JSON.parse(lead.documents) : (lead.documents || []),
        contract: typeof lead.contract === 'string' ? JSON.parse(lead.contract) : (lead.contract || {
            isSigned: false,
            signedAt: '',
            ipAddress: '',
            consentVersion: ''
        }),
    };
}
