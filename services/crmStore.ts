import { DocumentRequirement } from '../config/DocumentTemplates';

// Types pour les notes internes
export interface LeadNote {
    id: string;
    author: 'HQ' | 'AGENCY';
    authorName: string;
    content: string;
    createdAt: string;
}

// Types pour la vérification des documents
export type DocumentVerificationStatus = 'PENDING' | 'AUTO_VALIDATED' | 'MANUAL_VALIDATED' | 'REJECTED';

export interface LeadDocument {
    id: string;
    docType: string;
    fileName: string;
    uploadedAt: string;
    verificationStatus: DocumentVerificationStatus;
    aiConfidence?: number;
    rejectionReason?: string;
    validatedBy?: 'AI' | 'HUMAN';
    validatedAt?: string;
}

export interface Lead {
    id: string;
    name: string;
    email: string;
    phone: string;
    serviceId: string;
    serviceName: string;
    status: 'NEW' | 'PAID' | 'PROCESSING' | 'SUBMITTED' | 'COMPLETED';
    amountPaid: number;
    createdAt: string;
    originAgencyId?: string; // Pour le mode Kiosk
    notes?: LeadNote[]; // Notes internes
    documents?: LeadDocument[]; // Documents uploadés avec vérification IA
    requiredDocuments?: DocumentRequirement[]; // Checklist figée pour ce dossier
    // Audit Trail Juridique
    contract: {
        signedAt: string;
        ipAddress: string; // Simulé
        consentVersion: string; // ex: "v1.0"
        isSigned: boolean;
    };
}



export const CRM = {
    saveLead: (leadData: Omit<Lead, 'id' | 'createdAt' | 'status'>) => {
        const newLead: Lead = {
            ...leadData,
            id: "CMD-" + Math.random().toString(36).substr(2, 9).toUpperCase(),
            status: 'PAID',
            createdAt: new Date().toISOString(),
            notes: []
        };

        // Sauvegarde persistante
        const existing = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        localStorage.setItem('crm_leads', JSON.stringify([newLead, ...existing]));

        console.log(`[CRM] 🟢 NOUVEAU CLIENT SIGNÉ & PAYÉ`);
        console.log(`ID: ${newLead.id}`);
        console.log(`Preuve Signature: ${newLead.contract.signedAt} (IP: ${newLead.contract.ipAddress})`);

        return newLead;
    },

    getAllLeads: (): Lead[] => {
        return JSON.parse(localStorage.getItem('crm_leads') || '[]');
    },

    getLeadById: (leadId: string): Lead | null => {
        const leads = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        return leads.find((l: Lead) => l.id === leadId) || null;
    },

    // Ajouter une note à un dossier
    addNote: (leadId: string, note: Omit<LeadNote, 'id' | 'createdAt'>): Lead | null => {
        const leads: Lead[] = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        const leadIndex = leads.findIndex(l => l.id === leadId);

        if (leadIndex === -1) return null;

        const newNote: LeadNote = {
            ...note,
            id: 'NOTE-' + Math.random().toString(36).substr(2, 6).toUpperCase(),
            createdAt: new Date().toISOString()
        };

        if (!leads[leadIndex].notes) {
            leads[leadIndex].notes = [];
        }
        leads[leadIndex].notes!.push(newNote);

        localStorage.setItem('crm_leads', JSON.stringify(leads));
        console.log(`[CRM] 📝 Note ajoutée au dossier ${leadId} par ${note.author}`);

        return leads[leadIndex];
    },

    // Mettre à jour le statut d'un dossier (HQ uniquement)
    updateLeadStatus: (leadId: string, newStatus: Lead['status']): Lead | null => {
        const leads: Lead[] = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        const leadIndex = leads.findIndex(l => l.id === leadId);

        if (leadIndex === -1) return null;

        leads[leadIndex].status = newStatus;
        localStorage.setItem('crm_leads', JSON.stringify(leads));
        console.log(`[CRM] 🔄 Statut du dossier ${leadId} mis à jour: ${newStatus}`);

        return leads[leadIndex];
    },

    // Recherche par email (pour Magic Link)
    getLeadByEmail: (email: string): Lead | null => {
        const leads: Lead[] = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        const normalizedEmail = email.toLowerCase().trim();
        return leads.find(l => l.email.toLowerCase().trim() === normalizedEmail) || null;
    },

    // Récupérer tous les leads d'un email (peut avoir plusieurs dossiers)
    getAllLeadsByEmail: (email: string): Lead[] => {
        const leads: Lead[] = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        const normalizedEmail = email.toLowerCase().trim();
        return leads.filter(l => l.email.toLowerCase().trim() === normalizedEmail);
    }
};
