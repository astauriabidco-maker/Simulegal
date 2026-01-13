export interface Lead {
    id: string;
    name: string;
    email: string;
    phone: string;
    serviceId: string;
    status: 'NEW' | 'PAID' | 'PROCESSING' | 'COMPLETED';
    contractSignedAt?: string;
    amountPaid?: number;
    metadata?: any; // Pour stocker les détails du simulateur (ex: éligibilité, points...)
    createdAt: string;
}

// Simulation d'une DB en mémoire (et localStorage pour persister)
export const CRM = {
    saveLead: (leadData: Omit<Lead, 'id' | 'createdAt' | 'status'>) => {
        if (typeof window === 'undefined') return null;

        const newLead: Lead = {
            ...leadData,
            id: Math.random().toString(36).substr(2, 9),
            status: 'PAID',
            createdAt: new Date().toISOString()
        };

        // Sauvegarde simulée
        const existing = JSON.parse(localStorage.getItem('crm_leads') || '[]');
        localStorage.setItem('crm_leads', JSON.stringify([...existing, newLead]));

        console.log(`[CRM] 🟢 Nouveau dossier créé : ${newLead.id} pour ${newLead.name}`);
        return newLead;
    },

    getAllLeads: (): Lead[] => {
        if (typeof window === 'undefined') return [];
        return JSON.parse(localStorage.getItem('crm_leads') || '[]');
    }
};
