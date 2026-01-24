import { AuthStore } from './authStore';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const BillingStore = {
    // Enregistrer un paiement pour un lead
    recordPayment: async (leadId: string, amount: number, method: string, reference?: string) => {
        try {
            const response = await fetch(`${API_URL}/leads/${leadId}/payment`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify({ amount, method, reference })
            });

            if (!response.ok) throw new Error('Erreur lors de l\'enregistrement du paiement');
            return response.json();
        } catch (error) {
            console.error('[Billing] ❌ Erreur recordPayment:', error);
            return null;
        }
    },

    // Récupérer les données de facturation
    getInvoiceData: async (leadId: string) => {
        try {
            const response = await fetch(`${API_URL}/leads/${leadId}/invoice`, {
                headers: getHeaders()
            });

            if (!response.ok) throw new Error('Facture non trouvée');
            return response.json();
        } catch (error) {
            console.error('[Billing] ❌ Erreur getInvoiceData:', error);
            return null;
        }
    },

    // Télécharger le PDF (simulé)
    downloadInvoicePdf: async (leadId: string) => {
        try {
            window.open(`${API_URL}/leads/${leadId}/invoice/pdf`, '_blank');
            return true;
        } catch (error) {
            console.error('[Billing] ❌ Erreur downloadInvoicePdf:', error);
            return false;
        }
    }
};
