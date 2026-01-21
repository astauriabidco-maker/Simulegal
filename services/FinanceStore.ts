import { CRM, Lead } from './crmStore';
import { AgencyStore } from './AgencyStore';

export interface Payout {
    id: string;
    agencyId: string;
    amount: number;
    period: string; // ex: "2025-10"
    status: 'PENDING' | 'PAID';
    paidAt?: string;
    reference?: string; // N° virement
    createdAt: string;
}

export interface FinancialStats {
    totalGMV: number; // Volume total encaissé (Gross Merchandise Volume)
    totalPartnerDebt: number; // Dette Partenaire (Commissions en attente)
    totalCommissionsPaid: number; // Commissions déjà versées
    netRevenue: number; // GMV - Dette Partenaire - Commissions Versées
}

const PAYOUTS_KEY = 'finance_payouts';

import { AuthStore } from './authStore';

const API_URL = 'http://localhost:3001/finance';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const FinanceStore = {
    // Récupérer tous les versements
    getAllPayouts: async (): Promise<Payout[]> => {
        try {
            const response = await fetch(`${API_URL}/payouts`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement payouts');
            return response.json();
        } catch (error) {
            console.error('[Finance] ❌ Erreur API:', error);
            return [];
        }
    },

    // Statistiques Globales (Siège)
    getGlobalStats: async (): Promise<FinancialStats> => {
        try {
            const response = await fetch(`${API_URL}/stats`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement stats');
            return response.json();
        } catch (error) {
            console.error('[Finance] ❌ Erreur API stats:', error);
            return {
                totalGMV: 0,
                totalPartnerDebt: 0,
                totalCommissionsPaid: 0,
                netRevenue: 0
            };
        }
    },

    // Solde d'une agence spécifique
    getAgencyBalance: async (agencyId: string): Promise<number> => {
        try {
            const response = await fetch(`${API_URL}/balance?agencyId=${agencyId}`, {
                headers: getHeaders()
            });
            if (!response.ok) return 0;
            const data = await response.json();
            return data.balance || 0;
        } catch {
            return 0;
        }
    },

    // Créer un virement
    createPayout: async (agencyId: string, amount: number, period: string) => {
        try {
            const response = await fetch(`${API_URL}/payouts`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify({ agencyId, amount, period })
            });
            if (!response.ok) throw new Error('Erreur création payout');
            return response.json();
        } catch (error) {
            console.error('[Finance] ❌ Erreur API création payout:', error);
            return null;
        }
    },

    // Historique pour une agence
    getAgencyPayouts: async (agencyId: string): Promise<Payout[]> => {
        // Pour l'instant on filtre côté front car le back ne propose pas de filtre par agence dans getAllPayouts
        // mais on pourrait l'ajouter. Je vais utiliser l'API globale et filtrer.
        const all = await FinanceStore.getAllPayouts();
        return all.filter(p => p.agencyId === agencyId);
    },

    // Récupérer les règlements mensuels (breakdown par agence)
    getSettlements: async (month: string, year: string): Promise<any[]> => {
        try {
            const response = await fetch(`${API_URL}/settlements?month=${month}&year=${year}`, {
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Erreur chargement règlements');
            return response.json();
        } catch (error) {
            console.error('[Finance] ❌ Erreur API règlements:', error);
            return [];
        }
    }
};
