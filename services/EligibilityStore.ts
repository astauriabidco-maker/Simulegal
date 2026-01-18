/**
 * Store pour la gestion dynamique des critères d'éligibilité
 * Centralise les règles JSON et les seuils de configuration
 */

import rulesSejour from '../specs/rules_sejour.json';
import rulesNaturalisation from '../specs/rules_naturalisation.json';
import rulesFamily from '../specs/rules_family.json';
import defaultThresholds from '../specs/config_thresholds.json';
import { ProcedureRule } from '../types';

const THRESHOLDS_KEY = 'v2_eligibility_thresholds';
const RULES_KEY_PREFIX = 'v2_eligibility_rules_';

export const EligibilityStore = {
    // ============================================
    // GESTION DES SEUILS (THRESHOLDS)
    // ============================================


    /**
     * Synchronise les configurations depuis le Backend
     */
    syncWithBackend: async () => {
        if (typeof window === 'undefined') return;
        const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001';

        try {
            // 1. Thresholds
            const tRes = await fetch(`${API_URL}/eligibility/thresholds`);
            if (tRes.ok) {
                const thresholds = await tRes.json();
                localStorage.setItem(THRESHOLDS_KEY, JSON.stringify(thresholds));
            }

            // 2. Rules (Example for 'naturalisation')
            const rRes = await fetch(`${API_URL}/eligibility/rules/naturalisation`);
            if (rRes.ok) {
                const rules = await rRes.json();
                if (rules && rules.length > 0) {
                    localStorage.setItem(RULES_KEY_PREFIX + 'naturalisation', JSON.stringify(rules));
                }
            }
            console.log('[ELIGIBILITY] ✅ Sync complete');
        } catch (err) {
            console.warn('[ELIGIBILITY] ⚠️ Backend sync failed, using defaults', err);
        }
    },

    /**
     * Récupère tous les seuils (SMIC, durées, salaires...)
     */
    getThresholds: () => {
        if (typeof window === 'undefined') return defaultThresholds;
        const saved = localStorage.getItem(THRESHOLDS_KEY);
        if (saved) {
            try {
                return JSON.parse(saved);
            } catch {
                return defaultThresholds;
            }
        }
        // Trigger background sync if missing?
        // EligibilityStore.syncWithBackend(); 
        return defaultThresholds;
    },

    /**
     * Met à jour les seuils
     */
    updateThresholds: (newThresholds: any) => {
        localStorage.setItem(THRESHOLDS_KEY, JSON.stringify(newThresholds));
        // Force le re-calcul si nécessaire ou notifie
        console.log('[ELIGIBILITY] ✅ Seuils mis à jour');
    },

    /**
     * Réinitialise les seuils
     */
    resetThresholds: () => {
        localStorage.removeItem(THRESHOLDS_KEY);
        console.log('[ELIGIBILITY] 🔄 Seuils réinitialisés');
    },

    // ============================================
    // GESTION DES RÈGLES LOGIQUES (JSON)
    // ============================================

    /**
     * Récupère les règles d'une catégorie (sejour, naturalisation, family)
     */
    getRules: (category: 'sejour' | 'naturalisation' | 'family'): ProcedureRule[] => {
        if (typeof window === 'undefined') {
            if (category === 'sejour') return rulesSejour as ProcedureRule[];
            if (category === 'naturalisation') return rulesNaturalisation as ProcedureRule[];
            return rulesFamily as ProcedureRule[];
        }

        const saved = localStorage.getItem(RULES_KEY_PREFIX + category);
        if (saved) {
            try {
                return JSON.parse(saved);
            } catch {
                // Fallback
            }
        }

        if (category === 'sejour') return rulesSejour as ProcedureRule[];
        if (category === 'naturalisation') return rulesNaturalisation as ProcedureRule[];
        return rulesFamily as ProcedureRule[];
    },

    /**
     * Met à jour toute une catégorie de règles
     */
    updateRules: (category: 'sejour' | 'naturalisation' | 'family', newRules: ProcedureRule[]) => {
        localStorage.setItem(RULES_KEY_PREFIX + category, JSON.stringify(newRules));
        console.log(`[ELIGIBILITY] ✅ Règles "${category}" mises à jour`);
    },

    /**
     * Met à jour une règle spécifique
     */
    updateRule: (category: 'sejour' | 'naturalisation' | 'family', ruleId: string, updates: Partial<ProcedureRule>) => {
        const rules = EligibilityStore.getRules(category);
        const index = rules.findIndex(r => r.id === ruleId);
        if (index !== -1) {
            rules[index] = { ...rules[index], ...updates };
            EligibilityStore.updateRules(category, rules);
        }
    },

    /**
     * Réinitialise les règles d'une catégorie
     */
    resetRules: (category: 'sejour' | 'naturalisation' | 'family') => {
        localStorage.removeItem(RULES_KEY_PREFIX + category);
        console.log(`[ELIGIBILITY] 🔄 Règles "${category}" réinitialisées`);
    }
};

export default EligibilityStore;
