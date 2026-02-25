import { AuthStore } from './authStore';
import { LeadScoring } from './LeadScoring';
import { MarketingAutomation } from './MarketingAutomation';

export type ProspectSource = 'MANUAL' | 'CSV_IMPORT' | 'META_ADS' | 'GOOGLE_ADS' | 'TIKTOK_ADS' | 'PARTNER_API' | 'WEBSITE' | 'WEBHOOK' | 'REFERRAL' | 'NETWORKING' | 'PHONE';
export type ProspectStatus = 'NEW' | 'CONTACTED' | 'QUALIFIED' | 'MEETING_BOOKED' | 'SIGNED' | 'NO_SHOW' | 'LOST';

export interface ProspectNote {
    id?: string;
    authorId: string;
    text: string;
    date?: string;
    createdAt?: string;
}

export interface AppointmentInfo {
    date: string;              // ISO date-heure du RDV
    agencyId: string;          // ID de l'agence
    agencyName: string;        // Nom lisible ("Agence Paris 15")
    serviceId?: string;        // Service à traiter lors du RDV
    confirmed?: boolean;       // Confirmé par le lead ?
    confirmationSentVia?: 'SMS' | 'WHATSAPP' | 'EMAIL';
}

export interface EligibilityResult {
    isEligible: boolean;
    matchedProcedures: string[];  // IDs des procédures éligibles
    evaluatedAt: string;          // Date d'évaluation
    evaluatedBy?: string;         // ID du commercial
}

export interface Prospect {
    id: string;
    firstName: string;
    lastName: string;
    phone: string;
    email?: string;

    // Adresse (qualification progressive)
    address?: string;
    city?: string;
    zipCode?: string;
    country?: string;

    // Marketing
    source: ProspectSource;
    campaignName?: string;
    interestServiceId?: string;
    score: number; // 0 à 100

    // Logistique
    agencyId: string;
    assignedToSalesId?: string;

    // Pipeline
    status: ProspectStatus;

    // RDV en agence
    appointment?: AppointmentInfo;

    // Résultat simulateur (rempli lors du RDV en agence)
    eligibilityResult?: EligibilityResult;

    // Suivi des tentatives d'appel
    callAttempts: number;       // Nombre total de tentatives d'appel
    noAnswerCount: number;      // Compteur spécifique "pas de réponse"
    callbackCount: number;      // Compteur de demandes "rappeler plus tard"
    callbackRequestedAt?: string; // Dernière date de demande de rappel (ISO)
    callbackScheduledAt?: string; // Date+heure du prochain rappel programmé
    lastCallOutcome?: string;   // Dernier résultat d'appel

    // Pipeline tracking
    noShowCount: number;        // Nombre de RDV non honorés
    qualifiedAt?: string;       // Date de qualification
    stageEnteredAt?: string;    // Date d'entrée dans l'étape actuelle
    lostReason?: string;        // Raison de la perte
    tags?: string;              // JSON array de tags

    // Historique
    notes: ProspectNote[];
    createdAt: string;
    lastContactAt?: string;
    convertedLeadId?: string;  // Lien vers le Lead CRM après signature
}

// Mock database
let prospectsDB: Prospect[] = [
    {
        id: 'PROSPECT-001',
        firstName: 'Jean',
        lastName: 'Dupont',
        phone: '0612345678',
        email: 'jean.dupont@email.com',
        source: 'GOOGLE_ADS',
        campaignName: 'Search_Naturalisation_Paris',
        interestServiceId: 'naturalisation',
        score: 75,
        agencyId: 'HQ-001',
        status: 'NEW',
        callAttempts: 0,
        noAnswerCount: 0,
        callbackCount: 0,
        noShowCount: 0,
        notes: [],
        createdAt: new Date(Date.now() - 86400000).toISOString() // Hier
    },
    {
        id: 'PROSPECT-002',
        firstName: 'Sarah',
        lastName: 'Connor',
        phone: '0799887766',
        email: 'sarah.connor@skynet.com',
        source: 'TIKTOK_ADS',
        campaignName: 'Video_Permis_Humour',
        interestServiceId: 'permis_conduire',
        score: 40,
        agencyId: 'OWN-001',
        status: 'NEW',
        callAttempts: 0,
        noAnswerCount: 0,
        callbackCount: 0,
        noShowCount: 0,
        notes: [],
        createdAt: new Date().toISOString()
    }
];

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

export const SalesStore = {
    /**
     * Helper to get common headers
     */
    getHeaders: () => {
        const token = AuthStore.getToken();
        return {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${token}`
        };
    },

    /**
     * Fetch all prospects (Paginated with filters + search)
     */
    getProspects: async (
        page = 1,
        limit = 50,
        filters?: {
            status?: string;
            agencyId?: string;
            source?: string;
            dateFrom?: string;
            dateTo?: string;
            search?: string;
            tags?: string;
        }
    ): Promise<{ data: Prospect[], meta: any }> => {
        try {
            const params = new URLSearchParams({
                page: page.toString(),
                limit: limit.toString()
            });
            if (filters?.status) params.append('status', filters.status);
            if (filters?.agencyId) params.append('agencyId', filters.agencyId);
            if (filters?.source) params.append('source', filters.source);
            if (filters?.dateFrom) params.append('dateFrom', filters.dateFrom);
            if (filters?.dateTo) params.append('dateTo', filters.dateTo);
            if (filters?.search) params.append('search', filters.search);
            if (filters?.tags) params.append('tags', filters.tags);

            const response = await fetch(`${API_URL}/sales/prospects?${params.toString()}`, {
                headers: SalesStore.getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch prospects');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching prospects:', error);
            return { data: [], meta: { total: 0, page: 1, limit: 50, totalPages: 0 } };
        }
    },

    /**
     * Export prospects as CSV download
     */
    exportProspects: async (filters?: { agencyId?: string; source?: string; dateFrom?: string; dateTo?: string }): Promise<void> => {
        try {
            const params = new URLSearchParams();
            if (filters?.agencyId) params.append('agencyId', filters.agencyId);
            if (filters?.source) params.append('source', filters.source);
            if (filters?.dateFrom) params.append('dateFrom', filters.dateFrom);
            if (filters?.dateTo) params.append('dateTo', filters.dateTo);

            const response = await fetch(`${API_URL}/sales/export?${params.toString()}`, {
                headers: SalesStore.getHeaders()
            });
            if (!response.ok) throw new Error('Failed to export prospects');

            const result = await response.json();

            // Create and trigger download
            const blob = new Blob([result.content], { type: 'text/csv;charset=utf-8;' });
            const url = URL.createObjectURL(blob);
            const link = document.createElement('a');
            link.href = url;
            link.download = result.filename;
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            URL.revokeObjectURL(url);
        } catch (error) {
            console.error('[SalesStore] Error exporting prospects:', error);
            alert('Erreur lors de l\'export');
        }
    },

    /**
     * Crée un nouveau prospect
     */
    addProspect: async (prospectData: Partial<Prospect>): Promise<Prospect> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(prospectData)
            });
            if (!response.ok) throw new Error('Failed to create prospect');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error adding prospect:', error);
            throw error;
        }
    },

    /**
     * Met à jour un prospect
     */
    updateProspect: async (id: string, updates: Partial<Prospect>): Promise<Prospect | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${id}`, {
                method: 'PATCH',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(updates)
            });
            if (!response.ok) throw new Error('Failed to update prospect');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error updating prospect:', error);
            return null;
        }
    },

    /**
     * Fetch Analytics Stats
     */
    getAnalytics: async (period: 'TODAY' | 'WEEK' | 'MONTH'): Promise<any> => {
        try {
            const response = await fetch(`${API_URL}/sales/analytics?period=${period}`, {
                headers: SalesStore.getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch analytics');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching analytics:', error);
            // Return safe default
            return {
                kpis: { totalLeads: 0, newLeads: 0, convertedLeads: 0, conversionRate: 0, pipelineValue: 0 },
                funnel: [],
                sources: []
            };
        }
    },

    /**
     * Ajoute une note à un prospect
     */
    addNote: async (prospectId: string, text: string): Promise<ProspectNote | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/notes`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ text })
            });
            if (!response.ok) throw new Error('Failed to add note');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error adding note:', error);
            return null;
        }
    },

    /**
     * Simulation Round-Robin: Now handled on backend
     */
    assignLeadToSales: async (prospect: Prospect, agencyId: string): Promise<string> => {
        console.warn('[SalesStore] assignLeadToSales is deprecated on frontend. Use backend logic.');
        return '';
    },

    /**
     * Simulation Webhook Receiver: Now handled on backend
     */
    handleExternalLeadInjection: async (provider: string, payload: any, agencyId: string) => {
        console.warn('[SalesStore] handleExternalLeadInjection is deprecated on frontend. Use backend logic.');
        return null;
    },

    /**
     * CSV Import - Real Upload
     */
    importProspectsFromCSV: async (file: File): Promise<number> => {
        const formData = new FormData();
        formData.append('file', file);

        try {
            const response = await fetch(`${API_URL}/sales/import`, {
                method: 'POST',
                // Note: Do NOT set Content-Type header manually when using FormData, 
                // the browser sets it with the boundary automatically.
                headers: {
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
                body: formData
            });

            if (!response.ok) {
                const err = await response.text();
                throw new Error('Import failed: ' + err);
            }

            const data = await response.json();
            return data.count;
        } catch (error) {
            console.error('[SalesStore] Import error:', error);
            throw error;
        }
    },

    // ============================================
    // CALL HISTORY METHODS
    // ============================================

    /**
     * Start a new call log entry
     */
    startCall: async (prospectId: string, twilioCallSid?: string): Promise<any> => {
        try {
            const response = await fetch(`${API_URL}/sales/calls`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ prospectId, twilioCallSid })
            });
            if (!response.ok) throw new Error('Failed to start call log');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error starting call:', error);
            return null;
        }
    },

    /**
     * End a call and update its log
     */
    endCall: async (callId: string, duration: number, notes?: string, status: string = 'COMPLETED'): Promise<any> => {
        try {
            const response = await fetch(`${API_URL}/sales/calls/${callId}`, {
                method: 'PATCH',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ status, duration, notes })
            });
            if (!response.ok) throw new Error('Failed to end call log');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error ending call:', error);
            return null;
        }
    },

    /**
     * Get call history for a prospect
     */
    getCallHistory: async (prospectId: string): Promise<any[]> => {
        try {
            const response = await fetch(`${API_URL}/sales/calls/prospect/${prospectId}`, {
                headers: SalesStore.getHeaders()
            });
            if (!response.ok) throw new Error('Failed to fetch call history');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching call history:', error);
            return [];
        }
    },

    /**
     * Send simulation link to prospect via SMS or WhatsApp
     */
    sendSimulationLink: async (
        prospectId: string,
        prospectPhone: string,
        prospectFirstName: string,
        channel: 'SMS' | 'WHATSAPP' = 'SMS'
    ): Promise<{ success: boolean; messageId?: string; error?: string }> => {
        try {
            const response = await fetch(`${API_URL}/notifications/send-prospect-link`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({
                    prospectId,
                    prospectPhone,
                    prospectFirstName,
                    channel
                })
            });
            if (!response.ok) {
                const err = await response.text();
                throw new Error('Failed to send link: ' + err);
            }
            return await response.json();
        } catch (error: any) {
            console.error('[SalesStore] Error sending simulation link:', error);
            return { success: false, error: error.message };
        }
    },

    // ============================================
    // APPOINTMENT BOOKING
    // ============================================

    /**
     * Fixer un RDV en agence pour un prospect
     * Met à jour le statut en MEETING_BOOKED et stocke les infos RDV
     */
    bookAppointment: async (
        prospectId: string,
        appointment: AppointmentInfo,
        sendConfirmation: boolean = true
    ): Promise<Prospect | null> => {
        try {
            // Appeler le endpoint dédié qui crée un SalesAppointment + met à jour le prospect
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/book-appointment`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({
                    date: appointment.date,
                    agencyId: appointment.agencyId,
                    agencyName: appointment.agencyName,
                    serviceId: appointment.serviceId,
                    confirmed: sendConfirmation && appointment.confirmed,
                    confirmationSentVia: appointment.confirmationSentVia,
                })
            });

            if (!response.ok) {
                const errorData = await response.json().catch(() => ({}));
                throw new Error(errorData.message || `HTTP ${response.status}`);
            }

            const result = await response.json();

            if (!result.success) {
                throw new Error(result.error || 'Booking failed');
            }

            console.log(`[SalesStore] 📅 RDV créé pour ${prospectId} le ${appointment.date} à ${appointment.agencyName}`);
            return result.prospect;
        } catch (error) {
            console.error('[SalesStore] Error booking appointment:', error);
            return null;
        }
    },

    /**
     * Marquer un prospect comme non venu au RDV (NO_SHOW)
     */
    markNoShow: async (prospectId: string): Promise<Prospect | null> => {
        return SalesStore.updateProspect(prospectId, { status: 'NO_SHOW' } as any);
    },

    /**
     * Sauvegarder le résultat d'éligibilité après simulation en agence
     */
    saveEligibilityResult: async (
        prospectId: string,
        result: EligibilityResult
    ): Promise<Prospect | null> => {
        return SalesStore.updateProspect(prospectId, { eligibilityResult: result } as any);
    },

    /**
     * Convertir un prospect en Lead CRM (appel backend)
     * Retourne le leadId créé pour redirection
     */
    convertToLead: async (prospectId: string, serviceId?: string): Promise<{ leadId: string; success: boolean } | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/convert`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ serviceId }),
            });
            if (!response.ok) throw new Error('Conversion failed');
            const data = await response.json();
            if (data.success) {
                console.log(`[SalesStore] ✅ Prospect ${prospectId} converti en Lead ${data.leadId}`);
                return { leadId: data.leadId, success: true };
            }
            console.error('[SalesStore] ❌ Conversion échouée:', data.error);
            return null;
        } catch (error) {
            console.error('[SalesStore] ❌ Erreur conversion:', error);
            return null;
        }
    },

    /**
     * Réactiver un prospect perdu
     */
    reactivateProspect: async (id: string): Promise<Prospect | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${id}/reactivate`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
            });
            if (!response.ok) throw new Error('Failed to reactivate');
            const data = await response.json();
            return data.prospect || null;
        } catch (error) {
            console.error('[SalesStore] Error reactivating prospect:', error);
            return null;
        }
    },

    /**
     * Métriques de vélocité du pipeline
     */
    getPipelineVelocity: async (): Promise<any> => {
        try {
            const response = await fetch(`${API_URL}/sales/pipeline/velocity`, {
                headers: SalesStore.getHeaders(),
            });
            if (!response.ok) throw new Error('Failed to fetch velocity');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching velocity:', error);
            return null;
        }
    },

    // ═══════════════════════════════════════════════
    // SUPPRESSION
    // ═══════════════════════════════════════════════

    deleteProspect: async (id: string): Promise<boolean> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${id}`, {
                method: 'DELETE',
                headers: SalesStore.getHeaders(),
            });
            if (!response.ok) throw new Error('Failed to delete');
            const data = await response.json();
            return data.success;
        } catch (error) {
            console.error('[SalesStore] Error deleting prospect:', error);
            return false;
        }
    },

    // ═══════════════════════════════════════════════
    // ACTIONS EN MASSE
    // ═══════════════════════════════════════════════

    bulkUpdate: async (ids: string[], updates: { status?: string; assignedToSalesId?: string; tags?: string[] }): Promise<{ updated: number; failed: number }> => {
        try {
            const response = await fetch(`${API_URL}/sales/bulk/update`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ ids, updates }),
            });
            if (!response.ok) throw new Error('Bulk update failed');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error bulk update:', error);
            return { updated: 0, failed: ids.length };
        }
    },

    bulkDelete: async (ids: string[]): Promise<{ deleted: number }> => {
        try {
            const response = await fetch(`${API_URL}/sales/bulk/delete`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ ids }),
            });
            if (!response.ok) throw new Error('Bulk delete failed');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error bulk delete:', error);
            return { deleted: 0 };
        }
    },

    // ═══════════════════════════════════════════════
    // ANNULATION / REPROGRAMMATION RDV
    // ═══════════════════════════════════════════════

    cancelAppointment: async (prospectId: string, reason?: string): Promise<Prospect | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/cancel-appointment`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ reason }),
            });
            if (!response.ok) throw new Error('Cancel failed');
            const data = await response.json();
            return data.prospect || null;
        } catch (error) {
            console.error('[SalesStore] Error cancelling appointment:', error);
            return null;
        }
    },

    rescheduleAppointment: async (prospectId: string, appointmentData: { date: string; agencyId: string; agencyName: string; serviceId?: string }): Promise<any> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/reschedule-appointment`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(appointmentData),
            });
            if (!response.ok) throw new Error('Reschedule failed');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error rescheduling appointment:', error);
            return null;
        }
    },

    // ═══════════════════════════════════════════════
    // RELANCES PROGRAMMÉES
    // ═══════════════════════════════════════════════

    scheduleFollowUp: async (prospectId: string, scheduledAt: string, reason?: string): Promise<Prospect | null> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/schedule-followup`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({ scheduledAt, reason }),
            });
            if (!response.ok) throw new Error('Schedule failed');
            const data = await response.json();
            return data.prospect || null;
        } catch (error) {
            console.error('[SalesStore] Error scheduling followup:', error);
            return null;
        }
    },

    getDueFollowUps: async (agencyId?: string): Promise<Prospect[]> => {
        try {
            const params = agencyId ? `?agencyId=${agencyId}` : '';
            const response = await fetch(`${API_URL}/sales/followups/due${params}`, {
                headers: SalesStore.getHeaders(),
            });
            if (!response.ok) throw new Error('Failed to fetch');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching due followups:', error);
            return [];
        }
    },

    // ═══════════════════════════════════════════════
    // TIMELINE / HISTORIQUE COMMUNICATIONS
    // ═══════════════════════════════════════════════

    getTimeline: async (prospectId: string): Promise<any[]> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/timeline`, {
                headers: SalesStore.getHeaders(),
            });
            if (!response.ok) throw new Error('Failed to fetch timeline');
            return await response.json();
        } catch (error) {
            console.error('[SalesStore] Error fetching timeline:', error);
            return [];
        }
    },

    // ═══════════════════════════════════════════════
    // GÉNÉRATION DE DEVIS PDF
    // ═══════════════════════════════════════════════

    generateQuote: async (prospectId: string, options?: {
        serviceId?: string;
        serviceName?: string;
        priceEuros?: number;
        notes?: string;
    }): Promise<void> => {
        try {
            const response = await fetch(`${API_URL}/sales/prospects/${prospectId}/quote`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify(options || {}),
            });
            if (!response.ok) throw new Error('Failed to generate quote');

            const blob = await response.blob();
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `devis_${prospectId}.pdf`;
            document.body.appendChild(a);
            a.click();
            window.URL.revokeObjectURL(url);
            document.body.removeChild(a);
        } catch (error) {
            console.error('[SalesStore] Error generating quote:', error);
            throw error;
        }
    },
};
