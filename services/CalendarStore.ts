import { CRM } from './crmStore';
import { AuthStore } from './authStore';
import { v4 as uuidv4 } from 'uuid';

export type AppointmentType = 'VISIO_JURISTE' | 'PHYSICAL_AGENCY';

export interface Appointment {
    id: string;
    leadId: string;
    leadName: string;
    type: AppointmentType;

    start: string; // ISO String
    end: string;   // ISO String

    agencyId?: string;    // Required if PHYSICAL
    hostUserId?: string;  // Required if VISIO (The jurist)

    status: 'SCHEDULED' | 'COMPLETED' | 'CANCELLED' | 'NO_SHOW';
    meetingLink?: string; // Google Meet link if Visio

    // Computed fields from backend
    dossierStatus?: 'COMPLETE' | 'PARTIAL' | 'INCOMPLETE' | 'EMPTY';
    missingDocsCount?: number;
    lead?: any;
    hostUser?: { name: string };
}

export interface Availability {
    // Default: 09:00 - 18:00 Mon-Fri
    days: number[]; // [1, 2, 3, 4, 5]
    hours: { start: string; end: string };
}

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000';

export const CalendarStore = {
    getAllAppointments: async (filters?: { agencyId?: string, start?: string, end?: string }): Promise<Appointment[]> => {
        const token = AuthStore.getToken();
        const params = new URLSearchParams();
        if (filters?.agencyId) params.append('agencyId', filters.agencyId);
        if (filters?.start) params.append('start', filters.start);
        if (filters?.end) params.append('end', filters.end);

        const res = await fetch(`${API_URL}/appointments?${params.toString()}`, {
            headers: { 'Authorization': `Bearer ${token}` }
        });

        if (!res.ok) return [];
        return res.json();
    },

    getAppointmentsByAgency: async (agencyId: string): Promise<Appointment[]> => {
        return CalendarStore.getAllAppointments({ agencyId });
    },

    getAppointmentsByLead: async (leadId: string): Promise<Appointment[]> => {
        const token = AuthStore.getToken();
        const res = await fetch(`${API_URL}/appointments?leadId=${leadId}`, {
            headers: { 'Authorization': `Bearer ${token}` }
        });
        if (!res.ok) return [];
        return res.json();
    },

    getAvailableSlots: async (dateStr: string, agencyId?: string, serviceId?: string): Promise<string[]> => {
        const params = new URLSearchParams({ date: dateStr });
        if (agencyId) params.append('agencyId', agencyId);
        if (serviceId) params.append('serviceId', serviceId);

        try {
            const res = await fetch(`${API_URL}/appointments/slots?${params.toString()}`);
            if (!res.ok) return [];
            return res.json();
        } catch (err) {
            console.error('Error fetching slots:', err);
            return [];
        }
    },

    bookAppointment: async (
        slotIso: string,
        lead: { id: string, name: string, email: string },
        type: AppointmentType,
        agencyId?: string,
        serviceId?: string,
        hostUserId?: string
    ): Promise<Appointment> => {

        const payload = {
            slotIso,
            lead,
            type,
            agencyId,
            serviceId,
            hostUserId
        };

        const res = await fetch(`${API_URL}/appointments/book`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        if (!res.ok) {
            throw new Error('Booking failed');
        }

        const appointment = await res.json();

        // Update CRM
        if (type === 'VISIO_JURISTE') {
            await CRM.updateLeadStatus(lead.id, 'BOOKED');
            await CRM.addNote(lead.id, {
                author: 'SYSTEM',
                content: `📅 RDV Visio (Expertise: ${serviceId || 'Standard'}) confirmé pour le ${new Date(slotIso).toLocaleDateString()} à ${new Date(slotIso).toLocaleTimeString()}`
            });
        } else {
            await CRM.addNote(lead.id, {
                author: 'SYSTEM',
                content: `📅 RDV Agence confirmed for le ${new Date(slotIso).toLocaleDateString()} à ${new Date(slotIso).toLocaleTimeString()}`
            });
        }

        return appointment;
    },

    getAllAbsences: async (filters?: { userId?: string, start?: string, end?: string }): Promise<Absence[]> => {
        const token = AuthStore.getToken();
        const params = new URLSearchParams();
        if (filters?.userId) params.append('userId', filters.userId);
        if (filters?.start) params.append('start', filters.start);
        if (filters?.end) params.append('end', filters.end);

        const res = await fetch(`${API_URL}/absences?${params.toString()}`, {
            headers: { 'Authorization': `Bearer ${token}` }
        });

        if (!res.ok) return [];
        return res.json();
    },

    createAbsence: async (data: { userId: string, start: string, end: string, reason?: string }): Promise<Absence> => {
        const token = AuthStore.getToken();
        const res = await fetch(`${API_URL}/absences`, {
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${token}`,
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });

        if (!res.ok) throw new Error('Failed to create absence');
        return res.json();
    },

    deleteAbsence: async (id: string): Promise<boolean> => {
        const token = AuthStore.getToken();
        const res = await fetch(`${API_URL}/absences/${id}`, {
            method: 'DELETE',
            headers: { 'Authorization': `Bearer ${token}` }
        });

        return res.ok;
    },

    updateAppointment: async (id: string, data: { start?: string; end?: string; hostUserId?: string; agencyId?: string; status?: string; type?: string }): Promise<Appointment> => {
        const token = AuthStore.getToken();
        const res = await fetch(`${API_URL}/appointments/${id}`, {
            method: 'PATCH',
            headers: {
                'Authorization': `Bearer ${token}`,
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });

        if (!res.ok) throw new Error('Failed to update appointment');
        return res.json();
    },

    getAgendaStats: async (start: string, end: string): Promise<any> => {
        const token = AuthStore.getToken();
        const params = new URLSearchParams({ start, end });
        const res = await fetch(`${API_URL}/appointments/stats?${params.toString()}`, {
            headers: { 'Authorization': `Bearer ${token}` }
        });

        if (!res.ok) throw new Error('Failed to fetch agenda stats');
        return res.json();
    },

    cancelAppointment: async (id: string, reason: string): Promise<Appointment> => {
        const token = AuthStore.getToken();
        const res = await fetch(`${API_URL}/appointments/${id}/cancel`, {
            method: 'PATCH',
            headers: {
                'Authorization': `Bearer ${token}`,
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ reason })
        });

        if (!res.ok) throw new Error('Failed to cancel appointment');
        return res.json();
    }
};

export interface Absence {
    id: string;
    userId: string;
    start: string;
    end: string;
    reason?: string;
    user?: { name: string };
}
