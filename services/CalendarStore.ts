import { CRM } from './crmStore';
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
}

export interface Availability {
    // Default: 09:00 - 18:00 Mon-Fri
    days: number[]; // [1, 2, 3, 4, 5]
    hours: { start: string; end: string };
}

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001';

export const CalendarStore = {
    getAllAppointments: async (filters?: { agencyId?: string, start?: string, end?: string }): Promise<Appointment[]> => {
        const token = localStorage.getItem('token');
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

    getAvailableSlots: async (dateStr: string, agencyId?: string): Promise<string[]> => {
        const params = new URLSearchParams({ date: dateStr });
        if (agencyId) params.append('agencyId', agencyId);

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
        agencyId?: string
    ): Promise<Appointment> => {

        const payload = {
            slotIso,
            lead,
            type,
            agencyId
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

        // Update CRM (Frontend side effect or better handled by backend?)
        // The backend now handles creation, but CRM updates were client-side before. 
        // Ideally backend controller should trigger CRM updates via event or direct call.
        // Keeping frontend CRM update for immediate feedback for now, but backend should eventually own this.

        if (type === 'VISIO_JURISTE') {
            await CRM.updateLeadStatus(lead.id, 'BOOKED');
            await CRM.addNote(lead.id, {
                author: 'SYSTEM',
                content: `📅 RDV Visio confirmé pour le ${new Date(slotIso).toLocaleDateString()} à ${new Date(slotIso).toLocaleTimeString()}`
            });
        } else {
            await CRM.addNote(lead.id, {
                author: 'SYSTEM',
                content: `📅 RDV Agence confirmé pour le ${new Date(slotIso).toLocaleDateString()} à ${new Date(slotIso).toLocaleTimeString()}`
            });
        }

        return appointment;
    }
};
