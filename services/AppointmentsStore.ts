
const API_URL = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3005') + '/appointments-slots';

export interface AppointmentSlot {
    id: string;
    start: string;
    end: string;
    status: string;
    jurist: {
        id: string;
        name: string;
    };
}

export const AppointmentsStore = {
    getAvailableSlots: async (start: Date, end: Date): Promise<AppointmentSlot[]> => {
        try {
            const res = await fetch(`${API_URL}/available?start=${start.toISOString()}&end=${end.toISOString()}`);
            if (!res.ok) return [];
            return await res.json();
        } catch (e) {
            console.error('Failed to fetch slots', e);
            return [];
        }
    },

    lockSlot: async (slotId: string, leadId?: string): Promise<boolean> => {
        try {
            const res = await fetch(`${API_URL}/${slotId}/lock`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ leadId })
            });
            return res.ok;
        } catch (e) {
            return false;
        }
    }
};
