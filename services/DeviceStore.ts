/**
 * Device Fleet Management Store
 * Gère les tablettes/kiosques déployés dans les agences et points relais
 * Utilise l'API backend pour la persistance
 */

export type DeviceStatus = 'UNPAIRED' | 'ACTIVE' | 'OFFLINE';

export interface KioskDevice {
    id: string;
    pairingCode: string;
    status: DeviceStatus;
    assignedAgencyId?: string;
    assignedAgency?: {
        id: string;
        name: string;
    };
    name: string;
    lastHeartbeat: string;
    appVersion: string;
    createdAt: string;
}

const API_URL = 'http://localhost:3001/devices';

export const DeviceStore = {
    /**
     * Enregistre une nouvelle tablette inconnue (API)
     */
    registerNewDevice: async (): Promise<KioskDevice> => {
        const response = await fetch(`${API_URL}/register`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' }
        });

        if (!response.ok) {
            throw new Error('Failed to register device');
        }

        const device = await response.json();
        console.log(`[DEVICE] 📱 Nouveau terminal enregistré: ${device.id}`);
        console.log(`[DEVICE] 🔑 Code d'appairage: ${device.pairingCode}`);

        return device;
    },

    /**
     * Active le terminal avec un code unique
     */
    activateDevice: async (code: string): Promise<KioskDevice | null> => {
        try {
            const response = await fetch(`${API_URL}/activate`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ code })
            });

            if (!response.ok) return null;

            return response.json();
        } catch {
            return null;
        }
    },

    /**
     * Récupère un device par son ID (API)
     */
    getDeviceById: async (deviceId: string): Promise<KioskDevice | null> => {
        try {
            const response = await fetch(`${API_URL}/${deviceId}`);
            if (!response.ok) return null;
            return response.json();
        } catch {
            return null;
        }
    },

    /**
     * Le Siège valide l'appairage d'une tablette (API)
     */
    pairDevice: async (
        pairingCode: string,
        agencyId: string,
        agencyName: string,
        deviceName?: string
    ): Promise<KioskDevice | null> => {
        try {
            const response = await fetch(`${API_URL}/pair`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    pairingCode,
                    agencyId,
                    name: deviceName
                })
            });

            if (!response.ok) return null;

            const result = await response.json();
            if (!result.success) return null;

            console.log(`[DEVICE] ✅ Terminal appairé: ${result.device.id} → ${agencyName}`);
            return result.device;
        } catch (error) {
            console.error('[DEVICE] ❌ Erreur appairage:', error);
            return null;
        }
    },

    /**
     * La tablette envoie un heartbeat pour signaler qu'elle est active (API)
     */
    sendHeartbeat: async (deviceId: string): Promise<boolean> => {
        try {
            const response = await fetch(`${API_URL}/${deviceId}/heartbeat`, {
                method: 'PATCH'
            });
            return response.ok;
        } catch {
            return false;
        }
    },

    /**
     * Récupère tous les devices (API)
     */
    getAllDevices: async (): Promise<KioskDevice[]> => {
        try {
            const response = await fetch(API_URL);
            if (!response.ok) return [];
            return response.json();
        } catch {
            return [];
        }
    },

    /**
     * Récupère les devices d'une agence spécifique (API)
     */
    getDevicesByAgency: async (agencyId: string): Promise<KioskDevice[]> => {
        try {
            const response = await fetch(`${API_URL}?agencyId=${agencyId}`);
            if (!response.ok) return [];
            return response.json();
        } catch {
            return [];
        }
    },

    /**
     * Supprime un device (API)
     */
    removeDevice: async (deviceId: string): Promise<boolean> => {
        try {
            const response = await fetch(`${API_URL}/${deviceId}`, {
                method: 'DELETE'
            });
            return response.ok;
        } catch {
            return false;
        }
    },

    /**
     * Réinitialise un device pour réappairage (API)
     */
    resetDevice: async (deviceId: string): Promise<KioskDevice | null> => {
        try {
            const response = await fetch(`${API_URL}/${deviceId}/reset`, {
                method: 'PATCH'
            });
            if (!response.ok) return null;
            return response.json();
        } catch {
            return null;
        }
    }
};

export default DeviceStore;
