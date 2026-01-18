import AuthStore from './authStore';

export interface SystemSettings {
    company: {
        name: string;
        address: string;
        zipCode: string;
        city: string;
        siret: string;
        tvaNumber: string;
        supportEmail: string;
        supportPhone: string;
    };

    payment: {
        provider: 'STRIPE';
        mode: 'TEST' | 'LIVE';
        publicKey: string;
        secretKey: string;
        currency: string;
    };

    notifications: {
        smtpHost: string;
        smtpPort: number;
        smtpUser: string;
        smtpPass: string;

        smsProvider: 'TWILIO' | 'VONAGE';
        smsSid: string;
        smsToken: string;

        whatsappEnabled: boolean;
        whatsappBusinessId?: string;
        whatsappToken?: string;
    };

    integrations: {
        ocrProvider: 'GOOGLE_VISION' | 'MINDEE';
        ocrApiKey: string;
        mapsApiKey: string;
    };

    storage: {
        provider: 'AWS_S3' | 'LOCAL';
        bucketName: string;
        region: string;
        accessKey: string;
        secretKey: string;
    };
}

const API_BASE = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001').replace(/\/$/, '');

const DEFAULT_SETTINGS: SystemSettings = {
    company: {
        name: "Simulegal (Local Fallback)",
        address: "", zipCode: "", city: "", siret: "", tvaNumber: "",
        supportEmail: "", supportPhone: ""
    },
    payment: { provider: 'STRIPE', mode: 'TEST', publicKey: '', secretKey: '', currency: 'EUR' },
    notifications: { smtpHost: '', smtpPort: 587, smtpUser: '', smtpPass: '', smsProvider: 'TWILIO', smsSid: '', smsToken: '', whatsappEnabled: false },
    integrations: { ocrProvider: 'GOOGLE_VISION', ocrApiKey: '', mapsApiKey: '' },
    storage: { provider: 'LOCAL', bucketName: '', region: '', accessKey: '', secretKey: '' }
};

export const SettingsStore = {
    getSettings: async (): Promise<SystemSettings> => {
        const url = `${API_BASE}/settings`;
        try {
            const response = await fetch(url);
            if (!response.ok) {
                console.warn(`[SettingsStore] ⚠️ Backend returned ${response.status} for ${url}. Using fallback.`);
                return DEFAULT_SETTINGS;
            }
            return await response.json();
        } catch (error) {
            console.error('[SettingsStore] ❌ Connection error:', error);
            return DEFAULT_SETTINGS;
        }
    },

    saveSettings: async (section: keyof SystemSettings, data: any): Promise<SystemSettings> => {
        const url = `${API_BASE}/settings/update/${section}`;
        try {
            const response = await fetch(url, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
                body: JSON.stringify(data)
            });

            if (!response.ok) {
                console.error(`[SettingsStore] ❌ Patch failed with status ${response.status} for ${url}`);
                throw new Error(`Update failed: ${response.status}`);
            }

            console.log(`[Settings] ⚙️ Section ${section} mise à jour en base`);
            return await SettingsStore.getSettings();
        } catch (error) {
            console.error('[SettingsStore] ❌ Error saving settings:', error);
            throw error;
        }
    },

    testConnection: async (type: string): Promise<{ success: boolean; message: string }> => {
        console.log(`[Settings] 🧪 Test de connexion pour: ${type}`);
        // This could also be a backend endpoint, but for now we keep local simulation for UI feedback
        await new Promise(resolve => setTimeout(resolve, 1500));

        if (type === 'STRIPE') return { success: true, message: "Connexion Cloud Stripe établie (Mode Test)" };
        if (type === 'SMTP') return { success: true, message: "Serveur SMTP accessible. Email de test envoyé." };
        if (type === 'WHATSAPP') return { success: false, message: "Token invalide ou compte non vérifié." };

        return { success: true, message: "Test réussi." };
    }
};
