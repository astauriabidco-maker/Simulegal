import { AuthStore } from './authStore';

const BASE_API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:6060';
const API_URL = `${BASE_API_URL}/whatsapp`;

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const InboxAPI = {
    /**
     * Get all active conversations
     */
    getConversations: async () => {
        try {
            const response = await fetch(`${API_URL}/conversations`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement conversations');
            return response.json();
        } catch (error) {
            console.error('[InboxAPI] ❌ Erreur:', error);
            return [];
        }
    },

    /**
     * Get full message history for a lead or prospect
     */
    getMessages: async (type: 'LEAD' | 'PROSPECT', id: string) => {
        try {
            const response = await fetch(`${API_URL}/messages/${type}/${id}`, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur chargement messages');
            return response.json();
        } catch (error) {
            console.error('[InboxAPI] ❌ Erreur:', error);
            return [];
        }
    },

    /**
     * Send a manual reply
     */
    sendMessage: async (type: 'LEAD' | 'PROSPECT', id: string, content: string) => {
        try {
            const response = await fetch(`${API_URL}/send`, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify({ type, id, content })
            });
            if (!response.ok) throw new Error('Erreur envoi message');
            return response.json();
        } catch (error) {
            console.error('[InboxAPI] ❌ Erreur:', error);
            return { success: false };
        }
    }
};
