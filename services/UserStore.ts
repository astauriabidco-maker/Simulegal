import { AuthStore } from './authStore';

import { StaffUser } from '../types/backoffice';
export type { StaffUser }; // Re-export for convenience or just rely on import

const API_URL = 'http://localhost:3001/users';

const getHeaders = () => {
    const token = AuthStore.getToken();
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const UserStore = {
    getAllUsers: async (): Promise<StaffUser[]> => {
        try {
            const response = await fetch(API_URL, { headers: getHeaders() });
            if (!response.ok) throw new Error('Erreur lors du chargement des utilisateurs');
            return response.json();
        } catch (error) {
            console.error('[IAM] ❌ Erreur API:', error);
            return [];
        }
    },

    createUser: async (userData: Omit<StaffUser, 'id' | 'createdAt'>) => {
        try {
            const response = await fetch(API_URL, {
                method: 'POST',
                headers: getHeaders(),
                body: JSON.stringify(userData)
            });
            if (!response.ok) throw new Error('Erreur de création');
            const newUser = await response.json();
            console.log(`[IAM] 👤 Nouvel utilisateur créé via API: ${newUser.name}`);
            return newUser;
        } catch (error) {
            console.error('[IAM] ❌ Erreur API création:', error);
            return null;
        }
    },

    updateUser: async (id: string, data: Partial<StaffUser>) => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                method: 'PATCH',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Erreur de mise à jour');
            const updated = await response.json();
            console.log(`[IAM] 🔄 Utilisateur ${id} mis à jour via API`);
            return updated;
        } catch (error) {
            console.error('[IAM] ❌ Erreur API mise à jour:', error);
            return null;
        }
    },

    deleteUser: async (id: string) => {
        try {
            const response = await fetch(`${API_URL}/${id}`, {
                method: 'DELETE',
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Erreur de suppression');
            console.log(`[IAM] 🚫 Utilisateur ${id} supprimé via API`);
            return true;
        } catch (error) {
            console.error('[IAM] ❌ Erreur API suppression:', error);
            return false;
        }
    }
};
