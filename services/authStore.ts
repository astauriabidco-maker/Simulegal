/**
 * Service d'Authentification Back-Office
 * Gère les rôles et sessions des utilisateurs admin
 */

// Rôles standardisés du système + aliases legacy pour compatibilité
export type UserRole = 'SUPER_ADMIN' | 'HQ_ADMIN' | 'CASE_WORKER' | 'AGENCY_MANAGER' | 'SALES' | 'KIOSK_AGENT' | 'API_PARTNER'
    | 'SUPERADMIN' | 'HQ' | 'AGENCY'; // Legacy aliases

export interface AdminUser {
    id: string;
    email: string;
    name: string;
    role: UserRole;
    agencyId?: string; // Pour les utilisateurs AGENCY
    agencyName?: string;
    permissions: string[];
    lastLogin?: string;
}

const SESSION_KEY = 'admin_session';
const TOKEN_KEY = 'admin_token';
const API_URL = 'http://localhost:4000';

export const AuthStore = {
    /**
     * Tente de connecter un utilisateur via l'API Backend
     */
    login: async (email: string, password: string): Promise<{ success: boolean; user?: AdminUser; error?: string }> => {
        try {
            const response = await fetch(`${API_URL}/auth/login`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ email, password })
            });

            if (!response.ok) {
                const errorData = await response.json();
                return { success: false, error: errorData.message || 'Identifiants incorrects' };
            }

            const data = await response.json();
            const { access_token, user: apiUser } = data;

            // Mapping des rôles Backend -> Frontend (normalisation)
            const roleMap: Record<string, UserRole> = {
                'SUPER_ADMIN': 'SUPER_ADMIN', 'SUPERADMIN': 'SUPER_ADMIN',
                'HQ_ADMIN': 'HQ_ADMIN', 'HQ': 'HQ_ADMIN',
                'CASE_WORKER': 'CASE_WORKER',
                'AGENCY_MANAGER': 'AGENCY_MANAGER', 'AGENCY': 'AGENCY_MANAGER',
                'SALES': 'SALES',
                'KIOSK_AGENT': 'KIOSK_AGENT',
                'API_PARTNER': 'API_PARTNER',
            };
            const role: UserRole = roleMap[apiUser.role] || 'KIOSK_AGENT';

            const user: AdminUser = {
                id: apiUser.id,
                email: apiUser.email,
                name: apiUser.name,
                role: role,
                agencyId: apiUser.agencyId,
                agencyName: apiUser.agencyName,
                permissions: Array.isArray(apiUser.permissions) ? apiUser.permissions : (apiUser.permissions || '').split(','),
                lastLogin: new Date().toISOString()
            };

            // Sauvegarde la session et le token
            localStorage.setItem(TOKEN_KEY, access_token);
            localStorage.setItem(SESSION_KEY, JSON.stringify(user));

            console.log(`[AUTH] ✅ Connexion API réussie: ${user.name} (${user.role})`);
            return { success: true, user };
        } catch (error) {
            console.error('[AUTH] ❌ Erreur réseau:', error);
            return { success: false, error: 'Serveur injoignable' };
        }
    },

    /**
     * Déconnecte l'utilisateur
     */
    logout: (): void => {
        localStorage.removeItem(SESSION_KEY);
        localStorage.removeItem(TOKEN_KEY);
        window.location.href = '/staff-login';
    },

    /**
     * Récupère l'utilisateur actuellement connecté
     */
    getCurrentUser: (): AdminUser | null => {
        if (typeof window === 'undefined') return null;
        const session = localStorage.getItem(SESSION_KEY);
        if (!session) return null;

        try {
            return JSON.parse(session) as AdminUser;
        } catch {
            return null;
        }
    },

    /**
     * Récupère le token JWT
     */
    getToken: (): string | null => {
        if (typeof window === 'undefined') return null;
        return localStorage.getItem(TOKEN_KEY);
    },

    /**
     * Définit le token JWT (pour login externe comme demo-login)
     */
    setToken: (token: string): void => {
        if (typeof window !== 'undefined') {
            localStorage.setItem(TOKEN_KEY, token);
        }
    },

    /**
     * Définit l'utilisateur en session (pour login externe)
     */
    setUser: (user: { id: string; email: string; name: string; role: string }): void => {
        if (typeof window !== 'undefined') {
            const sessionUser: AdminUser = {
                id: user.id,
                email: user.email,
                name: user.name,
                role: user.role as UserRole,
                permissions: ['view_own_dossier'],
                lastLogin: new Date().toISOString()
            };
            localStorage.setItem(SESSION_KEY, JSON.stringify(sessionUser));
        }
    },

    /**
     * Vérifie si l'utilisateur est connecté
     */
    isAuthenticated: (): boolean => {
        return AuthStore.getCurrentUser() !== null;
    },

    /**
     * Vérifie si l'utilisateur a un rôle spécifique
     */
    hasRole: (role: UserRole): boolean => {
        const user = AuthStore.getCurrentUser();
        if (!user) return false;
        if (user.role === 'SUPERADMIN' || user.role === 'SUPER_ADMIN') return true; // Super admin a tous les rôles
        return user.role === role;
    },

    /**
     * Vérifie si l'utilisateur a une permission spécifique
     */
    hasPermission: (permission: string): boolean => {
        const user = AuthStore.getCurrentUser();
        if (!user) return false;
        if (user.permissions.includes('*')) return true; // Toutes les permissions
        return user.permissions.includes(permission);
    },

    /**
     * Récupère les identifiants de démo pour l'environnement de dev
     */
    getDemoCredentials: () => {
        return [
            { email: 'super.admin@simulegal.fr', password: 'demo', role: 'SUPER_ADMIN' as UserRole, name: 'Admin Système' },
            { email: 'hq.admin@simulegal.fr', password: 'demo', role: 'HQ_ADMIN' as UserRole, name: 'Sophie Martin (Siège)' },
            { email: 'juriste@simulegal.fr', password: 'demo', role: 'CASE_WORKER' as UserRole, name: 'Marie Dupont (Juriste)' },
            { email: 'agency.paris@simulegal.fr', password: 'demo', role: 'AGENCY_MANAGER' as UserRole, name: 'Agence Paris Louvre' },
            { email: 'commercial@simulegal.fr', password: 'demo', role: 'SALES' as UserRole, name: 'Karim Bensalem (Commercial)' },
        ];
    }
};

export default AuthStore;
