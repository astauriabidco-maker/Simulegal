/**
 * Service d'Authentification Back-Office
 * Gère les rôles et sessions des utilisateurs admin
 */

export type UserRole = 'HQ' | 'AGENCY' | 'SUPERADMIN';

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

// Utilisateurs de démonstration
const DEMO_USERS: Record<string, { password: string; user: AdminUser }> = {
    'hq@simulegal.fr': {
        password: 'demo123',
        user: {
            id: 'USR-HQ-001',
            email: 'hq@simulegal.fr',
            name: 'Admin Siège',
            role: 'HQ',
            permissions: ['view_all_leads', 'validate_documents', 'manage_agencies', 'view_reports']
        }
    },
    'juridique@simulegal.fr': {
        password: 'demo123',
        user: {
            id: 'USR-HQ-002',
            email: 'juridique@simulegal.fr',
            name: 'Marie Dupont',
            role: 'HQ',
            permissions: ['view_all_leads', 'validate_documents']
        }
    },
    'agence.paris@simulegal.fr': {
        password: 'demo123',
        user: {
            id: 'USR-AGC-001',
            email: 'agence.paris@simulegal.fr',
            name: 'Pierre Martin',
            role: 'AGENCY',
            agencyId: 'AGC-PARIS-001',
            agencyName: 'SimuLegal Paris 15ème',
            permissions: ['view_own_leads', 'add_notes']
        }
    },
    'agence.lyon@simulegal.fr': {
        password: 'demo123',
        user: {
            id: 'USR-AGC-002',
            email: 'agence.lyon@simulegal.fr',
            name: 'Sophie Bernard',
            role: 'AGENCY',
            agencyId: 'AGC-LYON-001',
            agencyName: 'SimuLegal Lyon Part-Dieu',
            permissions: ['view_own_leads', 'add_notes']
        }
    },
    'admin@simulegal.fr': {
        password: 'superadmin',
        user: {
            id: 'USR-SUPER-001',
            email: 'admin@simulegal.fr',
            name: 'Super Admin',
            role: 'SUPERADMIN',
            permissions: ['*'] // Toutes les permissions
        }
    }
};

const SESSION_KEY = 'admin_session';

export const AuthStore = {
    /**
     * Tente de connecter un utilisateur
     */
    login: (email: string, password: string): { success: boolean; user?: AdminUser; error?: string } => {
        const normalizedEmail = email.toLowerCase().trim();
        const userData = DEMO_USERS[normalizedEmail];

        if (!userData) {
            console.log(`[AUTH] ❌ Utilisateur non trouvé: ${normalizedEmail}`);
            return { success: false, error: 'Identifiants incorrects' };
        }

        if (userData.password !== password) {
            console.log(`[AUTH] ❌ Mot de passe incorrect pour: ${normalizedEmail}`);
            return { success: false, error: 'Identifiants incorrects' };
        }

        // Connexion réussie
        const user: AdminUser = {
            ...userData.user,
            lastLogin: new Date().toISOString()
        };

        // Sauvegarde la session
        localStorage.setItem(SESSION_KEY, JSON.stringify(user));
        console.log(`[AUTH] ✅ Connexion réussie: ${user.name} (${user.role})`);

        return { success: true, user };
    },

    /**
     * Déconnecte l'utilisateur
     */
    logout: (): void => {
        const user = AuthStore.getCurrentUser();
        if (user) {
            console.log(`[AUTH] 👋 Déconnexion: ${user.name}`);
        }
        localStorage.removeItem(SESSION_KEY);
    },

    /**
     * Récupère l'utilisateur actuellement connecté
     */
    getCurrentUser: (): AdminUser | null => {
        const session = localStorage.getItem(SESSION_KEY);
        if (!session) return null;

        try {
            return JSON.parse(session) as AdminUser;
        } catch {
            return null;
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
        if (user.role === 'SUPERADMIN') return true; // Super admin a tous les rôles
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
     * Récupère la liste des utilisateurs démo (pour affichage)
     */
    getDemoCredentials: (): { email: string; password: string; role: UserRole; name: string }[] => {
        return Object.entries(DEMO_USERS).map(([email, data]) => ({
            email,
            password: data.password,
            role: data.user.role,
            name: data.user.name
        }));
    }
};

export default AuthStore;
