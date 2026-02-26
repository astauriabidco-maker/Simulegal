import { PermissionKey } from '../config/PermissionRegistry';

export interface Role {
    id: string;
    label: string;
    description: string;
    permissions: PermissionKey[];
    isSystem?: boolean;
}

const STORAGE_KEY = 'admin_roles';

const DEFAULT_ROLES: Role[] = [
    {
        id: 'SUPER_ADMIN',
        label: 'Super Administrateur',
        description: 'Accès total et illimité à toutes les fonctions du système.',
        permissions: [
            'crm.view_all', 'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.delete', 'crm.assign',
            'sales.view', 'sales.manage', 'sales.tracking',
            'calendar.view', 'calendar.manage',
            'finance.view_global', 'finance.view_agency', 'finance.payout',
            'network.manage', 'fleet.manage', 'franchise.manage',
            'blog.view', 'blog.manage',
            'users.manage', 'roles.manage', 'settings.manage', 'automations.manage', 'audit.view',
            'inbox.view', 'inbox.send',
            'profile.view_own', 'profile.edit_own', 'profile.view_team',
        ],
        isSystem: true
    },
    {
        id: 'HQ_ADMIN',
        label: 'Administrateur Siège',
        description: 'Gestion opérationnelle des dossiers, du réseau et des utilisateurs.',
        permissions: [
            'crm.view_all', 'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.assign',
            'sales.view', 'sales.manage', 'sales.tracking',
            'calendar.view', 'calendar.manage',
            'finance.view_global', 'finance.view_agency',
            'network.manage', 'fleet.manage', 'franchise.manage',
            'blog.view', 'blog.manage',
            'users.manage', 'audit.view',
            'inbox.view', 'inbox.send',
            'profile.view_own', 'profile.edit_own', 'profile.view_team',
        ],
        isSystem: true
    },
    {
        id: 'CASE_WORKER',
        label: 'Juriste / Opérateur',
        description: 'Traitement juridique des dossiers, validation de documents.',
        permissions: [
            'crm.view_all', 'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.assign',
            'calendar.view', 'calendar.manage',
            'blog.view',
            'inbox.view', 'inbox.send',
            'profile.view_own', 'profile.edit_own', 'profile.view_team',
        ],
        isSystem: true
    },
    {
        id: 'AGENCY_MANAGER',
        label: 'Manager Agence',
        description: 'Gestion des leads de l\'agence, suivi des commissions.',
        permissions: [
            'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.assign',
            'sales.view', 'sales.manage', 'sales.tracking',
            'calendar.view', 'calendar.manage',
            'finance.view_agency',
            'inbox.view', 'inbox.send',
            'profile.view_own', 'profile.edit_own', 'profile.view_team',
        ],
        isSystem: true
    },
    {
        id: 'SALES',
        label: 'Commercial',
        description: 'Prospection commerciale, création de leads et communication client.',
        permissions: [
            'crm.view_own', 'crm.create',
            'sales.view', 'sales.tracking',
            'calendar.view',
            'inbox.view', 'inbox.send',
            'profile.view_own', 'profile.edit_own',
        ],
        isSystem: true
    },
    {
        id: 'KIOSK_AGENT',
        label: 'Agent de Kiosque',
        description: 'Création de dossiers depuis une tablette en agence.',
        permissions: ['crm.create', 'profile.view_own'],
        isSystem: true
    },
    {
        id: 'API_PARTNER',
        label: 'Partenaire API',
        description: 'Accès API pour intégrations externes.',
        permissions: ['crm.create', 'crm.view_own'],
        isSystem: true
    },
];

const API_URL = 'http://localhost:4000';

const getHeaders = () => {
    const token = localStorage.getItem('admin_token');
    return {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
    };
};

export const RoleStore = {
    /**
     * Récupère tous les rôles depuis le Backend
     */
    getAllRoles: async (): Promise<Role[]> => {
        try {
            const response = await fetch(`${API_URL}/roles`, {
                headers: getHeaders()
            });
            if (!response.ok) throw new Error('Erreur chargement rôles');

            const roles = await response.json();
            return roles.map((r: any) => ({
                ...r,
                permissions: r.permissions.split(',') as PermissionKey[]
            }));
        } catch (error) {
            console.error('[RoleStore] Erreur:', error);
            // Fallback sur les rôles par défaut si l'API échoue
            return DEFAULT_ROLES;
        }
    },

    /**
     * Récupère un rôle par son ID
     */
    getRole: async (id: string): Promise<Role | null> => {
        try {
            const response = await fetch(`${API_URL}/roles/${id}`, {
                headers: getHeaders()
            });
            if (!response.ok) return null;

            const r = await response.json();
            return {
                ...r,
                permissions: r.permissions.split(',') as PermissionKey[]
            };
        } catch {
            return null;
        }
    },

    /**
     * Sauvegarde ou met à jour un rôle sur le serveur
     */
    saveRole: async (role: Role): Promise<void> => {
        const isUpdate = await RoleStore.getRole(role.id);
        const data = {
            label: role.label,
            description: role.description,
            permissions: role.permissions.join(',')
        };

        try {
            const response = await fetch(`${API_URL}/roles${isUpdate ? `/${role.id}` : ''}`, {
                method: isUpdate ? 'PATCH' : 'POST',
                headers: getHeaders(),
                body: JSON.stringify(data)
            });
            if (!response.ok) throw new Error('Erreur sauvegarde');
        } catch (error) {
            console.error('[RoleStore] Erreur save:', error);
            throw error;
        }
    },

    /**
     * Supprime un rôle (Sauf si système)
     */
    deleteRole: async (id: string): Promise<boolean> => {
        try {
            const response = await fetch(`${API_URL}/roles/${id}`, {
                method: 'DELETE',
                headers: getHeaders()
            });
            return response.ok;
        } catch {
            return false;
        }
    }
};
