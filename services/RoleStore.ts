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
        id: 'SUPERADMIN',
        label: 'Super Administrateur',
        description: 'Accès total et illimité à toutes les fonctions du système.',
        permissions: [
            'crm.view_all', 'crm.view_agency', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.delete',
            'finance.view_global', 'finance.view_agency', 'finance.payout',
            'network.manage', 'fleet.manage', 'users.manage', 'roles.manage', 'settings.manage'
        ],
        isSystem: true
    },
    {
        id: 'HQ',
        label: 'Staff Siège (HQ)',
        description: 'Gestion opérationnelle des dossiers et du réseau.',
        permissions: [
            'crm.view_all', 'crm.view_agency', 'crm.create', 'crm.edit_technical', 'crm.validate_doc',
            'finance.view_global', 'finance.view_agency',
            'network.manage', 'fleet.manage', 'users.manage'
        ],
        isSystem: true
    },
    {
        id: 'AGENCY',
        label: 'Directeur d\'Agence',
        description: 'Gestion des leads de l\'agence et suivi des commissions.',
        permissions: ['crm.view_agency', 'crm.create', 'finance.view_agency'],
        isSystem: true
    },
    {
        id: 'AGENCY_SALES',
        label: 'Commercial Agence',
        description: 'Acquisition de nouveaux leads en agence.',
        permissions: ['crm.create', 'crm.view_agency'],
        isSystem: false
    }
];

const API_URL = 'http://localhost:3001';

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
