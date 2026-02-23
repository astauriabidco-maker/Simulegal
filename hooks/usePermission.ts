import { useState, useEffect } from 'react';
import { AuthStore } from '../services/authStore';
import { RoleStore, Role } from '../services/RoleStore';
import { PermissionKey } from '../config/PermissionRegistry';

/**
 * Hook de sécurité RBAC
 * Permet de vérifier les droits d'accès de l'utilisateur courant
 */
export const usePermission = () => {
    const user = AuthStore.getCurrentUser();
    const [role, setRole] = useState<Role | null>(null);
    const [isLoading, setIsLoading] = useState(true);

    useEffect(() => {
        const loadRole = async () => {
            if (!user) {
                setIsLoading(false);
                return;
            }

            // En mode hybride : si le user a déjà les permissions dans le JWT on peut bootstrap
            if (user.permissions && user.permissions.length > 0) {
                setRole({
                    id: user.role,
                    label: user.role, // Fallback label
                    description: '',
                    permissions: user.permissions as PermissionKey[]
                });
            }

            // Sync avec le serveur pour avoir le rôle complet et à jour
            try {
                const fullRole = await RoleStore.getRole(user.role);
                if (fullRole) setRole(fullRole);
            } catch (error) {
                console.error('[usePermission] Role sync error:', error);
            } finally {
                setIsLoading(false);
            }
        };

        loadRole();
    }, [user?.id, user?.role]);

    /**
     * Vérifie si l'utilisateur possède une permission spécifique
     */
    const can = (permission: PermissionKey): boolean => {
        if (!user) return false;

        // Le Super Administrateur a accès à tout (God Mode)
        if (user.role === 'SUPERADMIN' || user.role === 'SUPER_ADMIN') return true;

        // On vérifie soit dans le rôle complet, soit dans les permissions directes du user (JWT)
        const userPermissions = role?.permissions || (user.permissions as PermissionKey[]) || [];
        return userPermissions.includes(permission);
    };

    /**
     * Vérifie si l'utilisateur possède TOUTES les permissions demandées
     */
    const canAll = (permissions: PermissionKey[]): boolean => {
        return permissions.every(p => can(p));
    };

    /**
     * Vérifie si l'utilisateur possède AU MOINS UNE des permissions demandées
     */
    const canAny = (permissions: PermissionKey[]): boolean => {
        return permissions.some(p => can(p));
    };

    return {
        can,
        canAll,
        canAny,
        role,
        user,
        isLoading
    };
};
