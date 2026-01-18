import React from 'react';
import AuthStore from '../../services/authStore';
import { hasPermission, UserRole } from '../../config/Permissions';

interface PermissionGateProps {
    permission: string;
    children: React.ReactNode;
    fallback?: React.ReactNode;
}

/**
 * PermissionGate : Filtre l'affichage des composants selon les droits de l'utilisateur connecté.
 * @param permission La permission requise (ex: 'manage_users')
 * @param children Le contenu à afficher si autorisé
 * @param fallback Un composant optionnel à afficher si non autorisé (ou rien par défaut)
 */
export const PermissionGate = ({ permission, children, fallback = null }: PermissionGateProps) => {
    const user = AuthStore.getCurrentUser();

    if (!user) return <>{fallback}</>;

    // Mapping du rôle AuthStore vers le rôle IAM
    // Dans une version plus avancée, ces deux systèmes seraient fusionnés.
    let iamRole: UserRole = 'CASE_WORKER';
    if (user.role === 'SUPERADMIN' || user.role === 'HQ') iamRole = 'HQ_ADMIN';
    if (user.role === 'AGENCY') iamRole = 'AGENCY_MANAGER';

    const authorized = hasPermission(iamRole, permission);

    if (authorized) {
        return <>{children}</>;
    }

    return <>{fallback}</>;
};

export default PermissionGate;
