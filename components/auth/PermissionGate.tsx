import React from 'react';
import AuthStore from '../../services/authStore';
import { hasPermission, normalizeRole } from '../../config/Permissions';

interface PermissionGateProps {
    permission: string;
    children: React.ReactNode;
    fallback?: React.ReactNode;
}

/**
 * PermissionGate : Filtre l'affichage des composants selon les droits de l'utilisateur connecté.
 * Utilise normalizeRole() pour mapper automatiquement les anciennes valeurs de rôle.
 */
export const PermissionGate = ({ permission, children, fallback = null }: PermissionGateProps) => {
    const user = AuthStore.getCurrentUser();
    if (!user) return <>{fallback}</>;

    const role = normalizeRole(user.role || '');
    const authorized = hasPermission(role, permission);

    return authorized ? <>{children}</> : <>{fallback}</>;
};

export default PermissionGate;
