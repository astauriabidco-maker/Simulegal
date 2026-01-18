export type UserRole = 'HQ_ADMIN' | 'CASE_WORKER' | 'AGENCY_MANAGER' | 'KIOSK_AGENT';

// Définition fine des capacités par rôle
export const ROLE_PERMISSIONS: Record<UserRole, string[]> = {
    'HQ_ADMIN': ['manage_users', 'manage_settings', 'view_all_deals', 'edit_deals', 'validate_docs', 'view_finances'],
    'CASE_WORKER': ['view_all_deals', 'edit_deals', 'validate_docs'],
    'AGENCY_MANAGER': ['view_agency_deals', 'create_notes', 'view_commissions'],
    'KIOSK_AGENT': ['create_deal_only']
};

export const hasPermission = (userRole: UserRole, permission: string): boolean => {
    // Un HQ_ADMIN a virtuellement toutes les permissions si non listées explicitement mais on suit la matrice
    return ROLE_PERMISSIONS[userRole]?.includes(permission) || false;
};

// Labels pour l'UI
export const ROLE_LABELS: Record<UserRole, string> = {
    'HQ_ADMIN': 'Administrateur Siège',
    'CASE_WORKER': 'Juriste / Opérateur',
    'AGENCY_MANAGER': 'Manager Agence',
    'KIOSK_AGENT': 'Agent de Kiosque'
};
