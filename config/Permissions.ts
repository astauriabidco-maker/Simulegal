// ═══════════════════════════════════════════════
// MATRICE PERMISSIONS PAR RÔLE – SIMULEGAL
// Source de vérité unique pour frontend + backend
// ═══════════════════════════════════════════════

import { PermissionKey } from './PermissionRegistry';

export type UserRole =
    | 'SUPER_ADMIN'
    | 'HQ_ADMIN'
    | 'CASE_WORKER'
    | 'AGENCY_MANAGER'
    | 'SALES'
    | 'KIOSK_AGENT'
    | 'API_PARTNER';

// Matrice exhaustive des permissions par rôle
export const ROLE_PERMISSIONS: Record<UserRole, PermissionKey[]> = {
    'SUPER_ADMIN': [
        // Tout accès
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
    'HQ_ADMIN': [
        // Gestion opérationnelle complète, pas de suppression ni RBAC
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
    'CASE_WORKER': [
        // Traitement juridique des dossiers
        'crm.view_all', 'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.assign',
        'calendar.view', 'calendar.manage',
        'blog.view',
        'inbox.view', 'inbox.send',
        'profile.view_own', 'profile.edit_own', 'profile.view_team',
    ],
    'AGENCY_MANAGER': [
        // Vision agence + commissions + ventes
        'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.assign',
        'sales.view', 'sales.manage', 'sales.tracking',
        'calendar.view', 'calendar.manage',
        'finance.view_agency',
        'inbox.view', 'inbox.send',
        'profile.view_own', 'profile.edit_own', 'profile.view_team',
    ],
    'SALES': [
        // Prospection et création de leads
        'crm.view_own', 'crm.create',
        'sales.view', 'sales.tracking',
        'calendar.view',
        'inbox.view', 'inbox.send',
        'profile.view_own', 'profile.edit_own',
    ],
    'KIOSK_AGENT': [
        // Accès minimal depuis tablette
        'crm.create',
        'profile.view_own',
    ],
    'API_PARTNER': [
        // Accès API uniquement (pas d'UI back-office)
        'crm.create', 'crm.view_own',
    ],
};

// Vérification de permission
export const hasPermission = (userRole: UserRole, permission: string): boolean => {
    return ROLE_PERMISSIONS[userRole]?.includes(permission as PermissionKey) || false;
};

// Labels pour l'UI
export const ROLE_LABELS: Record<UserRole, string> = {
    'SUPER_ADMIN': 'Super Administrateur',
    'HQ_ADMIN': 'Administrateur Siège',
    'CASE_WORKER': 'Juriste / Opérateur',
    'AGENCY_MANAGER': 'Manager Agence',
    'SALES': 'Commercial',
    'KIOSK_AGENT': 'Agent de Kiosque',
    'API_PARTNER': 'Partenaire API',
};

export const ROLE_DESCRIPTIONS: Record<UserRole, string> = {
    'SUPER_ADMIN': 'Accès total et illimité à toutes les fonctions du système.',
    'HQ_ADMIN': 'Gestion opérationnelle des dossiers, du réseau et des utilisateurs.',
    'CASE_WORKER': 'Traitement juridique des dossiers, validation de documents, communication client.',
    'AGENCY_MANAGER': 'Gestion des leads de l\'agence, suivi des commissions, vision limitée à l\'agence.',
    'SALES': 'Prospection commerciale, création de leads et communication client.',
    'KIOSK_AGENT': 'Création de dossiers depuis une tablette en agence. Accès très limité.',
    'API_PARTNER': 'Accès API pour intégrations externes (pas d\'interface back-office).',
};

export const ROLE_COLORS: Record<UserRole, { bg: string; text: string; border: string }> = {
    'SUPER_ADMIN': { bg: 'bg-red-50', text: 'text-red-700', border: 'border-red-200' },
    'HQ_ADMIN': { bg: 'bg-amber-50', text: 'text-amber-700', border: 'border-amber-200' },
    'CASE_WORKER': { bg: 'bg-blue-50', text: 'text-blue-700', border: 'border-blue-200' },
    'AGENCY_MANAGER': { bg: 'bg-emerald-50', text: 'text-emerald-700', border: 'border-emerald-200' },
    'SALES': { bg: 'bg-violet-50', text: 'text-violet-700', border: 'border-violet-200' },
    'KIOSK_AGENT': { bg: 'bg-purple-50', text: 'text-purple-700', border: 'border-purple-200' },
    'API_PARTNER': { bg: 'bg-slate-50', text: 'text-slate-700', border: 'border-slate-200' },
};

// Mapping legacy frontend roles → UserRole standardisé
export const normalizeRole = (raw: string): UserRole => {
    const map: Record<string, UserRole> = {
        'SUPERADMIN': 'SUPER_ADMIN',
        'SUPER_ADMIN': 'SUPER_ADMIN',
        'HQ': 'HQ_ADMIN',
        'HQ_ADMIN': 'HQ_ADMIN',
        'AGENCY': 'AGENCY_MANAGER',
        'AGENCY_MANAGER': 'AGENCY_MANAGER',
        'CASE_WORKER': 'CASE_WORKER',
        'SALES': 'SALES',
        'KIOSK_AGENT': 'KIOSK_AGENT',
        'API_PARTNER': 'API_PARTNER',
    };
    return map[raw] || 'KIOSK_AGENT';
};
