export type PermissionKey =
    // CRM & Dossiers
    | 'crm.view_all'       // Voir tous les dossiers (Siège)
    | 'crm.view_agency'    // Voir dossiers de son agence
    | 'crm.create'         // Créer un lead
    | 'crm.edit_technical' // Modifier les champs juridiques/techniques
    | 'crm.validate_doc'   // Valider un document
    | 'crm.delete'         // Supprimer un dossier (Dangereux)

    // Finance
    | 'finance.view_global' // Voir tout le CA
    | 'finance.view_agency' // Voir ses commissions
    | 'finance.payout'      // Faire des virements

    // Réseau & Admin
    | 'network.manage'      // Créer/Modifier des Agences
    | 'fleet.manage'        // Gérer les tablettes
    | 'users.manage'        // Gérer les utilisateurs
    | 'roles.manage'        // Gérer les droits (Ce module)
    | 'settings.manage';    // Configurer les services

export const PERMISSION_GROUPS: Record<string, PermissionKey[]> = {
    'CRM': ['crm.view_all', 'crm.view_agency', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.delete'],
    'FINANCE': ['finance.view_global', 'finance.view_agency', 'finance.payout'],
    'NETWORK': ['network.manage', 'fleet.manage'],
    'SYSTEM': ['users.manage', 'roles.manage', 'settings.manage']
};

export const PERMISSION_LABELS: Record<PermissionKey, string> = {
    'crm.view_all': 'Voir tous les dossiers',
    'crm.view_agency': 'Voir les dossiers de son agence',
    'crm.create': 'Créer des dossiers',
    'crm.edit_technical': 'Édition technique & juridique',
    'crm.validate_doc': 'Validation de documents',
    'crm.delete': 'Suppression de dossiers',
    'finance.view_global': 'Vue financière globale',
    'finance.view_agency': 'Vue commissions agence',
    'finance.payout': 'Déclencher les virements',
    'network.manage': 'Gestion du réseau agences',
    'fleet.manage': 'Gestion de la flotte tablettes',
    'users.manage': 'Gestion des utilisateurs',
    'roles.manage': 'Gestion des rôles (RBAC)',
    'settings.manage': 'Configuration système'
};
