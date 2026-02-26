// ═══════════════════════════════════════════════
// REGISTRE CENTRALISÉ DES PERMISSIONS – SIMULEGAL
// ═══════════════════════════════════════════════

export type PermissionKey =
    // CRM & Dossiers
    | 'crm.view_all'        // Voir tous les dossiers (Siège)
    | 'crm.view_agency'     // Voir dossiers de son agence
    | 'crm.view_own'        // Voir uniquement ses dossiers assignés
    | 'crm.create'          // Créer un lead / dossier
    | 'crm.edit_technical'  // Modifier les champs juridiques/techniques
    | 'crm.validate_doc'    // Valider / rejeter un document
    | 'crm.delete'          // Supprimer un dossier (Dangereux)
    | 'crm.assign'          // Assigner / réassigner un dossier

    // Ventes & Pipeline
    | 'sales.view'          // Accéder au pipeline de ventes
    | 'sales.manage'        // Gérer le pipeline et les prospects
    | 'sales.tracking'      // Accéder au suivi commercial

    // Agenda & RDV
    | 'calendar.view'       // Voir l'agenda et les rendez-vous
    | 'calendar.manage'     // Créer/modifier des rendez-vous

    // Finance
    | 'finance.view_global' // Voir tout le CA
    | 'finance.view_agency' // Voir ses commissions
    | 'finance.payout'      // Faire des virements

    // Réseau & Flotte
    | 'network.manage'      // Créer/Modifier des Agences
    | 'fleet.manage'        // Gérer les tablettes
    | 'franchise.manage'    // Gérer le CRM franchise

    // Blog & Contenu
    | 'blog.view'           // Voir le blog côté admin
    | 'blog.manage'         // Créer/modifier/supprimer des articles

    // Système & Admin
    | 'users.manage'        // Gérer les utilisateurs
    | 'roles.manage'        // Gérer les droits (RBAC)
    | 'settings.manage'     // Configurer les services
    | 'automations.manage'  // Gérer les automatisations pipeline
    | 'audit.view'          // Voir les logs d'audit et la veille

    // Communication
    | 'inbox.view'          // Accéder à la WhatsApp Inbox
    | 'inbox.send'          // Envoyer des messages WhatsApp

    // Profil
    | 'profile.view_own'    // Voir son propre profil
    | 'profile.edit_own'    // Modifier son profil
    | 'profile.view_team';  // Voir les profils de son équipe

export const PERMISSION_GROUPS: Record<string, { label: string; permissions: PermissionKey[] }> = {
    'CRM': {
        label: 'CRM & Dossiers',
        permissions: ['crm.view_all', 'crm.view_agency', 'crm.view_own', 'crm.create', 'crm.edit_technical', 'crm.validate_doc', 'crm.delete', 'crm.assign'],
    },
    'SALES': {
        label: 'Ventes & Pipeline',
        permissions: ['sales.view', 'sales.manage', 'sales.tracking'],
    },
    'CALENDAR': {
        label: 'Agenda & RDV',
        permissions: ['calendar.view', 'calendar.manage'],
    },
    'FINANCE': {
        label: 'Finance',
        permissions: ['finance.view_global', 'finance.view_agency', 'finance.payout'],
    },
    'NETWORK': {
        label: 'Réseau & Flotte',
        permissions: ['network.manage', 'fleet.manage', 'franchise.manage'],
    },
    'BLOG': {
        label: 'Blog & Contenu',
        permissions: ['blog.view', 'blog.manage'],
    },
    'SYSTEM': {
        label: 'Système & Admin',
        permissions: ['users.manage', 'roles.manage', 'settings.manage', 'automations.manage', 'audit.view'],
    },
    'COMMUNICATION': {
        label: 'Communication',
        permissions: ['inbox.view', 'inbox.send'],
    },
    'PROFILE': {
        label: 'Profil',
        permissions: ['profile.view_own', 'profile.edit_own', 'profile.view_team'],
    },
};

export const PERMISSION_LABELS: Record<PermissionKey, string> = {
    // CRM
    'crm.view_all': 'Voir tous les dossiers',
    'crm.view_agency': 'Voir les dossiers de son agence',
    'crm.view_own': 'Voir ses propres dossiers',
    'crm.create': 'Créer des dossiers',
    'crm.edit_technical': 'Édition technique & juridique',
    'crm.validate_doc': 'Validation de documents',
    'crm.delete': 'Suppression de dossiers',
    'crm.assign': 'Assigner / réassigner des dossiers',
    // Ventes
    'sales.view': 'Accéder au pipeline de ventes',
    'sales.manage': 'Gérer le pipeline et les prospects',
    'sales.tracking': 'Accéder au suivi commercial',
    // Agenda
    'calendar.view': 'Voir l\'agenda et les RDV',
    'calendar.manage': 'Créer / modifier des rendez-vous',
    // Finance
    'finance.view_global': 'Vue financière globale',
    'finance.view_agency': 'Vue commissions agence',
    'finance.payout': 'Déclencher les virements',
    // Réseau
    'network.manage': 'Gestion du réseau agences',
    'fleet.manage': 'Gestion de la flotte tablettes',
    'franchise.manage': 'Gestion du CRM franchise',
    // Blog
    'blog.view': 'Consulter le blog (admin)',
    'blog.manage': 'Créer / modifier des articles',
    // Système
    'users.manage': 'Gestion des utilisateurs',
    'roles.manage': 'Gestion des rôles (RBAC)',
    'settings.manage': 'Configuration système',
    'automations.manage': 'Gestion des automatisations',
    'audit.view': 'Voir les logs d\'audit et veille',
    // Communication
    'inbox.view': 'Consulter la messagerie',
    'inbox.send': 'Envoyer des messages',
    // Profil
    'profile.view_own': 'Voir son profil',
    'profile.edit_own': 'Modifier son profil',
    'profile.view_team': 'Voir profils équipe',
};
