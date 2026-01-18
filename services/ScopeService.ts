import { Lead } from './crmStore';
import { StaffUser } from '../types/backoffice';

export const ScopeService = {
    /**
     * Filtre les dossiers visibles pour un utilisateur donné selon ses droits
     */
    filterLeadsForUser: (leads: Lead[], user: StaffUser): Lead[] => {
        // PROTECTION : Si pas d'utilisateur, on ne montre rien
        if (!user) return [];

        // CAS 1 : Utilisateur d'Agence (Intégrée, Franchise ou Corner)
        // Il ne voit que ce qui vient de SON agence (ou assigné à son agence)
        // Note: HQ est considéré comme une agence "spéciale" dans ce contexte si homeAgencyId = 'HQ'
        if (user.homeAgencyId && user.homeAgencyId !== 'HQ') {
            return leads.filter(l => l.originAgencyId === user.homeAgencyId);
        }

        // CAS 2 : Utilisateur Siège (homeAgencyId = 'HQ' ou null)

        // Sous-cas 2a : Super Admin ou HQ Admin sans restriction -> Voit TOUT
        // Si scopeAgencyIds est vide ou non défini, on considère l'accès global pour un HQ
        if (!user.scopeAgencyIds || user.scopeAgencyIds.length === 0) {
            return leads;
        }

        // Sous-cas 2b : Utilisateur Siège avec RESTRICTION (ex: Directeur Régional)
        // Il ne voit que les agences de son périmètre OU ses dossiers propres
        return leads.filter(l => {
            const isInScope = l.originAgencyId && user.scopeAgencyIds?.includes(l.originAgencyId);
            const isAssignedToMe = l.assignedUser === user.id || (l as any).assignedToUser === user.id;

            return isInScope || isAssignedToMe;
        });
    },

    /**
     * Vérifie si un utilisateur a le droit de voir un lead spécifique
     */
    canViewLead: (lead: Lead, user: StaffUser): boolean => {
        if (!user) return false;

        // Agence
        if (user.homeAgencyId && user.homeAgencyId !== 'HQ') {
            return lead.originAgencyId === user.homeAgencyId;
        }

        // Siège Global
        if (!user.scopeAgencyIds || user.scopeAgencyIds.length === 0) {
            return true;
        }

        // Siège Restreint
        const isInScope = lead.originAgencyId ? user.scopeAgencyIds.includes(lead.originAgencyId) : false;
        const isAssignedToMe = lead.assignedUser === user.id || (lead as any).assignedToUser === user.id;

        return isInScope || isAssignedToMe;
    }
};
