import { Lead, LeadNote } from '../services/crmStore';
import { WorkflowStage } from '../services/WorkflowService';

// Types pour le réseau d'agences
export type AgencyType = 'HQ' | 'OWNED' | 'FRANCHISE' | 'CORNER' | 'RELAY_POINT';

export interface Agency {
    id: string;
    name: string;
    type: AgencyType;
    region: string; // ex: "IDF", "PACA", "AURA"
    zipCodes: string[]; // Zones couvertes pour les rappels (ex: ['75001', '75002'])
    commissionRate: number; // 0% si OWNED (généralement), X% si FRANCHISE
    address?: string;
}

export interface UserRole {
    id: string;
    role: 'SUPER_ADMIN' | 'HQ_ADMIN' | 'AGENCY_MANAGER' | 'CASE_WORKER' | 'SALES' | 'KIOSK_AGENT';
    agencyId: string; // Lien vers l'agence d'appartenance
}

export interface StaffUser {
    id: string;
    name: string;
    email: string;
    isActive: boolean;
    role: 'HQ_ADMIN' | 'AGENCY_MANAGER' | 'CASE_WORKER' | 'SALES' | 'KIOSK_AGENT';

    // RATTACHEMENT PRIMAIRE (Où travaille-t-il physiquement ?)
    // - Si 'HQ' ou null : Il est au Siège.
    // - Si ID Agence : Il est employé de cette agence.
    homeAgencyId?: string;

    // PÉRIMÈTRE DE SUPERVISION (Pour le Siège uniquement)
    // - Si vide et role HQ : Voit TOUT (Super Admin).
    // - Si rempli : Ne voit QUE ces agences spécifiques.
    scopeAgencyIds?: string[];
}

// Extension du Lead pour le traçage Back-Office
export interface BackOfficeLead extends Lead {
    originAgencyId: string; // ID agence ou 'HQ'
    assignedToUser?: string; // Quel juriste du siège traite le dossier ?
    currentStage: WorkflowStage;
}

// Données simulées pour les agences
export const MOCK_AGENCIES: Agency[] = [
    {
        id: 'HQ-001',
        name: 'SimuLegal HQ (Siège Paris)',
        type: 'HQ',
        region: 'IDF',
        zipCodes: ['75001', '75002', '75003', '75004', '75005'],
        commissionRate: 0,
        address: '8 Rue de la Paix, 75002 Paris'
    },
    {
        id: 'OWN-001',
        name: 'SimuLegal Direct Lyon',
        type: 'OWNED',
        region: 'AURA',
        zipCodes: ['69001', '69002', '69003', '69004', '69005'],
        commissionRate: 0,
        address: '15 Place Bellecour, 69002 Lyon'
    },
    {
        id: 'FRAN-001',
        name: 'Franchise SimuLegal Marseille',
        type: 'FRANCHISE',
        region: 'PACA',
        zipCodes: ['13001', '13002', '13003', '13004'],
        commissionRate: 15,
        address: '40 Quai du Port, 13002 Marseille'
    },
    {
        id: 'RELAY-001',
        name: 'SimuLegal Corner Bordeaux',
        type: 'CORNER',
        region: 'NAQ',
        zipCodes: ['33000', '33100'],
        commissionRate: 5,
        address: '12 Cours de l\'Intendance, 33000 Bordeaux'
    }
];
