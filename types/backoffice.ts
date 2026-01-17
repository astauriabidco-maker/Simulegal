import { Lead } from '../services/crmStore';

// Types pour le réseau d'agences
export type AgencyType = 'HQ' | 'OWNED' | 'FRANCHISE' | 'KIOSK';

export interface Agency {
    id: string;
    name: string;
    type: AgencyType;
    zipCodes: string[]; // Zones couvertes pour les rappels (ex: ['75001', '75002'])
    commissionRate: number; // % sur les dossiers apportés
}

export interface UserRole {
    id: string;
    role: 'SUPER_ADMIN' | 'CASE_WORKER' | 'AGENCY_MANAGER';
    agencyId: string; // Lien vers l'agence d'appartenance
}

// Type pour les commentaires/notes sur un dossier
export interface Comment {
    id: string;
    userId: string;
    content: string;
    createdAt: string;
}

// Extension du Lead pour le traçage Back-Office
export interface BackOfficeLead extends Lead {
    originAgencyId: string; // Qui a apporté l'affaire ?
    assignedToUser?: string; // Quel juriste du siège traite le dossier ?
    currentStage: 'NEW' | 'DOCS_MISSING' | 'SUBMITTED_PREF' | 'DECISION_WAIT' | 'CLOSED';
    notes: Comment[];
}

// Données simulées pour les agences
export const MOCK_AGENCIES: Agency[] = [
    {
        id: 'HQ-001',
        name: 'Antigravity HQ (Paris)',
        type: 'HQ',
        zipCodes: ['75001', '75002', '75003', '75004', '75005', '75006', '75007', '75008', '75009', '75010'],
        commissionRate: 0
    },
    {
        id: 'OWN-001',
        name: 'Agence Lyon',
        type: 'OWNED',
        zipCodes: ['69001', '69002', '69003', '69004', '69005', '69006', '69007', '69008', '69009'],
        commissionRate: 0
    },
    {
        id: 'FRAN-001',
        name: 'Franchise Marseille',
        type: 'FRANCHISE',
        zipCodes: ['13001', '13002', '13003', '13004', '13005', '13006', '13007', '13008'],
        commissionRate: 15
    },
    {
        id: 'KIOSK-001',
        name: 'Kiosque Gare du Nord',
        type: 'KIOSK',
        zipCodes: ['75010', '75018', '75019'],
        commissionRate: 10
    }
];
