'use client';

import React, { useState, useEffect } from 'react';
import { useSearchParams } from 'next/navigation';
import ClientPortal from '../../components/client/ClientPortal';
import { CRM } from '../../services/crmStore';
import { AlertCircle } from 'lucide-react';
import RoleGuard from '../../components/auth/RoleGuard';

export default function EspaceClientPage() {
    const searchParams = useSearchParams();
    const [leadId, setLeadId] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const id = searchParams.get('id');
        if (id) {
            // Vérifie que le lead existe
            const lead = CRM.getLeadById(id);
            if (lead) {
                setLeadId(id);
                localStorage.setItem('active_lead_id', id); // Définit la session pour RoleGuard
            } else {
                setError('Dossier introuvable. Vérifiez votre lien d\'accès.');
            }
        } else {
            // Si pas d'ID dans l'URL, vérifie s'il y a une session active
            const session = localStorage.getItem('active_lead_id');
            if (session) {
                setLeadId(session);
            } else {
                setError('Aucun identifiant de dossier fourni.');
            }
        }
    }, [searchParams]);

    if (error) {
        // ... (reste identique)
    }

    if (!leadId) {
        return (
            <div className="min-h-screen bg-slate-100 flex items-center justify-center">
                <div className="w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
            </div>
        );
    }

    return (
        <RoleGuard allowedRoles={['CLIENT']}>
            <ClientPortal
                leadId={leadId}
                isOpen={true}
                onClose={() => {
                    localStorage.removeItem('active_lead_id');
                    window.location.href = '/';
                }}
            />
        </RoleGuard>
    );
}
