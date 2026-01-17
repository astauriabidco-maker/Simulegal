'use client';

import React, { useState, useEffect } from 'react';
import { useSearchParams } from 'next/navigation';
import ClientPortal from '../../components/client/ClientPortal';
import { CRM } from '../../services/crmStore';
import { AlertCircle } from 'lucide-react';

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
            } else {
                setError('Dossier introuvable. Vérifiez votre lien d\'accès.');
            }
        } else {
            setError('Aucun identifiant de dossier fourni.');
        }
    }, [searchParams]);

    if (error) {
        return (
            <div className="min-h-screen bg-slate-100 flex items-center justify-center p-4">
                <div className="bg-white rounded-2xl shadow-lg p-8 max-w-md text-center">
                    <div className="w-16 h-16 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4">
                        <AlertCircle className="text-red-600" size={32} />
                    </div>
                    <h1 className="text-xl font-black text-slate-900 mb-2">Accès refusé</h1>
                    <p className="text-slate-500">{error}</p>
                    <a
                        href="/"
                        className="inline-block mt-6 bg-indigo-600 hover:bg-indigo-700 text-white px-6 py-3 rounded-xl font-bold transition-colors"
                    >
                        Retour à l'accueil
                    </a>
                </div>
            </div>
        );
    }

    if (!leadId) {
        return (
            <div className="min-h-screen bg-slate-100 flex items-center justify-center">
                <div className="w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
            </div>
        );
    }

    return (
        <ClientPortal
            leadId={leadId}
            isOpen={true}
            onClose={() => window.location.href = '/'}
        />
    );
}
