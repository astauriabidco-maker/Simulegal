'use client';

import { Suspense, useState, useEffect } from 'react';
import { useSearchParams } from 'next/navigation';
import dynamic from 'next/dynamic';
import { CRM } from '../../services/crmStore';
import { AlertCircle } from 'lucide-react';

const ClientPortal = dynamic(() => import('../../components/client/ClientPortal'), { ssr: false });
const RoleGuard = dynamic(() => import('../../components/auth/RoleGuard'), { ssr: false });

function EspaceClientContent() {
    const searchParams = useSearchParams();
    const [leadId, setLeadId] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const id = searchParams.get('id');
        const checkLead = async (targetId: string) => {
            try {
                // Mock verification or actual call depending on status
                // For now we trust the ID if provided, as authentication handles security
                if (targetId) {
                    setLeadId(targetId);
                }
            } catch (err) {
                console.error('Error checking lead:', err);
                setError('Impossible de charger le dossier');
            }
        };

        if (id) {
            checkLead(id);
        }
    }, [searchParams]);

    if (error) {
        return (
            <div className="min-h-screen flex items-center justify-center bg-slate-50">
                <div className="bg-white p-8 rounded-lg shadow-sm border border-red-100 max-w-md text-center">
                    <div className="w-12 h-12 bg-red-50 rounded-full flex items-center justify-center mx-auto mb-4">
                        <AlertCircle className="w-6 h-6 text-red-600" />
                    </div>
                    <h2 className="text-xl font-semibold text-slate-900 mb-2">Erreur</h2>
                    <p className="text-slate-600">{error}</p>
                </div>
            </div>
        );
    }

    if (!leadId) {
        return (
            <div className="min-h-screen flex items-center justify-center bg-slate-50">
                <div className="w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
            </div>
        );
    }

    return (
        <RoleGuard allowedRoles={['CLIENT']}>
            <ClientPortal leadId={leadId} />
        </RoleGuard>
    );
}

export default function EspaceClientPage() {
    return (
        <Suspense fallback={
            <div className="min-h-screen flex items-center justify-center bg-slate-50">
                <div className="w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
            </div>
        }>
            <EspaceClientContent />
        </Suspense>
    );
}
