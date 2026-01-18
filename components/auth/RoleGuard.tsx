'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import AuthStore, { UserRole } from '../../services/authStore';

// Mapping des rôles demandés par l'utilisateur vers les rôles internes
// HQ_ADMIN -> HQ ou SUPERADMIN
// AGENCY -> AGENCY
// CLIENT -> Session client (basée sur leadId)

export type ProtectedRole = 'CLIENT' | 'AGENCY' | 'HQ_ADMIN' | 'SUPERADMIN' | 'AGENCY_MANAGER';

interface RoleGuardProps {
    children: React.ReactNode;
    allowedRoles: ProtectedRole[];
}

export default function RoleGuard({ children, allowedRoles }: RoleGuardProps) {
    const router = useRouter();
    const [isAuthorized, setIsAuthorized] = useState<boolean | null>(null);

    useEffect(() => {
        const checkAuth = () => {
            const user = AuthStore.getCurrentUser();

            // 1. Vérifie si l'utilisateur est connecté (Admin/Staff)
            if (user) {
                const currentRole = user.role;
                let normalizedRole: ProtectedRole;

                if (currentRole === 'SUPERADMIN') {
                    normalizedRole = 'SUPERADMIN';
                } else if (currentRole === 'HQ' || currentRole === 'HQ_ADMIN') {
                    normalizedRole = 'HQ_ADMIN';
                } else if (currentRole === 'AGENCY_MANAGER') {
                    normalizedRole = 'AGENCY_MANAGER';
                } else {
                    normalizedRole = 'AGENCY';
                }

                if (allowedRoles.includes(normalizedRole)) {
                    setIsAuthorized(true);
                } else {
                    console.error(`SecurityAlert: User ${user.id} attempted unauthorized access to a protected area restricted to ${allowedRoles.join(', ')}`);
                    setIsAuthorized(false);
                    router.push('/forbidden');
                }
                return;
            }

            // 2. Vérifie si c'est un CLIENT (Basé sur le localStorage ou paramètre spécifique)
            // Note: Pour le moment, l'espace client utilise leadId dans l'URL. 
            // On pourrait stocker une session client légère.
            const leadSession = localStorage.getItem('active_lead_id');
            if (leadSession && allowedRoles.includes('CLIENT')) {
                setIsAuthorized(true);
                return;
            }

            // 3. Pas connecté -> Redirection
            console.log('[RoleGuard] Redirecting to login (no session found)');
            if (allowedRoles.includes('CLIENT')) {
                router.push('/connexion');
            } else {
                router.push('/staff-login');
            }
            setIsAuthorized(false);
        };

        checkAuth();
    }, [allowedRoles, router]);

    if (isAuthorized === null) {
        return (
            <div className="min-h-screen bg-slate-50 flex items-center justify-center">
                <div className="w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
            </div>
        );
    }

    return isAuthorized ? <>{children}</> : null;
}
