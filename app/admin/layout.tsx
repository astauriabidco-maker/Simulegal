'use client';

import React, { Suspense } from 'react';
import { usePathname, useRouter, useSearchParams } from 'next/navigation';
import DashboardLayout from '../../components/admin/DashboardLayout';
import { usePermission } from '../../hooks/usePermission';
import { AuthStore } from '../../services/authStore';

function AdminLayoutInner({ children }: { children: React.ReactNode }) {
    const pathname = usePathname();
    const router = useRouter();
    const { user, isLoading } = usePermission();

    const searchParams = useSearchParams();

    // Map pathname to activeMenuItem
    const getActiveMenu = (path: string) => {
        if (path === '/admin') return 'overview';
        const parts = path.split('/');
        const base = parts[2] || 'overview';

        const tab = searchParams.get('tab');
        const view = searchParams.get('view');

        if (base === 'audit' && tab) {
            return `audit-veille-${tab}`;
        }
        if (base === 'blog' && tab) {
            return `blog-${tab}`;
        }
        if (base === 'sales' && view) {
            return `sales-${view}`;
        }
        if (base === 'franchise-leads' && view) {
            return `franchise-leads-${view}`;
        }
        if (base === 'settings' && tab) {
            if (tab === 'SERVICE_PRICING') return 'settings-pricing';
            if (tab === 'DOCUMENTS') return 'settings-docs';
            if (tab === 'LEGAL_DOCS') return 'settings-legal';
        }
        if (base === 'settings') return 'settings-general';
        if (base === 'staff') return 'management-staff';
        if (base === 'rbac') return 'management-roles';
        if (base === 'finances' && parts[3]) {
            return `finances-${parts[3]}`;
        }

        return base;
    };

    const handleLogout = () => {
        AuthStore.logout();
        router.push('/staff-login');
    };

    if (isLoading) {
        return (
            <div className="min-h-screen bg-slate-100 flex items-center justify-center">
                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full"></div>
            </div>
        );
    }

    if (!user) {
        return null;
    }

    return (
        <DashboardLayout
            currentUser={{
                name: user.name,
                role: user.role,
                agencyName: user.agencyName || 'Simulegal'
            }}
            activeMenuItem={getActiveMenu(pathname)}
            onLogout={handleLogout}
        >
            {children}
        </DashboardLayout>
    );
}

export default function AdminLayout({ children }: { children: React.ReactNode }) {
    return (
        <Suspense fallback={
            <div className="min-h-screen bg-slate-100 flex items-center justify-center">
                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full"></div>
            </div>
        }>
            <AdminLayoutInner>{children}</AdminLayoutInner>
        </Suspense>
    );
}
