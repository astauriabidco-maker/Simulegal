'use client';

import React from 'react';
import { usePathname, useRouter } from 'next/navigation';
import DashboardLayout from '../../components/admin/DashboardLayout';
import { usePermission } from '../../hooks/usePermission';
import { AuthStore } from '../../services/authStore';

export default function AdminLayout({ children }: { children: React.ReactNode }) {
    const pathname = usePathname();
    const router = useRouter();
    const { user, isLoading } = usePermission();

    // Map pathname to activeMenuItem
    const getActiveMenu = (path: string) => {
        if (path === '/admin') return 'overview';
        // Extract the second segment: /admin/network -> network
        const parts = path.split('/');
        // parts[0] = "", parts[1] = "admin", parts[2] = "network"
        return parts[2] || 'overview'; // Default to overview
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
        // Redirect handled by RoleGuard usually, but as a fallback:
        // router.push('/staff-login');
        // Let's return a loader while redirect happens elsewhere or show access denied
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
