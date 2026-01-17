'use client';

import React, { useState } from 'react';
import DashboardLayout from '../../../components/admin/DashboardLayout';
import HQDashboard from '../../../components/backoffice/HQDashboard';

// Simule l'utilisateur connecté
const CURRENT_USER = {
    name: 'Admin Principal',
    role: 'SUPER_ADMIN' as const,
    agencyName: 'Antigravity HQ (Paris)'
};

export default function DossiersPage() {
    const [activeMenu, setActiveMenu] = useState('dossiers');

    return (
        <DashboardLayout
            currentUser={CURRENT_USER}
            activeMenuItem={activeMenu}
            onMenuClick={setActiveMenu}
        >
            <HQDashboard />
        </DashboardLayout>
    );
}
