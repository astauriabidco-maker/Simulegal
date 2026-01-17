'use client';

import React, { useState } from 'react';
import DashboardLayout from '../../../components/admin/DashboardLayout';
import AgencyDashboard from '../../../components/backoffice/AgencyDashboard';

// Simule l'utilisateur connecté (Franchisé)
const CURRENT_USER = {
    name: 'Marie Dupont',
    role: 'AGENCY_MANAGER' as const,
    agencyName: 'Franchise Marseille'
};

export default function AgencyLeadsPage() {
    const [activeMenu, setActiveMenu] = useState('leads');

    return (
        <DashboardLayout
            currentUser={CURRENT_USER}
            activeMenuItem={activeMenu}
            onMenuClick={setActiveMenu}
        >
            <AgencyDashboard />
        </DashboardLayout>
    );
}
