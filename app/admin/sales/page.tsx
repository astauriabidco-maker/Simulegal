'use client';

import React from 'react';
import dynamic from 'next/dynamic';
import SalesDashboard from '../../../components/sales/SalesDashboard';

// Dynamic import for RoleGuard to avoid SSR issues with auth
const RoleGuard = dynamic(() => import('../../../components/auth/RoleGuard'), { ssr: false });

export default function SalesPage() {
    return (
        <RoleGuard allowedRoles={['HQ_ADMIN', 'AGENCY_MANAGER', 'SUPERADMIN']}>
            <div className="h-screen w-full">
                <SalesDashboard />
            </div>
        </RoleGuard>
    );
}
