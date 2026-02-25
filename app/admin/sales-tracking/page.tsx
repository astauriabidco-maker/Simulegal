'use client';

import React from 'react';
import dynamic from 'next/dynamic';

const RoleGuard = dynamic(() => import('../../../components/auth/RoleGuard'), { ssr: false });
const SalesTrackingDashboard = dynamic(() => import('../../../components/sales/SalesTrackingDashboard'), { ssr: false });

export default function SalesTrackingPage() {
    return (
        <RoleGuard allowedRoles={['HQ_ADMIN', 'AGENCY_MANAGER', 'SUPERADMIN']}>
            <div className="h-full w-full">
                <SalesTrackingDashboard />
            </div>
        </RoleGuard>
    );
}
