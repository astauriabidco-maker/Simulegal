'use client';

import React from 'react';
import AgendaDashboard from '../../../components/admin/calendar/AgendaDashboard';
import { usePermission } from '../../../hooks/usePermission';

export default function AgendaDashboardPage() {
    const { user } = usePermission();

    if (!user) return null;

    return (
        <div className="h-[calc(100vh-2rem)]">
            <AgendaDashboard />
        </div>
    );
}
