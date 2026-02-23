'use client';

import React from 'react';
import AgendaDashboard from '../../../components/admin/calendar/AgendaDashboard';
import { usePermission } from '../../../hooks/usePermission';
import AiReportingChat from '../../../components/admin/dashboard/AiReportingChat';

export default function AgendaDashboardPage() {
    const { user } = usePermission();

    if (!user) return null;

    return (
        <div className="h-[calc(100vh-2rem)] flex flex-col">
            <div className="px-8 pt-8">
                <AiReportingChat />
            </div>
            <div className="flex-1 overflow-hidden">
                <AgendaDashboard />
            </div>
        </div>
    );
}
