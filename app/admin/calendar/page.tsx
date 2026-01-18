'use client';

import React from 'react';
import StaffCalendarView from '../../../components/admin/calendar/StaffCalendarView';
import { usePermission } from '../../../hooks/usePermission';

export default function CalendarPage() {
    const { user } = usePermission();

    if (!user) return null;

    return (
        <div className="h-[calc(100vh-2rem)]">
            <h1 className="text-2xl font-black text-slate-900 mb-6">Agenda & Rendez-vous</h1>
            <div className="h-[calc(100%-4rem)]">
                <StaffCalendarView
                    currentUserRole={user.role as any}
                    currentUserAgencyId={user.agencyId}
                />
            </div>
        </div>
    );
}
