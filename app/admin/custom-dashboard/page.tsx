'use client';

import DashboardLayout from '../../../components/admin/DashboardLayout';
import DashboardBuilder from '../../../components/admin/DashboardBuilder';
import { AuthStore } from '../../../services/authStore';

export default function CustomDashboardPage() {
    const user = AuthStore.getCurrentUser();
    if (!user) return null;

    return (
        <DashboardLayout
            currentUser={{ name: user.name, role: user.role, agencyName: user.agencyName }}
            activeMenuItem="custom-dashboard"
        >
            <DashboardBuilder />
        </DashboardLayout>
    );
}
