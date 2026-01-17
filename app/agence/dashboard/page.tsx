'use client';

import AdminPage from '../../admin/page';
import RoleGuard from '../../../components/auth/RoleGuard';

export default function AgenceDashboardPage() {
    return (
        <RoleGuard allowedRoles={['AGENCY']}>
            <AdminPage />
        </RoleGuard>
    );
}
