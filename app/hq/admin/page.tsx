'use client';

import AdminPage from '../../admin/page';
import RoleGuard from '../../../components/auth/RoleGuard';

export default function HQAdminPage() {
    return (
        <RoleGuard allowedRoles={['HQ_ADMIN']}>
            <AdminPage />
        </RoleGuard>
    );
}
