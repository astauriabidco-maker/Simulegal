'use client';

import RoleGuard from '../../../components/auth/RoleGuard';
import GlobalSettingsPanel from '../../../components/admin/settings/GlobalSettingsPanel';

export default function AdminSettingsPage() {
    return (
        <RoleGuard allowedRoles={['SUPERADMIN']}>
            <div className="p-8">
                <GlobalSettingsPanel />
            </div>
        </RoleGuard>
    );
}
