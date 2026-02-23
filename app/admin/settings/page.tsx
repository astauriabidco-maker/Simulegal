'use client';

import { useSearchParams } from 'next/navigation';
import RoleGuard from '../../../components/auth/RoleGuard';
import GlobalSettingsPanel from '../../../components/admin/settings/GlobalSettingsPanel';

export default function AdminSettingsPage() {
    const searchParams = useSearchParams();
    const initialTab = searchParams.get('tab') || undefined;

    return (
        <RoleGuard allowedRoles={['SUPERADMIN']}>
            <div className="p-8">
                <GlobalSettingsPanel initialTab={initialTab} />
            </div>
        </RoleGuard>
    );
}
