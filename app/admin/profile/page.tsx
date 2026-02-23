'use client';

import DashboardLayout from '../../../components/admin/DashboardLayout';
import ProfilePage from '../../../components/admin/profile/ProfilePage';
import { AuthStore } from '../../../services/authStore';
import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';

export default function ProfileRoutePage() {
    const [user, setUser] = useState<any>(null);
    const router = useRouter();

    useEffect(() => {
        const currentUser = AuthStore.getCurrentUser();
        if (!currentUser) {
            router.push('/staff-login');
            return;
        }
        setUser(currentUser);
    }, [router]);

    if (!user) return null;

    return (
        <DashboardLayout
            currentUser={{ name: user.name, role: user.role, agencyName: user.agencyName }}
            activeMenuItem="profile"
            onLogout={() => AuthStore.logout()}
        >
            <ProfilePage />
        </DashboardLayout>
    );
}
