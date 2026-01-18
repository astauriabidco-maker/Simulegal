'use client';

import UserManagementPanel from '../../../components/admin/UserManagementPanel';

export default function StaffPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Gestion de l'Équipe</h1>
            <UserManagementPanel />
        </div>
    );
}
