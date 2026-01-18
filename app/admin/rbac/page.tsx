'use client';

import RoleManager from '../../../components/admin/rbac/RoleManager';

export default function RolesPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Gestion des Rôles et Permissions</h1>
            <RoleManager />
        </div>
    );
}
