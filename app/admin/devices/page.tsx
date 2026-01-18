'use client';

import DeviceManagementPanel from '../../../components/admin/DeviceManagementPanel';

export default function DevicesPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Gestion des Terminaux</h1>
            <DeviceManagementPanel />
        </div>
    );
}
