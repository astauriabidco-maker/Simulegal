'use client';

import FranchiseMasterPanel from '../../../components/admin/FranchiseMasterPanel';

export default function NetworkPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Réseau d'Agences</h1>
            <FranchiseMasterPanel />
        </div>
    );
}
