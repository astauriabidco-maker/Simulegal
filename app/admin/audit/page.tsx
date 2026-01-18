'use client';

import AuditVeillePanel from '../../../components/backoffice/AuditVeillePanel';

export default function AuditPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Audit et Veille Juridique</h1>
            <AuditVeillePanel />
        </div>
    );
}
