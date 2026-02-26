'use client';

import { useSearchParams } from 'next/navigation';
import AuditVeillePanel from '../../../components/backoffice/AuditVeillePanel';

export default function AuditPage() {
    const searchParams = useSearchParams();
    const initialTab = searchParams.get('tab') || undefined;

    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Audit et Veille Juridique</h1>
            <AuditVeillePanel initialTab={initialTab} />
        </div>
    );
}
