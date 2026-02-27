'use client';

import React from 'react';
import LegalDocumentsTab from '../../../../components/admin/settings/LegalDocumentsTab';

export default function LegalSettingsPage() {
    return (
        <div className="animate-in fade-in duration-500">
            <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest border-b border-slate-100 pb-4 mb-6">Contrats & Conditions Générales</h2>
            <LegalDocumentsTab />
        </div>
    );
}
