'use client';

import React from 'react';
import DocumentCatalogTab from '../../../../components/admin/settings/DocumentCatalogTab';

export default function CatalogSettingsPage() {
    return (
        <div className="animate-in fade-in duration-500">
            <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest border-b border-slate-100 pb-4 mb-6">Référentiel des Pièces Client</h2>
            <DocumentCatalogTab />
        </div>
    );
}
