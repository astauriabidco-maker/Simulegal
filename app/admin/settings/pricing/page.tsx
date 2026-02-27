'use client';

import React from 'react';
import ServicePricingTab from '../../../../components/admin/settings/ServicePricingTab';
import ServiceManagerTab from '../../../../components/admin/settings/ServiceManagerTab';

export default function PricingSettingsPage() {
    return (
        <div className="space-y-12 animate-in fade-in duration-500">
            <div>
                <ServiceManagerTab />
            </div>

            <div className="pt-8 border-t border-slate-100">
                <ServicePricingTab />
            </div>
        </div>
    );
}
