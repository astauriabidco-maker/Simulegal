'use client';

import React, { useState, useEffect } from 'react';
import { SettingsStore, SystemSettings, IntegrationSettings } from '../../../../services/SettingsStore';
import IntegrationsTab from '../../../../components/admin/settings/IntegrationsTab';
import SystemUsersTab from '../../../../components/admin/settings/SystemUsersTab';
import PipelineAutomationsTab from '../../../../components/admin/settings/PipelineAutomationsTab';

export default function AdvancedSettingsPage() {
    const [settings, setSettings] = useState<SystemSettings | null>(null);

    useEffect(() => {
        SettingsStore.getSettings().then(setSettings);
    }, []);

    if (!settings) return <div className="animate-pulse flex space-x-4">Chargement...</div>;

    const handleIntegrationUpdate = (val: IntegrationSettings) => {
        setSettings(prev => prev ? { ...prev, integrations: val } : prev);
        SettingsStore.saveSettings('integrations', val).catch(err => {
            console.error('[IntegrationsTab] Backend save failed:', err);
        });
    };

    return (
        <div className="space-y-12 animate-in fade-in duration-500">
            <div>
                <IntegrationsTab settings={settings.integrations} onUpdate={handleIntegrationUpdate} />
            </div>

            <div className="pt-8 border-t border-slate-100">
                <SystemUsersTab />
            </div>

            <div className="pt-8 border-t border-slate-100">
                <div className="bg-slate-900 rounded-[2rem] p-8 border border-slate-800 shadow-sm">
                    <PipelineAutomationsTab />
                </div>
            </div>
        </div>
    );
}
