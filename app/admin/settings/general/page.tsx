'use client';

import React, { useState, useEffect } from 'react';
import { SettingsStore, SystemSettings } from '../../../../services/SettingsStore';
import SettingInput from '../../../../components/admin/settings/SettingInput';

export default function GeneralSettingsPage() {
    const [settings, setSettings] = useState<SystemSettings | null>(null);

    useEffect(() => {
        SettingsStore.getSettings().then(setSettings);
    }, []);

    const handleSave = async (section: keyof SystemSettings, data: any) => {
        const updated = await SettingsStore.saveSettings(section, data);
        setSettings(updated);
    };

    if (!settings) return <div className="animate-pulse flex space-x-4">Chargement...</div>;

    return (
        <div className="space-y-6 animate-in fade-in duration-500">
            <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest border-b border-slate-100 pb-4">Identité de l'entreprise</h2>

            <div className="grid grid-cols-2 gap-4">
                <SettingInput
                    label="Nom Commercial"
                    value={settings.company.name}
                    onChange={(v) => handleSave('company', { name: v })}
                />
                <SettingInput
                    label="SIRET"
                    value={settings.company.siret}
                    onChange={(v) => handleSave('company', { siret: v })}
                />
                <div className="col-span-2">
                    <SettingInput
                        label="Adresse Siège Social"
                        value={settings.company.address}
                        onChange={(v) => handleSave('company', { address: v })}
                    />
                </div>
                <SettingInput
                    label="Code Postal"
                    value={settings.company.zipCode}
                    onChange={(v) => handleSave('company', { zipCode: v })}
                />
                <SettingInput
                    label="Ville"
                    value={settings.company.city}
                    onChange={(v) => handleSave('company', { city: v })}
                />
                <SettingInput
                    label="Email Support"
                    value={settings.company.supportEmail}
                    onChange={(v) => handleSave('company', { supportEmail: v })}
                />
                <SettingInput
                    label="Téléphone Support"
                    value={settings.company.supportPhone}
                    onChange={(v) => handleSave('company', { supportPhone: v })}
                />
            </div>
        </div>
    );
}
