'use client';

import React, { useState, useEffect } from 'react';
import { Mail, MessageSquare } from 'lucide-react';
import { SettingsStore, SystemSettings } from '../../../../services/SettingsStore';
import SettingInput from '../../../../components/admin/settings/SettingInput';
import TwilioTemplatesTab from '../../../../components/admin/settings/TwilioTemplatesTab';

export default function NotificationsSettingsPage() {
    const [settings, setSettings] = useState<SystemSettings | null>(null);
    const [showSecrets, setShowSecrets] = useState<Record<string, boolean>>({});

    useEffect(() => {
        SettingsStore.getSettings().then(setSettings);
    }, []);

    const toggleSecret = (key: string) => {
        setShowSecrets(prev => ({ ...prev, [key]: !prev[key] }));
    };

    const handleSave = async (section: keyof SystemSettings, data: any) => {
        const updated = await SettingsStore.saveSettings(section, data);
        setSettings(updated);
    };

    const runTest = async (type: string) => {
        await SettingsStore.testConnection(type);
    };

    if (!settings) return <div className="animate-pulse flex space-x-4">Chargement...</div>;

    return (
        <div className="space-y-12 animate-in fade-in duration-500">
            {/* SECTION EMAIL */}
            <div className="space-y-6">
                <div className="flex items-center gap-3 border-b border-slate-100 pb-4 mb-6">
                    <div className="w-8 h-8 bg-indigo-50 rounded-lg flex items-center justify-center text-indigo-600">
                        <Mail size={18} />
                    </div>
                    <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Configuration Email (SMTP)</h2>
                </div>

                <div className="grid grid-cols-3 gap-4">
                    <div className="col-span-2">
                        <SettingInput
                            label="Hôte SMTP"
                            value={settings.notifications.smtpHost}
                            onChange={(v) => handleSave('notifications', { smtpHost: v })}
                        />
                    </div>
                    <SettingInput
                        label="Port"
                        type="number"
                        value={settings.notifications.smtpPort.toString()}
                        onChange={(v) => handleSave('notifications', { smtpPort: parseInt(v) })}
                    />
                    <SettingInput
                        label="Utilisateur"
                        value={settings.notifications.smtpUser}
                        onChange={(v) => handleSave('notifications', { smtpUser: v })}
                    />
                    <SettingInput
                        label="Mot de passe"
                        value={settings.notifications.smtpPass}
                        isSecret
                        showSecret={showSecrets['smtp_pass']}
                        onToggleSecret={() => toggleSecret('smtp_pass')}
                        onChange={(v) => handleSave('notifications', { smtpPass: v })}
                    />
                </div>
                <div className="pt-2">
                    <button
                        onClick={() => runTest('SMTP')}
                        className="flex items-center gap-2 px-5 py-2.5 bg-slate-50 text-slate-700 border border-slate-200 rounded-xl text-xs font-black hover:bg-slate-100 transition-all shadow-sm"
                    >
                        Envoyer un email de test
                    </button>
                </div>
            </div>

            {/* SECTION WHATSAPP */}
            <div className="space-y-6 pt-8 border-t border-slate-100">
                <div className="flex items-center justify-between border-b border-slate-100 pb-4 mb-6">
                    <div className="flex items-center gap-3">
                        <div className="w-8 h-8 bg-[#25D366]/10 rounded-lg flex items-center justify-center text-[#25D366]">
                            <MessageSquare size={18} />
                        </div>
                        <div>
                            <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">WhatsApp Business API</h2>
                        </div>
                    </div>
                    <button
                        onClick={() => handleSave('notifications', { whatsappEnabled: !settings.notifications.whatsappEnabled })}
                        className={`w-14 h-8 rounded-full relative transition-all ${settings.notifications.whatsappEnabled ? 'bg-[#25D366]' : 'bg-slate-200'}`}
                    >
                        <div className={`absolute top-1 w-6 h-6 bg-white rounded-full transition-all shadow-sm border border-black/5 ${settings.notifications.whatsappEnabled ? 'left-7' : 'left-1'}`} />
                    </button>
                </div>

                {settings.notifications.whatsappEnabled && (
                    <div className="grid grid-cols-1 gap-4 animate-in fade-in slide-in-from-top-2 duration-300">
                        <SettingInput
                            label="Account Business ID"
                            value={settings.notifications.whatsappBusinessId || ''}
                            onChange={(v) => handleSave('notifications', { whatsappBusinessId: v })}
                        />
                        <SettingInput
                            label="Token d'accès Permanent"
                            value={settings.notifications.whatsappToken || ''}
                            isSecret
                            showSecret={showSecrets['wa_token']}
                            onToggleSecret={() => toggleSecret('wa_token')}
                            onChange={(v) => handleSave('notifications', { whatsappToken: v })}
                        />
                    </div>
                )}
            </div>

            <div className="pt-8 border-t border-slate-100">
                <TwilioTemplatesTab />
            </div>
        </div>
    );
}
