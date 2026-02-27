'use client';

import React, { useState, useEffect } from 'react';
import { CreditCard, RefreshCcw, CheckCircle, AlertTriangle } from 'lucide-react';
import { SettingsStore, SystemSettings } from '../../../../services/SettingsStore';
import SettingInput from '../../../../components/admin/settings/SettingInput';

export default function BillingSettingsPage() {
    const [settings, setSettings] = useState<SystemSettings | null>(null);
    const [showSecrets, setShowSecrets] = useState<Record<string, boolean>>({});
    const [testResults, setTestResults] = useState<Record<string, { success: boolean, message: string } | null>>({});

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
        setTestResults(prev => ({ ...prev, [type]: null }));
        const result = await SettingsStore.testConnection(type);
        setTestResults(prev => ({ ...prev, [type]: result }));
    };

    if (!settings) return <div className="animate-pulse flex space-x-4">Chargement...</div>;

    return (
        <div className="space-y-6 animate-in fade-in duration-500">
            <div className="flex justify-between items-center border-b border-slate-100 pb-4 mb-6">
                <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Passerelle de Paiement</h2>
                <div className={`px-4 py-1.5 rounded-full text-[10px] font-black uppercase tracking-widest border border-dashed ${settings.payment.mode === 'TEST'
                    ? 'bg-amber-50 text-amber-600 border-amber-200'
                    : 'bg-red-50 text-red-600 border-red-200'
                    }`}>
                    {settings.payment.mode === 'TEST' ? 'Mode Sablier (Test)' : 'Mode Production (Live)'}
                </div>
            </div>

            <div className="space-y-4">
                <div className="flex items-center gap-4 p-5 bg-slate-50 rounded-2xl border border-slate-100 mb-8">
                    <div className="w-12 h-12 bg-white rounded-xl shadow-sm border border-slate-100 flex items-center justify-center">
                        <CreditCard className="text-indigo-600" />
                    </div>
                    <div>
                        <p className="font-black text-slate-900">Stripe Connect</p>
                        <p className="text-xs text-slate-500 font-medium mt-0.5">Traitement des paiements clients et virements agences.</p>
                    </div>
                    <button
                        onClick={() => handleSave('payment', { mode: settings.payment.mode === 'TEST' ? 'LIVE' : 'TEST' })}
                        className="ml-auto px-4 py-2 bg-white text-indigo-600 border border-slate-200 rounded-xl text-xs font-black uppercase hover:bg-indigo-50 hover:border-indigo-200 transition-all shadow-sm"
                    >
                        Basculer en {settings.payment.mode === 'TEST' ? 'Production' : 'Test'}
                    </button>
                </div>

                <div className="grid grid-cols-1 gap-4">
                    <SettingInput
                        label="Clé Publique (Publishable Key)"
                        value={settings.payment.publicKey}
                        onChange={(v) => handleSave('payment', { publicKey: v })}
                    />

                    <SettingInput
                        label="Clé Secrète"
                        value={settings.payment.secretKey}
                        isSecret
                        showSecret={showSecrets['stripe_secret']}
                        onToggleSecret={() => toggleSecret('stripe_secret')}
                        onChange={(v) => handleSave('payment', { secretKey: v })}
                    />
                </div>

                <div className="pt-4 flex items-center gap-4">
                    <button
                        onClick={() => runTest('STRIPE')}
                        className="flex items-center gap-2 px-6 py-3 bg-slate-900 text-white rounded-xl text-sm font-black hover:bg-black transition-all shadow-md"
                    >
                        <RefreshCcw size={16} className={testResults['STRIPE'] === null ? 'animate-spin' : ''} />
                        Tester la connexion Stripe
                    </button>
                    {testResults['STRIPE'] && (
                        <div className={`text-xs font-bold flex items-center gap-2 ${testResults['STRIPE'].success ? 'text-emerald-600' : 'text-red-500'}`}>
                            {testResults['STRIPE'].success ? <CheckCircle size={14} /> : <AlertTriangle size={14} />}
                            {testResults['STRIPE'].message}
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
}
