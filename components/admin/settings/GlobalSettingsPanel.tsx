'use client';

import React, { useState, useEffect } from 'react';
import {
    Building2, CreditCard, Bell, Cpu, Save,
    RefreshCcw, Eye, EyeOff, ShieldAlert,
    Mail, MessageSquare, HardDrive, Smartphone,
    CheckCircle, AlertTriangle
} from 'lucide-react';
import { SettingsStore, SystemSettings } from '../../../services/SettingsStore';

type TabType = 'COMPANY' | 'PAYMENT' | 'NOTIFICATIONS' | 'INTEGRATIONS';

export default function GlobalSettingsPanel() {
    const [settings, setSettings] = useState<SystemSettings | null>(null);
    const [activeTab, setActiveTab] = useState<TabType>('COMPANY');
    const [isSaving, setIsSaving] = useState(false);
    const [showSecrets, setShowSecrets] = useState<Record<string, boolean>>({});
    const [testResults, setTestResults] = useState<Record<string, { success: boolean, message: string } | null>>({});

    useEffect(() => {
        const load = async () => {
            const data = await SettingsStore.getSettings();
            setSettings(data);
        };
        load();
    }, []);

    const toggleSecret = (key: string) => {
        setShowSecrets(prev => ({ ...prev, [key]: !prev[key] }));
    };

    const handleSave = async (section: keyof SystemSettings, data: any) => {
        setIsSaving(true);
        try {
            const updated = await SettingsStore.saveSettings(section, data);
            setSettings(updated);
        } catch (error) {
            console.error('Save failed', error);
        } finally {
            setIsSaving(false);
        }
    };

    const runTest = async (type: string) => {
        setTestResults(prev => ({ ...prev, [type]: null }));
        const result = await SettingsStore.testConnection(type);
        setTestResults(prev => ({ ...prev, [type]: result }));
    };

    if (!settings) return null;

    return (
        <div className="space-y-6">
            <div className="flex justify-between items-center">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Paramètres Généraux</h1>
                    <p className="text-slate-500 font-medium text-sm">Configuration de l'infrastructure et de l'identité système.</p>
                </div>
                <div className="flex items-center gap-2 px-3 py-1.5 bg-amber-50 rounded-full border border-amber-100">
                    <ShieldAlert size={14} className="text-amber-600" />
                    <span className="text-[10px] font-black uppercase text-amber-700 tracking-tighter">Accès Super-Admin uniquement</span>
                </div>
            </div>

            {/* TABS HEADER */}
            <div className="flex p-1 bg-slate-100 rounded-2xl w-fit">
                {[
                    { id: 'COMPANY', label: 'Société', icon: Building2 },
                    { id: 'PAYMENT', label: 'Paiement', icon: CreditCard },
                    { id: 'NOTIFICATIONS', label: 'Notifications', icon: Bell },
                    { id: 'INTEGRATIONS', label: 'Intégrations', icon: Cpu },
                ].map(tab => (
                    <button
                        key={tab.id}
                        onClick={() => setActiveTab(tab.id as TabType)}
                        className={`flex items-center gap-2 px-6 py-3 rounded-xl font-black text-sm transition-all ${activeTab === tab.id
                            ? 'bg-white text-indigo-600 shadow-sm'
                            : 'text-slate-500 hover:text-slate-700'
                            }`}
                    >
                        <tab.icon size={16} />
                        {tab.label}
                    </button>
                ))}
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                {/* MAIN FORM */}
                <div className="lg:col-span-2 space-y-6">
                    {activeTab === 'COMPANY' && (
                        <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                            <div className="space-y-4">
                                <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Identité de l'entreprise</h3>
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
                        </div>
                    )}

                    {activeTab === 'PAYMENT' && (
                        <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                            <div className="space-y-6">
                                <div className="flex justify-between items-center">
                                    <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Passerelle de Paiement</h3>
                                    <div className={`px-4 py-1.5 rounded-full text-[10px] font-black uppercase tracking-widest border-2 ${settings.payment.mode === 'TEST'
                                        ? 'bg-amber-50 text-amber-600 border-amber-100'
                                        : 'bg-red-50 text-red-600 border-red-100'
                                        }`}>
                                        {settings.payment.mode === 'TEST' ? 'Mode Sablier (Test)' : 'Mode Production (Live)'}
                                    </div>
                                </div>

                                <div className="space-y-4">
                                    <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100">
                                        <div className="w-12 h-12 bg-white rounded-xl shadow-sm flex items-center justify-center">
                                            <CreditCard className="text-indigo-600" />
                                        </div>
                                        <div>
                                            <p className="font-black text-slate-900">Stripe Connect</p>
                                            <p className="text-xs text-slate-500 font-medium">Traitement des paiements clients et virements agences.</p>
                                        </div>
                                        <button
                                            onClick={() => handleSave('payment', { mode: settings.payment.mode === 'TEST' ? 'LIVE' : 'TEST' })}
                                            className="ml-auto px-4 py-2 bg-indigo-50 text-indigo-600 rounded-lg text-xs font-black uppercase hover:bg-indigo-100 transition-colors"
                                        >
                                            Basculer en {settings.payment.mode === 'TEST' ? 'Production' : 'Test'}
                                        </button>
                                    </div>

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

                                    <div className="pt-4 flex items-center gap-4">
                                        <button
                                            onClick={() => runTest('STRIPE')}
                                            className="flex items-center gap-2 px-6 py-3 bg-slate-900 text-white rounded-xl text-sm font-black hover:bg-black transition-all shadow-lg"
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
                        </div>
                    )}

                    {activeTab === 'NOTIFICATIONS' && (
                        <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
                            {/* SECTION EMAIL */}
                            <div className="space-y-6">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 bg-indigo-50 rounded-lg flex items-center justify-center text-indigo-600">
                                        <Mail size={18} />
                                    </div>
                                    <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Configuration Email (SMTP)</h3>
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
                                        className="flex items-center gap-2 px-5 py-2.5 bg-slate-100 text-slate-700 rounded-lg text-xs font-black hover:bg-slate-200 transition-all"
                                    >
                                        Envoyer un email de test
                                    </button>
                                </div>
                            </div>

                            {/* SECTION WHATSAPP */}
                            <div className="space-y-6 border-t border-slate-100 pt-8">
                                <div className="flex items-center justify-between">
                                    <div className="flex items-center gap-3">
                                        <div className="w-8 h-8 bg-[#25D366]/10 rounded-lg flex items-center justify-center text-[#25D366]">
                                            <MessageSquare size={18} />
                                        </div>
                                        <div>
                                            <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">WhatsApp Business API</h3>
                                            <p className="text-[10px] text-slate-400 font-bold uppercase tracking-tighter">Relances automatiques Clients</p>
                                        </div>
                                    </div>
                                    <button
                                        onClick={() => handleSave('notifications', { whatsappEnabled: !settings.notifications.whatsappEnabled })}
                                        className={`w-14 h-8 rounded-full relative transition-all ${settings.notifications.whatsappEnabled ? 'bg-indigo-600' : 'bg-slate-200'}`}
                                    >
                                        <div className={`absolute top-1 w-6 h-6 bg-white rounded-full transition-all ${settings.notifications.whatsappEnabled ? 'left-7' : 'left-1'}`} />
                                    </button>
                                </div>

                                {settings.notifications.whatsappEnabled && (
                                    <div className="grid grid-cols-1 gap-4 animate-in zoom-in-95 duration-300">
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
                        </div>
                    )}

                    {activeTab === 'INTEGRATIONS' && (
                        <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
                            {/* OCR */}
                            <div className="space-y-6">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 bg-purple-50 rounded-lg flex items-center justify-center text-purple-600">
                                        <Smartphone size={18} />
                                    </div>
                                    <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Vision & OCR (Lecture Documents)</h3>
                                </div>

                                <div className="grid grid-cols-2 gap-4">
                                    <div className="space-y-1.5 col-span-2">
                                        <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Fournisseur OCR</label>
                                        <select
                                            className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl font-bold focus:border-indigo-600 outline-none"
                                            value={settings.integrations.ocrProvider}
                                            onChange={e => handleSave('integrations', { ocrProvider: e.target.value })}
                                        >
                                            <option value="GOOGLE_VISION">Google Cloud Vision</option>
                                            <option value="MINDEE">Mindee API</option>
                                        </select>
                                    </div>
                                    <div className="col-span-2">
                                        <SettingInput
                                            label="Clé API OCR"
                                            value={settings.integrations.ocrApiKey}
                                            isSecret
                                            showSecret={showSecrets['ocr_key']}
                                            onToggleSecret={() => toggleSecret('ocr_key')}
                                            onChange={(v) => handleSave('integrations', { ocrApiKey: v })}
                                        />
                                    </div>
                                </div>
                            </div>

                            {/* STORAGE */}
                            <div className="space-y-6 border-t border-slate-100 pt-8">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 bg-blue-50 rounded-lg flex items-center justify-center text-blue-600">
                                        <HardDrive size={18} />
                                    </div>
                                    <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Stockage Sécurisé (S3)</h3>
                                </div>

                                <div className="grid grid-cols-2 gap-4">
                                    <SettingInput
                                        label="Nom du Bucket"
                                        value={settings.storage.bucketName}
                                        onChange={(v) => handleSave('storage', { bucketName: v })}
                                    />
                                    <SettingInput
                                        label="Région"
                                        value={settings.storage.region}
                                        onChange={(v) => handleSave('storage', { region: v })}
                                    />
                                    <SettingInput
                                        label="AWS Access Key"
                                        value={settings.storage.accessKey}
                                        onChange={(v) => handleSave('storage', { accessKey: v })}
                                    />
                                    <SettingInput
                                        label="AWS Secret Key"
                                        value={settings.storage.secretKey}
                                        isSecret
                                        showSecret={showSecrets['s3_secret']}
                                        onToggleSecret={() => toggleSecret('s3_secret')}
                                        onChange={(v) => handleSave('storage', { secretKey: v })}
                                    />
                                </div>

                                <div className="p-4 bg-amber-50 rounded-2xl border border-amber-100 flex gap-4">
                                    <ShieldAlert className="text-amber-600 shrink-0" size={24} />
                                    <p className="text-xs text-amber-800 font-medium italic">
                                        Ces clés permettent l'accès aux documents confidentiels des clients.
                                        Toute modification impacte immédiatement l'accessibilité des dossiers.
                                    </p>
                                </div>
                            </div>
                        </div>
                    )}
                </div>

                {/* PREVIEW SIDEBAR */}
                <div className="space-y-6">
                    <div className="bg-slate-900 rounded-[2rem] p-8 text-white shadow-2xl sticky top-6 overflow-hidden">
                        <div className="absolute top-0 right-0 p-8 opacity-10 rotate-12">
                            <Building2 size={120} />
                        </div>

                        <div className="relative z-10 space-y-6">
                            <h3 className="font-black uppercase text-xs tracking-[0.2em] text-indigo-400">Aperçu En-tête Facture</h3>

                            <div className="bg-white rounded-2xl p-6 text-slate-900 space-y-4 shadow-xl">
                                <div className="flex justify-between items-start border-b border-slate-100 pb-4">
                                    <div>
                                        <p className="font-black text-xs uppercase tracking-tight">{settings.company.name}</p>
                                        <p className="text-[9px] text-slate-500 font-bold leading-tight">
                                            {settings.company.address}<br />
                                            {settings.company.zipCode} {settings.company.city}
                                        </p>
                                    </div>
                                    <div className="w-10 h-10 bg-indigo-600 rounded-lg flex items-center justify-center text-white scale-75">
                                        <Building2 size={24} />
                                    </div>
                                </div>

                                <div className="space-y-1">
                                    <p className="text-[8px] font-black text-slate-400 uppercase tracking-widest">Identifiants Légaux</p>
                                    <div className="flex justify-between text-[9px] font-bold">
                                        <span className="text-slate-500">SIRET :</span>
                                        <span>{settings.company.siret}</span>
                                    </div>
                                    <div className="flex justify-between text-[9px] font-bold">
                                        <span className="text-slate-500">TVA :</span>
                                        <span>{settings.company.tvaNumber}</span>
                                    </div>
                                </div>

                                <div className="bg-slate-50 p-3 rounded-xl space-y-1 border border-slate-100">
                                    <p className="text-[8px] font-black text-slate-400 uppercase tracking-widest">Contact Client</p>
                                    <p className="text-[9px] font-bold">{settings.company.supportEmail}</p>
                                    <p className="text-[9px] font-bold">{settings.company.supportPhone}</p>
                                </div>
                            </div>

                            <div className="p-4 bg-white/5 rounded-2xl border border-white/10">
                                <p className="text-[10px] font-medium text-slate-400 leading-relaxed italic text-center">
                                    "Cet en-tête sera généré automatiquement sur tous les mandats, contrats et factures PDF."
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

function SettingInput({ label, value, type = 'text', onChange, isSecret, showSecret, onToggleSecret }: {
    label: string,
    value: string,
    type?: string,
    onChange: (v: string) => void,
    isSecret?: boolean,
    showSecret?: boolean,
    onToggleSecret?: () => void
}) {
    const [localValue, setLocalValue] = useState(value);

    return (
        <div className="space-y-1.5">
            <div className="flex justify-between items-center ml-1">
                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400">{label}</label>
                {isSecret && (
                    <button onClick={onToggleSecret} className="text-slate-400 hover:text-indigo-600">
                        {showSecret ? <EyeOff size={14} /> : <Eye size={14} />}
                    </button>
                )}
            </div>
            <input
                type={isSecret && !showSecret ? 'password' : type}
                value={localValue}
                onChange={e => setLocalValue(e.target.value)}
                onBlur={() => onChange(localValue)}
                className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl font-bold focus:border-indigo-600 focus:bg-white outline-none transition-all placeholder:text-slate-300"
            />
        </div>
    );
}
