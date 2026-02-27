'use client';

import React, { useState, useEffect } from 'react';
import {
    Building2, CreditCard, Bell, Cpu, Save,
    RefreshCcw, Eye, EyeOff, ShieldAlert,
    Mail, MessageSquare, HardDrive, Smartphone,
    CheckCircle, AlertTriangle, Users, DollarSign, FileText, Zap, FileStack, LayoutGrid, ChevronRight, ArrowLeft
} from 'lucide-react';
import { SettingsStore, SystemSettings, IntegrationSettings } from '../../../services/SettingsStore';
import IntegrationsTab from './IntegrationsTab';
import SystemUsersTab from './SystemUsersTab';
import TwilioTemplatesTab from './TwilioTemplatesTab';
import ServicePricingTab from './ServicePricingTab';
import LegalDocumentsTab from './LegalDocumentsTab';
import PipelineAutomationsTab from './PipelineAutomationsTab';
import DocumentCatalogTab from './DocumentCatalogTab';
import ServiceManagerTab from './ServiceManagerTab';

type TabType = 'COMPANY' | 'PAYMENT' | 'NOTIFICATIONS' | 'WHATSAPP_TEMPLATES' | 'SERVICE_PRICING' | 'SERVICES' | 'DOCUMENTS' | 'LEGAL_DOCS' | 'AUTOMATIONS' | 'INTEGRATIONS' | 'SYSTEM_USERS';

const VALID_TABS: TabType[] = ['COMPANY', 'PAYMENT', 'NOTIFICATIONS', 'WHATSAPP_TEMPLATES', 'SERVICE_PRICING', 'SERVICES', 'DOCUMENTS', 'LEGAL_DOCS', 'AUTOMATIONS', 'INTEGRATIONS', 'SYSTEM_USERS'];

const CATEGORIES = [
    {
        id: 'legal',
        title: 'Identité & Légal',
        description: 'Informations de l\'entreprise, contrats et conditions générales.',
        icon: Building2,
        color: 'text-indigo-600',
        bgColor: 'bg-indigo-50',
        hoverBg: 'hover:bg-indigo-800/5',
        borderColor: 'border-indigo-100',
        borderHover: 'hover:border-indigo-300',
        tabs: [
            { id: 'COMPANY', label: 'Société', icon: Building2 },
            { id: 'LEGAL_DOCS', label: 'CGV & Contrats', icon: FileText }
        ]
    },
    {
        id: 'finance',
        title: 'Finance & Offres',
        description: 'Paiements Stripe, tarifs, et catalogue de services.',
        icon: CreditCard,
        color: 'text-emerald-600',
        bgColor: 'bg-emerald-50',
        hoverBg: 'hover:bg-emerald-800/5',
        borderColor: 'border-emerald-100',
        borderHover: 'hover:border-emerald-300',
        tabs: [
            { id: 'PAYMENT', label: 'Paiement', icon: CreditCard },
            { id: 'SERVICE_PRICING', label: 'Tarifs', icon: DollarSign },
            { id: 'SERVICES', label: 'Services', icon: LayoutGrid }
        ]
    },
    {
        id: 'communication',
        title: 'Communication & Process',
        description: 'Emails, WhatsApp business et automatisations de pipeline.',
        icon: Bell,
        color: 'text-amber-600',
        bgColor: 'bg-amber-50',
        hoverBg: 'hover:bg-amber-800/5',
        borderColor: 'border-amber-100',
        borderHover: 'hover:border-amber-300',
        tabs: [
            { id: 'NOTIFICATIONS', label: 'Notifications', icon: Bell },
            { id: 'WHATSAPP_TEMPLATES', label: 'WhatsApp', icon: Smartphone },
            { id: 'AUTOMATIONS', label: 'Automatisations', icon: Zap }
        ]
    },
    {
        id: 'system',
        title: 'Système & IT',
        description: 'Pièces requises, intégrations API et administration utilisateurs.',
        icon: Cpu,
        color: 'text-violet-600',
        bgColor: 'bg-violet-50',
        hoverBg: 'hover:bg-violet-800/5',
        borderColor: 'border-violet-100',
        borderHover: 'hover:border-violet-300',
        tabs: [
            { id: 'DOCUMENTS', label: 'Pièces', icon: FileStack },
            { id: 'INTEGRATIONS', label: 'Intégrations', icon: Cpu },
            { id: 'SYSTEM_USERS', label: 'Système', icon: Users }
        ]
    }
];

export default function GlobalSettingsPanel({ initialTab }: { initialTab?: string }) {
    const defaultTab = (initialTab && VALID_TABS.includes(initialTab as TabType)) ? initialTab as TabType : null;
    let initialCategory = null;

    if (defaultTab) {
        initialCategory = CATEGORIES.find(c => c.tabs.some(t => t.id === defaultTab))?.id || null;
    }

    const [settings, setSettings] = useState<SystemSettings | null>(null);
    const [activeCategory, setActiveCategory] = useState<string | null>(initialCategory);
    const [activeTab, setActiveTab] = useState<TabType | null>(defaultTab);
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

    // View: HUB
    if (!activeCategory || !activeTab) {
        return (
            <div className="space-y-8 animate-in fade-in duration-500">
                <div className="flex justify-between items-center">
                    <div>
                        <h1 className="text-3xl font-black text-slate-900 tracking-tight">Paramètres</h1>
                        <p className="text-slate-500 font-medium mt-1">Gérez l\'ensemble de l\'infrastructure, des politiques et des préférences du réseau.</p>
                    </div>
                    <div className="flex items-center gap-2 px-3 py-1.5 bg-indigo-50 rounded-full border border-indigo-100 shadow-sm">
                        <ShieldAlert size={14} className="text-indigo-600" />
                        <span className="text-[10px] font-black uppercase text-indigo-700 tracking-widest">Super-Admin Access</span>
                    </div>
                </div>

                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    {CATEGORIES.map(cat => (
                        <div
                            key={cat.id}
                            onClick={() => { setActiveCategory(cat.id); setActiveTab(cat.tabs[0].id as TabType); }}
                            className={`group cursor-pointer bg-white rounded-3xl p-8 border-2 border-slate-100 shadow-sm hover:shadow-lg transition-all duration-300 ${cat.borderHover} ${cat.hoverBg}`}
                        >
                            <div className="flex items-start justify-between">
                                <div className="space-y-4">
                                    <div className={`w-14 h-14 rounded-2xl flex items-center justify-center border ${cat.bgColor} ${cat.color} ${cat.borderColor} group-hover:scale-110 transition-transform`}>
                                        <cat.icon size={28} />
                                    </div>
                                    <div>
                                        <h2 className="text-xl font-black text-slate-900 mb-2">{cat.title}</h2>
                                        <p className="text-sm text-slate-500 font-medium leading-relaxed">{cat.description}</p>
                                    </div>
                                </div>
                                <div className="w-8 h-8 rounded-full bg-slate-50 flex items-center justify-center text-slate-400 group-hover:bg-white group-hover:text-slate-900 group-hover:shadow-sm border border-slate-100 transition-all">
                                    <ChevronRight size={18} />
                                </div>
                            </div>

                            <div className="mt-8 pt-6 border-t border-slate-100 border-dashed flex gap-4">
                                {cat.tabs.map(tab => (
                                    <span key={tab.id} className="flex items-center gap-1.5 text-xs font-bold text-slate-400 group-hover:text-slate-600 transition-colors">
                                        <tab.icon size={12} />
                                        {tab.label}
                                    </span>
                                ))}
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        );
    }

    // View: DETAIL
    const currentCategory = CATEGORIES.find(c => c.id === activeCategory);

    return (
        <div className="animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* Context Header */}
            <div className="flex items-center justify-between mb-8">
                <div className="flex items-center gap-4">
                    <button
                        onClick={() => { setActiveCategory(null); setActiveTab(null); }}
                        className="w-10 h-10 rounded-xl bg-white border border-slate-200 flex items-center justify-center text-slate-500 hover:text-slate-900 hover:bg-slate-50 shadow-sm transition-all"
                    >
                        <ArrowLeft size={18} />
                    </button>
                    <div>
                        <h2 className="text-2xl font-black text-slate-900 tracking-tight flex items-center gap-3">
                            {currentCategory?.icon && <currentCategory.icon className={currentCategory?.color} />}
                            {currentCategory?.title}
                        </h2>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
                {/* VERTICAL SIDEBAR */}
                <div className="lg:col-span-1 space-y-2">
                    {currentCategory?.tabs.map(tab => (
                        <button
                            key={tab.id}
                            onClick={() => setActiveTab(tab.id as TabType)}
                            className={`w-full flex items-center justify-between px-5 py-3.5 rounded-2xl font-bold text-sm transition-all border-2 ${activeTab === tab.id
                                ? `bg-white border-slate-200 shadow-sm text-slate-900`
                                : 'border-transparent text-slate-500 hover:bg-slate-200/50 hover:text-slate-700'
                                }`}
                        >
                            <span className="flex items-center gap-3">
                                <tab.icon size={16} className={activeTab === tab.id ? currentCategory.color : "text-slate-400"} />
                                {tab.label}
                            </span>
                        </button>
                    ))}
                </div>

                {/* TAB CONTENT */}
                <div className="lg:col-span-3">
                    {activeTab === 'COMPANY' && (
                        <div className="bg-white rounded-[2rem] p-8 border border-slate-100 shadow-sm space-y-8">
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
                        <div className="bg-white rounded-[2rem] p-8 border border-slate-100 shadow-sm space-y-8">
                            <div className="space-y-6">
                                <div className="flex justify-between items-center">
                                    <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Passerelle de Paiement</h3>
                                    <div className={`px-4 py-1.5 rounded-full text-[10px] font-black uppercase tracking-widest border border-dashed ${settings.payment.mode === 'TEST'
                                        ? 'bg-amber-50 text-amber-600 border-amber-200'
                                        : 'bg-red-50 text-red-600 border-red-200'
                                        }`}>
                                        {settings.payment.mode === 'TEST' ? 'Mode Sablier (Test)' : 'Mode Production (Live)'}
                                    </div>
                                </div>

                                <div className="space-y-4">
                                    <div className="flex items-center gap-4 p-5 bg-slate-50 rounded-2xl border border-slate-100">
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
                        </div>
                    )}

                    {activeTab === 'NOTIFICATIONS' && (
                        <div className="bg-white rounded-[2rem] p-8 border border-slate-100 shadow-sm space-y-12">
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
                                        className="flex items-center gap-2 px-5 py-2.5 bg-slate-50 text-slate-700 border border-slate-200 rounded-xl text-xs font-black hover:bg-slate-100 transition-all shadow-sm"
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
                                            <p className="text-[10px] text-slate-400 font-bold uppercase tracking-tighter mt-1">Relances automatiques Clients</p>
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
                        </div>
                    )}

                    {activeTab === 'WHATSAPP_TEMPLATES' && <TwilioTemplatesTab />}
                    {activeTab === 'SERVICE_PRICING' && <ServicePricingTab />}
                    {activeTab === 'SERVICES' && <ServiceManagerTab />}
                    {activeTab === 'LEGAL_DOCS' && <LegalDocumentsTab />}
                    {activeTab === 'DOCUMENTS' && <DocumentCatalogTab />}
                    {activeTab === 'INTEGRATIONS' && (
                        <IntegrationsTab
                            settings={settings.integrations}
                            onUpdate={(val: IntegrationSettings) => {
                                setSettings(prev => prev ? { ...prev, integrations: val } : prev);
                                SettingsStore.saveSettings('integrations', val).catch(err => {
                                    console.error('[IntegrationsTab] Backend save failed:', err);
                                });
                            }}
                        />
                    )}
                    {activeTab === 'SYSTEM_USERS' && <SystemUsersTab />}
                    {activeTab === 'AUTOMATIONS' && (
                        <div className="bg-slate-900 rounded-[2rem] p-8 border border-slate-800 shadow-sm">
                            <PipelineAutomationsTab />
                        </div>
                    )}
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
                    <button onClick={onToggleSecret} className="text-slate-400 hover:text-indigo-600 transition-colors">
                        {showSecret ? <EyeOff size={14} /> : <Eye size={14} />}
                    </button>
                )}
            </div>
            <input
                type={isSecret && !showSecret ? 'password' : type}
                value={localValue}
                onChange={e => setLocalValue(e.target.value)}
                onBlur={() => onChange(localValue)}
                className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-100 rounded-2xl font-bold focus:border-indigo-600 focus:bg-white outline-none transition-all placeholder:text-slate-300"
            />
        </div>
    );
}
