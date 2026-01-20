'use client';

import React, { useState } from 'react';
import {
    CreditCard, MessageSquare, Eye, EyeOff,
    HardDrive, Map, Copy, Check, ExternalLink,
    Power, ShieldCheck, AlertCircle, Loader2,
    RefreshCcw, Smartphone
} from 'lucide-react';
import { ApiConfig, IntegrationSettings, SettingsStore } from '../../../services/SettingsStore';

interface ServiceCardProps {
    id: keyof IntegrationSettings;
    title: string;
    description: string;
    icon: React.ReactNode;
    config: ApiConfig;
    webhookUrl?: string;
    fields: { key: string; label: string; isSecret?: boolean }[];
    onSave: (id: keyof IntegrationSettings, config: ApiConfig) => void;
    onTest: (id: keyof IntegrationSettings) => Promise<{ success: boolean; message: string }>;
}

function ServiceCard({ id, title, description, icon, config, webhookUrl, fields, onSave, onTest }: ServiceCardProps) {
    const [isSaving, setIsSaving] = useState(false);
    const [isTesting, setIsTesting] = useState(false);
    const [showSecrets, setShowSecrets] = useState<Record<string, boolean>>({});
    const [testResult, setTestResult] = useState<{ success: boolean; message: string } | null>(null);
    const [copied, setCopied] = useState(false);

    const toggleSecret = (key: string) => {
        setShowSecrets(prev => ({ ...prev, [key]: !prev[key] }));
    };

    const handleCopy = () => {
        if (webhookUrl) {
            navigator.clipboard.writeText(webhookUrl);
            setCopied(true);
            setTimeout(() => setCopied(false), 2000);
        }
    };

    const handleFieldChange = (key: string, value: string) => {
        const newConfig = {
            ...config,
            credentials: {
                ...config.credentials,
                [key]: value
            }
        };
        onSave(id, newConfig);
    };

    const handleTest = async () => {
        setIsTesting(true);
        setTestResult(null);
        try {
            const result = await onTest(id);
            setTestResult(result);

            // Update status based on test result locally
            const newConfig = {
                ...config,
                status: result.success ? 'CONNECTED' : 'ERROR' as any,
                lastCheck: new Date().toISOString()
            };
            onSave(id, newConfig);
        } catch (error) {
            setTestResult({ success: false, message: "Erreur lors du test de connexion" });
        } finally {
            setIsTesting(false);
        }
    };

    if (!config) return null;

    return (
        <div className="bg-white rounded-[2rem] border-2 border-slate-100 shadow-sm overflow-hidden flex flex-col h-full animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* Header */}
            <div className="p-6 pb-4 flex items-center justify-between border-b border-slate-50">
                <div className="flex items-center gap-3">
                    <div className="w-12 h-12 bg-slate-50 rounded-2xl flex items-center justify-center text-slate-800 border border-slate-100">
                        {icon}
                    </div>
                    <div>
                        <h3 className="font-black text-slate-900 leading-none mb-1">{title}</h3>
                        <p className="text-[10px] text-slate-400 font-bold uppercase tracking-tighter">{description}</p>
                    </div>
                </div>
                <button
                    onClick={() => onSave(id, { ...config, enabled: !config.enabled })}
                    className={`p-2 rounded-xl border-2 transition-all ${config.enabled
                        ? 'bg-emerald-50 border-emerald-100 text-emerald-600'
                        : 'bg-slate-50 border-slate-100 text-slate-400 opacity-50'}`}
                >
                    <Power size={18} />
                </button>
            </div>

            {/* Status & Mode */}
            <div className="px-6 py-4 flex items-center justify-between bg-slate-50/50">
                <div className="flex items-center gap-2">
                    <div className={`w-2 h-2 rounded-full ${config.status === 'CONNECTED' ? 'bg-emerald-500' : config.status === 'ERROR' ? 'bg-red-500' : 'bg-slate-300'}`} />
                    <span className={`text-[10px] font-black uppercase tracking-widest ${config.status === 'CONNECTED' ? 'text-emerald-600' : config.status === 'ERROR' ? 'text-red-500' : 'text-slate-500'}`}>
                        {config.status === 'CONNECTED' ? 'Connecté' : config.status === 'ERROR' ? 'Erreur Configuration' : 'État Inconnu'}
                    </span>
                </div>

                <div className="flex p-0.5 bg-white rounded-lg border border-slate-200">
                    <button
                        onClick={() => onSave(id, { ...config, mode: 'SANDBOX' })}
                        className={`px-3 py-1 rounded-md text-[9px] font-black transition-all ${config.mode === 'SANDBOX' ? 'bg-slate-900 text-white shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                    >
                        SANDBOX
                    </button>
                    <button
                        onClick={() => onSave(id, { ...config, mode: 'LIVE' })}
                        className={`px-3 py-1 rounded-md text-[9px] font-black transition-all ${config.mode === 'LIVE' ? 'bg-red-600 text-white shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                    >
                        LIVE
                    </button>
                </div>
            </div>

            {/* Form Fields */}
            <div className={`p-6 space-y-4 flex-1 transition-all ${!config.enabled && 'grayscale opacity-70 pointer-events-none'}`}>
                {fields.map(field => (
                    <div key={field.key} className="space-y-1.5">
                        <div className="flex justify-between items-center ml-1">
                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400">{field.label}</label>
                            {field.isSecret && (
                                <button onClick={() => toggleSecret(field.key)} className="text-slate-400 hover:text-indigo-600">
                                    {showSecrets[field.key] ? <EyeOff size={14} /> : <Eye size={14} />}
                                </button>
                            )}
                        </div>
                        <input
                            type={field.isSecret && !showSecrets[field.key] ? 'password' : 'text'}
                            value={config.credentials[field.key] || ''}
                            onChange={e => handleFieldChange(field.key, e.target.value)}
                            className="w-full h-11 px-4 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all placeholder:text-slate-300"
                            placeholder={field.isSecret ? "••••••••••••••••" : `Entrez ${field.label}...`}
                        />
                    </div>
                ))}

                {webhookUrl && (
                    <div className="p-4 bg-indigo-50/50 rounded-2xl border border-indigo-100 space-y-2">
                        <div className="flex justify-between items-center">
                            <span className="text-[10px] font-black uppercase tracking-widest text-indigo-400">URL de Webhook</span>
                            <button onClick={handleCopy} className="text-indigo-600 hover:text-indigo-800 flex items-center gap-1">
                                {copied ? <Check size={12} className="text-emerald-600" /> : <Copy size={12} />}
                                <span className="text-[9px] font-bold uppercase">{copied ? 'Copié' : 'Copier'}</span>
                            </button>
                        </div>
                        <div className="text-[10px] font-mono text-slate-600 break-all bg-white/50 p-2 rounded-lg border border-indigo-50">
                            {webhookUrl}
                        </div>
                    </div>
                )}
            </div>

            {/* Footer Actions */}
            <div className="p-6 pt-0 mt-auto">
                <button
                    onClick={handleTest}
                    disabled={isTesting || !config.enabled}
                    className="w-full h-12 bg-slate-900 text-white rounded-2xl font-black text-sm flex items-center justify-center gap-2 hover:bg-slate-800 transition-all active:scale-95 disabled:bg-slate-200 disabled:text-slate-400 shadow-lg shadow-slate-100"
                >
                    {isTesting ? <Loader2 size={16} className="animate-spin" /> : <RefreshCcw size={16} />}
                    Tester la connexion
                </button>
                {testResult && (
                    <div className={`mt-3 p-3 rounded-xl border flex gap-2 items-center text-xs font-bold animate-in bounce-in duration-300 ${testResult.success ? 'bg-emerald-50 border-emerald-100 text-emerald-600' : 'bg-red-50 border-red-100 text-red-600'}`}>
                        {testResult.success ? <ShieldCheck size={16} /> : <AlertCircle size={16} />}
                        {testResult.message}
                    </div>
                )}
            </div>
        </div>
    );
}

export default function IntegrationsTab({ settings, onUpdate }: { settings: IntegrationSettings, onUpdate: (data: IntegrationSettings) => void }) {

    const handleSaveConfig = (id: keyof IntegrationSettings, config: ApiConfig) => {
        onUpdate({
            ...settings,
            [id]: config
        });
    };

    const handleTest = async (id: keyof IntegrationSettings): Promise<{ success: boolean; message: string }> => {
        const type = settings[id].provider;
        return await SettingsStore.testConnection(type);
    };

    return (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 animate-in fade-in slide-in-from-bottom-6 duration-700">
            {/* Payment Integration */}
            <ServiceCard
                id="payment"
                title="Stripe"
                description="Infrastructure de Paiement"
                icon={<CreditCard className="text-indigo-600" size={24} />}
                config={settings.payment}
                webhookUrl="https://api.simulegal.fr/webhooks/stripe"
                fields={[
                    { key: 'publicKey', label: 'Clé Publique (Publishable Key)' },
                    { key: 'secretKey', label: 'Clé Secrète (Secret Key)', isSecret: true }
                ]}
                onSave={handleSaveConfig}
                onTest={handleTest}
            />

            {/* Messaging Integration */}
            <ServiceCard
                id="messaging"
                title="Twilio"
                description="SMS & WhatsApp Business"
                icon={<MessageSquare className="text-[#F22F46]" size={24} />}
                config={settings.messaging}
                webhookUrl="https://api.simulegal.fr/webhooks/twilio"
                fields={[
                    { key: 'accountSid', label: 'Account SID' },
                    { key: 'authToken', label: 'Auth Token', isSecret: true },
                    { key: 'fromNumber', label: 'Numéro d\'envoi' }
                ]}
                onSave={handleSaveConfig}
                onTest={handleTest}
            />

            {/* OCR Integration */}
            <ServiceCard
                id="ocr"
                title="Google Vision"
                description="Intelligence Artificielle OCR"
                icon={<Smartphone className="text-[#4285F4]" size={24} />}
                config={settings.ocr}
                fields={[
                    { key: 'apiKey', label: 'API Key (Google Cloud)', isSecret: true },
                    { key: 'projectId', label: 'Project ID' }
                ]}
                onSave={handleSaveConfig}
                onTest={handleTest}
            />

            {/* Storage Integration */}
            <ServiceCard
                id="storage"
                title="AWS S3"
                description="Stockage Cloud Sécurisé"
                icon={<HardDrive className="text-[#FF9900]" size={24} />}
                config={settings.storage}
                fields={[
                    { key: 'bucketName', label: 'Nom du Bucket' },
                    { key: 'region', label: 'Région (ex: eu-west-3)' },
                    { key: 'accessKey', label: 'Access Key ID' },
                    { key: 'secretKey', label: 'Secret Access Key', isSecret: true }
                ]}
                onSave={handleSaveConfig}
                onTest={handleTest}
            />

            {/* Maps Integration */}
            <ServiceCard
                id="maps"
                title="Google Maps"
                description="Géolocalisation & Adresses"
                icon={<Map className="text-[#34A853]" size={24} />}
                config={settings.maps}
                fields={[
                    { key: 'apiKey', label: 'API Key (Maps Javascript)', isSecret: true }
                ]}
                onSave={handleSaveConfig}
                onTest={handleTest}
            />

            {/* Info Card */}
            <div className="bg-indigo-900 rounded-[2rem] p-8 text-white relative overflow-hidden flex flex-col justify-center">
                <div className="absolute top-0 right-0 p-8 opacity-10 rotate-12">
                    <ShieldCheck size={120} />
                </div>
                <div className="relative z-10 space-y-4">
                    <h3 className="text-xl font-black uppercase tracking-tighter">Sécurité Hub API</h3>
                    <p className="text-indigo-100 text-sm font-medium leading-relaxed">
                        Toutes les clés API sont chiffrées au repos côté serveur.
                        Les clés secrètes ne sont jamais renvoyées en clair une fois sauvegardées.
                    </p>
                    <div className="pt-4 flex items-center gap-3">
                        <div className="w-10 h-10 bg-white/10 rounded-xl flex items-center justify-center border border-white/20 backdrop-blur-sm">
                            <ExternalLink size={20} />
                        </div>
                        <p className="text-[10px] font-black uppercase tracking-widest text-indigo-300">Consulter la documentation <br /> des intégrations</p>
                    </div>
                </div>
            </div>
        </div>
    );
}
