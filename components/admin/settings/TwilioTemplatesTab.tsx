'use client';

import React, { useState, useEffect } from 'react';
import {
    MessageSquare, CheckCircle, XCircle, Copy,
    Save, RefreshCcw, Eye, EyeOff, Terminal,
    Smartphone, ArrowRight, ExternalLink, Zap,
    Cloud, Server
} from 'lucide-react';
import { AuthStore } from '../../../services/authStore';

interface TwilioTemplate {
    key: string;
    friendlyName: string;
    envKey: string;
    body?: string;
    actions?: { type: string; title: string; url?: string }[];
    contentSid: string | null;
    configured: boolean;
}

const TEMPLATE_LABELS: Record<string, { label: string; emoji: string; description: string }> = {
    order_checklist: {
        label: 'Confirmation de commande',
        emoji: '✅',
        description: 'Envoyé après le paiement. Liste les documents requis avec bouton d\'accès à l\'espace client.'
    },
    document_rejected: {
        label: 'Document refusé',
        emoji: '⚠️',
        description: 'Envoyé quand un juriste rejette un document. Inclut le motif et un bouton de re-upload.'
    },
    document_validated: {
        label: 'Document validé',
        emoji: '✅',
        description: 'Envoyé quand un juriste valide un document. Bouton vers l\'espace client.'
    },
    all_documents_validated: {
        label: 'Dossier complet',
        emoji: '🎉',
        description: 'Envoyé quand tous les documents requis sont validés. Le dossier passe en traitement.'
    },
    jurist_assigned: {
        label: 'Juriste assigné',
        emoji: '💼',
        description: 'Envoyé quand un juriste est assigné au dossier du client.'
    }
};

export default function TwilioTemplatesTab() {
    const [templates, setTemplates] = useState<TwilioTemplate[]>([]);
    const [provider, setProvider] = useState<string>('NONE');
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [editValues, setEditValues] = useState<Record<string, string>>({});
    const [showBody, setShowBody] = useState<Record<string, boolean>>({});
    const [savedMessage, setSavedMessage] = useState('');

    const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

    useEffect(() => {
        loadTemplates();
    }, []);

    const loadTemplates = async () => {
        setLoading(true);
        try {
            const token = AuthStore.getToken();
            const res = await fetch(`${API}/settings/twilio-templates`, {
                headers: { 'Authorization': `Bearer ${token}` }
            });
            if (res.ok) {
                const data = await res.json();
                // Gérer les deux formats de réponse (ancien: tableau, nouveau: {provider, templates})
                const tplList = Array.isArray(data) ? data : (data.templates || []);
                if (data.provider) setProvider(data.provider);
                setTemplates(tplList);
                const values: Record<string, string> = {};
                tplList.forEach((t: TwilioTemplate) => {
                    if (t.contentSid) values[t.key] = t.contentSid;
                });
                setEditValues(values);
            }
        } catch (e) {
            console.error('Failed to load templates:', e);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async () => {
        setSaving(true);
        setSavedMessage('');
        try {
            const token = AuthStore.getToken();
            const res = await fetch(`${API}/settings/twilio-templates`, {
                method: 'PATCH',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ templates: editValues })
            });
            if (res.ok) {
                const data = await res.json();
                const tplList = Array.isArray(data) ? data : (data.templates || []);
                if (data.provider) setProvider(data.provider);
                setTemplates(tplList);
                setSavedMessage('Configuration sauvegardée ✅');
                setTimeout(() => setSavedMessage(''), 3000);
            }
        } catch (e) {
            console.error('Failed to save templates:', e);
        } finally {
            setSaving(false);
        }
    };

    const configuredCount = templates.filter(t => t.configured).length;
    const totalCount = templates.length;

    if (loading) {
        return (
            <div className="bg-white rounded-[2rem] p-12 border-2 border-slate-100 shadow-sm animate-pulse">
                <div className="h-6 bg-slate-100 rounded w-1/3 mb-6"></div>
                <div className="space-y-4">
                    <div className="h-20 bg-slate-50 rounded-2xl"></div>
                    <div className="h-20 bg-slate-50 rounded-2xl"></div>
                    <div className="h-20 bg-slate-50 rounded-2xl"></div>
                </div>
            </div>
        );
    }

    return (
        <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* HEADER */}
            <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                    <div className="w-10 h-10 bg-[#25D366]/10 rounded-xl flex items-center justify-center">
                        <Smartphone size={20} className="text-[#25D366]" />
                    </div>
                    <div>
                        <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">
                            Content Templates WhatsApp
                        </h3>
                        <p className="text-[10px] text-slate-400 font-bold uppercase tracking-tighter">
                            {provider === 'META' ? 'Meta Cloud API — Direct' : provider === 'TWILIO' ? 'Twilio Content API' : 'Non configuré'} — Boutons interactifs
                        </p>
                    </div>
                </div>

                <div className="flex items-center gap-3">
                    {/* Provider badge */}
                    <div className={`flex items-center gap-1.5 px-3 py-1.5 rounded-full text-[10px] font-black uppercase tracking-widest border-2 ${provider === 'META' ? 'bg-blue-50 text-blue-600 border-blue-100'
                            : provider === 'TWILIO' ? 'bg-red-50 text-red-600 border-red-100'
                                : 'bg-slate-50 text-slate-400 border-slate-100'
                        }`}>
                        {provider === 'META' ? <Cloud size={12} /> : provider === 'TWILIO' ? <Server size={12} /> : null}
                        {provider === 'META' ? 'Meta API' : provider === 'TWILIO' ? 'Twilio' : 'Aucun'}
                    </div>

                    {/* Status badge */}
                    <div className={`px-4 py-1.5 rounded-full text-[10px] font-black uppercase tracking-widest border-2 ${configuredCount === totalCount
                        ? 'bg-emerald-50 text-emerald-600 border-emerald-100'
                        : configuredCount > 0
                            ? 'bg-amber-50 text-amber-600 border-amber-100'
                            : 'bg-slate-50 text-slate-400 border-slate-100'
                        }`}>
                        {configuredCount}/{totalCount} configurés
                    </div>

                    <button
                        onClick={loadTemplates}
                        className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-all"
                        title="Rafraîchir"
                    >
                        <RefreshCcw size={16} />
                    </button>
                </div>
            </div>

            {/* INFO BOX */}
            <div className="p-4 bg-indigo-50/50 rounded-2xl border border-indigo-100/50">
                <div className="flex items-start gap-3">
                    <Zap size={16} className="text-indigo-500 mt-0.5 flex-shrink-0" />
                    <div className="text-xs text-indigo-700 space-y-1">
                        <p className="font-bold">Comment activer les boutons interactifs WhatsApp :</p>
                        {provider === 'META' ? (
                            <ol className="list-decimal list-inside space-y-0.5 text-indigo-600">
                                <li>Créez les templates dans <strong>Meta Business Suite</strong> → WhatsApp → Message Templates</li>
                                <li>Attendez l'approbation WhatsApp (quelques minutes à 24h)</li>
                                <li>Collez les <strong>noms de template</strong> ci-dessous (ex: <code className="bg-indigo-100 px-1.5 py-0.5 rounded text-[10px]">simulegal_order_checklist</code>)</li>
                            </ol>
                        ) : (
                            <ol className="list-decimal list-inside space-y-0.5 text-indigo-600">
                                <li>Lancez <code className="bg-indigo-100 px-1.5 py-0.5 rounded text-[10px]">npm run twilio:templates</code> pour créer les templates</li>
                                <li>Attendez l'approbation WhatsApp (24-48h)</li>
                                <li>Collez les <code className="bg-indigo-100 px-1.5 py-0.5 rounded text-[10px]">contentSid</code> ci-dessous</li>
                            </ol>
                        )}
                        <p className="text-indigo-400 italic">Sans configuration, les boutons sont envoyés en mode texte (fallback).</p>
                    </div>
                </div>
            </div>

            {/* TEMPLATES LIST */}
            <div className="space-y-4">
                {templates.map(template => {
                    const meta = TEMPLATE_LABELS[template.key] || {
                        label: template.key,
                        emoji: '📄',
                        description: ''
                    };

                    return (
                        <div
                            key={template.key}
                            className={`rounded-2xl border-2 transition-all duration-300 ${template.configured
                                ? 'border-emerald-100 bg-emerald-50/30'
                                : 'border-slate-100 bg-slate-50/30 hover:border-slate-200'
                                }`}
                        >
                            {/* Template header */}
                            <div className="p-5">
                                <div className="flex items-center justify-between mb-3">
                                    <div className="flex items-center gap-3">
                                        <span className="text-lg">{meta.emoji}</span>
                                        <div>
                                            <p className="font-black text-sm text-slate-900">{meta.label}</p>
                                            <p className="text-[10px] text-slate-400 font-bold uppercase tracking-wider">
                                                {template.friendlyName}
                                            </p>
                                        </div>
                                    </div>

                                    <div className="flex items-center gap-2">
                                        {template.configured ? (
                                            <div className="flex items-center gap-1.5 px-3 py-1 rounded-full bg-emerald-100 text-emerald-700">
                                                <CheckCircle size={12} />
                                                <span className="text-[10px] font-black uppercase">Actif</span>
                                            </div>
                                        ) : (
                                            <div className="flex items-center gap-1.5 px-3 py-1 rounded-full bg-slate-100 text-slate-400">
                                                <XCircle size={12} />
                                                <span className="text-[10px] font-black uppercase">Fallback texte</span>
                                            </div>
                                        )}

                                        <button
                                            onClick={() => setShowBody(prev => ({ ...prev, [template.key]: !prev[template.key] }))}
                                            className="p-1.5 text-slate-400 hover:text-indigo-600 rounded-lg transition-all"
                                            title="Aperçu du message"
                                        >
                                            {showBody[template.key] ? <EyeOff size={14} /> : <Eye size={14} />}
                                        </button>
                                    </div>
                                </div>

                                <p className="text-xs text-slate-500 mb-4">{meta.description}</p>

                                {/* Content SID input */}
                                <div className="flex items-center gap-3">
                                    <div className="flex-1">
                                        <label className="text-[9px] font-black uppercase tracking-widest text-slate-400 mb-1 block ml-1">
                                            {provider === 'META' ? 'Template Name (Meta)' : 'Content SID (Twilio)'}
                                        </label>
                                        <input
                                            type="text"
                                            placeholder={provider === 'META' ? template.friendlyName : 'HXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'}
                                            value={editValues[template.key] || ''}
                                            onChange={e => setEditValues(prev => ({ ...prev, [template.key]: e.target.value }))}
                                            className="w-full p-3 bg-white border-2 border-slate-100 rounded-xl font-mono text-sm focus:border-indigo-500 focus:ring-2 focus:ring-indigo-100 outline-none transition-all placeholder:text-slate-200"
                                        />
                                    </div>
                                </div>

                                {/* Boutons CTA preview */}
                                {(template as any).actions && (template as any).actions.length > 0 && (
                                    <div className="mt-3 flex gap-2 flex-wrap">
                                        {(template as any).actions.map((action: any, i: number) => (
                                            <div key={i} className="flex items-center gap-1.5 px-3 py-1.5 bg-[#25D366]/10 rounded-lg border border-[#25D366]/20">
                                                <span className="text-[10px] font-bold text-[#25D366]">{action.title}</span>
                                                <ExternalLink size={10} className="text-[#25D366]/60" />
                                            </div>
                                        ))}
                                    </div>
                                )}
                            </div>

                            {/* Message body preview (collapsible) */}
                            {showBody[template.key] && (template as any).body && (
                                <div className="px-5 pb-5 animate-in slide-in-from-top-2 duration-200">
                                    <div className="bg-slate-900 rounded-xl p-4 relative">
                                        <div className="flex items-center gap-2 mb-2">
                                            <Terminal size={12} className="text-slate-500" />
                                            <span className="text-[9px] font-black uppercase tracking-widest text-slate-500">Aperçu du message</span>
                                        </div>
                                        <pre className="text-xs text-emerald-400 font-mono whitespace-pre-wrap leading-relaxed">
                                            {(template as any).body}
                                        </pre>
                                        <button
                                            onClick={() => navigator.clipboard.writeText((template as any).body || '')}
                                            className="absolute top-3 right-3 p-1.5 text-slate-500 hover:text-white transition-colors"
                                            title="Copier"
                                        >
                                            <Copy size={12} />
                                        </button>
                                    </div>
                                </div>
                            )}
                        </div>
                    );
                })}
            </div>

            {/* SAVE BUTTON */}
            <div className="flex items-center justify-between pt-4 border-t border-slate-100">
                <div className="flex items-center gap-3">
                    {savedMessage && (
                        <div className="flex items-center gap-2 text-emerald-600 animate-in fade-in duration-300">
                            <CheckCircle size={14} />
                            <span className="text-xs font-bold">{savedMessage}</span>
                        </div>
                    )}
                </div>

                <button
                    onClick={handleSave}
                    disabled={saving}
                    className="flex items-center gap-2 px-8 py-3 bg-slate-900 text-white rounded-xl text-sm font-black hover:bg-black transition-all shadow-lg disabled:opacity-50 disabled:cursor-not-allowed"
                >
                    {saving ? (
                        <RefreshCcw size={16} className="animate-spin" />
                    ) : (
                        <Save size={16} />
                    )}
                    {saving ? 'Sauvegarde...' : 'Enregistrer les templates'}
                </button>
            </div>
        </div>
    );
}
