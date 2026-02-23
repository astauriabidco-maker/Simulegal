'use client';

import React, { useState, useEffect } from 'react';
import {
    FileText, Save, Eye, EyeOff, CheckCircle,
    Clock, Hash, RefreshCcw, AlertTriangle,
    Scale, ScrollText, BookOpen
} from 'lucide-react';
import AuthStore from '../../../services/authStore';

const API_BASE = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000').replace(/\/$/, '');

interface LegalDocument {
    content: string;
    version: string;
    updatedAt: string;
    updatedBy?: string;
}

type DocType = 'cgv' | 'contrat_representation' | 'mentions_legales';

const DOC_LABELS: Record<DocType, { label: string; icon: any; color: string; description: string }> = {
    cgv: {
        label: 'Conditions Générales de Vente',
        icon: Scale,
        color: 'indigo',
        description: 'Document contractuel présenté au client lors du paiement. Doit contenir les conditions de vente, de rétractation et de litige.'
    },
    contrat_representation: {
        label: 'Contrat de Représentation',
        icon: ScrollText,
        color: 'purple',
        description: 'Mandat signé par le client autorisant Simulegal à le représenter auprès des autorités. Les variables [TYPE_PROCEDURE], [NOM_SERVICE], [MONTANT] et [DATE] sont remplacées automatiquement.'
    },
    mentions_legales: {
        label: 'Mentions Légales',
        icon: BookOpen,
        color: 'emerald',
        description: 'Informations légales obligatoires affichées sur le site. Inclut les coordonnées, SIRET, TVA, DPO, hébergeur.'
    },
};

export default function LegalDocumentsTab() {
    const [documents, setDocuments] = useState<Record<string, LegalDocument>>({});
    const [activeDoc, setActiveDoc] = useState<DocType>('cgv');
    const [editingContent, setEditingContent] = useState('');
    const [preview, setPreview] = useState(false);
    const [saving, setSaving] = useState(false);
    const [saveSuccess, setSaveSuccess] = useState(false);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        loadDocuments();
    }, []);

    useEffect(() => {
        if (documents[activeDoc]) {
            setEditingContent(documents[activeDoc].content);
            setPreview(false);
        }
    }, [activeDoc, documents]);

    const loadDocuments = async () => {
        try {
            const res = await fetch(`${API_BASE}/settings/legal-documents`);
            const data = await res.json();
            setDocuments(data);
        } catch (e) {
            console.error('Failed to load legal documents:', e);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async () => {
        setSaving(true);
        setSaveSuccess(false);
        try {
            const res = await fetch(`${API_BASE}/settings/legal-documents/${activeDoc}`, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
                body: JSON.stringify({ content: editingContent })
            });
            const data = await res.json();
            if (data.success) {
                setDocuments(data.documents);
                setSaveSuccess(true);
                setTimeout(() => setSaveSuccess(false), 3000);
            }
        } catch (e) {
            console.error('Save failed:', e);
        } finally {
            setSaving(false);
        }
    };

    // Simple Markdown → HTML renderer (basique mais suffisant)
    const renderMarkdown = (md: string): string => {
        return md
            .replace(/^### (.*$)/gm, '<h3 class="text-lg font-bold text-slate-800 mt-6 mb-2">$1</h3>')
            .replace(/^## (.*$)/gm, '<h2 class="text-xl font-bold text-slate-900 mt-8 mb-3 border-b border-slate-200 pb-2">$1</h2>')
            .replace(/^# (.*$)/gm, '<h1 class="text-2xl font-black text-slate-900 mb-4">$1</h1>')
            .replace(/\*\*(.*?)\*\*/g, '<strong class="font-bold text-slate-800">$1</strong>')
            .replace(/\*(.*?)\*/g, '<em>$1</em>')
            .replace(/^- (.*$)/gm, '<li class="ml-6 text-slate-600 py-0.5 list-disc">$1</li>')
            .replace(/\n\n/g, '</p><p class="text-slate-600 mb-3">')
            .replace(/^\|(.*)\|$/gm, (match) => {
                const cells = match.split('|').filter(c => c.trim());
                const isHeader = cells.some(c => c.match(/^-+$/));
                if (isHeader) return '';
                return `<tr>${cells.map(c => `<td class="border border-slate-200 px-4 py-2 text-sm">${c.trim()}</td>`).join('')}</tr>`;
            });
    };

    const currentDoc = documents[activeDoc];
    const docMeta = DOC_LABELS[activeDoc];
    const hasUnsavedChanges = currentDoc && editingContent !== currentDoc.content;

    if (loading) {
        return (
            <div className="flex items-center justify-center h-64">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-indigo-600" />
            </div>
        );
    }

    return (
        <div className="space-y-6">
            {/* Header */}
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <FileText size={20} className="text-indigo-600" />
                        Documents Juridiques
                    </h2>
                    <p className="text-slate-500 text-sm mt-1">
                        Éditez les CGV, contrats de représentation et mentions légales en Markdown.
                    </p>
                </div>
            </div>

            {/* Tabs */}
            <div className="flex p-1 bg-slate-100 rounded-2xl w-fit">
                {(Object.keys(DOC_LABELS) as DocType[]).map(docType => {
                    const meta = DOC_LABELS[docType];
                    const Icon = meta.icon;
                    const isActive = activeDoc === docType;
                    return (
                        <button
                            key={docType}
                            onClick={() => setActiveDoc(docType)}
                            className={`flex items-center gap-2 px-4 py-2.5 rounded-xl text-sm font-bold transition-all ${isActive
                                ? 'bg-white text-indigo-600 shadow-sm'
                                : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            <Icon size={16} />
                            {meta.label.split(' ').slice(0, 2).join(' ')}
                        </button>
                    );
                })}
            </div>

            {/* Metadata */}
            {currentDoc && (
                <div className={`bg-gradient-to-r from-${docMeta.color}-50 to-white p-4 rounded-2xl border border-${docMeta.color}-100`}>
                    <div className="flex items-start justify-between">
                        <div>
                            <h3 className="font-bold text-slate-800">{docMeta.label}</h3>
                            <p className="text-slate-500 text-xs mt-1">{docMeta.description}</p>
                        </div>
                        <div className="flex items-center gap-4 text-xs text-slate-500">
                            <span className="flex items-center gap-1">
                                <Hash size={12} /> Version {currentDoc.version}
                            </span>
                            <span className="flex items-center gap-1">
                                <Clock size={12} /> {new Date(currentDoc.updatedAt).toLocaleDateString('fr-FR', {
                                    day: 'numeric', month: 'long', year: 'numeric', hour: '2-digit', minute: '2-digit'
                                })}
                            </span>
                        </div>
                    </div>
                </div>
            )}

            {/* Editor / Preview */}
            <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
                {/* Toolbar */}
                <div className="flex items-center justify-between px-4 py-3 border-b border-slate-100 bg-slate-50/50">
                    <div className="flex items-center gap-2">
                        <button
                            onClick={() => setPreview(false)}
                            className={`px-3 py-1.5 rounded-lg text-xs font-bold transition-all ${!preview
                                ? 'bg-indigo-600 text-white'
                                : 'text-slate-500 hover:bg-slate-100'
                                }`}
                        >
                            ✏️ Édition
                        </button>
                        <button
                            onClick={() => setPreview(true)}
                            className={`px-3 py-1.5 rounded-lg text-xs font-bold transition-all flex items-center gap-1 ${preview
                                ? 'bg-indigo-600 text-white'
                                : 'text-slate-500 hover:bg-slate-100'
                                }`}
                        >
                            <Eye size={12} /> Aperçu
                        </button>
                    </div>

                    <div className="flex items-center gap-3">
                        {hasUnsavedChanges && (
                            <span className="flex items-center gap-1 text-amber-600 text-xs font-bold animate-pulse">
                                <AlertTriangle size={12} /> Modifications non enregistrées
                            </span>
                        )}

                        <button
                            onClick={handleSave}
                            disabled={saving || !hasUnsavedChanges}
                            className={`px-4 py-2 rounded-xl text-sm font-bold text-white transition-all flex items-center gap-2 ${saveSuccess
                                ? 'bg-emerald-500'
                                : saving
                                    ? 'bg-indigo-400'
                                    : hasUnsavedChanges
                                        ? 'bg-indigo-600 hover:bg-indigo-700 shadow-sm'
                                        : 'bg-slate-300 cursor-not-allowed'
                                }`}
                        >
                            {saveSuccess ? <><CheckCircle size={14} /> Enregistré !</> :
                                saving ? <><RefreshCcw size={14} className="animate-spin" /> Enregistrement...</> :
                                    <><Save size={14} /> Enregistrer</>}
                        </button>
                    </div>
                </div>

                {/* Content */}
                {!preview ? (
                    <textarea
                        value={editingContent}
                        onChange={(e) => setEditingContent(e.target.value)}
                        className="w-full p-6 min-h-[600px] font-mono text-sm text-slate-700 leading-relaxed resize-y outline-none border-none focus:ring-0"
                        placeholder="Saisissez le contenu en Markdown..."
                        spellCheck={true}
                    />
                ) : (
                    <div
                        className="p-8 min-h-[600px] prose prose-slate max-w-none"
                        dangerouslySetInnerHTML={{ __html: `<p class="text-slate-600 mb-3">${renderMarkdown(editingContent)}</p>` }}
                    />
                )}
            </div>

            {/* Help */}
            <div className="bg-slate-50 rounded-2xl p-4 border border-slate-200">
                <h4 className="font-bold text-slate-700 text-sm mb-2">💡 Aide Markdown</h4>
                <div className="grid grid-cols-3 gap-3 text-xs text-slate-500">
                    <div><code className="bg-white px-1.5 py-0.5 rounded"># Titre</code> → Titre principal</div>
                    <div><code className="bg-white px-1.5 py-0.5 rounded">## Section</code> → Sous-titre</div>
                    <div><code className="bg-white px-1.5 py-0.5 rounded">**gras**</code> → <strong>gras</strong></div>
                    <div><code className="bg-white px-1.5 py-0.5 rounded">*italique*</code> → <em>italique</em></div>
                    <div><code className="bg-white px-1.5 py-0.5 rounded">- item</code> → Liste à puces</div>
                    <div><code className="bg-white px-1.5 py-0.5 rounded">[TYPE_PROCEDURE]</code> → Variable auto</div>
                </div>
            </div>
        </div>
    );
}
