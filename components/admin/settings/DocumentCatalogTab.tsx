'use client';

import React, { useState } from 'react';
import {
    FileStack,
    Search,
    ChevronRight,
    Info,
    Plus,
    ShieldCheck,
    Briefcase,
    Globe,
    CreditCard,
    GraduationCap,
    CheckCircle2,
    XCircle,
    LayoutPanelLeft,
    Box,
    Loader2,
    Save,
    Trash2,
    Check,
    AlertCircle,
    RefreshCw,
    Edit3
} from 'lucide-react';
import { DOC_CATALOG as DEFAULT_DOC_CATALOG, SERVICE_TEMPLATES as DEFAULT_SERVICE_TEMPLATES, DocumentCategory, DocumentRequirement, OCRType } from '../../../config/DocumentTemplates';
import { SERVICES_CATALOG } from '../../../data/services';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

const CATEGORY_ICONS: Record<DocumentCategory, any> = {
    IDENTITY: Globe,
    FINANCIAL: CreditCard,
    RESIDENCE: Globe,
    CIVIL: CheckCircle2,
    PROFESSIONAL: Briefcase,
    EDUCATION: GraduationCap,
    OTHER: Box
};

const CATEGORY_COLORS: Record<DocumentCategory, string> = {
    IDENTITY: 'text-blue-600 bg-blue-50 border-blue-100',
    FINANCIAL: 'text-emerald-600 bg-emerald-50 border-emerald-100',
    RESIDENCE: 'text-amber-600 bg-amber-50 border-amber-100',
    CIVIL: 'text-purple-600 bg-purple-50 border-purple-100',
    PROFESSIONAL: 'text-indigo-600 bg-indigo-50 border-indigo-100',
    EDUCATION: 'text-cyan-600 bg-cyan-50 border-cyan-100',
    OTHER: 'text-slate-600 bg-slate-50 border-slate-100'
};

const OCR_TYPES: OCRType[] = ['PASSPORT', 'ID_CARD', 'TAX_NOTICE', 'RESIDENCE_PROOF', 'BIRTH_CERTIFICATE', 'PHOTO', 'DRIVING_LICENSE', 'GENERIC'];

export default function DocumentCatalogTab() {
    const [searchQuery, setSearchQuery] = useState('');
    const [selectedCategory, setSelectedCategory] = useState<DocumentCategory | 'ALL'>('ALL');
    const [selectedDocId, setSelectedDocId] = useState<string | null>(null);
    const [viewMode, setViewMode] = useState<'CATALOG' | 'MAPPING'>('CATALOG');

    // Data State
    const [catalog, setCatalog] = useState<Record<string, DocumentRequirement>>({});
    const [templates, setTemplates] = useState<Record<string, string[]>>({});
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    // Modals State
    const [docEditorOpen, setDocEditorOpen] = useState(false);
    const [editingDoc, setEditingDoc] = useState<DocumentRequirement | undefined>(undefined);
    const [mappingEditorOpen, setMappingEditorOpen] = useState(false);
    const [editingServiceId, setEditingServiceId] = useState<string | null>(null);

    React.useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        setLoading(true);
        setError(null);
        try {
            const token = localStorage.getItem('admin_token');
            const [catRes, tempRes] = await Promise.all([
                fetch(`${API_URL}/settings/document-catalog`, {
                    headers: { 'Authorization': `Bearer ${token}` }
                }),
                fetch(`${API_URL}/settings/service-templates`, {
                    headers: { 'Authorization': `Bearer ${token}` }
                })
            ]);

            const catData = await catRes.json();
            const tempData = await tempRes.json();

            if (Object.keys(catData).length === 0) {
                setCatalog({});
                setTemplates({});
            } else {
                setCatalog(catData);
                setTemplates(tempData);
            }
        } catch (err) {
            setError("Erreur lors du chargement des données.");
        } finally {
            setLoading(false);
        }
    };

    const handleInitialize = async () => {
        setSaving(true);
        try {
            const token = localStorage.getItem('admin_token');
            await Promise.all([
                fetch(`${API_URL}/settings/document-catalog`, {
                    method: 'PATCH',
                    headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                    body: JSON.stringify({ catalog: DEFAULT_DOC_CATALOG })
                }),
                fetch(`${API_URL}/settings/service-templates`, {
                    method: 'PATCH',
                    headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                    body: JSON.stringify({ templates: DEFAULT_SERVICE_TEMPLATES })
                })
            ]);
            await loadData();
        } catch (err) {
            setError("Erreur lors de l'initialisation.");
        } finally {
            setSaving(false);
        }
    };

    const handleSaveDoc = async (doc: DocumentRequirement) => {
        setSaving(true);
        try {
            const token = localStorage.getItem('admin_token');
            const newCatalog = { ...catalog, [doc.id]: doc };
            await fetch(`${API_URL}/settings/document-catalog`, {
                method: 'PATCH',
                headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                body: JSON.stringify({ catalog: newCatalog })
            });
            setCatalog(newCatalog);
            setDocEditorOpen(false);
        } catch (err) {
            setError("Erreur lors de l'enregistrement du document.");
        } finally {
            setSaving(false);
        }
    };

    const handleDeleteDoc = async (docId: string) => {
        if (!confirm(`Supprimer définitivement la pièce "${effectiveCatalog[docId].label}" ?`)) return;
        setSaving(true);
        try {
            const token = localStorage.getItem('admin_token');
            const newCatalog = { ...catalog };
            delete newCatalog[docId];

            const newTemplates = { ...templates };
            Object.keys(newTemplates).forEach(sId => {
                newTemplates[sId] = newTemplates[sId].filter(id => id !== docId);
            });

            await Promise.all([
                fetch(`${API_URL}/settings/document-catalog`, {
                    method: 'PATCH',
                    headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                    body: JSON.stringify({ catalog: newCatalog })
                }),
                fetch(`${API_URL}/settings/service-templates`, {
                    method: 'PATCH',
                    headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                    body: JSON.stringify({ templates: newTemplates })
                })
            ]);

            setCatalog(newCatalog);
            setTemplates(newTemplates);
            setSelectedDocId(null);
        } catch (err) {
            setError("Erreur lors de la suppression.");
        } finally {
            setSaving(false);
        }
    };

    const handleSaveMapping = async (serviceId: string, docIds: string[]) => {
        setSaving(true);
        try {
            const token = localStorage.getItem('admin_token');
            const newTemplates = { ...templates, [serviceId]: docIds };
            await fetch(`${API_URL}/settings/service-templates`, {
                method: 'PATCH',
                headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                body: JSON.stringify({ templates: newTemplates })
            });
            setTemplates(newTemplates);
            setMappingEditorOpen(false);
        } catch (err) {
            setError("Erreur lors de l'enregistrement du mappage.");
        } finally {
            setSaving(false);
        }
    };

    const effectiveCatalog = Object.keys(catalog).length > 0 ? catalog : DEFAULT_DOC_CATALOG;
    const effectiveTemplates = Object.keys(catalog).length > 0 ? templates : DEFAULT_SERVICE_TEMPLATES;

    const allDocs = Object.values(effectiveCatalog);

    const filteredDocs = allDocs.filter(doc => {
        const matchesSearch = doc.label.toLowerCase().includes(searchQuery.toLowerCase()) ||
            doc.id.toLowerCase().includes(searchQuery.toLowerCase());
        const matchesCat = selectedCategory === 'ALL' || doc.category === selectedCategory;
        return matchesSearch && matchesCat;
    });

    const selectedDoc = selectedDocId ? effectiveCatalog[selectedDocId] : null;

    const servicesUsingDoc = selectedDocId ? Object.entries(effectiveTemplates)
        .filter(([_, docs]) => docs.includes(selectedDocId))
        .map(([serviceId]) => {
            const service = SERVICES_CATALOG.find(s => s.id === serviceId);
            return { id: serviceId, label: service?.title || serviceId };
        }) : [];

    return (
        <div className="space-y-6 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* Header / Sub-tabs */}
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <FileStack size={20} className="text-indigo-600" />
                        Référentiel des Pièces
                    </h2>
                    <p className="text-slate-500 text-sm mt-1">
                        Gestion du catalogue des documents et des exigences par service.
                    </p>
                </div>
                <div className="flex p-1 bg-slate-100 rounded-xl">
                    <button
                        onClick={() => setViewMode('CATALOG')}
                        className={`flex items-center gap-2 px-4 py-2 rounded-lg text-xs font-black uppercase tracking-tight transition-all ${viewMode === 'CATALOG' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                    >
                        <Box size={14} /> Catalogue
                    </button>
                    <button
                        onClick={() => setViewMode('MAPPING')}
                        className={`flex items-center gap-2 px-4 py-2 rounded-lg text-xs font-black uppercase tracking-tight transition-all ${viewMode === 'MAPPING' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                    >
                        <LayoutPanelLeft size={14} /> Mappage Services
                    </button>
                </div>
            </div>

            {loading && (
                <div className="p-12 text-center animate-pulse">
                    <Loader2 size={40} className="text-indigo-600 animate-spin mx-auto mb-4" />
                    <p className="text-slate-500 font-bold">Synchronisation du référentiel...</p>
                </div>
            )}

            {!loading && (
                <>
                    {error && (
                        <div className="p-4 bg-red-50 border border-red-200 rounded-2xl flex items-center gap-3 text-red-600 text-sm font-bold animate-in slide-in-from-top-2">
                            <XCircle size={18} />
                            {error}
                            <button onClick={() => setError(null)} className="ml-auto text-red-400 hover:text-red-600">×</button>
                        </div>
                    )}

                    {viewMode === 'CATALOG' ? (
                        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                            <div className="lg:col-span-2 space-y-4">
                                <div className="flex flex-wrap items-center gap-3">
                                    <div className="relative flex-1 min-w-[300px]">
                                        <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={16} />
                                        <input
                                            type="text"
                                            placeholder="Rechercher un document (ex: passeport)..."
                                            className="w-full pl-11 pr-4 py-3 bg-white border-2 border-slate-100 rounded-2xl font-bold text-sm outline-none focus:border-indigo-600 transition-all shadow-sm shadow-slate-100"
                                            value={searchQuery}
                                            onChange={e => setSearchQuery(e.target.value)}
                                        />
                                    </div>
                                    <div className="flex p-1 bg-slate-100 rounded-xl overflow-x-auto max-w-full">
                                        {(['ALL', 'IDENTITY', 'FINANCIAL', 'RESIDENCE', 'PROFESSIONAL', 'EDUCATION'] as const).map(cat => (
                                            <button
                                                key={cat}
                                                onClick={() => setSelectedCategory(cat)}
                                                className={`px-3 py-1.5 rounded-lg text-[10px] font-black uppercase tracking-tight whitespace-nowrap transition-all ${selectedCategory === cat ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-500 hover:text-slate-700'}`}
                                            >
                                                {cat === 'ALL' ? 'Tous' : cat}
                                            </button>
                                        ))}
                                    </div>
                                </div>

                                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                                    {filteredDocs.map(doc => {
                                        const Icon = CATEGORY_ICONS[doc.category] || Box;
                                        const isSelected = selectedDocId === doc.id;
                                        return (
                                            <div
                                                key={doc.id}
                                                onClick={() => setSelectedDocId(doc.id)}
                                                className={`group p-4 bg-white rounded-2xl border-2 transition-all cursor-pointer hover:shadow-xl hover:shadow-slate-100 ${isSelected ? 'border-indigo-600 ring-4 ring-indigo-50' : 'border-slate-100 hover:border-slate-200'}`}
                                            >
                                                <div className="flex items-start gap-4">
                                                    <div className={`w-10 h-10 rounded-xl flex items-center justify-center shrink-0 border ${CATEGORY_COLORS[doc.category]}`}>
                                                        <Icon size={20} />
                                                    </div>
                                                    <div className="flex-1 min-w-0">
                                                        <div className="flex items-center justify-between gap-2">
                                                            <h4 className="font-black text-slate-900 text-sm truncate uppercase tracking-tight">{doc.label}</h4>
                                                            <div className="flex gap-1 shrink-0">
                                                                {doc.ocrType && (
                                                                    <div className="flex items-center gap-1 px-1.5 py-0.5 bg-indigo-50 text-indigo-600 rounded-md text-[9px] font-black uppercase">
                                                                        <Plus size={8} /> OCR
                                                                    </div>
                                                                )}
                                                            </div>
                                                        </div>
                                                        <p className="text-[11px] text-slate-500 font-medium line-clamp-2 mt-1 leading-tight">{doc.description}</p>
                                                        <div className="flex items-center gap-3 mt-3">
                                                            <span className="text-[10px] font-black uppercase text-indigo-400 tracking-widest">{doc.id}</span>
                                                            <span className={`text-[10px] font-black uppercase tracking-widest ${doc.required ? 'text-red-400' : 'text-slate-300'}`}>
                                                                {doc.required ? 'Obligatoire' : 'Facultatif'}
                                                            </span>
                                                        </div>
                                                    </div>
                                                </div>
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>

                            <div className="space-y-6">
                                {selectedDoc ? (
                                    <div className="sticky top-6 bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-2xl space-y-8 animate-in zoom-in-95 duration-300">
                                        <div className="space-y-4 text-center">
                                            <div className={`w-20 h-20 mx-auto rounded-3xl flex items-center justify-center border-2 ${CATEGORY_COLORS[selectedDoc.category]}`}>
                                                {React.createElement(CATEGORY_ICONS[selectedDoc.category] || Box, { size: 32 })}
                                            </div>
                                            <div>
                                                <h3 className="text-xl font-black text-slate-900 uppercase tracking-tighter">{selectedDoc.label}</h3>
                                                <p className="text-sm text-slate-500 font-bold">{selectedDoc.category}</p>
                                            </div>
                                            <div className="flex justify-center gap-2">
                                                <div className="px-3 py-1 bg-slate-900 text-white rounded-full text-[10px] font-black uppercase tracking-widest">
                                                    ID: {selectedDoc.id}
                                                </div>
                                                {selectedDoc.ocrType && (
                                                    <div className="px-3 py-1 bg-indigo-600 text-white rounded-full text-[10px] font-black uppercase tracking-widest flex items-center gap-1">
                                                        <ShieldCheck size={10} /> OCR ACTIF
                                                    </div>
                                                )}
                                            </div>
                                        </div>

                                        <div className="space-y-4 pt-6 border-t border-slate-100">
                                            <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Configuration Technique</h4>
                                            <div className="grid grid-cols-1 gap-3">
                                                <div className="p-4 bg-slate-50 rounded-2xl border border-slate-100 space-y-2">
                                                    <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Description Administrative</p>
                                                    <p className="text-xs font-medium text-slate-600 leading-relaxed italic">"{selectedDoc.description}"</p>
                                                </div>
                                                <div className="flex items-center justify-between p-4 bg-slate-50 rounded-2xl border border-slate-100">
                                                    <span className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Taille Max</span>
                                                    <span className="text-sm font-black text-slate-900">{selectedDoc.maxSizeMB || 5} MB</span>
                                                </div>
                                                <div className="flex items-center justify-between p-4 bg-slate-50 rounded-2xl border border-slate-100">
                                                    <span className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Formats</span>
                                                    <span className="text-[10px] font-black text-indigo-600">PDF, JPG, PNG</span>
                                                </div>
                                            </div>
                                        </div>

                                        <div className="space-y-4 pt-6 border-t border-slate-100">
                                            <div className="flex items-center justify-between">
                                                <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Utilisation dans les services</h4>
                                                <span className="px-2 py-0.5 bg-slate-100 rounded text-[10px] font-black text-slate-600">{servicesUsingDoc.length}</span>
                                            </div>
                                            <div className="space-y-2 max-h-[200px] overflow-auto pr-2 custom-scrollbar">
                                                {servicesUsingDoc.length > 0 ? servicesUsingDoc.map(s => (
                                                    <div key={s.id} className="flex items-center gap-3 p-3 bg-slate-50 rounded-xl border border-slate-100 group hover:border-indigo-200 transition-all">
                                                        <div className="w-1.5 h-1.5 rounded-full bg-indigo-400 group-hover:scale-150 transition-all" />
                                                        <span className="text-[11px] font-bold text-slate-700 truncate">{s.label}</span>
                                                        <ChevronRight size={12} className="ml-auto text-slate-300" />
                                                    </div>
                                                )) : (
                                                    <p className="text-center py-4 text-[10px] font-bold text-slate-400 uppercase italic">Non utilisé pour le moment</p>
                                                )}
                                            </div>
                                        </div>

                                        <div className="flex gap-2">
                                            <button
                                                onClick={() => { setEditingDoc(selectedDoc); setDocEditorOpen(true); }}
                                                className="flex-1 py-4 bg-slate-900 text-white rounded-2xl font-black text-sm hover:bg-black transition-all shadow-xl shadow-slate-200 flex items-center justify-center gap-2"
                                            >
                                                <Edit3 size={16} /> Éditer la pièce
                                            </button>
                                            <button
                                                onClick={() => handleDeleteDoc(selectedDoc.id)}
                                                className="w-14 h-14 bg-red-50 text-red-500 rounded-2xl flex items-center justify-center hover:bg-red-500 hover:text-white transition-all border border-red-100"
                                                title="Supprimer du catalogue"
                                            >
                                                <Trash2 size={20} />
                                            </button>
                                        </div>
                                    </div>
                                ) : (
                                    <div className="sticky top-6 bg-slate-50 rounded-[2rem] p-12 border-2 border-dashed border-slate-200 flex flex-col items-center justify-center text-center space-y-4 min-h-[500px]">
                                        <div className="w-16 h-16 bg-white rounded-3xl flex items-center justify-center shadow-lg text-slate-300">
                                            <Info size={32} />
                                        </div>
                                        <div>
                                            <h3 className="text-lg font-black text-slate-900 uppercase tracking-tighter">Inspecteur de Pièces</h3>
                                            <p className="text-sm text-slate-400 font-medium">Sélectionnez un document pour voir ses détails techniques et son taux d'utilisation.</p>
                                        </div>
                                    </div>
                                )}
                            </div>
                        </div>
                    ) : (
                        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
                            <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm overflow-auto max-h-[800px]">
                                <table className="w-full border-separate border-spacing-y-2">
                                    <thead>
                                        <tr className="text-left">
                                            <th className="px-4 py-2 text-[10px] font-black uppercase tracking-widest text-slate-400">Service / Procédure</th>
                                            <th className="px-4 py-2 text-[10px] font-black uppercase tracking-widest text-slate-400 text-center">Nb Pièces</th>
                                            <th className="px-4 py-2 text-[10px] font-black uppercase tracking-widest text-slate-400 text-right">Actions</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {Object.entries(effectiveTemplates).map(([serviceId, docIds]) => {
                                            const service = SERVICES_CATALOG.find(s => s.id === serviceId);
                                            return (
                                                <tr key={serviceId} className="group hover:bg-slate-50 transition-all rounded-xl">
                                                    <td className="px-4 py-4 border-y border-l border-slate-100 rounded-l-xl">
                                                        <div>
                                                            <p className="font-black text-slate-900 text-xs uppercase truncate max-w-[250px]">{service?.title || serviceId}</p>
                                                            <p className="text-[9px] text-slate-400 font-bold uppercase tracking-tighter mt-0.5">{serviceId}</p>
                                                        </div>
                                                    </td>
                                                    <td className="px-4 py-4 border-y border-slate-100 text-center">
                                                        <div className="inline-flex items-center justify-center w-8 h-8 rounded-lg bg-indigo-50 text-indigo-600 font-black text-xs">
                                                            {(docIds as string[]).length}
                                                        </div>
                                                    </td>
                                                    <td className="px-4 py-4 border-y border-r border-slate-100 rounded-r-xl text-right">
                                                        <button
                                                            onClick={() => { setEditingServiceId(serviceId); setMappingEditorOpen(true); }}
                                                            className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-all"
                                                        >
                                                            <Edit3 size={18} />
                                                        </button>
                                                    </td>
                                                </tr>
                                            );
                                        })}
                                    </tbody>
                                </table>
                            </div>

                            <div className="space-y-6">
                                <div className="bg-indigo-900 rounded-[2rem] p-8 text-white shadow-2xl space-y-6 overflow-hidden relative">
                                    <div className="absolute top-0 right-0 p-8 opacity-10 rotate-12">
                                        <FileStack size={120} />
                                    </div>
                                    <div className="relative z-10 space-y-4">
                                        <h3 className="text-2xl font-black uppercase tracking-tighter">Éditeur de Mappage</h3>
                                        <p className="text-indigo-200 text-xs font-medium leading-relaxed">
                                            Le mappage détermine quelle liste de documents un client verra sur son Dashboard une fois qu'il aura souscrit au service.
                                        </p>
                                        <div className="p-6 bg-white/10 rounded-2xl border border-white/10 space-y-4">
                                            <div className="flex items-center gap-3">
                                                <div className="w-2 h-2 rounded-full bg-emerald-400" />
                                                <p className="text-[10px] font-black uppercase tracking-widest text-emerald-400">Optimisation KYC</p>
                                            </div>
                                            <p className="text-[11px] font-bold text-white leading-relaxed">
                                                Conseil SimuLegal : Ne demandez que le strict nécessaire pour éviter l'abandon client. Activez l'OCR pour les pièces d'identité afin d'accélérer la vérification de conformité.
                                            </p>
                                        </div>
                                    </div>
                                </div>

                                <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-6">
                                    <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Statistiques Catalogue</h4>
                                    <div className="grid grid-cols-2 gap-4">
                                        <div className="p-6 bg-slate-50 rounded-2xl border border-slate-100">
                                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Documents</p>
                                            <p className="text-3xl font-black text-slate-900 mt-1">{Object.keys(effectiveCatalog).length}</p>
                                        </div>
                                        <div className="p-6 bg-slate-50 rounded-2xl border border-slate-100">
                                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Modèles</p>
                                            <p className="text-3xl font-black text-slate-900 mt-1">{Object.keys(effectiveTemplates).length}</p>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    )}

                    {Object.keys(catalog).length === 0 && (
                        <div className="bg-amber-50 rounded-[2rem] p-8 border-2 border-amber-200 border-dashed text-center space-y-4 mt-8">
                            <div className="w-12 h-12 bg-white rounded-2xl flex items-center justify-center mx-auto shadow-md text-amber-500">
                                <AlertCircle size={24} />
                            </div>
                            <div>
                                <h4 className="font-black text-amber-900 uppercase tracking-tight">Catalogue en lecture seule</h4>
                                <p className="text-amber-700 text-sm font-medium">Le catalogue est actuellement chargé depuis les fichiers statiques. Pour pouvoir modifier les pièces ou les mappages, vous devez initialiser la base de données.</p>
                            </div>
                            <button
                                onClick={handleInitialize}
                                disabled={saving}
                                className="px-6 py-3 bg-amber-600 text-white rounded-xl text-xs font-black uppercase tracking-widest hover:bg-amber-700 transition-all flex items-center gap-2 mx-auto disabled:opacity-50"
                            >
                                {saving ? <Loader2 size={16} className="animate-spin" /> : <RefreshCw size={16} />}
                                Initialiser la base de données
                            </button>
                        </div>
                    )}
                </>
            )}

            {/* Modals */}
            {docEditorOpen && (
                <DocEditorModal
                    doc={editingDoc}
                    onSave={handleSaveDoc}
                    onClose={() => setDocEditorOpen(false)}
                    isSaving={saving}
                />
            )}

            {mappingEditorOpen && editingServiceId && (
                <MappingEditorModal
                    serviceId={editingServiceId}
                    initialDocs={effectiveTemplates[editingServiceId] || []}
                    onSave={handleSaveMapping}
                    onClose={() => setMappingEditorOpen(false)}
                    catalog={effectiveCatalog}
                    isSaving={saving}
                />
            )}

            <button
                onClick={() => { setEditingDoc(undefined); setDocEditorOpen(true); }}
                className="fixed bottom-8 right-8 w-16 h-16 bg-indigo-600 text-white rounded-full shadow-2xl shadow-indigo-200 flex items-center justify-center hover:scale-110 active:scale-95 transition-all z-40 group"
            >
                <Plus size={32} />
                <span className="absolute right-full mr-4 px-3 py-1 bg-slate-900 text-white text-[10px] font-black uppercase tracking-widest rounded-lg opacity-0 group-hover:opacity-100 transition-opacity whitespace-nowrap">
                    Ajouter une pièce
                </span>
            </button>
        </div>
    );
}

// ─── MODALS ────────────────────────────────────────────────────────

function DocEditorModal({ doc, onSave, onClose, isSaving }: { doc?: DocumentRequirement, onSave: (doc: DocumentRequirement) => void, onClose: () => void, isSaving: boolean }) {
    const [formData, setFormData] = useState<DocumentRequirement>(doc || {
        id: '',
        label: '',
        description: '',
        category: 'OTHER',
        required: true,
        maxSizeMB: 5
    });

    return (
        <div className="fixed inset-0 bg-slate-900/40 backdrop-blur-sm z-[100] flex items-center justify-center p-4">
            <div className="bg-white rounded-[2.5rem] shadow-2xl w-full max-w-xl overflow-hidden animate-in zoom-in-95 duration-300">
                <div className="p-8 border-b border-slate-100 flex items-center justify-between">
                    <div>
                        <h3 className="text-xl font-black text-slate-900 uppercase tracking-tighter">
                            {doc ? 'Éditer la pièce' : 'Nouvelle pièce'}
                        </h3>
                        <p className="text-slate-400 text-xs font-bold uppercase tracking-widest mt-1">Référentiel Documentaire</p>
                    </div>
                    <button onClick={onClose} className="w-10 h-10 bg-slate-50 text-slate-400 rounded-xl flex items-center justify-center hover:bg-slate-100 transition-all">
                        <XCircle size={20} />
                    </button>
                </div>

                <div className="p-8 space-y-6">
                    <div className="grid grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Identifiant Technique</label>
                            <input
                                type="text"
                                disabled={!!doc}
                                placeholder="PASSPORT_V2"
                                className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600 disabled:opacity-50"
                                value={formData.id}
                                onChange={e => setFormData({ ...formData, id: e.target.value.toUpperCase() })}
                            />
                        </div>
                        <div className="space-y-2">
                            <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Catégorie</label>
                            <select
                                className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600"
                                value={formData.category}
                                onChange={e => setFormData({ ...formData, category: e.target.value as DocumentCategory })}
                            >
                                {(['IDENTITY', 'FINANCIAL', 'RESIDENCE', 'CIVIL', 'PROFESSIONAL', 'EDUCATION', 'OTHER'] as const).map(c => (
                                    <option key={c} value={c}>{c}</option>
                                ))}
                            </select>
                        </div>
                    </div>

                    <div className="space-y-2">
                        <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Libellé (Affiché au client)</label>
                        <input
                            type="text"
                            placeholder="Passeport en cours de validité"
                            className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600"
                            value={formData.label}
                            onChange={e => setFormData({ ...formData, label: e.target.value })}
                        />
                    </div>

                    <div className="space-y-2">
                        <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Description / Aide</label>
                        <textarea
                            placeholder="Scannez la page avec la photo et la signature..."
                            rows={3}
                            className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600 resize-none"
                            value={formData.description}
                            onChange={e => setFormData({ ...formData, description: e.target.value })}
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Type OCR (IA)</label>
                            <select
                                className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600"
                                value={formData.ocrType || ''}
                                onChange={e => setFormData({ ...formData, ocrType: (e.target.value || undefined) as OCRType })}
                            >
                                <option value="">Aucun OCR</option>
                                {OCR_TYPES.map(t => <option key={t} value={t}>{t}</option>)}
                            </select>
                        </div>
                        <div className="space-y-2">
                            <label className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Taille Max (MB)</label>
                            <input
                                type="number"
                                className="w-full px-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-xl font-bold text-sm outline-none focus:border-indigo-600"
                                value={formData.maxSizeMB || 5}
                                onChange={e => setFormData({ ...formData, maxSizeMB: parseInt(e.target.value) })}
                            />
                        </div>
                    </div>

                    <div className="flex items-center gap-4">
                        <label className="flex items-center gap-2 cursor-pointer group">
                            <div className={`w-5 h-5 rounded border-2 flex items-center justify-center transition-all ${formData.required ? 'bg-indigo-600 border-indigo-600' : 'bg-white border-slate-200 group-hover:border-indigo-400'}`}>
                                <input
                                    type="checkbox"
                                    className="hidden"
                                    checked={formData.required}
                                    onChange={e => setFormData({ ...formData, required: e.target.checked })}
                                />
                                {formData.required && <Check size={14} className="text-white" />}
                            </div>
                            <span className="text-xs font-bold text-slate-700">Pièce obligatoire par défaut</span>
                        </label>
                    </div>

                    <div className="flex gap-4 pt-4">
                        <button
                            onClick={onClose}
                            className="flex-1 py-4 bg-slate-100 text-slate-500 rounded-2xl font-black text-sm hover:bg-slate-200 transition-all uppercase tracking-widest"
                        >
                            Annuler
                        </button>
                        <button
                            onClick={() => onSave(formData)}
                            disabled={isSaving || !formData.id || !formData.label}
                            className="flex-1 py-4 bg-indigo-600 text-white rounded-2xl font-black text-sm hover:bg-indigo-700 transition-all uppercase tracking-widest shadow-xl shadow-indigo-200 flex items-center justify-center gap-2 disabled:opacity-50"
                        >
                            {isSaving ? <Loader2 size={18} className="animate-spin" /> : <Save size={18} />}
                            Enregistrer
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}

function MappingEditorModal({ serviceId, initialDocs, onSave, onClose, catalog, isSaving }: { serviceId: string, initialDocs: string[], onSave: (sId: string, docs: string[]) => void, onClose: () => void, catalog: Record<string, DocumentRequirement>, isSaving: boolean }) {
    const [selectedIds, setSelectedIds] = useState<string[]>(initialDocs);
    const [search, setSearch] = useState('');
    const service = SERVICES_CATALOG.find(s => s.id === serviceId);

    const toggleId = (id: string) => {
        setSelectedIds(prev => prev.includes(id) ? prev.filter(i => i !== id) : [...prev, id]);
    };

    const categories: DocumentCategory[] = ['IDENTITY', 'FINANCIAL', 'RESIDENCE', 'CIVIL', 'PROFESSIONAL', 'EDUCATION', 'OTHER'];

    return (
        <div className="fixed inset-0 bg-slate-900/40 backdrop-blur-sm z-[100] flex items-center justify-center p-4">
            <div className="bg-white rounded-[2.5rem] shadow-2xl w-full max-w-4xl overflow-hidden animate-in zoom-in-95 duration-300 flex flex-col max-h-[90vh]">
                <div className="p-8 border-b border-slate-100 flex items-center justify-between shrink-0">
                    <div>
                        <h3 className="text-xl font-black text-slate-900 uppercase tracking-tighter">
                            Éditeur de Mappage : {service?.title || serviceId}
                        </h3>
                        <p className="text-slate-400 text-xs font-bold uppercase tracking-widest mt-1">Établissez la liste des pièces requises</p>
                    </div>
                    <button onClick={onClose} className="w-10 h-10 bg-slate-50 text-slate-400 rounded-xl flex items-center justify-center hover:bg-slate-100 transition-all">
                        <XCircle size={20} />
                    </button>
                </div>

                <div className="flex-1 overflow-hidden flex">
                    <div className="w-80 border-r border-slate-100 p-8 bg-slate-50/50 flex flex-col gap-6 overflow-auto">
                        <div className="space-y-2">
                            <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Pièces sélectionnées ({selectedIds.length})</h4>
                            <div className="space-y-2">
                                {selectedIds.map(id => (
                                    <div key={id} className="p-3 bg-white rounded-xl border border-indigo-200 shadow-sm flex items-center justify-between group">
                                        <span className="text-[10px] font-black text-slate-700 truncate">{catalog[id]?.label || id}</span>
                                        <button onClick={() => toggleId(id)} className="text-slate-300 hover:text-red-500 transition-colors">
                                            <Trash2 size={12} />
                                        </button>
                                    </div>
                                ))}
                                {selectedIds.length === 0 && (
                                    <p className="text-[10px] font-bold text-slate-400 italic text-center py-8">Aucune pièce sélectionnée</p>
                                )}
                            </div>
                        </div>
                    </div>

                    <div className="flex-1 p-8 flex flex-col gap-6">
                        <div className="relative">
                            <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={16} />
                            <input
                                type="text"
                                placeholder="Rechercher dans le catalogue..."
                                className="w-full pl-11 pr-4 py-3 bg-slate-50 border-2 border-slate-100 rounded-2xl font-bold text-sm outline-none focus:border-indigo-600"
                                value={search}
                                onChange={e => setSearch(e.target.value)}
                            />
                        </div>

                        <div className="flex-1 overflow-auto pr-2 custom-scrollbar space-y-8">
                            {categories.map(cat => {
                                const docs = Object.values(catalog).filter(d => d.category === cat && d.label.toLowerCase().includes(search.toLowerCase()));
                                if (docs.length === 0) return null;
                                return (
                                    <div key={cat} className="space-y-2">
                                        <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">{cat}</h4>
                                        <div className="grid grid-cols-2 gap-2">
                                            {docs.map(doc => {
                                                const isActive = selectedIds.includes(doc.id);
                                                return (
                                                    <div
                                                        key={doc.id}
                                                        onClick={() => toggleId(doc.id)}
                                                        className={`p-3 rounded-xl border-2 cursor-pointer transition-all flex items-center justify-between group ${isActive ? 'bg-indigo-600 border-indigo-600 text-white' : 'bg-white border-slate-100 hover:border-indigo-200'}`}
                                                    >
                                                        <span className="text-[11px] font-bold truncate max-w-[150px]">{doc.label}</span>
                                                        <div className={`w-5 h-5 rounded-full flex items-center justify-center transition-all ${isActive ? 'bg-white text-indigo-600' : 'border-2 border-slate-100 bg-slate-50 text-transparent group-hover:border-indigo-200'}`}>
                                                            <Check size={12} />
                                                        </div>
                                                    </div>
                                                );
                                            })}
                                        </div>
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                </div>

                <div className="p-8 border-t border-slate-100 shrink-0 bg-white">
                    <div className="flex gap-4">
                        <button
                            onClick={onClose}
                            className="flex-1 py-4 bg-slate-100 text-slate-500 rounded-2xl font-black text-sm hover:bg-slate-200 transition-all uppercase tracking-widest"
                        >
                            Annuler
                        </button>
                        <button
                            onClick={() => onSave(serviceId, selectedIds)}
                            disabled={isSaving}
                            className="flex-1 py-4 bg-indigo-600 text-white rounded-2xl font-black text-sm hover:bg-indigo-700 transition-all uppercase tracking-widest shadow-xl shadow-indigo-200 flex items-center justify-center gap-2 disabled:opacity-50"
                        >
                            {isSaving ? <Loader2 size={18} className="animate-spin" /> : <Save size={18} />}
                            Sauvegarder le mappage
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}
