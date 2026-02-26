'use client';

import React, { useState, useCallback, useMemo, useEffect } from 'react';
import {
    Eye,
    Settings,
    Brain,
    Scale,
    AlertTriangle,
    Clock,
    ChevronRight,
    Bookmark,
    FileText,
    History,
    Zap,
    Plus,
    X,
    Trash2,
    CheckCircle,
    ExternalLink,
    Edit3,
    ChevronDown,
    Send,
    Radar,
    Bot,
    Loader2
} from 'lucide-react';
import ServiceConfigPanel from './ServiceConfigPanel';
import EligibilityConfigPanel from './EligibilityConfigPanel';
import LegalMatrixModule from './LegalMatrixModule';
import VeilleStore, { VeilleNote } from '../../services/VeilleStore';

type TabType = 'veille' | 'services' | 'eligibility' | 'matrix';

// ─── Note Composer / Editor Modal ─────────────────────────────────
function NoteComposer({ note, onSave, onClose }: {
    note?: VeilleNote;
    onSave: (data: Omit<VeilleNote, 'id' | 'date' | 'applied'>) => void;
    onClose: () => void;
}) {
    const [title, setTitle] = useState(note?.title || '');
    const [summary, setSummary] = useState(note?.summary || '');
    const [category, setCategory] = useState(note?.category || VeilleStore.CATEGORIES[0]);
    const [severity, setSeverity] = useState<'high' | 'medium' | 'low'>(note?.severity || 'medium');
    const [sourceUrl, setSourceUrl] = useState(note?.sourceUrl || '');
    const [authorName, setAuthorName] = useState(note?.authorName || '');
    const [linkedRuleIds, setLinkedRuleIds] = useState<string[]>(note?.linkedRuleIds || []);
    const [ruleSearchQuery, setRuleSearchQuery] = useState('');

    // Load all available rules for linking
    const allRules = useMemo(() => {
        const categories = [
            { key: 'sejour', label: '🛂 Séjour' },
            { key: 'naturalisation', label: '🇫🇷 Nat.' },
            { key: 'family', label: '👨‍👩‍👧 Famille' },
            { key: 'asile', label: '🛡️ Asile' },
        ];
        const result: { id: string; name: string; catLabel: string }[] = [];
        try {
            const { EligibilityStore } = require('../../services/EligibilityStore');
            categories.forEach(cat => {
                const rules = EligibilityStore.getRules(cat.key);
                rules.forEach((r: any) => result.push({ id: r.id, name: r.name, catLabel: cat.label }));
            });
        } catch { /* fallback: empty */ }
        return result;
    }, []);

    const filteredRules = useMemo(() => {
        if (!ruleSearchQuery) return allRules.slice(0, 15);
        return allRules.filter(r =>
            r.name.toLowerCase().includes(ruleSearchQuery.toLowerCase()) ||
            r.id.toLowerCase().includes(ruleSearchQuery.toLowerCase())
        ).slice(0, 15);
    }, [allRules, ruleSearchQuery]);

    const toggleRuleLink = (ruleId: string) => {
        setLinkedRuleIds(prev =>
            prev.includes(ruleId) ? prev.filter(id => id !== ruleId) : [...prev, ruleId]
        );
    };

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        if (!title.trim() || !summary.trim()) return;
        onSave({ title, summary, category, severity, sourceUrl, authorName, linkedRuleIds });
    };

    return (
        <div className="fixed inset-0 bg-black/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
            <div className="bg-white rounded-3xl shadow-2xl w-full max-w-2xl max-h-[90vh] overflow-auto animate-in fade-in slide-in-from-bottom-4 duration-300">
                <div className="flex items-center justify-between p-6 border-b border-slate-100">
                    <h2 className="text-lg font-black text-slate-900 uppercase tracking-tight flex items-center gap-2">
                        <Edit3 className="text-indigo-600" size={20} />
                        {note ? 'Modifier la note' : 'Nouvelle note de veille'}
                    </h2>
                    <button onClick={onClose} className="w-8 h-8 rounded-xl bg-slate-100 flex items-center justify-center hover:bg-slate-200 transition-colors">
                        <X size={16} />
                    </button>
                </div>
                <form onSubmit={handleSubmit} className="p-6 space-y-5">
                    <div>
                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">Titre *</label>
                        <input type="text" value={title} onChange={(e) => setTitle(e.target.value)}
                            placeholder="Ex: Nouveau décret sur le Passeport Talent"
                            className="w-full h-12 px-4 bg-slate-50 border border-slate-200 rounded-xl text-sm font-bold text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent" required />
                    </div>
                    <div>
                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">Résumé *</label>
                        <textarea value={summary} onChange={(e) => setSummary(e.target.value)}
                            placeholder="Résumez l'impact sur les procédures..." rows={3}
                            className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl text-sm font-medium text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent resize-none" required />
                    </div>
                    <div className="grid grid-cols-2 gap-4">
                        <div>
                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">Catégorie</label>
                            <select value={category} onChange={(e) => setCategory(e.target.value)}
                                className="w-full h-12 px-4 bg-slate-50 border border-slate-200 rounded-xl text-sm font-bold text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500">
                                {VeilleStore.CATEGORIES.map(cat => (<option key={cat} value={cat}>{cat}</option>))}
                            </select>
                        </div>
                        <div>
                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">Priorité</label>
                            <div className="flex gap-2">
                                {([
                                    { value: 'high', label: 'Haute', color: 'border-red-300 bg-red-50 text-red-700' },
                                    { value: 'medium', label: 'Moyenne', color: 'border-amber-300 bg-amber-50 text-amber-700' },
                                    { value: 'low', label: 'Basse', color: 'border-blue-300 bg-blue-50 text-blue-700' },
                                ] as const).map(sev => (
                                    <button key={sev.value} type="button" onClick={() => setSeverity(sev.value)}
                                        className={`flex-1 h-12 rounded-xl text-xs font-black uppercase tracking-wider border-2 transition-all ${severity === sev.value
                                            ? sev.color + ' ring-2 ring-offset-1 ring-current'
                                            : 'border-slate-200 bg-white text-slate-400 hover:border-slate-300'}`}>
                                        {sev.label}
                                    </button>
                                ))}
                            </div>
                        </div>
                    </div>
                    <div>
                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">URL de la source (optionnel)</label>
                        <input type="url" value={sourceUrl} onChange={(e) => setSourceUrl(e.target.value)}
                            placeholder="https://www.legifrance.gouv.fr/..."
                            className="w-full h-12 px-4 bg-slate-50 border border-slate-200 rounded-xl text-sm font-medium text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent" />
                    </div>
                    <div>
                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">Juriste rédacteur (optionnel)</label>
                        <input type="text" value={authorName} onChange={(e) => setAuthorName(e.target.value)}
                            placeholder="Ex: Me. Dupont"
                            className="w-full h-12 px-4 bg-slate-50 border border-slate-200 rounded-xl text-sm font-medium text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent" />
                    </div>

                    {/* Linked Rules */}
                    <div>
                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2">
                            🔗 Règles impactées ({linkedRuleIds.length} sélectionnée{linkedRuleIds.length > 1 ? 's' : ''})
                        </label>
                        <input
                            type="text"
                            value={ruleSearchQuery}
                            onChange={(e) => setRuleSearchQuery(e.target.value)}
                            placeholder="Rechercher une règle..."
                            className="w-full h-10 px-4 mb-2 bg-slate-50 border border-slate-200 rounded-xl text-sm font-medium text-slate-900 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                        />
                        <div className="max-h-32 overflow-auto border border-slate-200 rounded-xl p-2 space-y-1 bg-slate-50">
                            {filteredRules.map(rule => (
                                <label key={rule.id} className={`flex items-center gap-2 px-2 py-1.5 rounded-lg cursor-pointer text-xs transition-colors ${linkedRuleIds.includes(rule.id) ? 'bg-indigo-100 text-indigo-800 font-bold' : 'hover:bg-slate-100 text-slate-600'
                                    }`}>
                                    <input
                                        type="checkbox"
                                        checked={linkedRuleIds.includes(rule.id)}
                                        onChange={() => toggleRuleLink(rule.id)}
                                        className="rounded border-slate-300"
                                    />
                                    <span className="text-[10px] font-black text-slate-400">{rule.catLabel}</span>
                                    <span className="truncate">{rule.name}</span>
                                </label>
                            ))}
                            {filteredRules.length === 0 && (
                                <p className="text-xs text-slate-400 text-center py-2">Aucune règle trouvée</p>
                            )}
                        </div>
                    </div>

                    <div className="flex gap-3 pt-2">
                        <button type="button" onClick={onClose}
                            className="flex-1 h-12 border-2 border-slate-200 rounded-xl text-sm font-black text-slate-500 hover:bg-slate-50 transition-all uppercase tracking-widest">
                            Annuler
                        </button>
                        <button type="submit" disabled={!title.trim() || !summary.trim()}
                            className="flex-1 h-12 bg-indigo-600 rounded-xl text-sm font-black text-white hover:bg-indigo-700 transition-all uppercase tracking-widest flex items-center justify-center gap-2 disabled:opacity-40 disabled:cursor-not-allowed shadow-lg shadow-indigo-200">
                            <Send size={16} /> {note ? 'Mettre à jour' : 'Publier'}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}

// ─── Main Panel ───────────────────────────────────────────────────
export default function AuditVeillePanel() {
    const [activeTab, setActiveTab] = useState<TabType>('veille');
    const [notes, setNotes] = useState<VeilleNote[]>([]);
    const [composerOpen, setComposerOpen] = useState(false);
    const [editingNote, setEditingNote] = useState<VeilleNote | undefined>(undefined);
    const [showArchives, setShowArchives] = useState(false);
    const [notification, setNotification] = useState('');
    const [loading, setLoading] = useState(true);
    const [scanning, setScanning] = useState(false);

    const showNotif = useCallback((msg: string) => {
        setNotification(msg);
        setTimeout(() => setNotification(''), 4000);
    }, []);

    // ─── Load notes (sync from backend on mount) ──────────────
    const loadNotes = useCallback(async () => {
        setLoading(true);
        try {
            const fetched = await VeilleStore.syncFromBackend();
            setNotes(fetched.sort((a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()));
        } catch {
            setNotes(VeilleStore.getAll());
        }
        setLoading(false);
    }, []);

    // ─── Scan RSS sources ─────────────────────────────────────
    const handleScan = useCallback(async () => {
        setScanning(true);
        try {
            const result = await VeilleStore.triggerScan();
            showNotif(result.created > 0
                ? `📡 ${result.created} nouvelle(s) note(s) détectée(s) depuis ${result.sourcesUsed.join(', ')}`
                : '📡 Aucune nouvelle note détectée — vos sources sont à jour');
            if (result.created > 0) await loadNotes();
        } catch (err) {
            showNotif('❌ Erreur lors du scan des sources');
        }
        setScanning(false);
    }, [loadNotes, showNotif]);

    const initialLoadDone = React.useRef(false);

    useEffect(() => {
        loadNotes();
    }, [loadNotes]);

    useEffect(() => {
        if (!loading && !initialLoadDone.current && notes.length > 0) {
            const pending = notes.filter(n => !n.applied).length;
            if (pending > 0) {
                showNotif(`⚠️ ${pending} note(s) de veille en attente d'application`);
            }
            initialLoadDone.current = true;
        }
    }, [loading, notes, showNotif]);

    // ─── Dynamic stats ────────────────────────────────────────
    const stats = useMemo(() => VeilleStore.getStats(), [notes]);
    const displayedNotes = showArchives ? notes : notes.filter(n => !n.applied);

    // ─── Handlers ─────────────────────────────────────────────
    const handleSaveNote = async (data: Omit<VeilleNote, 'id' | 'date' | 'applied'>) => {
        if (editingNote) {
            await VeilleStore.update(editingNote.id, data);
            showNotif('✅ Note mise à jour');
        } else {
            await VeilleStore.add(data);
            showNotif('✅ Note publiée');
        }
        setComposerOpen(false);
        setEditingNote(undefined);
        await loadNotes();
    };

    const handleApply = async (note: VeilleNote) => {
        await VeilleStore.markAsApplied(note.id);
        showNotif(`✅ "${note.title}" marquée comme appliquée`);
        await loadNotes();
        if (note.category === 'Immigration Professionnelle' || note.title.toLowerCase().includes('seuil')) {
            setActiveTab('eligibility');
        } else {
            setActiveTab('services');
        }
    };

    const handleDelete = async (id: string) => {
        await VeilleStore.remove(id);
        showNotif('🗑 Note supprimée');
        await loadNotes();
    };

    const handleEdit = (note: VeilleNote) => {
        setEditingNote(note);
        setComposerOpen(true);
    };

    const handlePublishToBlog = async (note: VeilleNote) => {
        const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
        const token = typeof window !== 'undefined' ? localStorage.getItem('admin_token') : null;
        try {
            const res = await fetch(`${API_URL}/blog`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    ...(token ? { 'Authorization': `Bearer ${token}` } : {}),
                },
                body: JSON.stringify({
                    title: note.title,
                    content: `## ${note.title}\n\n${note.summary}\n\n### Ce que vous devez savoir\n\nCette actualite juridique concerne directement les personnes engagees dans des demarches de ${note.category.toLowerCase()} en France.\n\n${note.sourceUrl ? `> **Source** : [Consulter la source](${note.sourceUrl})` : ''}\n\n### Nos recommandations\n\n1. **Verifiez votre situation** : Utilisez notre simulateur pour evaluer l'impact\n2. **Constituez votre dossier** : Rassemblez les pieces justificatives\n3. **Anticipez les delais** : Les modifications peuvent impacter les temps de traitement\n\n---\n\n*Article genere depuis la veille juridique SimuLegal.*`,
                    excerpt: note.summary.substring(0, 160),
                    category: note.category.toUpperCase().replace(/ /g, '_').substring(0, 20),
                    status: 'DRAFT',
                    tags: note.category,
                    authorName: note.authorName || 'Veille Juridique',
                    authorRole: 'Equipe juridique',
                    metaTitle: note.title,
                    metaDescription: note.summary.substring(0, 155),
                    readTimeMin: 3,
                }),
            });
            if (res.ok) {
                showNotif(`📝 Article de blog cree en brouillon : "${note.title}"`);
            } else {
                const err = await res.json().catch(() => ({}));
                showNotif(`⚠️ Erreur : ${err.message || 'Impossible de creer l\'article'}`);
            }
        } catch {
            showNotif('❌ Erreur de connexion au serveur');
        }
    };

    const handleConsult = (note: VeilleNote) => {
        if (note.sourceUrl) {
            window.open(note.sourceUrl, '_blank', 'noopener,noreferrer');
        } else {
            showNotif('⚠️ Aucune URL de source renseignée');
        }
    };

    // ─── Severity helpers ─────────────────────────────────────
    const severityColor = (s: string) => s === 'high' ? 'bg-red-500' : s === 'medium' ? 'bg-amber-500' : 'bg-blue-500';
    const severityBadge = (s: string) => {
        if (s === 'high') return { bg: 'bg-red-50', text: 'text-red-700', label: 'HAUTE' };
        if (s === 'medium') return { bg: 'bg-amber-50', text: 'text-amber-700', label: 'MOYENNE' };
        return { bg: 'bg-blue-50', text: 'text-blue-700', label: 'BASSE' };
    };

    // ─── Veille Tab ───────────────────────────────────────────
    const renderVeilleJuridique = () => (
        <div className="p-6 space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {notification && (
                <div className="fixed top-6 right-6 z-50 bg-slate-900 text-white px-6 py-3 rounded-2xl text-sm font-bold shadow-2xl animate-in fade-in slide-in-from-right-4 duration-300">
                    {notification}
                </div>
            )}

            {/* Stats */}
            <div className="grid grid-cols-1 md:grid-cols-5 gap-4">
                {[
                    { icon: <History size={22} />, color: 'indigo', label: 'Dernière note', value: stats.lastUpdateLabel },
                    { icon: <AlertTriangle size={22} />, color: 'amber', label: 'À appliquer', value: `${stats.pendingCount} en attente` },
                    { icon: <CheckCircle size={22} />, color: 'emerald', label: 'Appliquées', value: `${stats.appliedCount} / ${stats.totalCount}` },
                    { icon: <Bot size={22} />, color: 'cyan', label: 'Auto-détectées', value: `${stats.autoDetectedCount || 0} notes` },
                    { icon: <Scale size={22} />, color: 'violet', label: 'Conformité', value: `${stats.conformityPercent}%` },
                ].map((stat, i) => (
                    <div key={i} className={`bg-white p-5 rounded-3xl border border-slate-200 shadow-sm flex items-center gap-4 group hover:border-${stat.color}-500 transition-all`}>
                        <div className={`w-11 h-11 bg-${stat.color}-100 rounded-2xl flex items-center justify-center text-${stat.color}-600 group-hover:scale-110 transition-transform`}>
                            {stat.icon}
                        </div>
                        <div>
                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest">{stat.label}</p>
                            <p className="text-base font-black text-slate-900">{stat.value}</p>
                        </div>
                    </div>
                ))}
            </div>

            {/* Notes list */}
            <div className="space-y-4">
                <div className="flex items-center justify-between">
                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <Eye className="text-indigo-600" size={24} /> Flux de Veille Juridique
                    </h2>
                    <div className="flex items-center gap-3">
                        <button onClick={() => setShowArchives(!showArchives)}
                            className={`text-sm font-bold transition-colors ${showArchives ? 'text-indigo-600' : 'text-slate-400 hover:text-slate-600'}`}>
                            {showArchives ? '🔍 Masquer les appliquées' : '📂 Voir toutes les archives'}
                        </button>
                        <button onClick={handleScan} disabled={scanning}
                            className="flex items-center gap-2 px-4 py-2.5 bg-cyan-600 text-white rounded-xl text-xs font-black uppercase tracking-widest hover:bg-cyan-700 transition-all shadow-lg shadow-cyan-200 active:scale-95 disabled:opacity-50 disabled:cursor-not-allowed">
                            {scanning ? <Loader2 size={16} className="animate-spin" /> : <Radar size={16} />}
                            {scanning ? 'Scan en cours...' : 'Scanner les sources'}
                        </button>
                        <button onClick={() => { setEditingNote(undefined); setComposerOpen(true); }}
                            className="flex items-center gap-2 px-4 py-2.5 bg-indigo-600 text-white rounded-xl text-xs font-black uppercase tracking-widest hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-200 active:scale-95">
                            <Plus size={16} /> Nouvelle note
                        </button>
                    </div>
                </div>

                {loading && (
                    <div className="bg-white rounded-3xl border border-slate-200 p-8 text-center">
                        <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full mx-auto mb-3"></div>
                        <p className="text-sm font-bold text-slate-400">Chargement...</p>
                    </div>
                )}

                {!loading && displayedNotes.length === 0 && (
                    <div className="bg-white rounded-3xl border border-dashed border-slate-300 p-12 text-center">
                        <Eye className="mx-auto text-slate-300 mb-3" size={40} />
                        <p className="text-sm font-bold text-slate-400">
                            {showArchives ? 'Aucune note de veille.' : 'Toutes les notes ont été appliquées. 🎉'}
                        </p>
                    </div>
                )}

                <div className="space-y-4">
                    {displayedNotes.map((note) => {
                        const badge = severityBadge(note.severity);
                        const dateFormatted = new Date(note.date).toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' });
                        return (
                            <div key={note.id} className={`bg-white rounded-3xl border p-6 flex gap-4 hover:shadow-xl hover:shadow-indigo-500/5 transition-all group ${note.applied ? 'border-emerald-200 bg-emerald-50/30' : 'border-slate-200'}`}>
                                <div className={`w-1 font-black rounded-full ${severityColor(note.severity)}`} />
                                <div className="flex-1 space-y-2">
                                    <div className="flex items-center justify-between">
                                        <div className="flex items-center gap-2 flex-wrap">
                                            <span className="text-[10px] font-black uppercase tracking-widest px-2 py-1 bg-slate-100 text-slate-500 rounded-lg">{note.category}</span>
                                            <span className={`text-[10px] font-black uppercase tracking-widest px-2 py-1 rounded-lg ${badge.bg} ${badge.text}`}>{badge.label}</span>
                                            {note.isAutoDetected && (
                                                <span className="text-[10px] font-black uppercase tracking-widest px-2 py-1 bg-cyan-100 text-cyan-700 rounded-lg flex items-center gap-1">
                                                    <Bot size={10} /> Auto
                                                </span>
                                            )}
                                            {note.applied && (
                                                <span className="text-[10px] font-black uppercase tracking-widest px-2 py-1 bg-emerald-100 text-emerald-700 rounded-lg flex items-center gap-1">
                                                    <CheckCircle size={10} /> Appliquée
                                                </span>
                                            )}
                                        </div>
                                        <span className="text-xs font-bold text-slate-400">{dateFormatted}</span>
                                    </div>
                                    <h3 className="font-black text-slate-900 group-hover:text-indigo-600 transition-colors uppercase tracking-tight">{note.title}</h3>
                                    <p className="text-sm text-slate-500 leading-relaxed font-medium">{note.summary}</p>
                                    {note.authorName && <p className="text-[10px] text-slate-400 font-bold">Par {note.authorName}</p>}
                                    <div className="flex gap-2 pt-2">
                                        <button onClick={() => handleConsult(note)} disabled={!note.sourceUrl}
                                            className={`flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold transition-all ${note.sourceUrl ? 'text-slate-600 hover:bg-indigo-600 hover:text-white hover:border-indigo-600' : 'text-slate-300 cursor-not-allowed'}`}>
                                            <ExternalLink size={14} /> Consulter
                                        </button>
                                        {!note.applied && (
                                            <button onClick={() => handleApply(note)}
                                                className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-emerald-600 hover:text-white hover:border-emerald-600 transition-all">
                                                <Zap size={14} /> Appliquer
                                            </button>
                                        )}
                                        <button onClick={() => handleEdit(note)}
                                            className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-violet-600 hover:text-white hover:border-violet-600 transition-all">
                                            <Edit3 size={14} /> Modifier
                                        </button>
                                        <button onClick={() => handlePublishToBlog(note)}
                                            className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-orange-600 hover:text-white hover:border-orange-600 transition-all">
                                            <FileText size={14} /> Publier en article
                                        </button>
                                        <button onClick={() => handleDelete(note.id)}
                                            className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-red-600 hover:text-white hover:border-red-600 transition-all ml-auto">
                                            <Trash2 size={14} />
                                        </button>
                                    </div>
                                </div>
                            </div>
                        );
                    })}
                </div>
            </div>

            {/* CTA Section */}
            <div className="bg-slate-900 rounded-[2.5rem] p-10 text-white relative overflow-hidden group">
                <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-600 rounded-full blur-[100px] opacity-20 -mr-32 -mt-32 group-hover:opacity-40 transition-opacity"></div>
                <div className="relative z-10 space-y-6">
                    <h2 className="text-2xl font-black uppercase tracking-tighter">Outils de Publication</h2>
                    <p className="text-slate-400 font-medium max-w-xl">
                        En tant que juriste référent, vous pouvez publier des notes de veille qui apparaîtront dans les dashboards des agences locales.
                    </p>
                    <div className="flex gap-4">
                        <button onClick={() => { setEditingNote(undefined); setComposerOpen(true); }}
                            className="px-8 py-4 bg-white text-slate-900 rounded-2xl font-black shadow-2xl hover:bg-slate-100 transition-all active:scale-95 flex items-center gap-2">
                            <Plus size={18} /> Rédiger une note de veille
                        </button>
                        <button onClick={() => setActiveTab('eligibility')}
                            className="px-8 py-4 bg-indigo-600 text-white rounded-2xl font-black hover:bg-indigo-700 transition-all active:scale-95 flex items-center gap-2">
                            <Zap size={18} /> Modifier les règles
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );

    return (
        <div className="h-full flex flex-col bg-slate-50 overflow-hidden">
            {composerOpen && (
                <NoteComposer
                    note={editingNote}
                    onSave={handleSaveNote}
                    onClose={() => { setComposerOpen(false); setEditingNote(undefined); }}
                />
            )}
            <div className="bg-white border-b border-slate-200 px-8 pt-6">
                <div className="flex items-center justify-between mb-6">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900 tracking-tight flex items-center gap-2">
                            <Scale className="text-indigo-600" size={28} /> Audit et Veille Juridique
                        </h1>
                        <p className="text-slate-500 font-medium text-sm">Gérez la conformité et suivez les évolutions législatives.</p>
                    </div>
                    {stats.pendingCount > 0 && (
                        <div className="flex items-center gap-2 px-4 py-2 bg-amber-50 border border-amber-200 rounded-2xl">
                            <AlertTriangle className="text-amber-600" size={16} />
                            <span className="text-xs font-black text-amber-700 uppercase tracking-widest">
                                {stats.pendingCount} note{stats.pendingCount > 1 ? 's' : ''} à appliquer
                            </span>
                        </div>
                    )}
                </div>
                <div className="flex gap-8">
                    {[
                        { id: 'veille', label: '📊 Veille Juridique', icon: <Eye size={18} /> },
                        { id: 'matrix', label: '🧠 Matrice IA', icon: <Brain size={18} /> },
                        { id: 'services', label: '⚙️ Services & Documents', icon: <Settings size={18} /> },
                        { id: 'eligibility', label: '🎓 Éligibilité No-Code', icon: <Zap size={18} /> }
                    ].map((tab) => (
                        <button key={tab.id} onClick={() => setActiveTab(tab.id as TabType)}
                            className={`flex items-center gap-2 px-1 py-4 text-sm font-black uppercase tracking-widest border-b-2 transition-all ${activeTab === tab.id
                                ? 'border-indigo-600 text-indigo-600'
                                : 'border-transparent text-slate-400 hover:text-slate-600 hover:border-slate-300'}`}>
                            {tab.icon} {tab.label}
                        </button>
                    ))}
                </div>
            </div>
            <div className="flex-1 overflow-auto">
                {activeTab === 'veille' && renderVeilleJuridique()}
                {activeTab === 'matrix' && <LegalMatrixModule />}
                {activeTab === 'services' && <ServiceConfigPanel />}
                {activeTab === 'eligibility' && <EligibilityConfigPanel />}
            </div>
        </div>
    );
}
