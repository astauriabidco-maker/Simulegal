'use client';

import React, { useState, useEffect, useMemo, useCallback } from 'react';
import {
    DollarSign, Save, RotateCcw, Tag, Clock, Search,
    ChevronDown, ChevronUp, CheckCircle, AlertTriangle, Sparkles, Percent,
    Eye, History, X, ArrowRight, Filter, CreditCard, Layers
} from 'lucide-react';
import AuthStore from '../../../services/authStore';

const API_BASE = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000').replace(/\/$/, '');

// ── PÔLES (super-catégories frontend) ──
const POLE_CONFIG: Record<string, { label: string; emoji: string; color: string; categories: string[] }> = {
    PROCEDURES: {
        label: 'Pôle Procédures',
        emoji: '📋',
        color: 'indigo',
        categories: ['TITRE_SEJOUR', 'NATURALISATION', 'REGROUPEMENT_FAMILIAL'],
    },
    EXPERTISE: {
        label: 'Pôle Expertise',
        emoji: '⚖️',
        color: 'amber',
        categories: ['PERMIS_CONDUIRE', 'CONSULTATION'],
    },
    INTEGRATION: {
        label: 'Pôle Intégration',
        emoji: '🎓',
        color: 'teal',
        categories: ['FORMATION', 'QUALIFICATION'],
    },
};

const CATEGORY_LABELS: Record<string, string> = {
    'TITRE_SEJOUR': '📋 Titres de Séjour',
    'NATURALISATION': '🇫🇷 Naturalisation',
    'REGROUPEMENT_FAMILIAL': '👨‍👩‍👧 Regroupement Familial',
    'PERMIS_CONDUIRE': '🚗 Permis de Conduire',
    'FORMATION': '🎓 Formations & Examens',
    'CONSULTATION': '📞 Consultations & RDV',
    'QUALIFICATION': '📥 Qualification / Rappels',
};

const CATEGORY_COLORS: Record<string, string> = {
    'TITRE_SEJOUR': 'indigo',
    'NATURALISATION': 'blue',
    'REGROUPEMENT_FAMILIAL': 'purple',
    'PERMIS_CONDUIRE': 'amber',
    'FORMATION': 'teal',
    'CONSULTATION': 'emerald',
    'QUALIFICATION': 'orange',
};

interface ServicePricing {
    id: string;
    name: string;
    shortName: string;
    category: string;
    defaultPrice: number;
    currentPrice: number;
    promoPrice: number | null;
    promoUntil: string | null;
    notes: string;
    isOverridden: boolean;
    estimatedDuration: string;
    updatedAt: string | null;
}

interface HistoryEntry {
    serviceId: string;
    timestamp: string;
    changeTypes: string[];
    previousPrice: number | null;
    newPrice: number | null;
    previousPromoPrice: number | null;
    newPromoPrice: number | null;
    previousPromoUntil: string | null;
    newPromoUntil: string | null;
    notes: string;
}

// ── COMPOSANT : Aperçu Client ──
function ClientPreviewModal({ service, onClose }: { service: ServicePricing; onClose: () => void }) {
    const effectivePrice = service.promoPrice || service.currentPrice;
    const priceEuros = (effectivePrice / 100).toFixed(0);
    const pricePer3 = Math.ceil(effectivePrice / 100 / 3);
    const hasPromo = !!service.promoPrice;
    const promoActive = hasPromo && service.promoUntil && new Date(service.promoUntil) > new Date();

    return (
        <div className="fixed inset-0 z-[9999] bg-black/50 backdrop-blur-sm flex items-center justify-center p-4" onClick={onClose}>
            <div className="bg-white rounded-3xl shadow-2xl w-full max-w-md p-0 overflow-hidden animate-in fade-in zoom-in-95 duration-200" onClick={e => e.stopPropagation()}>
                {/* Header */}
                <div className="bg-gradient-to-br from-indigo-600 via-indigo-700 to-purple-700 p-6 text-white relative overflow-hidden">
                    <div className="absolute top-0 right-0 w-32 h-32 bg-white/5 rounded-full -translate-y-10 translate-x-10" />
                    <div className="absolute bottom-0 left-0 w-20 h-20 bg-white/5 rounded-full translate-y-6 -translate-x-6" />
                    <div className="relative z-10">
                        <div className="flex items-center justify-between mb-4">
                            <span className="px-2.5 py-1 bg-white/20 text-white text-[10px] font-bold rounded-full uppercase tracking-wider">
                                Aperçu client
                            </span>
                            <button onClick={onClose} className="p-1 hover:bg-white/20 rounded-full transition-colors">
                                <X size={18} />
                            </button>
                        </div>
                        <h3 className="text-lg font-black leading-tight">{service.name}</h3>
                        <p className="text-sm text-indigo-200 mt-1">{service.estimatedDuration}</p>
                    </div>
                </div>

                {/* Pricing Display */}
                <div className="p-6">
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-3">
                        Ce que verra le prospect sur le bouton d'encaissement :
                    </p>

                    {/* Bouton simulé "Encaisser" */}
                    <div className="space-y-3">
                        <div className="flex items-center justify-center gap-2 py-3 px-4 bg-indigo-600 text-white rounded-xl text-sm font-bold shadow-lg shadow-indigo-200">
                            {promoActive && <Sparkles size={14} className="text-yellow-300" />}
                            <CreditCard size={14} />
                            Encaisser {priceEuros}€
                            {promoActive && <span className="text-xs text-yellow-300">(Promo)</span>}
                        </div>
                        <div className="flex items-center justify-center gap-2 py-3 px-4 bg-emerald-600 text-white rounded-xl text-sm font-bold shadow-lg shadow-emerald-200">
                            <CreditCard size={14} />
                            Payer en 3× — {pricePer3}€/mois
                        </div>
                    </div>

                    {/* Détail */}
                    <div className="mt-6 bg-slate-50 rounded-xl p-4 space-y-2">
                        <div className="flex justify-between text-sm">
                            <span className="text-slate-500">Prix catalogue (défaut)</span>
                            <span className="font-bold text-slate-700">{(service.defaultPrice / 100).toFixed(0)}€</span>
                        </div>
                        {service.isOverridden && (
                            <div className="flex justify-between text-sm">
                                <span className="text-slate-500">Prix admin (custom)</span>
                                <span className="font-bold text-amber-600">{(service.currentPrice / 100).toFixed(0)}€</span>
                            </div>
                        )}
                        {hasPromo && (
                            <>
                                <div className="flex justify-between text-sm">
                                    <span className="text-slate-500">Prix promo</span>
                                    <span className="font-bold text-red-600">{(service.promoPrice! / 100).toFixed(0)}€</span>
                                </div>
                                <div className="flex justify-between text-sm">
                                    <span className="text-slate-500">Fin promo</span>
                                    <span className={`font-bold ${promoActive ? 'text-emerald-600' : 'text-red-400'}`}>
                                        {service.promoUntil ? new Date(service.promoUntil).toLocaleDateString('fr-FR') : '—'}
                                        {promoActive ? ' ✅' : ' ❌ expirée'}
                                    </span>
                                </div>
                            </>
                        )}
                        <hr className="border-slate-200" />
                        <div className="flex justify-between text-sm font-black">
                            <span className="text-slate-700">Prix final client</span>
                            <span className="text-emerald-600 text-lg">{priceEuros}€</span>
                        </div>
                    </div>

                    {/* Source */}
                    <div className="mt-4 flex items-center justify-center gap-2 text-xs text-slate-400">
                        <span>Source :</span>
                        <span className="px-2 py-0.5 bg-slate-100 text-slate-600 rounded-md font-bold">
                            {promoActive ? '🏷️ PROMO' : service.isOverridden ? '⚙️ ADMIN' : '📋 CATALOGUE'}
                        </span>
                    </div>
                </div>
            </div>
        </div>
    );
}

// ── COMPOSANT : Historique ──
function PricingHistoryPanel({ onClose }: { onClose: () => void }) {
    const [history, setHistory] = useState<HistoryEntry[]>([]);
    const [loading, setLoading] = useState(true);
    const [filterServiceId, setFilterServiceId] = useState('');

    useEffect(() => {
        const load = async () => {
            try {
                const url = filterServiceId
                    ? `${API_BASE}/settings/service-pricing/history?serviceId=${filterServiceId}&limit=100`
                    : `${API_BASE}/settings/service-pricing/history?limit=100`;
                const res = await fetch(url, {
                    headers: { 'Authorization': `Bearer ${AuthStore.getToken()}` }
                });
                const data = await res.json();
                setHistory(data.history || []);
            } catch (e) {
                console.error('Failed to load history:', e);
            } finally {
                setLoading(false);
            }
        };
        load();
    }, [filterServiceId]);

    const formatRelativeTime = (timestamp: string) => {
        const diff = Date.now() - new Date(timestamp).getTime();
        const minutes = Math.floor(diff / 60000);
        const hours = Math.floor(diff / 3600000);
        const days = Math.floor(diff / 86400000);

        if (minutes < 1) return 'À l\'instant';
        if (minutes < 60) return `Il y a ${minutes}min`;
        if (hours < 24) return `Il y a ${hours}h`;
        if (days < 7) return `Il y a ${days}j`;
        return new Date(timestamp).toLocaleDateString('fr-FR', { day: '2-digit', month: 'short', year: 'numeric' });
    };

    const changeTypeBadge = (type: string) => {
        const config: Record<string, { label: string; color: string }> = {
            'PRICE': { label: 'Prix', color: 'bg-indigo-100 text-indigo-700' },
            'PROMO': { label: 'Promo', color: 'bg-red-100 text-red-700' },
            'PROMO_DATE': { label: 'Date promo', color: 'bg-orange-100 text-orange-700' },
            'NOTES': { label: 'Notes', color: 'bg-slate-100 text-slate-600' },
            'RESET': { label: 'Reset', color: 'bg-amber-100 text-amber-700' },
        };
        const c = config[type] || { label: type, color: 'bg-slate-100 text-slate-600' };
        return (
            <span key={type} className={`px-1.5 py-0.5 rounded text-[9px] font-black uppercase ${c.color}`}>
                {c.label}
            </span>
        );
    };

    return (
        <div className="fixed inset-0 z-[9999] bg-black/50 backdrop-blur-sm flex items-center justify-center p-4" onClick={onClose}>
            <div className="bg-white rounded-3xl shadow-2xl w-full max-w-2xl max-h-[80vh] flex flex-col overflow-hidden animate-in fade-in zoom-in-95 duration-200" onClick={e => e.stopPropagation()}>
                {/* Header */}
                <div className="p-6 border-b border-slate-100 flex-shrink-0">
                    <div className="flex items-center justify-between mb-3">
                        <h3 className="text-lg font-black text-slate-900 flex items-center gap-2">
                            <History size={20} className="text-indigo-600" />
                            Historique des modifications
                        </h3>
                        <button onClick={onClose} className="p-2 hover:bg-slate-100 rounded-xl transition-colors">
                            <X size={18} className="text-slate-400" />
                        </button>
                    </div>
                    <div className="relative">
                        <input
                            type="text"
                            placeholder="Filtrer par ID de service..."
                            value={filterServiceId}
                            onChange={e => setFilterServiceId(e.target.value)}
                            className="w-full pl-9 pr-4 py-2 bg-slate-50 border border-slate-200 rounded-xl text-sm focus:ring-2 focus:ring-indigo-500 outline-none"
                        />
                        <Filter className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={14} />
                    </div>
                </div>

                {/* List */}
                <div className="overflow-y-auto flex-1 p-4 space-y-2">
                    {loading ? (
                        <div className="flex items-center justify-center h-32">
                            <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-indigo-600" />
                        </div>
                    ) : history.length === 0 ? (
                        <div className="text-center py-12 text-slate-400">
                            <History size={32} className="mx-auto mb-2 opacity-30" />
                            <p className="text-sm">Aucune modification enregistrée</p>
                        </div>
                    ) : (
                        history.map((entry, i) => (
                            <div key={i} className="bg-slate-50 rounded-xl p-3 hover:bg-slate-100 transition-colors">
                                <div className="flex items-start justify-between gap-3">
                                    <div className="flex-1 min-w-0">
                                        <div className="flex items-center gap-2 flex-wrap">
                                            <span className="font-bold text-sm text-slate-800 truncate">
                                                {entry.serviceId}
                                            </span>
                                            {entry.changeTypes.map(t => changeTypeBadge(t))}
                                        </div>
                                        <div className="flex items-center gap-3 mt-1.5 text-xs text-slate-500">
                                            {entry.changeTypes.includes('PRICE') && (
                                                <span className="flex items-center gap-1">
                                                    {entry.previousPrice ? `${(entry.previousPrice / 100).toFixed(0)}€` : '—'}
                                                    <ArrowRight size={10} className="text-slate-400" />
                                                    <span className="font-bold text-indigo-600">
                                                        {entry.newPrice ? `${(entry.newPrice / 100).toFixed(0)}€` : 'Défaut'}
                                                    </span>
                                                </span>
                                            )}
                                            {entry.changeTypes.includes('PROMO') && (
                                                <span className="flex items-center gap-1">
                                                    Promo: {entry.previousPromoPrice ? `${(entry.previousPromoPrice / 100).toFixed(0)}€` : '—'}
                                                    <ArrowRight size={10} className="text-slate-400" />
                                                    <span className="font-bold text-red-600">
                                                        {entry.newPromoPrice ? `${(entry.newPromoPrice / 100).toFixed(0)}€` : '—'}
                                                    </span>
                                                </span>
                                            )}
                                            {entry.changeTypes.includes('RESET') && (
                                                <span className="text-amber-600 font-bold">↩ Remis au prix par défaut</span>
                                            )}
                                        </div>
                                    </div>
                                    <span className="text-[10px] text-slate-400 whitespace-nowrap mt-0.5">
                                        {formatRelativeTime(entry.timestamp)}
                                    </span>
                                </div>
                            </div>
                        ))
                    )}
                </div>

                {/* Footer */}
                {history.length > 0 && (
                    <div className="p-4 border-t border-slate-100 text-center flex-shrink-0">
                        <p className="text-xs text-slate-400">{history.length} modification(s) affichée(s)</p>
                    </div>
                )}
            </div>
        </div>
    );
}

// ── COMPOSANT PRINCIPAL ──
export default function ServicePricingTab() {
    const [services, setServices] = useState<ServicePricing[]>([]);
    const [editingPrices, setEditingPrices] = useState<Record<string, { price: string; promoPrice: string; promoUntil: string; notes: string }>>({});
    const [saving, setSaving] = useState<string | null>(null);
    const [saveSuccess, setSaveSuccess] = useState<string | null>(null);
    const [searchQuery, setSearchQuery] = useState('');
    const [filterCategory, setFilterCategory] = useState('all');
    const [filterPole, setFilterPole] = useState('all');
    const [loading, setLoading] = useState(true);
    const [previewService, setPreviewService] = useState<ServicePricing | null>(null);
    const [showHistory, setShowHistory] = useState(false);
    const [collapsedPoles, setCollapsedPoles] = useState<Record<string, boolean>>({});

    useEffect(() => {
        loadPricing();
    }, []);

    const loadPricing = async () => {
        try {
            const res = await fetch(`${API_BASE}/settings/service-pricing`);
            const data = await res.json();
            setServices(data.services);
            const edits: Record<string, any> = {};
            data.services.forEach((s: ServicePricing) => {
                edits[s.id] = {
                    price: (s.currentPrice / 100).toFixed(2),
                    promoPrice: s.promoPrice ? (s.promoPrice / 100).toFixed(2) : '',
                    promoUntil: s.promoUntil || '',
                    notes: s.notes || '',
                };
            });
            setEditingPrices(edits);
        } catch (e) {
            console.error('Failed to load pricing:', e);
        } finally {
            setLoading(false);
        }
    };

    const handleSavePrice = async (serviceId: string) => {
        setSaving(serviceId);
        setSaveSuccess(null);
        try {
            const edit = editingPrices[serviceId];
            const pricing: any = {
                price: Math.round(parseFloat(edit.price) * 100),
            };
            if (edit.promoPrice) pricing.promoPrice = Math.round(parseFloat(edit.promoPrice) * 100);
            if (edit.promoUntil) pricing.promoUntil = edit.promoUntil;
            if (edit.notes) pricing.notes = edit.notes;

            await fetch(`${API_BASE}/settings/service-pricing`, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
                body: JSON.stringify({ pricing: { [serviceId]: pricing } })
            });
            setSaveSuccess(serviceId);
            setTimeout(() => setSaveSuccess(null), 2000);
            await loadPricing();
        } catch (e) {
            console.error('Save failed:', e);
        } finally {
            setSaving(null);
        }
    };

    const handleResetPrice = async (serviceId: string) => {
        setSaving(serviceId);
        try {
            await fetch(`${API_BASE}/settings/service-pricing/reset/${serviceId}`, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
            });
            await loadPricing();
        } catch (e) {
            console.error('Reset failed:', e);
        } finally {
            setSaving(null);
        }
    };

    const updateField = (serviceId: string, field: string, value: string) => {
        setEditingPrices(prev => ({
            ...prev,
            [serviceId]: { ...prev[serviceId], [field]: value }
        }));
    };

    const togglePole = (poleId: string) => {
        setCollapsedPoles(prev => ({ ...prev, [poleId]: !prev[poleId] }));
    };

    // Grouper par pôle puis catégorie
    const filteredServices = useMemo(() => {
        return services.filter(s => {
            if (filterCategory !== 'all' && s.category !== filterCategory) return false;
            if (filterPole !== 'all') {
                const pole = POLE_CONFIG[filterPole];
                if (pole && !pole.categories.includes(s.category)) return false;
            }
            if (searchQuery) {
                const q = searchQuery.toLowerCase();
                return s.name.toLowerCase().includes(q) || s.shortName.toLowerCase().includes(q) || s.id.toLowerCase().includes(q);
            }
            return true;
        });
    }, [services, filterCategory, filterPole, searchQuery]);

    const poleGroups = useMemo(() => {
        const groups: Record<string, Record<string, ServicePricing[]>> = {};
        for (const [poleId, pole] of Object.entries(POLE_CONFIG)) {
            const poleServices = filteredServices.filter(s => pole.categories.includes(s.category));
            if (poleServices.length === 0) continue;
            groups[poleId] = {};
            poleServices.forEach(s => {
                if (!groups[poleId][s.category]) groups[poleId][s.category] = [];
                groups[poleId][s.category].push(s);
            });
        }
        return groups;
    }, [filteredServices]);

    // Stats
    const stats = useMemo(() => {
        const total = services.length;
        const overridden = services.filter(s => s.isOverridden).length;
        const withPromo = services.filter(s => s.promoPrice && s.promoUntil && new Date(s.promoUntil) > new Date()).length;
        return { total, overridden, withPromo };
    }, [services]);

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
            <div className="flex flex-col md:flex-row items-start md:items-center justify-between gap-4">
                <div>
                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <DollarSign size={20} className="text-emerald-600" />
                        Tarification des Services
                    </h2>
                    <p className="text-slate-500 text-sm mt-1">
                        Modifiez les prix en temps réel. Les modifications prennent effet immédiatement.
                    </p>
                </div>

                <div className="flex items-center gap-2">
                    {/* Bouton Historique */}
                    <button
                        onClick={() => setShowHistory(true)}
                        className="flex items-center gap-1.5 px-3 py-2 bg-slate-100 hover:bg-slate-200 text-slate-700 rounded-xl text-sm font-medium transition-colors"
                    >
                        <History size={14} />
                        Historique
                    </button>
                </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-3 gap-3">
                <div className="bg-gradient-to-br from-indigo-50 to-indigo-100/50 rounded-xl p-3 border border-indigo-100">
                    <p className="text-[10px] font-bold text-indigo-400 uppercase tracking-wider">Total services</p>
                    <p className="text-2xl font-black text-indigo-900 mt-0.5">{stats.total}</p>
                </div>
                <div className="bg-gradient-to-br from-amber-50 to-amber-100/50 rounded-xl p-3 border border-amber-100">
                    <p className="text-[10px] font-bold text-amber-500 uppercase tracking-wider">Prix customisés</p>
                    <p className="text-2xl font-black text-amber-900 mt-0.5">{stats.overridden}</p>
                </div>
                <div className="bg-gradient-to-br from-red-50 to-red-100/50 rounded-xl p-3 border border-red-100">
                    <p className="text-[10px] font-bold text-red-400 uppercase tracking-wider">Promos actives</p>
                    <p className="text-2xl font-black text-red-900 mt-0.5">{stats.withPromo}</p>
                </div>
            </div>

            {/* Filtres */}
            <div className="flex items-center gap-3 flex-wrap">
                {/* Filtre Pôle */}
                <select
                    value={filterPole}
                    onChange={(e) => { setFilterPole(e.target.value); setFilterCategory('all'); }}
                    className="px-3 py-2 bg-white border border-slate-200 rounded-xl text-sm font-medium text-slate-700"
                >
                    <option value="all">Tous les pôles</option>
                    {Object.entries(POLE_CONFIG).map(([key, pole]) => (
                        <option key={key} value={key}>{pole.emoji} {pole.label}</option>
                    ))}
                </select>

                {/* Filtre catégorie */}
                <select
                    value={filterCategory}
                    onChange={(e) => setFilterCategory(e.target.value)}
                    className="px-3 py-2 bg-white border border-slate-200 rounded-xl text-sm font-medium text-slate-700"
                >
                    <option value="all">Toutes catégories</option>
                    {Object.entries(CATEGORY_LABELS)
                        .filter(([key]) => filterPole === 'all' || POLE_CONFIG[filterPole]?.categories.includes(key))
                        .map(([key, label]) => (
                            <option key={key} value={key}>{label}</option>
                        ))}
                </select>

                {/* Recherche */}
                <div className="relative flex-1 min-w-[200px]">
                    <input
                        type="text"
                        placeholder="Rechercher un service..."
                        value={searchQuery}
                        onChange={(e) => setSearchQuery(e.target.value)}
                        className="w-full pl-9 pr-4 py-2 bg-white border border-slate-200 rounded-xl text-sm focus:ring-2 focus:ring-indigo-500 outline-none"
                    />
                    <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={14} />
                </div>
            </div>

            {/* Services par pôle → catégorie */}
            {Object.entries(poleGroups).map(([poleId, catGroups]) => {
                const pole = POLE_CONFIG[poleId];
                const isCollapsed = collapsedPoles[poleId];
                const poleServiceCount = Object.values(catGroups).flat().length;

                return (
                    <div key={poleId} className="rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
                        {/* Pôle Header */}
                        <button
                            onClick={() => togglePole(poleId)}
                            className={`w-full px-6 py-4 flex items-center justify-between bg-gradient-to-r from-${pole.color}-50 via-${pole.color}-50/50 to-white hover:from-${pole.color}-100 transition-colors`}
                        >
                            <div className="flex items-center gap-3">
                                <Layers size={18} className={`text-${pole.color}-600`} />
                                <h2 className="font-black text-slate-900">
                                    {pole.emoji} {pole.label}
                                </h2>
                                <span className="text-xs font-medium text-slate-400">
                                    ({poleServiceCount} service{poleServiceCount > 1 ? 's' : ''})
                                </span>
                            </div>
                            {isCollapsed ? <ChevronDown size={18} className="text-slate-400" /> : <ChevronUp size={18} className="text-slate-400" />}
                        </button>

                        {/* Categories inside pole */}
                        {!isCollapsed && Object.entries(catGroups).map(([category, catServices]) => (
                            <div key={category} className="border-t border-slate-100">
                                {/* Category Header */}
                                <div className={`px-6 py-2.5 bg-gradient-to-r from-${CATEGORY_COLORS[category] || 'slate'}-50/70 to-transparent`}>
                                    <h3 className="font-bold text-slate-700 text-xs flex items-center gap-2">
                                        {CATEGORY_LABELS[category] || category}
                                        <span className="text-slate-400 font-medium">
                                            ({catServices.length})
                                        </span>
                                    </h3>
                                </div>

                                {/* Services */}
                                <div className="divide-y divide-slate-100">
                                    {catServices.map(service => {
                                        const edit = editingPrices[service.id];
                                        if (!edit) return null;
                                        const hasChanged = parseFloat(edit.price) !== (service.defaultPrice / 100);
                                        const isSaving = saving === service.id;
                                        const isSuccess = saveSuccess === service.id;

                                        return (
                                            <div key={service.id} className="px-6 py-4 hover:bg-slate-50/50 transition-colors">
                                                <div className="flex items-center gap-4 flex-wrap lg:flex-nowrap">
                                                    {/* Nom du service */}
                                                    <div className="flex-1 min-w-0">
                                                        <div className="flex items-center gap-2">
                                                            <p className="font-bold text-slate-800 text-sm truncate">{service.name}</p>
                                                            {service.isOverridden && (
                                                                <span className="px-1.5 py-0.5 bg-amber-100 text-amber-700 text-[10px] font-black rounded-md uppercase flex-shrink-0">
                                                                    Custom
                                                                </span>
                                                            )}
                                                            {service.promoPrice && service.promoUntil && new Date(service.promoUntil) > new Date() && (
                                                                <span className="px-1.5 py-0.5 bg-red-100 text-red-600 text-[10px] font-black rounded-md uppercase flex-shrink-0 animate-pulse">
                                                                    Promo
                                                                </span>
                                                            )}
                                                        </div>
                                                        <div className="flex items-center gap-3 mt-1">
                                                            <span className="text-xs text-slate-400 flex items-center gap-1">
                                                                <Clock size={10} /> {service.estimatedDuration}
                                                            </span>
                                                            <span className="text-xs text-slate-400">
                                                                Défaut: {(service.defaultPrice / 100).toFixed(0)}€
                                                            </span>
                                                        </div>
                                                    </div>

                                                    {/* Prix principal */}
                                                    <div className="flex items-center gap-2">
                                                        <label className="text-xs font-bold text-slate-500 uppercase">Prix</label>
                                                        <div className="relative">
                                                            <input
                                                                type="number"
                                                                step="0.01"
                                                                min="0"
                                                                value={edit.price}
                                                                onChange={(e) => updateField(service.id, 'price', e.target.value)}
                                                                className={`w-24 px-3 py-2 pr-6 text-right font-bold rounded-xl border text-sm transition-colors ${hasChanged || service.isOverridden
                                                                    ? 'border-amber-300 bg-amber-50 text-amber-800'
                                                                    : 'border-slate-200 bg-white text-slate-800'
                                                                    }`}
                                                            />
                                                            <span className="absolute right-2 top-1/2 -translate-y-1/2 text-xs text-slate-400">€</span>
                                                        </div>
                                                    </div>

                                                    {/* Prix promo */}
                                                    <div className="flex items-center gap-2">
                                                        <label className="text-xs font-bold text-slate-500 uppercase flex items-center gap-0.5">
                                                            <Percent size={10} /> Promo
                                                        </label>
                                                        <input
                                                            type="number"
                                                            step="0.01"
                                                            min="0"
                                                            placeholder="—"
                                                            value={edit.promoPrice}
                                                            onChange={(e) => updateField(service.id, 'promoPrice', e.target.value)}
                                                            className="w-20 px-2 py-2 text-right font-medium rounded-xl border border-slate-200 bg-white text-sm text-emerald-700 placeholder:text-slate-300"
                                                        />
                                                    </div>

                                                    {/* Date fin promo */}
                                                    <div className="flex items-center gap-2">
                                                        <input
                                                            type="date"
                                                            value={edit.promoUntil}
                                                            onChange={(e) => updateField(service.id, 'promoUntil', e.target.value)}
                                                            className="w-36 px-2 py-2 text-sm rounded-xl border border-slate-200 bg-white text-slate-600"
                                                        />
                                                    </div>

                                                    {/* Actions */}
                                                    <div className="flex items-center gap-1.5">
                                                        {/* Aperçu client */}
                                                        <button
                                                            onClick={() => setPreviewService(service)}
                                                            className="p-2 rounded-xl text-slate-500 bg-slate-50 hover:bg-indigo-50 hover:text-indigo-600 transition-colors"
                                                            title="Aperçu client"
                                                        >
                                                            <Eye size={16} />
                                                        </button>

                                                        {/* Enregistrer */}
                                                        <button
                                                            onClick={() => handleSavePrice(service.id)}
                                                            disabled={isSaving}
                                                            className={`p-2 rounded-xl text-white transition-all ${isSuccess
                                                                ? 'bg-emerald-500'
                                                                : 'bg-indigo-600 hover:bg-indigo-700'
                                                                } ${isSaving ? 'opacity-50' : ''}`}
                                                            title="Enregistrer"
                                                        >
                                                            {isSuccess ? <CheckCircle size={16} /> : <Save size={16} />}
                                                        </button>

                                                        {/* Reset */}
                                                        {service.isOverridden && (
                                                            <button
                                                                onClick={() => handleResetPrice(service.id)}
                                                                disabled={isSaving}
                                                                className="p-2 rounded-xl text-amber-600 bg-amber-50 hover:bg-amber-100 transition-colors"
                                                                title="Remettre le prix par défaut"
                                                            >
                                                                <RotateCcw size={16} />
                                                            </button>
                                                        )}
                                                    </div>
                                                </div>

                                                {/* Notes */}
                                                <input
                                                    type="text"
                                                    placeholder="Notes internes sur ce tarif..."
                                                    value={edit.notes}
                                                    onChange={(e) => updateField(service.id, 'notes', e.target.value)}
                                                    className="mt-2 w-full px-3 py-1.5 text-xs text-slate-500 bg-slate-50 border border-transparent hover:border-slate-200 rounded-lg focus:border-indigo-300 transition-colors outline-none"
                                                />
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>
                        ))}
                    </div>
                );
            })}

            {Object.keys(poleGroups).length === 0 && (
                <div className="text-center py-12 text-slate-400">
                    <DollarSign size={40} className="mx-auto mb-3 opacity-30" />
                    <p className="font-medium">Aucun service trouvé</p>
                    <p className="text-sm mt-1">Essayez de modifier vos filtres</p>
                </div>
            )}

            {/* Modals */}
            {previewService && (
                <ClientPreviewModal service={previewService} onClose={() => setPreviewService(null)} />
            )}
            {showHistory && (
                <PricingHistoryPanel onClose={() => setShowHistory(false)} />
            )}
        </div>
    );
}
