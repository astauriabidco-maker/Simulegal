'use client';

import React, { useState, useEffect, useMemo } from 'react';
import {
    DollarSign, Save, RotateCcw, Tag, Clock, Search,
    ChevronDown, CheckCircle, AlertTriangle, Sparkles, Percent
} from 'lucide-react';
import AuthStore from '../../../services/authStore';

const API_BASE = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000').replace(/\/$/, '');

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

export default function ServicePricingTab() {
    const [services, setServices] = useState<ServicePricing[]>([]);
    const [editingPrices, setEditingPrices] = useState<Record<string, { price: string; promoPrice: string; promoUntil: string; notes: string }>>({});
    const [saving, setSaving] = useState<string | null>(null);
    const [saveSuccess, setSaveSuccess] = useState<string | null>(null);
    const [searchQuery, setSearchQuery] = useState('');
    const [filterCategory, setFilterCategory] = useState('all');
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        loadPricing();
    }, []);

    const loadPricing = async () => {
        try {
            const res = await fetch(`${API_BASE}/settings/service-pricing`);
            const data = await res.json();
            setServices(data.services);
            // Initialiser les champs d'édition
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

    // Grouper par catégorie
    const categories = useMemo(() => {
        const cats: Record<string, ServicePricing[]> = {};
        services
            .filter(s => {
                if (filterCategory !== 'all' && s.category !== filterCategory) return false;
                if (searchQuery) {
                    const q = searchQuery.toLowerCase();
                    return s.name.toLowerCase().includes(q) || s.shortName.toLowerCase().includes(q);
                }
                return true;
            })
            .forEach(s => {
                if (!cats[s.category]) cats[s.category] = [];
                cats[s.category].push(s);
            });
        return cats;
    }, [services, filterCategory, searchQuery]);

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

                <div className="flex items-center gap-3">
                    {/* Filtre catégorie */}
                    <select
                        value={filterCategory}
                        onChange={(e) => setFilterCategory(e.target.value)}
                        className="px-3 py-2 bg-white border border-slate-200 rounded-xl text-sm font-medium text-slate-700"
                    >
                        <option value="all">Toutes catégories</option>
                        {Object.entries(CATEGORY_LABELS).map(([key, label]) => (
                            <option key={key} value={key}>{label}</option>
                        ))}
                    </select>

                    {/* Recherche */}
                    <div className="relative">
                        <input
                            type="text"
                            placeholder="Rechercher..."
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="pl-9 pr-4 py-2 bg-white border border-slate-200 rounded-xl text-sm w-48 focus:ring-2 focus:ring-indigo-500 outline-none"
                        />
                        <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={14} />
                    </div>
                </div>
            </div>

            {/* Services par catégorie */}
            {Object.entries(categories).map(([category, catServices]) => (
                <div key={category} className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
                    {/* Header catégorie */}
                    <div className={`px-6 py-3 bg-gradient-to-r from-${CATEGORY_COLORS[category] || 'slate'}-50 to-white border-b border-slate-100`}>
                        <h3 className="font-bold text-slate-800 text-sm">
                            {CATEGORY_LABELS[category] || category}
                            <span className="ml-2 text-xs font-medium text-slate-400">
                                ({catServices.length} service{catServices.length > 1 ? 's' : ''})
                            </span>
                        </h3>
                    </div>

                    {/* Table */}
                    <div className="divide-y divide-slate-100">
                        {catServices.map(service => {
                            const edit = editingPrices[service.id];
                            if (!edit) return null;
                            const hasChanged = parseFloat(edit.price) !== (service.defaultPrice / 100);
                            const isSaving = saving === service.id;
                            const isSuccess = saveSuccess === service.id;

                            return (
                                <div key={service.id} className="px-6 py-4 hover:bg-slate-50/50 transition-colors">
                                    <div className="flex items-center gap-6">
                                        {/* Nom du service */}
                                        <div className="flex-1 min-w-0">
                                            <div className="flex items-center gap-2">
                                                <p className="font-bold text-slate-800 text-sm truncate">{service.name}</p>
                                                {service.isOverridden && (
                                                    <span className="px-1.5 py-0.5 bg-amber-100 text-amber-700 text-[10px] font-black rounded-md uppercase">
                                                        Custom
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
                                        <div className="flex items-center gap-2">
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

            {Object.keys(categories).length === 0 && (
                <div className="text-center py-12 text-slate-400">
                    <DollarSign size={40} className="mx-auto mb-3 opacity-30" />
                    <p className="font-medium">Aucun service trouvé</p>
                </div>
            )}
        </div>
    );
}
