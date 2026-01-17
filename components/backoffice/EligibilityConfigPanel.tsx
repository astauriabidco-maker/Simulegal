'use client';

import React, { useState, useEffect } from 'react';
import EligibilityStore from '../../services/EligibilityStore';
import { ProcedureRule } from '../../types';
import {
    Save,
    RotateCcw,
    CheckCircle,
    AlertCircle,
    TrendingUp,
    Clock,
    DollarSign,
    Zap,
    Code,
    ChevronRight,
    Search
} from 'lucide-react';

export default function EligibilityConfigPanel() {
    const [thresholds, setThresholds] = useState<any>(null);
    const [rulesSejour, setRulesSejour] = useState<ProcedureRule[]>([]);
    const [rulesNat, setRulesNat] = useState<ProcedureRule[]>([]);
    const [category, setCategory] = useState<'thresholds' | 'rules'>('thresholds');
    const [subCategory, setSubCategory] = useState<'sejour' | 'naturalisation'>('sejour');
    const [showNotification, setShowNotification] = useState(false);
    const [notificationMessage, setNotificationMessage] = useState('');
    const [searchQuery, setSearchQuery] = useState('');

    useEffect(() => {
        setThresholds(EligibilityStore.getThresholds());
        setRulesSejour(EligibilityStore.getRules('sejour'));
        setRulesNat(EligibilityStore.getRules('naturalisation'));
    }, []);

    const showNotificationMsg = (message: string) => {
        setNotificationMessage(message);
        setShowNotification(true);
        setTimeout(() => setShowNotification(false), 3000);
    };

    const handleSaveThresholds = () => {
        EligibilityStore.updateThresholds(thresholds);
        showNotificationMsg('✅ Seuils sauvegardés');
    };

    const handleResetThresholds = () => {
        if (!confirm('Réinitialiser tous les seuils aux valeurs par défaut ?')) return;
        EligibilityStore.resetThresholds();
        setThresholds(EligibilityStore.getThresholds());
        showNotificationMsg('🔄 Seuils réinitialisés');
    };

    const handleSaveRules = () => {
        if (subCategory === 'sejour') {
            EligibilityStore.updateRules('sejour', rulesSejour);
        } else {
            EligibilityStore.updateRules('naturalisation', rulesNat);
        }
        showNotificationMsg('✅ Règles sauvegardées');
    };

    const updateNestedThreshold = (path: string[], value: any) => {
        const newThresholds = { ...thresholds };
        let current = newThresholds;
        for (let i = 0; i < path.length - 1; i++) {
            current = current[path[i]];
        }
        current[path[path.length - 1]] = value;
        setThresholds(newThresholds);
    };

    if (!thresholds) return null;

    return (
        <div className="h-full flex flex-col bg-slate-100 p-6 overflow-auto">
            {/* Header */}
            <div className="flex items-center justify-between mb-8">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Critères d'Éligibilité</h1>
                    <p className="text-slate-500 font-medium">Configurez les seuils financiers et les règles logiques du simulateur.</p>
                </div>
                <div className="flex bg-white p-1 rounded-xl shadow-sm border border-slate-200">
                    <button
                        onClick={() => setCategory('thresholds')}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${category === 'thresholds' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        💰 Seuils (SMIC, Durées)
                    </button>
                    <button
                        onClick={() => setCategory('rules')}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${category === 'rules' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        🧠 Logique (Règles JSON)
                    </button>
                </div>
            </div>

            {category === 'thresholds' ? (
                <div className="space-y-6 max-w-4xl">
                    {/* SALAIRES ET FINANCES */}
                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm space-y-6">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4">
                            <DollarSign className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">Seuils Financiers (nets mensuels/annuels)</h2>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            <div className="space-y-4">
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs">Références de Base</h3>
                                <div>
                                    <label className="block text-sm font-bold text-slate-700 mb-1">SMIC Mensuel Net (Réf)</label>
                                    <input
                                        type="number"
                                        value={thresholds.financial_thresholds.salary_monthly_gross.smic}
                                        onChange={(e) => updateNestedThreshold(['financial_thresholds', 'salary_monthly_gross', 'smic'], parseFloat(e.target.value))}
                                        className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                    />
                                </div>
                            </div>

                            <div className="space-y-4">
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs">Passeport Talent (Annuel)</h3>
                                {[
                                    { label: 'Salarié Qualifié', key: 'passeport_talent_salarie_qualifie' },
                                    { label: 'Carte Bleue UE', key: 'passeport_talent_carte_bleue_eu' }
                                ].map(item => (
                                    <div key={item.key}>
                                        <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                        <input
                                            type="number"
                                            value={thresholds.financial_thresholds.salary_annual_gross[item.key]}
                                            onChange={(e) => updateNestedThreshold(['financial_thresholds', 'salary_annual_gross', item.key], parseFloat(e.target.value))}
                                            className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                        />
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>

                    {/* DUREES DE RESIDENCE ET MARIAGE */}
                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm space-y-6">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4">
                            <Clock className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">Durées de résidence (Années)</h2>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            <div className="space-y-4">
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs">Immigration</h3>
                                {[
                                    { label: 'Naturalisation Standard', key: 'naturalisation_standard' },
                                    { label: 'Naturalisation (Études Sup)', key: 'naturalisation_etudes_sup' },
                                    { label: 'Carte Résident Longue Durée', key: 'resident_longue_duree_ue' }
                                ].map(item => (
                                    <div key={item.key}>
                                        <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                        <input
                                            type="number"
                                            value={thresholds.duration_thresholds.residence_in_france[item.key]}
                                            onChange={(e) => updateNestedThreshold(['duration_thresholds', 'residence_in_france', item.key], parseFloat(e.target.value))}
                                            className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                        />
                                    </div>
                                ))}
                            </div>

                            <div className="space-y-4">
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs">Mariage</h3>
                                {[
                                    { label: 'Pour Nationalité', key: 'pour_nationalite_mariage' },
                                    { label: 'Pour Carte Résident', key: 'pour_carte_resident_conjoint_francais' }
                                ].map(item => (
                                    <div key={item.key}>
                                        <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                        <input
                                            type="number"
                                            value={thresholds.duration_thresholds.marriage_duration[item.key]}
                                            onChange={(e) => updateNestedThreshold(['duration_thresholds', 'marriage_duration', item.key], parseFloat(e.target.value))}
                                            className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                        />
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>

                    <div className="flex gap-4">
                        <button
                            onClick={handleSaveThresholds}
                            className="flex-1 py-4 bg-indigo-600 hover:bg-indigo-700 text-white rounded-2xl font-black shadow-xl shadow-indigo-100 flex items-center justify-center gap-2"
                        >
                            <Save size={20} /> Sauvegarder les seuils
                        </button>
                        <button
                            onClick={handleResetThresholds}
                            className="px-6 py-4 bg-white border border-slate-200 text-slate-500 hover:text-red-600 rounded-2xl font-bold transition-colors"
                        >
                            <RotateCcw size={20} />
                        </button>
                    </div>
                </div>
            ) : (
                <div className="flex flex-col h-full overflow-hidden">
                    <div className="flex items-center gap-4 mb-4">
                        <select
                            value={subCategory}
                            onChange={(e) => setSubCategory(e.target.value as any)}
                            className="px-4 py-2 bg-white border border-slate-200 rounded-xl font-bold text-slate-700 shadow-sm"
                        >
                            <option value="sejour">🛂 Titres de Séjour</option>
                            <option value="naturalisation">🇫🇷 Naturalisation</option>
                        </select>

                        <div className="relative flex-1">
                            <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                            <input
                                type="text"
                                placeholder="Rechercher une règle par nom ou ID..."
                                value={searchQuery}
                                onChange={(e) => setSearchQuery(e.target.value)}
                                className="w-full pl-12 pr-4 py-2 bg-white border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500"
                            />
                        </div>

                        <button
                            onClick={handleSaveRules}
                            className="px-6 py-2 bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl font-bold flex items-center gap-2 shadow-lg"
                        >
                            <Save size={18} /> Sauvegarder {subCategory === 'sejour' ? rulesSejour.length : rulesNat.length} règles
                        </button>
                    </div>

                    <div className="flex-1 overflow-auto bg-white rounded-3xl border border-slate-200 shadow-inner p-2 space-y-2">
                        {(subCategory === 'sejour' ? rulesSejour : rulesNat)
                            .filter(r => r.name.toLowerCase().includes(searchQuery.toLowerCase()) || r.id.includes(searchQuery))
                            .map((rule, idx) => (
                                <div key={rule.id} className="p-4 bg-slate-50 rounded-2xl border border-slate-100 group">
                                    <div className="flex items-center justify-between mb-4">
                                        <div className="flex items-center gap-3">
                                            <div className="w-8 h-8 bg-indigo-100 text-indigo-600 rounded-lg flex items-center justify-center font-bold text-xs">
                                                {rule.priority}
                                            </div>
                                            <div>
                                                <h4 className="font-black text-slate-900">{rule.name}</h4>
                                                <p className="text-xs text-slate-400 font-mono">{rule.id}</p>
                                            </div>
                                        </div>
                                        <div className="flex items-center gap-2">
                                            <label className="text-xs font-bold text-slate-400">Priorité :</label>
                                            <input
                                                type="number"
                                                value={rule.priority}
                                                onChange={(e) => {
                                                    const val = parseInt(e.target.value);
                                                    if (subCategory === 'sejour') {
                                                        const newRules = [...rulesSejour];
                                                        const i = newRules.findIndex(r => r.id === rule.id);
                                                        newRules[i].priority = val;
                                                        setRulesSejour(newRules);
                                                    } else {
                                                        const newRules = [...rulesNat];
                                                        const i = newRules.findIndex(r => r.id === rule.id);
                                                        newRules[i].priority = val;
                                                        setRulesNat(newRules);
                                                    }
                                                }}
                                                className="w-16 px-2 py-1 border border-slate-200 rounded-lg text-sm font-bold"
                                            />
                                        </div>
                                    </div>
                                    <div className="space-y-2">
                                        <div className="flex items-center gap-2 text-indigo-600 text-xs font-bold">
                                            <Code size={14} /> Logique JSON (Conditions déligibilité)
                                        </div>
                                        <textarea
                                            value={JSON.stringify(rule.conditions, null, 2)}
                                            onChange={(e) => {
                                                try {
                                                    const newConditions = JSON.parse(e.target.value);
                                                    if (subCategory === 'sejour') {
                                                        const newRules = [...rulesSejour];
                                                        const i = newRules.findIndex(r => r.id === rule.id);
                                                        newRules[i].conditions = newConditions;
                                                        setRulesSejour(newRules);
                                                    } else {
                                                        const newRules = [...rulesNat];
                                                        const i = newRules.findIndex(r => r.id === rule.id);
                                                        newRules[i].conditions = newConditions;
                                                        setRulesNat(newRules);
                                                    }
                                                } catch (err) {
                                                    // Invalid JSON, don't update state but maybe show error
                                                }
                                            }}
                                            spellCheck={false}
                                            rows={8}
                                            className="w-full p-4 bg-slate-900 text-emerald-400 font-mono text-xs rounded-xl border border-slate-800 focus:ring-2 focus:ring-emerald-500 resize-none overflow-auto"
                                        />
                                    </div>
                                </div>
                            ))}
                    </div>
                </div>
            )}

            {/* Notifications */}
            {showNotification && (
                <div className="fixed bottom-10 right-10 bg-slate-900 text-white px-8 py-4 rounded-3xl shadow-2xl flex items-center gap-3 animate-in fade-in slide-in-from-bottom-10">
                    <CheckCircle className="text-emerald-400" size={24} />
                    <span className="font-bold">{notificationMessage}</span>
                </div>
            )}
        </div>
    );
}
