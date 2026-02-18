'use client';

import React, { useState, useEffect, useCallback } from 'react';
import EligibilityStore from '../../services/EligibilityStore';
import { ProcedureRule, RuleCondition } from '../../types';
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
    Search,
    Plus,
    Trash2,
    Settings,
    Layers,
    Binary,
    History,
    User
} from 'lucide-react';

// Cartographie des variables techniques vers des noms lisibles
const VARIABLE_LABELS: Record<string, string> = {
    'identity.age': 'Âge',
    'identity.nationality_group': 'Groupe Nationalité',
    'identity.born_in_france': 'Né en France',
    'timeline.years_continuous_residence': 'Années de résidence continue',
    'admin.current_visa_type': 'Type de visa actuel',
    'family.spouse_nationality': 'Nationalité du conjoint',
    'family.marriage_duration_years': 'Années de mariage',
    'family.community_of_life': 'Communauté de vie',
    'family.has_french_child': 'Enfant français',
    'work.contract_type': 'Type de contrat (CDI/CDD...)',
    'work.salary_monthly_gross': 'Salaire mensuel brut',
    'work.annual_gross_salary': 'Salaire annuel brut',
    'education.diploma_level': 'Niveau de diplôme',
    'integration.french_level': 'Niveau de français (A1-C1)',
    'integration.civic_exam_passed': 'Examen civique obtenu',
    'civic.clean_criminal_record': 'Casier judiciaire vierge',
    'civic.no_expulsion_order': 'Aucune mesure d\'éloignement (OQTF)',
    'financial.resources_stable_sufficient': 'Ressources stables et suffisantes'
};

const OPERATORS = [
    { value: 'EQ', label: 'égal à' },
    { value: 'NEQ', label: 'différent de' },
    { value: 'GT', label: 'supérieur à' },
    { value: 'GTE', label: 'supér. ou égal à' },
    { value: 'LT', label: 'inférieur à' },
    { value: 'LTE', label: 'infér. ou égal à' },
    { value: 'IN', label: 'parmi la liste' }
];

interface ConditionBuilderProps {
    condition: RuleCondition;
    onChange: (newCondition: RuleCondition) => void;
    level?: number;
}

const ConditionBuilder: React.FC<ConditionBuilderProps> = ({ condition, onChange, level = 0 }) => {
    const isGroup = condition.AND || condition.OR;
    const groupType = condition.AND ? 'AND' : (condition.OR ? 'OR' : null);

    const handleAddCondition = (type: 'AND' | 'OR') => {
        const newSub = { var: 'identity.age', op: 'GTE' as any, val: 18 };
        if (type === groupType) {
            onChange({ ...condition, [type]: [...(condition[type] || []), newSub] });
        } else {
            // Upgrade leaf to group
            onChange({ [type]: [condition, newSub] });
        }
    };

    const updateSubCondition = (idx: number, sub: RuleCondition) => {
        const type = groupType!;
        const newList = [...(condition[type] || [])];
        newList[idx] = sub;
        onChange({ ...condition, [type]: newList });
    };

    const deleteSubCondition = (idx: number) => {
        const type = groupType!;
        const newList = [...(condition[type] || [])];
        newList.splice(idx, 1);

        if (newList.length === 1) {
            // Downgrade group to leaf
            onChange(newList[0]);
        } else {
            onChange({ ...condition, [type]: newList });
        }
    };

    if (isGroup && groupType) {
        return (
            <div className={`p-4 rounded-2xl border-2 ${groupType === 'AND' ? 'border-indigo-100 bg-indigo-50/30' : 'border-amber-100 bg-amber-50/30'} space-y-4`}>
                <div className="flex items-center justify-between">
                    <div className="flex items-center gap-2">
                        <span className={`px-2 py-1 rounded text-[10px] font-black uppercase tracking-widest ${groupType === 'AND' ? 'bg-indigo-600 text-white' : 'bg-amber-600 text-white'}`}>
                            Bloc {groupType === 'AND' ? 'TOUTES' : 'AU MOINS UNE'}
                        </span>
                        <span className="text-xs text-slate-500 font-bold italic">
                            {groupType === 'AND' ? 'Toutes les conditions doivent être vraies' : 'Une seule condition suffit'}
                        </span>
                    </div>
                </div>

                <div className="space-y-3">
                    {condition[groupType]?.map((sub: RuleCondition, idx: number) => (
                        <div key={idx} className="flex gap-4 items-start">
                            <div className="flex-1">
                                <ConditionBuilder
                                    condition={sub}
                                    onChange={(newSub) => updateSubCondition(idx, newSub)}
                                    level={level + 1}
                                />
                            </div>
                            <button
                                onClick={() => deleteSubCondition(idx)}
                                className="p-2 text-slate-300 hover:text-red-500 transition-colors bg-white rounded-lg border border-slate-100 shadow-sm"
                            >
                                <Trash2 size={14} />
                            </button>
                        </div>
                    ))}
                </div>

                <div className="flex gap-2 pt-2">
                    <button
                        onClick={() => handleAddCondition(groupType)}
                        className="flex items-center gap-1 px-3 py-1.5 bg-white border border-slate-200 rounded-lg text-xs font-bold text-slate-600 hover:bg-slate-50 shadow-sm"
                    >
                        <Plus size={14} /> Ajouter une condition
                    </button>
                    <button
                        onClick={() => handleAddCondition(groupType === 'AND' ? 'OR' : 'AND')}
                        className="flex items-center gap-1 px-3 py-1.5 bg-white border border-slate-200 rounded-lg text-xs font-bold text-slate-600 hover:bg-slate-50 shadow-sm"
                    >
                        <Layers size={14} /> Nouveau sous-groupe {groupType === 'AND' ? 'OU' : 'ET'}
                    </button>
                </div>
            </div>
        );
    }

    return (
        <div className="p-4 bg-white rounded-xl border border-slate-200 shadow-sm flex flex-wrap gap-3 items-center">
            {/* Variable Selection */}
            <select
                value={condition.var}
                onChange={(e) => onChange({ ...condition, var: e.target.value })}
                className="bg-slate-50 border border-slate-200 rounded-lg px-3 py-2 text-sm font-bold text-slate-700 min-w-[200px]"
            >
                {Object.entries(VARIABLE_LABELS).map(([val, label]) => (
                    <option key={val} value={val}>{label}</option>
                ))}
            </select>

            {/* Operator Selection */}
            <select
                value={condition.op}
                onChange={(e) => onChange({ ...condition, op: e.target.value as any })}
                className="bg-indigo-50 border border-indigo-100 rounded-lg px-3 py-2 text-sm font-black text-indigo-700"
            >
                {OPERATORS.map(op => (
                    <option key={op.value} value={op.value}>{op.label}</option>
                ))}
            </select>

            {/* Value Input */}
            <div className="flex-1 min-w-[150px]">
                {typeof condition.val === 'boolean' ? (
                    <div className="flex bg-slate-100 p-1 rounded-lg">
                        <button
                            onClick={() => onChange({ ...condition, val: true })}
                            className={`flex-1 px-3 py-1 text-xs font-bold rounded ${condition.val ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                        >
                            OUI
                        </button>
                        <button
                            onClick={() => onChange({ ...condition, val: false })}
                            className={`flex-1 px-3 py-1 text-xs font-bold rounded ${!condition.val ? 'bg-white shadow text-red-600' : 'text-slate-500'}`}
                        >
                            NON
                        </button>
                    </div>
                ) : Array.isArray(condition.val) ? (
                    <input
                        type="text"
                        value={condition.val.join(', ')}
                        onChange={(e) => onChange({ ...condition, val: e.target.value.split(',').map(s => s.trim()) })}
                        placeholder="valeur1, valeur2..."
                        className="w-full bg-slate-50 border border-slate-200 rounded-lg px-3 py-2 text-sm shadow-inner"
                    />
                ) : (
                    <input
                        type={typeof condition.val === 'number' ? 'number' : 'text'}
                        value={condition.val}
                        onChange={(e) => onChange({ ...condition, val: typeof condition.val === 'number' ? parseFloat(e.target.value) : e.target.value })}
                        className="w-full bg-slate-50 border border-slate-200 rounded-lg px-3 py-2 text-sm font-bold shadow-inner"
                    />
                )}
            </div>

            {/* Threshold Reference Indicator */}
            {typeof condition.val === 'string' && condition.val.startsWith('@config:') && (
                <div className="flex items-center gap-1 text-[10px] bg-emerald-100 text-emerald-700 font-black px-2 py-1 rounded-full uppercase tracking-tighter shadow-sm">
                    <Zap size={10} /> Lié aux seuils
                </div>
            )}
        </div>
    );
};

export default function EligibilityConfigPanel() {
    const [thresholds, setThresholds] = useState<any>(null);
    const [rulesSejour, setRulesSejour] = useState<ProcedureRule[]>([]);
    const [rulesNat, setRulesNat] = useState<ProcedureRule[]>([]);
    const [category, setCategory] = useState<'thresholds' | 'rules' | 'audit'>('thresholds');
    const [subCategory, setSubCategory] = useState<'sejour' | 'naturalisation'>('sejour');
    const [showNotification, setShowNotification] = useState(false);
    const [notificationMessage, setNotificationMessage] = useState('');
    const [searchQuery, setSearchQuery] = useState('');
    const [editMode, setEditMode] = useState<'visual' | 'json'>('visual');
    const [auditLog, setAuditLog] = useState<any[]>([]);
    const [auditLoading, setAuditLoading] = useState(false);

    const loadAuditLog = useCallback(async () => {
        setAuditLoading(true);
        const log = await EligibilityStore.fetchAuditLog(50);
        setAuditLog(log);
        setAuditLoading(false);
    }, []);

    useEffect(() => {
        setThresholds(EligibilityStore.getThresholds());
        setRulesSejour(EligibilityStore.getRules('sejour'));
        setRulesNat(EligibilityStore.getRules('naturalisation'));
        loadAuditLog();
    }, [loadAuditLog]);

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

    const updateRuleConditions = (ruleId: string, newConditions: RuleCondition) => {
        if (subCategory === 'sejour') {
            const newRules = [...rulesSejour];
            const i = newRules.findIndex(r => r.id === ruleId);
            if (i !== -1) {
                newRules[i].conditions = newConditions;
                setRulesSejour(newRules);
            }
        } else {
            const newRules = [...rulesNat];
            const i = newRules.findIndex(r => r.id === ruleId);
            if (i !== -1) {
                newRules[i].conditions = newConditions;
                setRulesNat(newRules);
            }
        }
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
                        🧠 Logique (Règles métier)
                    </button>
                    <button
                        onClick={() => { setCategory('audit'); loadAuditLog(); }}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all flex items-center gap-1.5 ${category === 'audit' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        <History size={16} /> Historique ({auditLog.length})
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
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Références de Base</h3>
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
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Passeport Talent (Annuel)</h3>
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
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Immigration</h3>
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
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Mariage</h3>
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
                            className="flex-1 py-4 bg-indigo-600 hover:bg-indigo-700 text-white rounded-2xl font-black shadow-xl shadow-indigo-100 flex items-center justify-center gap-2 transition-all active:scale-95"
                        >
                            <Save size={20} /> Sauvegarder les seuils
                        </button>
                        <button
                            onClick={handleResetThresholds}
                            className="px-6 py-4 bg-white border border-slate-200 text-slate-500 hover:text-red-600 rounded-2xl font-bold transition-all hover:bg-red-50"
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
                                className="w-full pl-12 pr-4 py-2 bg-white border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 shadow-sm"
                            />
                        </div>

                        <div className="flex bg-slate-200 p-1 rounded-xl">
                            <button
                                onClick={() => setEditMode('visual')}
                                className={`px-4 py-1.5 rounded-lg text-xs font-black transition-all ${editMode === 'visual' ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                            >
                                VISUEL
                            </button>
                            <button
                                onClick={() => setEditMode('json')}
                                className={`px-4 py-1.5 rounded-lg text-xs font-black transition-all ${editMode === 'json' ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                            >
                                JSON
                            </button>
                        </div>

                        <button
                            onClick={handleSaveRules}
                            className="px-6 py-2 bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl font-bold flex items-center gap-2 shadow-lg transition-all active:scale-95"
                        >
                            <Save size={18} /> Sauvegarder {subCategory === 'sejour' ? rulesSejour.length : rulesNat.length} règles
                        </button>
                    </div>

                    <div className="flex-1 overflow-auto bg-slate-200/50 rounded-3xl border border-slate-200 shadow-inner p-4 space-y-4">
                        {(subCategory === 'sejour' ? rulesSejour : rulesNat)
                            .filter(r => r.name.toLowerCase().includes(searchQuery.toLowerCase()) || r.id.includes(searchQuery))
                            .map((rule) => (
                                <div key={rule.id} className="p-6 bg-white rounded-3xl border border-slate-200 shadow-sm group hover:border-indigo-300 transition-colors">
                                    <div className="flex items-center justify-between mb-6">
                                        <div className="flex items-center gap-3">
                                            <div className="w-10 h-10 bg-indigo-100 text-indigo-600 rounded-2xl flex items-center justify-center font-black">
                                                {rule.priority}
                                            </div>
                                            <div>
                                                <h4 className="font-black text-slate-900 text-lg">{rule.name}</h4>
                                                <p className="text-xs text-slate-400 font-mono tracking-tighter uppercase">{rule.id}</p>
                                            </div>
                                        </div>
                                        <div className="flex items-center gap-3">
                                            <label className="text-xs font-black text-slate-400 uppercase">Priorité</label>
                                            <input
                                                type="number"
                                                value={rule.priority}
                                                onChange={(e) => {
                                                    const val = parseInt(e.target.value);
                                                    if (subCategory === 'sejour') {
                                                        const i = rulesSejour.findIndex(r => r.id === rule.id);
                                                        const newRules = [...rulesSejour];
                                                        newRules[i].priority = val;
                                                        setRulesSejour(newRules);
                                                    } else {
                                                        const i = rulesNat.findIndex(r => r.id === rule.id);
                                                        const newRules = [...rulesNat];
                                                        newRules[i].priority = val;
                                                        setRulesNat(newRules);
                                                    }
                                                }}
                                                className="w-16 px-2 py-2 bg-slate-50 border border-slate-200 rounded-xl text-center font-black text-slate-700 focus:ring-2 focus:ring-indigo-500"
                                            />
                                        </div>
                                    </div>

                                    <div className="space-y-3">
                                        <div className="flex items-center justify-between">
                                            <div className="flex items-center gap-2 text-indigo-600 text-xs font-black uppercase tracking-widest">
                                                {editMode === 'visual' ? <Binary size={14} /> : <Code size={14} />}
                                                Conditions de cette règle
                                            </div>
                                        </div>

                                        {editMode === 'visual' ? (
                                            <ConditionBuilder
                                                condition={rule.conditions}
                                                onChange={(newConditions) => updateRuleConditions(rule.id, newConditions)}
                                            />
                                        ) : (
                                            <textarea
                                                value={JSON.stringify(rule.conditions, null, 2)}
                                                onChange={(e) => {
                                                    try {
                                                        const newConditions = JSON.parse(e.target.value);
                                                        updateRuleConditions(rule.id, newConditions);
                                                    } catch (err) { /* Invalid JSON */ }
                                                }}
                                                spellCheck={false}
                                                rows={8}
                                                className="w-full p-6 bg-slate-900 text-emerald-400 font-mono text-xs rounded-2xl border border-slate-800 focus:ring-2 focus:ring-emerald-500 resize-none shadow-xl"
                                            />
                                        )}
                                    </div>
                                </div>
                            ))}
                    </div>
                </div>
            )}

            {/* Audit Trail Tab */}
            {category === 'audit' && (
                <div className="space-y-6 max-w-4xl">
                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4 mb-6">
                            <History className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">Historique des modifications</h2>
                            <button onClick={loadAuditLog}
                                className="ml-auto px-4 py-2 bg-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-slate-200 transition-all flex items-center gap-1">
                                <RotateCcw size={14} /> Actualiser
                            </button>
                        </div>

                        {auditLoading ? (
                            <div className="text-center py-12">
                                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full mx-auto mb-3"></div>
                                <p className="text-sm font-bold text-slate-400">Chargement...</p>
                            </div>
                        ) : auditLog.length === 0 ? (
                            <div className="text-center py-12">
                                <History className="mx-auto text-slate-300 mb-3" size={40} />
                                <p className="text-sm font-bold text-slate-400">Aucune modification enregistrée</p>
                                <p className="text-xs text-slate-300 mt-1">Les modifications apparaîtront ici dès qu'une règle sera éditée via le backend.</p>
                            </div>
                        ) : (
                            <div className="space-y-4">
                                {auditLog.map((entry: any) => {
                                    const date = new Date(entry.createdAt).toLocaleDateString('fr-FR', {
                                        day: 'numeric', month: 'long', year: 'numeric', hour: '2-digit', minute: '2-digit'
                                    });
                                    const actionColor = entry.action === 'CREATE' ? 'bg-emerald-100 text-emerald-700'
                                        : entry.action === 'DELETE' ? 'bg-red-100 text-red-700'
                                            : 'bg-amber-100 text-amber-700';
                                    return (
                                        <div key={entry.id} className="p-5 bg-slate-50 rounded-2xl border border-slate-100 hover:border-indigo-200 transition-colors">
                                            <div className="flex items-center justify-between mb-2">
                                                <div className="flex items-center gap-2">
                                                    <span className={`text-[10px] font-black uppercase tracking-widest px-2 py-1 rounded-lg ${actionColor}`}>
                                                        {entry.action}
                                                    </span>
                                                    <span className="text-[10px] font-black uppercase tracking-widest px-2 py-1 bg-slate-200 text-slate-600 rounded-lg">
                                                        {entry.category}
                                                    </span>
                                                </div>
                                                <span className="text-xs font-bold text-slate-400">{date}</span>
                                            </div>
                                            <h4 className="font-black text-slate-900">{entry.ruleName}</h4>
                                            {entry.changeDetails && (
                                                <p className="text-sm text-slate-500 font-medium mt-1">{entry.changeDetails}</p>
                                            )}
                                            <div className="flex items-center gap-2 mt-2">
                                                <User size={12} className="text-slate-400" />
                                                <span className="text-xs font-bold text-slate-500">{entry.changedBy}</span>
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>
                        )}
                    </div>
                </div>
            )}

            {/* Notifications */}
            {showNotification && (
                <div className="fixed bottom-10 right-10 bg-slate-900 text-white px-8 py-4 rounded-3xl shadow-2xl flex items-center gap-3 animate-in fade-in slide-in-from-bottom-10 z-50">
                    <CheckCircle className="text-emerald-400" size={24} />
                    <span className="font-bold">{notificationMessage}</span>
                </div>
            )}
        </div>
    );
}
