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
    User,
    AlertTriangle,
    Brain
} from 'lucide-react';

// Cartographie COMPLÈTE des variables techniques vers des noms lisibles (96 variables)
const VARIABLE_LABELS: Record<string, string> = {
    // ── Identité ──
    'identity.age': 'Âge',
    'identity.nationality_group': 'Groupe nationalité',
    'identity.born_in_france': 'Né(e) en France',
    'identity.lost_french_nationality': 'A perdu la nationalité française',

    // ── Chronologie / Résidence ──
    'timeline.years_continuous_residence': 'Années de résidence continue',
    'timeline.years_residence_since_age_11': 'Années de résidence depuis 11 ans',
    'timeline.years_residence_since_age_8': 'Années de résidence depuis 8 ans',
    'timeline.age_at_entry': 'Âge à l\'entrée en France',
    'timeline.resides_in_france': 'Réside en France',
    'timeline.residence_since_age_6': 'Résidence depuis l\'âge de 6 ans',

    // ── Situation administrative ──
    'admin.current_visa_type': 'Type de visa actuel',
    'admin.entered_legally': 'Entré légalement',
    'admin.entry_mode': 'Mode d\'entrée',
    'admin.has_valid_visa_or_permit': 'Visa / titre de séjour valide',
    'admin.health_insurance': 'Assurance maladie',

    // ── Famille ──
    'family.spouse_nationality': 'Nationalité du conjoint',
    'family.marriage_duration_years': 'Années de mariage',
    'family.community_of_life': 'Communauté de vie',
    'family.has_french_child': 'Enfant français',
    'family.child_residence_france': 'Enfant réside en France',
    'family.cohabitation_duration_years': 'Durée cohabitation (années)',
    'family.contributes_to_education': 'Contribue à l\'éducation',
    'family.has_french_sibling': 'Frère/Sœur français(e)',
    'family.is_ascendant_of_french': 'Ascendant de Français',
    'family.is_pacsed_with_french': 'Pacsé(e) avec un Français',
    'family.is_polygamous': 'Situation de polygamie',
    'family.spouse_kept_nationality': 'Conjoint a conservé sa nationalité',
    'family.spouse_has_passport_talent': 'Conjoint titulaire Passeport Talent',
    'family.has_handicap_allowance': 'Allocation handicap (AAH)',
    'family.housing_status': 'Situation de logement',
    'family.income_source': 'Source de revenus',
    'family.presence_duration': 'Durée de présence',
    'family.rf_family_members_count': 'Nombre membres famille (RF)',
    'family.rf_has_valid_titre_sejour': 'Titre de séjour valide (RF)',
    'family.rf_housing_surface': 'Surface logement m² (RF)',
    'family.rf_marital_status': 'Situation matrimoniale (RF)',
    'family.sponsor_nationality': 'Nationalité du demandeur (RF)',

    // ── Travail / Emploi ──
    'work.contract_type': 'Type de contrat (CDI/CDD…)',
    'work.salary_monthly_gross': 'Salaire mensuel brut',
    'work.annual_gross_salary': 'Salaire annuel brut',
    'work.has_work_authorization': 'Autorisation de travail',
    'work.has_payslips': 'Bulletins de salaire',
    'work.wants_to_work': 'Souhaite travailler',
    'work.contract_duration_months': 'Durée contrat (mois)',
    'work.company_role': 'Rôle dans l\'entreprise',
    'work.group_seniority_months': 'Ancienneté groupe (mois)',
    'work.business_project_viable': 'Projet d\'entreprise viable',
    'work.job_in_tension_list': 'Métier en tension',
    'work.job_related_to_rd': 'Emploi lié à la R&D',
    'work.years_experience_comparable': 'Années d\'expérience comparable',
    'work.has_hosting_agreement': 'Convention d\'accueil',
    'work.has_legion_honneur': 'Légion d\'honneur',
    'work.has_work_accident_pension': 'Rente accident du travail',
    'work.work_accident_rate': 'Taux incapacité accident (%)',
    'work.served_french_military': 'Service militaire français',
    'work.is_artist': 'Artiste / Interprète',
    'work.is_au_pair': 'Jeune au pair',
    'work.is_entrepreneur': 'Entrepreneur',
    'work.is_ict_transfer': 'Transfert intra-groupe (ICT)',
    'work.is_innovative_company': 'Entreprise innovante (JEI)',
    'work.is_intern': 'Stagiaire',
    'work.is_manager_or_expert': 'Cadre dirigeant / Expert',
    'work.is_researcher': 'Chercheur',
    'work.is_salarie_mission': 'Salarié en mission',
    'work.is_sportif_haut_niveau': 'Sportif de haut niveau',
    'work.is_volunteer': 'Volontaire / Service civique',

    // ── Études / Éducation ──
    'education.diploma_level': 'Niveau de diplôme',
    'education.has_french_higher_education_diploma': 'Diplôme supérieur français',
    'education.is_enrolled_higher_ed': 'Inscrit dans l\'enseignement supérieur',
    'education.schooling_in_france_age_6_to_16': 'Scolarisé en France (6-16 ans)',
    'education.years_higher_education': 'Années d\'études supérieures',
    'education.years_schooling_france': 'Années de scolarité en France',

    // ── Intégration / Civisme ──
    'integration.french_level': 'Niveau de français (A1–C1)',
    'integration.civic_exam_passed': 'Examen civique obtenu',
    'integration.adheres_to_republican_values': 'Adhésion aux valeurs républicaines',
    'civic.clean_criminal_record': 'Casier judiciaire vierge',
    'civic.no_expulsion_order': 'Aucune mesure d\'éloignement (OQTF)',

    // ── Finances ──
    'financial.resources_stable_sufficient': 'Ressources stables et suffisantes',
    'financial.resources_annual_total': 'Ressources annuelles totales',
    'financial.resources_monthly_average': 'Ressources mensuelles moyennes',
    'investment.amount': 'Montant investissement',
    'investment.creates_jobs': 'Création d\'emplois',

    // ── Asile ──
    'asylum.is_asylum_seeker': 'Demandeur d\'asile',
    'asylum.asylum_application_pending': 'Demande d\'asile en cours',

    // ── Santé ──
    'health.personal_needs_treatment': 'Besoin de soins personnels',
    'health.treatment_unavailable_in_origin': 'Soins indisponibles au pays',
    'health.child_needs_care': 'Enfant nécessitant des soins',

    // ── Vulnérabilité ──
    'vulnerability.is_victim_domestic_violence': 'Victime de violences conjugales',
    'vulnerability.has_protection_order_violence': 'Ordonnance de protection',
    'vulnerability.is_victim_trafficking': 'Victime de traite',

    // ── Régularisation / AES ──
    'regularisation.years_presence_france': 'Années de présence en France',
    'regularisation.has_children_schooled_3y': 'Enfants scolarisés 3 ans',
    'regularisation.has_exceptional_talent': 'Talent exceptionnel',

    // ── Nationalité (extra) ──
    'nationality_extra.possession_etat_francais': 'Possession d\'état de Français',

    // ── Logement / Résidence ──
    'residence.maintains_home_abroad': 'Conserve domicile à l\'étranger',

    // ── Projet ──
    'project.is_real_and_serious': 'Projet réel et sérieux',
};

// Groupes pour l'affichage <optgroup> dans le sélecteur
const VARIABLE_GROUPS: { label: string; prefix: string[] }[] = [
    { label: '👤 Identité', prefix: ['identity.'] },
    { label: '📅 Chronologie / Résidence', prefix: ['timeline.', 'residence.'] },
    { label: '📋 Situation administrative', prefix: ['admin.'] },
    { label: '👨‍👩‍👧 Famille', prefix: ['family.'] },
    { label: '💼 Travail / Emploi', prefix: ['work.'] },
    { label: '🎓 Études / Éducation', prefix: ['education.'] },
    { label: '🇫🇷 Intégration / Civisme', prefix: ['integration.', 'civic.'] },
    { label: '💰 Finances / Investissement', prefix: ['financial.', 'investment.'] },
    { label: '🛡️ Asile', prefix: ['asylum.'] },
    { label: '🏥 Santé', prefix: ['health.'] },
    { label: '⚠️ Vulnérabilité', prefix: ['vulnerability.'] },
    { label: '📄 Régularisation', prefix: ['regularisation.'] },
    { label: '🏠 Nationalité / Divers', prefix: ['nationality_extra.', 'project.'] },
];

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
        const newSub = { var: 'civic.clean_criminal_record', op: 'EQ' as any, val: true };
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
                className="bg-slate-50 border border-slate-200 rounded-lg px-3 py-2 text-sm font-bold text-slate-700 min-w-[240px]"
            >
                {VARIABLE_GROUPS.map(group => {
                    const entries = Object.entries(VARIABLE_LABELS).filter(([key]) =>
                        group.prefix.some(p => key.startsWith(p))
                    );
                    if (entries.length === 0) return null;
                    return (
                        <optgroup key={group.label} label={group.label}>
                            {entries.map(([val, label]) => (
                                <option key={val} value={val}>{label}</option>
                            ))}
                        </optgroup>
                    );
                })}
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
    const [rulesMap, setRulesMap] = useState<Record<string, ProcedureRule[]>>({});
    const [category, setCategory] = useState<'thresholds' | 'rules' | 'audit' | 'diagnostic'>('thresholds');
    const [subCategory, setSubCategory] = useState<string>('sejour');
    const [showNotification, setShowNotification] = useState(false);
    const [notificationMessage, setNotificationMessage] = useState('');
    const [searchQuery, setSearchQuery] = useState('');
    const [editMode, setEditMode] = useState<'visual' | 'json'>('visual');
    const [auditLog, setAuditLog] = useState<any[]>([]);
    const [auditLoading, setAuditLoading] = useState(false);
    const [healthData, setHealthData] = useState<any>(null);
    const [diagnosticData, setDiagnosticData] = useState<any>(null);
    const [diagnosticLoading, setDiagnosticLoading] = useState(false);

    const API_BASE = typeof window !== 'undefined' ? (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000') : 'http://localhost:4000';

    // Helper: extract .value from versioned threshold or return raw number
    const tv = (val: any): number => {
        if (val && typeof val === 'object' && 'value' in val) return val.value;
        return val ?? 0;
    };

    const loadHealthData = useCallback(async () => {
        try {
            const res = await fetch(`${API_BASE}/eligibility/thresholds/health`);
            if (res.ok) setHealthData(await res.json());
        } catch (err) { console.warn('[ELIGIBILITY] Health check failed', err); }
    }, [API_BASE]);

    const loadDiagnostic = useCallback(async () => {
        setDiagnosticLoading(true);
        try {
            const res = await fetch(`${API_BASE}/eligibility/consistency-check`);
            if (res.ok) setDiagnosticData(await res.json());
        } catch (err) { console.warn('[ELIGIBILITY] Consistency check failed', err); }
        setDiagnosticLoading(false);
    }, [API_BASE]);

    const loadAuditLog = useCallback(async () => {
        setAuditLoading(true);
        const log = await EligibilityStore.fetchAuditLog(50);
        setAuditLog(log);
        setAuditLoading(false);
    }, []);

    useEffect(() => {
        setThresholds(EligibilityStore.getThresholds());
        const cats = ['sejour', 'naturalisation', 'family', 'asile'];
        const map: Record<string, ProcedureRule[]> = {};
        cats.forEach(c => { map[c] = EligibilityStore.getRules(c as any); });
        setRulesMap(map);
        loadAuditLog();
        loadHealthData();
    }, [loadAuditLog, loadHealthData]);

    const showNotificationMsg = (message: string) => {
        setNotificationMessage(message);
        setShowNotification(true);
        setTimeout(() => setShowNotification(false), 3000);
    };

    const handleSaveThresholds = async () => {
        EligibilityStore.updateThresholds(thresholds);
        // Persist to backend with audit trail
        const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
        try {
            await fetch(`${API_URL}/eligibility/thresholds`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ thresholds, changedBy: 'admin' }),
            });
        } catch (err) { console.warn('[ELIGIBILITY] Backend threshold save failed', err); }
        showNotificationMsg('✅ Seuils sauvegardés');
        loadAuditLog();
    };

    const handleResetThresholds = () => {
        if (!confirm('Réinitialiser tous les seuils aux valeurs par défaut ?')) return;
        EligibilityStore.resetThresholds();
        setThresholds(EligibilityStore.getThresholds());
        showNotificationMsg('🔄 Seuils réinitialisés');
    };

    const handleSaveRules = async () => {
        const rules = rulesMap[subCategory] || [];
        for (const rule of rules) {
            await EligibilityStore.saveRuleToBackend(subCategory, rule.id, rule.conditions, 'admin');
        }
        EligibilityStore.updateRules(subCategory as any, rules);
        showNotificationMsg(`✅ ${rules.length} règles sauvegardées (${subCategory})`);
        loadAuditLog();
    };

    const updateNestedThreshold = (path: string[], value: any) => {
        // For versioned thresholds, update the .value property instead of replacing the whole object
        const newThresholds = { ...thresholds };
        let current = newThresholds;
        for (let i = 0; i < path.length - 1; i++) {
            current = current[path[i]];
        }
        const lastKey = path[path.length - 1];
        const existing = current[lastKey];
        // If versioned (object with .value), update .value only
        if (existing && typeof existing === 'object' && 'value' in existing) {
            current[lastKey] = { ...existing, value };
        } else {
            current[lastKey] = value;
        }
        setThresholds(newThresholds);
    };

    const updateRuleConditions = (ruleId: string, newConditions: RuleCondition) => {
        const rules = [...(rulesMap[subCategory] || [])];
        const i = rules.findIndex(r => r.id === ruleId);
        if (i !== -1) {
            rules[i] = { ...rules[i], conditions: newConditions };
            setRulesMap(prev => ({ ...prev, [subCategory]: rules }));
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
                        💰 Seuils
                    </button>
                    <button
                        onClick={() => setCategory('rules')}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${category === 'rules' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        🧠 Règles
                    </button>
                    <button
                        onClick={() => { setCategory('audit'); loadAuditLog(); }}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all flex items-center gap-1.5 ${category === 'audit' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        <History size={16} /> Audit
                    </button>
                    <button
                        onClick={() => { setCategory('diagnostic'); loadDiagnostic(); }}
                        className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${category === 'diagnostic' ? 'bg-indigo-600 text-white' : 'text-slate-600 hover:text-slate-900'
                            }`}
                    >
                        🩺 Diagnostic
                    </button>
                </div>
            </div>

            {category === 'thresholds' && (
                <div className="space-y-6 max-w-4xl">
                    {/* SALAIRES ET FINANCES */}
                    {/* Health Banner */}
                    {healthData && healthData.status === 'WARNING' && (
                        <div className="bg-amber-50 border-2 border-amber-300 rounded-2xl p-5 flex items-start gap-3">
                            <AlertTriangle className="text-amber-600 flex-shrink-0 mt-0.5" size={20} />
                            <div>
                                <p className="font-black text-amber-800 text-sm">{healthData.recommendation}</p>
                                {healthData.alerts?.filter((a: any) => a.isStale).map((a: any, i: number) => (
                                    <p key={i} className="text-xs text-amber-600 mt-1">⚠️ {a.label}: {a.currentValue}€ (valide depuis {a.validFrom})</p>
                                ))}
                                {healthData.meta?.reviewOverdue && (
                                    <p className="text-xs text-amber-600 mt-1">📅 Revue à planifier (prévue le {healthData.meta.nextReviewDue})</p>
                                )}
                            </div>
                        </div>
                    )}

                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm space-y-6">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4">
                            <DollarSign className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">Seuils Financiers</h2>
                        </div>

                        {/* Salaires annuels bruts */}
                        <div className="space-y-4">
                            <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Salaires Annuels Bruts</h3>
                            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                                {[
                                    { label: 'PT — Salarié Qualifié', key: 'passeport_talent_salarie_qualifie' },
                                    { label: 'PT — Carte Bleue UE', key: 'passeport_talent_carte_bleue_eu' },
                                    { label: 'PT — Mission', key: 'passeport_talent_mission' },
                                    { label: 'PT — Mandataire Social', key: 'passeport_talent_mandataire_social' },
                                    { label: 'Visiteur — Ressources annuelles', key: 'visiteur_ressources_annuelles' },
                                ].map(item => {
                                    const entry = thresholds.financial_thresholds.salary_annual_gross[item.key];
                                    const ref = entry?.source_ref || '';
                                    return (
                                        <div key={item.key} className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                                            <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                            {ref && <span className="text-[10px] text-indigo-500 font-mono">{ref}</span>}
                                            <input
                                                type="number"
                                                value={tv(entry)}
                                                onChange={(e) => updateNestedThreshold(['financial_thresholds', 'salary_annual_gross', item.key], parseFloat(e.target.value))}
                                                className="w-full mt-1 px-4 py-3 bg-white border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                            />
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* Salaires mensuels bruts */}
                        <div className="space-y-4">
                            <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Salaires Mensuels Bruts</h3>
                            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                                {[
                                    { label: 'SMIC Mensuel Brut', key: 'smic' },
                                    { label: 'Entrepreneur — Viabilité', key: 'entrepreneur_viabilite' },
                                    { label: 'Algérien Étudiant', key: 'algerien_etudiant_ressources' },
                                    { label: 'ICT Détaché', key: 'ict_detache_ressources' },
                                    { label: 'Regroupement Familial', key: 'regroupement_familial_resources' },
                                ].map(item => {
                                    const entry = thresholds.financial_thresholds.salary_monthly_gross[item.key];
                                    const ref = entry?.source_ref || '';
                                    return (
                                        <div key={item.key} className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                                            <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                            {ref && <span className="text-[10px] text-indigo-500 font-mono">{ref}</span>}
                                            <input
                                                type="number"
                                                value={tv(entry)}
                                                onChange={(e) => updateNestedThreshold(['financial_thresholds', 'salary_monthly_gross', item.key], parseFloat(e.target.value))}
                                                className="w-full mt-1 px-4 py-3 bg-white border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                            />
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* Investissements */}
                        <div className="space-y-4">
                            <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Investissements</h3>
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                                {[
                                    { label: 'Création Entreprise — Fonds', key: 'creation_entreprise_fonds' },
                                    { label: 'Investisseur Économique — Fonds', key: 'investisseur_eco_fonds' },
                                ].map(item => {
                                    const entry = thresholds.financial_thresholds.investments[item.key];
                                    const ref = entry?.source_ref || '';
                                    return (
                                        <div key={item.key} className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                                            <label className="block text-sm font-bold text-slate-700 mb-1">{item.label}</label>
                                            {ref && <span className="text-[10px] text-indigo-500 font-mono">{ref}</span>}
                                            <input
                                                type="number"
                                                value={tv(entry)}
                                                onChange={(e) => updateNestedThreshold(['financial_thresholds', 'investments', item.key], parseFloat(e.target.value))}
                                                className="w-full mt-1 px-4 py-3 bg-white border border-slate-200 rounded-xl font-bold focus:ring-2 focus:ring-indigo-500"
                                            />
                                        </div>
                                    );
                                })}
                            </div>
                        </div>
                    </div>

                    {/* DUREES DE RESIDENCE ET MARIAGE */}
                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm space-y-6">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4">
                            <Clock className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">Durées de résidence & mariage (années)</h2>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            <div className="space-y-4">
                                <h3 className="font-bold text-slate-400 uppercase tracking-widest text-xs font-mono">Résidence en France</h3>
                                {[
                                    { label: 'Naturalisation Standard', key: 'naturalisation_standard' },
                                    { label: 'Naturalisation (Études Sup)', key: 'naturalisation_etudes_sup' },
                                    { label: 'Carte Résident Longue Durée UE', key: 'resident_longue_duree_ue' },
                                    { label: 'Regroupement Familial', key: 'regroupement_familial_resident' },
                                    { label: 'Ascendant de Français', key: 'ascendant_de_francais' },
                                    { label: 'Enfant Né en France (continu)', key: 'enfant_ne_en_france_continuous' },
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
                                    { label: 'Pour Nationalité (mariage)', key: 'pour_nationalite_mariage' },
                                    { label: 'Pour Carte Résident (conjoint FR)', key: 'pour_carte_resident_conjoint_francais' },
                                    { label: 'Pour Carte Résident (conjoint TN)', key: 'pour_carte_resident_conjoint_francais_tunisien' },
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
            )}

            {category === 'rules' && (
                <div className="flex flex-col h-full overflow-hidden">
                    <div className="flex items-center gap-4 mb-4">
                        <select
                            value={subCategory}
                            onChange={(e) => setSubCategory(e.target.value as any)}
                            className="px-4 py-2 bg-white border border-slate-200 rounded-xl font-bold text-slate-700 shadow-sm"
                        >
                            <option value="sejour">🛂 Titres de Séjour</option>
                            <option value="naturalisation">🇫🇷 Naturalisation</option>
                            <option value="family">👨‍👩‍👧 Regroupement Familial</option>
                            <option value="asile">🛡️ Asile</option>
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
                            <Save size={18} /> Sauvegarder {(rulesMap[subCategory] || []).length} règles
                        </button>
                    </div>

                    <div className="flex-1 overflow-auto bg-slate-200/50 rounded-3xl border border-slate-200 shadow-inner p-4 space-y-4">
                        {(rulesMap[subCategory] || [])
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
                                                    const rules = [...(rulesMap[subCategory] || [])];
                                                    const i = rules.findIndex((r: ProcedureRule) => r.id === rule.id);
                                                    if (i !== -1) {
                                                        rules[i] = { ...rules[i], priority: val };
                                                        setRulesMap(prev => ({ ...prev, [subCategory]: rules }));
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

            {/* Diagnostic Tab */}
            {category === 'diagnostic' && (
                <div className="space-y-6 max-w-5xl">
                    <div className="bg-white rounded-3xl p-8 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 border-b border-slate-100 pb-4 mb-6">
                            <Brain className="text-indigo-600" size={24} />
                            <h2 className="text-xl font-black text-slate-900">🩺 Diagnostic de Cohérence</h2>
                            <button onClick={loadDiagnostic} className="ml-auto px-4 py-2 bg-indigo-600 text-white rounded-xl text-xs font-black hover:bg-indigo-700 transition-colors">
                                Relancer le diagnostic
                            </button>
                        </div>

                        {diagnosticLoading ? (
                            <div className="text-center py-12">
                                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full mx-auto mb-3"></div>
                                <p className="text-sm font-bold text-slate-400">Analyse en cours...</p>
                            </div>
                        ) : !diagnosticData ? (
                            <div className="text-center py-12">
                                <Brain className="mx-auto text-slate-300 mb-3" size={40} />
                                <p className="text-sm font-bold text-slate-400">Cliquez sur &quot;Relancer le diagnostic&quot; pour analyser</p>
                            </div>
                        ) : (
                            <>
                                {/* Summary Cards */}
                                <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
                                    <div className="bg-slate-50 rounded-2xl p-4 text-center border border-slate-100">
                                        <div className="text-2xl font-black text-slate-900">{diagnosticData.summary?.totalProfiles || 0}</div>
                                        <div className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Profils testés</div>
                                    </div>
                                    <div className="bg-slate-50 rounded-2xl p-4 text-center border border-slate-100">
                                        <div className="text-2xl font-black text-slate-900">{diagnosticData.summary?.totalRules || 0}</div>
                                        <div className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Règles totales</div>
                                    </div>
                                    <div className={`rounded-2xl p-4 text-center border ${diagnosticData.summary?.orphanRulesCount > 0 ? 'bg-amber-50 border-amber-200' : 'bg-emerald-50 border-emerald-200'}`}>
                                        <div className={`text-2xl font-black ${diagnosticData.summary?.orphanRulesCount > 0 ? 'text-amber-700' : 'text-emerald-700'}`}>{diagnosticData.summary?.orphanRulesCount || 0}</div>
                                        <div className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Orphelines</div>
                                    </div>
                                    <div className={`rounded-2xl p-4 text-center border ${diagnosticData.summary?.profilesWithIssues > 0 ? 'bg-red-50 border-red-200' : 'bg-emerald-50 border-emerald-200'}`}>
                                        <div className={`text-2xl font-black ${diagnosticData.summary?.profilesWithIssues > 0 ? 'text-red-700' : 'text-emerald-700'}`}>{diagnosticData.summary?.profilesWithIssues || 0}</div>
                                        <div className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Profils ⚠️</div>
                                    </div>
                                </div>

                                {/* Profile Results */}
                                <h3 className="font-black text-slate-900 text-sm uppercase tracking-widest mb-3">Résultats par profil</h3>
                                <div className="space-y-2 mb-6">
                                    {diagnosticData.profileResults?.map((p: any) => (
                                        <div key={p.profileId} className={`p-4 rounded-xl border flex items-center gap-3 ${p.status === '✅' ? 'bg-emerald-50 border-emerald-200' : 'bg-amber-50 border-amber-200'}`}>
                                            <span className="text-lg">{p.status}</span>
                                            <div className="flex-1 min-w-0">
                                                <span className="font-black text-sm text-slate-900">{p.profileName}</span>
                                                <span className="text-xs text-slate-500 ml-2">{p.totalMatches} match{p.totalMatches !== 1 ? 'es' : ''}</span>
                                                {p.missingExpected?.length > 0 && (
                                                    <span className="text-xs text-red-600 ml-2">❌ Manquant: {p.missingExpected.join(', ')}</span>
                                                )}
                                                {p.unexpectedCategories?.length > 0 && (
                                                    <span className="text-xs text-amber-600 ml-2">⚠️ Inattendu: {p.unexpectedCategories.join(', ')}</span>
                                                )}
                                            </div>
                                            <div className="flex gap-1">
                                                {p.actualCategories?.map((c: string) => (
                                                    <span key={c} className="text-[10px] font-black uppercase px-2 py-0.5 rounded-lg bg-indigo-100 text-indigo-700">{c}</span>
                                                ))}
                                            </div>
                                        </div>
                                    ))}
                                </div>

                                {/* Orphan Rules */}
                                {diagnosticData.orphanRules?.length > 0 && (
                                    <>
                                        <h3 className="font-black text-slate-900 text-sm uppercase tracking-widest mb-3">Règles orphelines ({diagnosticData.orphanRules.length})</h3>
                                        <div className="bg-amber-50 rounded-xl border border-amber-200 p-4 max-h-48 overflow-auto">
                                            <div className="grid grid-cols-2 md:grid-cols-3 gap-1">
                                                {diagnosticData.orphanRules.map((r: string) => (
                                                    <span key={r} className="text-xs font-mono text-amber-700">⭕ {r}</span>
                                                ))}
                                            </div>
                                        </div>
                                    </>
                                )}

                                {/* Threshold Health */}
                                {healthData && (
                                    <div className="mt-6">
                                        <h3 className="font-black text-slate-900 text-sm uppercase tracking-widest mb-3">Santé des seuils</h3>
                                        <div className={`rounded-xl border p-4 ${healthData.status === 'OK' ? 'bg-emerald-50 border-emerald-200' : 'bg-amber-50 border-amber-200'}`}>
                                            <div className="flex items-center gap-2 mb-2">
                                                <span className={`text-sm font-black ${healthData.status === 'OK' ? 'text-emerald-700' : 'text-amber-700'}`}>
                                                    {healthData.status === 'OK' ? '✅ Tous les seuils sont à jour' : '⚠️ Attention requise'}
                                                </span>
                                                {healthData.meta?.lastReviewed && (
                                                    <span className="text-xs text-slate-500 ml-auto">Dernière revue : {healthData.meta.lastReviewed}</span>
                                                )}
                                            </div>
                                            <div className="space-y-1">
                                                {healthData.alerts?.map((a: any, i: number) => (
                                                    <div key={i} className="flex items-center gap-2 text-xs">
                                                        <span>{a.isStale ? '⚠️' : '✅'}</span>
                                                        <span className="font-bold text-slate-700">{a.label}</span>
                                                        <span className="text-slate-500">{a.currentValue}€</span>
                                                        {a.validFrom && <span className="text-slate-400">depuis {a.validFrom}</span>}
                                                        {a.sourceRef && <span className="text-indigo-500">({a.sourceRef})</span>}
                                                    </div>
                                                ))}
                                            </div>
                                        </div>
                                    </div>
                                )}
                            </>
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
