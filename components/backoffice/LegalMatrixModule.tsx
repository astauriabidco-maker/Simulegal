'use client';

import React, { useState, useMemo } from 'react';
import {
    Grid3X3,
    Bot,
    CheckCircle,
    XCircle,
    Zap,
    ArrowRight,
    Info,
    AlertCircle,
    BrainCircuit,
    Cpu,
    Workflow,
    Layers,
    Link2,
    X,
    Maximize2,
    Share2,
    Search,
    Filter,
    Download,
    ChevronDown,
    Activity,
    FileText,
    AlertTriangle,
    Lightbulb,
    ShieldAlert,
    TrendingUp
} from 'lucide-react';
import EligibilityStore from '../../services/EligibilityStore';
import { RuleCondition, ProcedureRule } from '../../types';
import { DOC_CATALOG } from '../../config/DocumentTemplates';

// ─── Critères analysés ─────────────────────────────────────────────
const CRITERIA = [
    { id: 'identity.age', label: 'Âge', group: 'Général', color: 'bg-blue-500' },
    { id: 'identity.nationality_group', label: 'Nat.', group: 'Général', color: 'bg-indigo-500' },
    { id: 'timeline.years_continuous_residence', label: 'Résidence', group: 'Parcours', color: 'bg-emerald-500' },
    { id: 'work.annual_gross_salary', label: 'Salaire', group: 'Finances', color: 'bg-amber-500' },
    { id: 'family.marriage_duration_years', label: 'Mariage', group: 'Famille', color: 'bg-pink-500' },
    { id: 'integration.french_level', label: 'Français', group: 'Intégration', color: 'bg-purple-500' },
    { id: 'education.diploma_level', label: 'Diplôme', group: 'Études', color: 'bg-cyan-500' },
    { id: 'civic.clean_criminal_record', label: 'Sécurité', group: 'Sécurité', color: 'bg-red-500' },
    { id: 'financial.resources_stable_sufficient', label: 'Ressources', group: 'Finances', color: 'bg-yellow-500' },
    { id: 'admin.has_valid_visa_or_permit', label: 'Visa', group: 'Admin', color: 'bg-teal-500' },
];

// ─── Catégories de procédures ──────────────────────────────────────
const CATEGORIES = [
    { key: 'sejour' as const, label: '🛂 Séjour', color: 'bg-blue-500' },
    { key: 'naturalisation' as const, label: '🇫🇷 Naturalisation', color: 'bg-emerald-500' },
    { key: 'family' as const, label: '👨‍👩‍👧 Famille', color: 'bg-pink-500' },
    { key: 'permis' as const, label: '🚗 Permis', color: 'bg-amber-500' },
];

// ─── Types IA ──────────────────────────────────────────────────────
interface IAAlert {
    type: 'error' | 'warning' | 'info' | 'suggestion';
    title: string;
    description: string;
    ruleId?: string;
    category?: string;
}

export default function LegalMatrixModule() {
    const [selectedCategory, setSelectedCategory] = useState<string>('all');
    const [searchQuery, setSearchQuery] = useState('');
    const [hoveredRow, setHoveredRow] = useState<string | null>(null);
    const [hoveredCol, setHoveredCol] = useState<string | null>(null);
    const [showGraph, setShowGraph] = useState(false);
    const [selectedProcedureId, setSelectedProcedureId] = useState<string | null>(null);
    const [showAlerts, setShowAlerts] = useState(false);

    // ─── Charger TOUTES les catégories ─────────────────────────
    const allProcedures = useMemo(() => {
        const all: (ProcedureRule & { _category: string })[] = [];
        CATEGORIES.forEach(cat => {
            const rules = EligibilityStore.getRules(cat.key);
            rules.forEach(r => all.push({ ...r, _category: cat.key }));
        });
        return all;
    }, []);

    // ─── Extraction des variables ──────────────────────────────
    const extractVariables = (condition: RuleCondition): Set<string> => {
        const vars = new Set<string>();
        if (!condition) return vars;
        if (condition.var) vars.add(condition.var);
        if (condition.AND) condition.AND.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        if (condition.OR) condition.OR.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        return vars;
    };

    const extractConditionCount = (condition: RuleCondition): number => {
        if (!condition) return 0;
        let count = condition.var ? 1 : 0;
        if (condition.AND) condition.AND.forEach(c => count += extractConditionCount(c));
        if (condition.OR) condition.OR.forEach(c => count += extractConditionCount(c));
        return count;
    };

    const matrixMetadata = useMemo(() => {
        return allProcedures.map(proc => ({
            id: proc.id,
            name: proc.name,
            variables: extractVariables(proc.conditions),
            conditionCount: extractConditionCount(proc.conditions),
            category: proc._category,
            tier: proc.tier || 'STANDARD',
            hasDocs: (proc.documents?.length || 0) > 0,
            duration: proc.duration_years,
            worksRight: proc.gives_work_right,
        }));
    }, [allProcedures]);

    // ─── Filtrage ──────────────────────────────────────────────
    const filteredProcedures = useMemo(() => {
        return matrixMetadata.filter(p => {
            const matchesSearch = p.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
                p.id.toLowerCase().includes(searchQuery.toLowerCase());
            const matchesCat = selectedCategory === 'all' || p.category === selectedCategory;
            return matchesSearch && matchesCat;
        });
    }, [matrixMetadata, searchQuery, selectedCategory]);

    // ═══════════════════════════════════════════════════════════
    // MOTEUR D'ANALYSE IA — Détection d'anomalies et suggestions
    // ═══════════════════════════════════════════════════════════

    const iaAlerts = useMemo((): IAAlert[] => {
        const alerts: IAAlert[] = [];

        // 1. Règles sans conditions (potentiel oubli)
        matrixMetadata.forEach(proc => {
            if (proc.conditionCount === 0) {
                alerts.push({
                    type: 'error',
                    title: `Règle sans condition : ${proc.name}`,
                    description: `La procédure "${proc.id}" n'a aucune condition d'éligibilité. Tous les profils seront éligibles.`,
                    ruleId: proc.id,
                    category: proc.category,
                });
            }
        });

        // 2. Règles avec trop peu de conditions (< 2, potentiel trop permissif)
        matrixMetadata.forEach(proc => {
            if (proc.conditionCount > 0 && proc.conditionCount < 2 && proc.tier !== 'FALLBACK') {
                alerts.push({
                    type: 'warning',
                    title: `Règle trop permissive : ${proc.name}`,
                    description: `Seulement ${proc.conditionCount} condition(s). Envisager d'ajouter des critères de vérification.`,
                    ruleId: proc.id,
                    category: proc.category,
                });
            }
        });

        // 3. Variables orphelines (utilisées par 1 seule règle)
        const varUsage: Record<string, string[]> = {};
        matrixMetadata.forEach(m => {
            m.variables.forEach(v => {
                if (!varUsage[v]) varUsage[v] = [];
                varUsage[v].push(m.id);
            });
        });
        const orphans = Object.entries(varUsage).filter(([, rules]) => rules.length === 1);
        if (orphans.length > 0) {
            alerts.push({
                type: 'info',
                title: `${orphans.length} variable(s) utilisée(s) par une seule règle`,
                description: `Variables spécifiques : ${orphans.slice(0, 5).map(([v]) => v.split('.').pop()).join(', ')}${orphans.length > 5 ? '...' : ''}. Vérifiez si d'autres procédures devraient aussi les utiliser.`,
            });
        }

        // 4. Doublons potentiels (>80% de variables communes)
        const checked = new Set<string>();
        matrixMetadata.forEach(a => {
            matrixMetadata.forEach(b => {
                if (a.id >= b.id) return;
                const key = `${a.id}|${b.id}`;
                if (checked.has(key)) return;
                checked.add(key);
                const common = new Set([...a.variables].filter(x => b.variables.has(x)));
                const maxSize = Math.max(a.variables.size, b.variables.size, 1);
                const similarity = common.size / maxSize;
                if (similarity >= 0.8 && common.size >= 3) {
                    alerts.push({
                        type: 'warning',
                        title: `Doublons potentiels détectés`,
                        description: `"${a.name}" et "${b.name}" partagent ${Math.round(similarity * 100)}% de critères (${common.size} variables communes). Vérifiez qu'elles ne sont pas redondantes.`,
                        ruleId: a.id,
                    });
                }
            });
        });

        // 5. Procédures sans documents
        const noDocs = matrixMetadata.filter(m => !m.hasDocs);
        if (noDocs.length > 0) {
            alerts.push({
                type: 'suggestion',
                title: `${noDocs.length} procédure(s) sans pièces justificatives`,
                description: `Les procédures suivantes n'ont pas de documents liés : ${noDocs.slice(0, 3).map(m => m.name).join(', ')}${noDocs.length > 3 ? '...' : ''}. Ajoutez les documents requis via l'onglet Éligibilité.`,
            });
        }

        // 6. Couverture des critères clés
        const criticalVars = ['civic.clean_criminal_record', 'identity.nationality_group'];
        criticalVars.forEach(cv => {
            const using = matrixMetadata.filter(m => m.variables.has(cv));
            const total = matrixMetadata.length;
            const pct = total > 0 ? Math.round((using.length / total) * 100) : 0;
            if (pct < 50) {
                const label = CRITERIA.find(c => c.id === cv)?.label || cv;
                alerts.push({
                    type: 'suggestion',
                    title: `Critère "${label}" sous-utilisé (${pct}%)`,
                    description: `Seulement ${using.length}/${total} procédures vérifient ce critère. Pour la conformité CESEDA, il est recommandé de l'ajouter aux procédures manquantes.`,
                });
            }
        });

        // 7. Distribution par catégorie
        const catStats: Record<string, number> = {};
        matrixMetadata.forEach(m => { catStats[m.category] = (catStats[m.category] || 0) + 1; });
        CATEGORIES.forEach(cat => {
            if (!catStats[cat.key] || catStats[cat.key] === 0) {
                alerts.push({
                    type: 'error',
                    title: `Catégorie vide : ${cat.label}`,
                    description: `Aucune règle d'éligibilité trouvée pour la catégorie "${cat.key}". Vérifiez le fichier rules_${cat.key}.json.`,
                    category: cat.key,
                });
            }
        });

        return alerts.sort((a, b) => {
            const order: Record<string, number> = { error: 0, warning: 1, suggestion: 2, info: 3 };
            return (order[a.type] || 99) - (order[b.type] || 99);
        });
    }, [matrixMetadata]);

    // ─── Global Stats ─────────────────────────────────────────
    const globalStats = useMemo(() => {
        const stats = {
            totalRules: matrixMetadata.length,
            byCategory: {} as Record<string, number>,
            variableUsage: {} as Record<string, number>,
            avgConditions: 0,
            maxConditions: 0,
        };

        let totalCond = 0;
        matrixMetadata.forEach(m => {
            stats.byCategory[m.category] = (stats.byCategory[m.category] || 0) + 1;
            m.variables.forEach(v => { stats.variableUsage[v] = (stats.variableUsage[v] || 0) + 1; });
            totalCond += m.conditionCount;
            if (m.conditionCount > stats.maxConditions) stats.maxConditions = m.conditionCount;
        });
        stats.avgConditions = matrixMetadata.length > 0 ? Math.round(totalCond / matrixMetadata.length) : 0;

        const topVariables = Object.entries(stats.variableUsage)
            .sort(([, a], [, b]) => b - a)
            .slice(0, 6)
            .map(([id, count]) => {
                const criteria = CRITERIA.find(c => c.id === id);
                return { id, label: criteria?.label || id.split('.').pop() || id, count, color: criteria?.color || 'bg-slate-400' };
            });

        return { ...stats, topVariables };
    }, [matrixMetadata]);

    // ─── Similarities ─────────────────────────────────────────
    const similarities = useMemo(() => {
        if (!selectedProcedureId) return [];
        const currentProc = matrixMetadata.find(m => m.id === selectedProcedureId);
        if (!currentProc) return [];

        return matrixMetadata
            .filter(m => m.id !== selectedProcedureId)
            .map(m => {
                const common = new Set([...currentProc.variables].filter(x => m.variables.has(x)));
                const score = Math.round((common.size / Math.max(currentProc.variables.size, 1)) * 100);
                return { id: m.id, name: m.name, category: m.category, commonCount: common.size, commonVars: Array.from(common), score };
            })
            .filter(sim => sim.commonCount > 0)
            .sort((a, b) => b.score - a.score)
            .slice(0, 6);
    }, [selectedProcedureId, matrixMetadata]);

    // ─── Export CSV ────────────────────────────────────────────
    const exportToCSV = () => {
        const headers = ['ID', 'Nom', 'Catégorie', 'Conditions', 'Tier', ...CRITERIA.map(c => c.label)];
        const rows = matrixMetadata.map(m => [
            m.id, m.name, m.category, m.conditionCount, m.tier,
            ...CRITERIA.map(c => m.variables.has(c.id) ? '1' : '0')
        ]);
        const csvContent = [headers.join(';'), ...rows.map(r => r.join(';'))].join('\n');
        const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
        const link = document.createElement('a');
        link.href = URL.createObjectURL(blob);
        link.download = `matrice_ia_${new Date().toISOString().split('T')[0]}.csv`;
        link.click();
    };

    // ─── Alert colors ─────────────────────────────────────────
    const alertStyle = (type: string) => {
        switch (type) {
            case 'error': return { bg: 'bg-red-50', border: 'border-red-200', text: 'text-red-700', icon: <ShieldAlert size={16} /> };
            case 'warning': return { bg: 'bg-amber-50', border: 'border-amber-200', text: 'text-amber-700', icon: <AlertTriangle size={16} /> };
            case 'suggestion': return { bg: 'bg-blue-50', border: 'border-blue-200', text: 'text-blue-700', icon: <Lightbulb size={16} /> };
            case 'info': return { bg: 'bg-slate-50', border: 'border-slate-200', text: 'text-slate-600', icon: <Info size={16} /> };
            default: return { bg: 'bg-slate-50', border: 'border-slate-200', text: 'text-slate-600', icon: <Info size={16} /> };
        }
    };

    // ─── Severity Color for IA score ──────────────────────────
    const iaScore = useMemo(() => {
        const errors = iaAlerts.filter(a => a.type === 'error').length;
        const warnings = iaAlerts.filter(a => a.type === 'warning').length;
        const score = Math.max(0, 100 - errors * 20 - warnings * 5);
        return { score, errors, warnings, suggestions: iaAlerts.filter(a => a.type === 'suggestion').length };
    }, [iaAlerts]);

    return (
        <div className="flex flex-col h-full bg-slate-50 animate-in fade-in duration-500">
            {/* IA Alerts Panel (slide-over) */}
            {showAlerts && (
                <div className="fixed inset-0 bg-black/50 backdrop-blur-sm z-50 flex justify-end" onClick={() => setShowAlerts(false)}>
                    <div className="w-full max-w-lg bg-white h-full overflow-auto p-8 shadow-2xl animate-in slide-in-from-right duration-300"
                        onClick={e => e.stopPropagation()}>
                        <div className="flex items-center justify-between mb-6">
                            <div className="flex items-center gap-3">
                                <div className="w-10 h-10 bg-indigo-600 rounded-2xl flex items-center justify-center text-white">
                                    <BrainCircuit size={20} />
                                </div>
                                <div>
                                    <h3 className="text-lg font-black text-slate-900">Audit IA</h3>
                                    <p className="text-xs text-slate-400 font-bold">{iaAlerts.length} alertes detectees</p>
                                </div>
                            </div>
                            <button onClick={() => setShowAlerts(false)} className="w-8 h-8 rounded-xl bg-slate-100 flex items-center justify-center hover:bg-slate-200">
                                <X size={16} />
                            </button>
                        </div>

                        {/* IA Score */}
                        <div className={`p-6 rounded-3xl mb-6 ${iaScore.score >= 80 ? 'bg-emerald-50 border border-emerald-200' : iaScore.score >= 50 ? 'bg-amber-50 border border-amber-200' : 'bg-red-50 border border-red-200'}`}>
                            <div className="flex items-center justify-between">
                                <div>
                                    <p className="text-[10px] font-black uppercase tracking-widest text-slate-400">Score Qualite IA</p>
                                    <p className={`text-4xl font-black ${iaScore.score >= 80 ? 'text-emerald-600' : iaScore.score >= 50 ? 'text-amber-600' : 'text-red-600'}`}>
                                        {iaScore.score}%
                                    </p>
                                </div>
                                <div className="text-right space-y-1">
                                    {iaScore.errors > 0 && <p className="text-xs font-bold text-red-600">{iaScore.errors} erreur(s)</p>}
                                    {iaScore.warnings > 0 && <p className="text-xs font-bold text-amber-600">{iaScore.warnings} avertissement(s)</p>}
                                    {iaScore.suggestions > 0 && <p className="text-xs font-bold text-blue-600">{iaScore.suggestions} suggestion(s)</p>}
                                </div>
                            </div>
                        </div>

                        {/* Alert List */}
                        <div className="space-y-3">
                            {iaAlerts.map((alert, i) => {
                                const style = alertStyle(alert.type);
                                return (
                                    <div key={i} className={`p-4 rounded-2xl border ${style.bg} ${style.border}`}>
                                        <div className="flex items-start gap-3">
                                            <div className={`mt-0.5 ${style.text}`}>{style.icon}</div>
                                            <div>
                                                <p className={`text-sm font-black ${style.text}`}>{alert.title}</p>
                                                <p className="text-xs text-slate-500 mt-1 leading-relaxed">{alert.description}</p>
                                                {alert.ruleId && (
                                                    <button
                                                        onClick={() => { setSelectedProcedureId(alert.ruleId!); setShowAlerts(false); }}
                                                        className="mt-2 text-[10px] font-black uppercase text-indigo-600 hover:underline"
                                                    >
                                                        Voir la regle →
                                                    </button>
                                                )}
                                            </div>
                                        </div>
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                </div>
            )}

            {/* Graphe de dépendances (modal) */}
            {showGraph && selectedProcedureId && (
                <div className="fixed inset-0 bg-black/50 backdrop-blur-sm z-50 flex items-center justify-center p-8" onClick={() => setShowGraph(false)}>
                    <div className="bg-white rounded-3xl shadow-2xl w-full max-w-4xl max-h-[80vh] overflow-auto p-8" onClick={e => e.stopPropagation()}>
                        <div className="flex items-center justify-between mb-6">
                            <h3 className="text-lg font-black text-slate-900 flex items-center gap-2">
                                <Workflow className="text-indigo-600" size={20} />
                                Graphe de dependances
                            </h3>
                            <button onClick={() => setShowGraph(false)} className="w-8 h-8 rounded-xl bg-slate-100 flex items-center justify-center hover:bg-slate-200">
                                <X size={16} />
                            </button>
                        </div>

                        {/* SVG Graph */}
                        <div className="bg-slate-50 rounded-2xl p-6 border border-slate-200">
                            <svg viewBox="0 0 800 500" className="w-full h-[400px]">
                                {/* Central node */}
                                <circle cx="400" cy="250" r="45" fill="#4F46E5" />
                                <text x="400" y="246" textAnchor="middle" fill="white" fontSize="10" fontWeight="bold">
                                    {matrixMetadata.find(m => m.id === selectedProcedureId)?.name.substring(0, 15)}
                                </text>
                                <text x="400" y="260" textAnchor="middle" fill="white" fontSize="8" opacity="0.7">FOCUS</text>

                                {/* Sister procedures */}
                                {similarities.map((sim, i) => {
                                    const angle = (i * 360 / Math.max(similarities.length, 1)) * (Math.PI / 180);
                                    const radius = 160 + (i % 2) * 40;
                                    const x = 400 + Math.cos(angle) * radius;
                                    const y = 250 + Math.sin(angle) * radius;
                                    const lineOpacity = sim.score / 100;
                                    const nodeR = 20 + sim.score / 10;

                                    return (
                                        <g key={sim.id}>
                                            <line x1="400" y1="250" x2={x} y2={y} stroke="#4F46E5" strokeWidth={Math.max(1, sim.score / 25)} strokeDasharray={sim.score < 50 ? "4 2" : "none"} opacity={lineOpacity * 0.5} />
                                            <circle cx={x} cy={y} r={nodeR} fill={sim.score >= 70 ? '#059669' : sim.score >= 40 ? '#D97706' : '#94A3B8'} opacity="0.9" />
                                            <text x={x} y={y - 4} textAnchor="middle" fill="white" fontSize="8" fontWeight="bold">
                                                {sim.name.substring(0, 12)}
                                            </text>
                                            <text x={x} y={y + 8} textAnchor="middle" fill="white" fontSize="9" fontWeight="bold">
                                                {sim.score}%
                                            </text>
                                        </g>
                                    );
                                })}

                                {/* Criteria nodes around center */}
                                {Array.from(matrixMetadata.find(m => m.id === selectedProcedureId)?.variables || []).slice(0, 8).map((v, i) => {
                                    const angle = (i * 360 / 8 + 22.5) * (Math.PI / 180);
                                    const x = 400 + Math.cos(angle) * 80;
                                    const y = 250 + Math.sin(angle) * 80;
                                    const crit = CRITERIA.find(c => c.id === v);

                                    return (
                                        <g key={v}>
                                            <line x1="400" y1="250" x2={x} y2={y} stroke="#E2E8F0" strokeWidth="1" />
                                            <circle cx={x} cy={y} r="14" fill="#F1F5F9" stroke="#CBD5E1" strokeWidth="1" />
                                            <text x={x} y={y + 3} textAnchor="middle" fill="#475569" fontSize="7" fontWeight="bold">
                                                {crit?.label || v.split('.').pop()?.substring(0, 6)}
                                            </text>
                                        </g>
                                    );
                                })}
                            </svg>
                        </div>

                        <div className="mt-4 flex items-center gap-6 justify-center">
                            <div className="flex items-center gap-2"><div className="w-3 h-3 rounded-full bg-emerald-500" /><span className="text-xs font-bold text-slate-500">Similarite haute (≥70%)</span></div>
                            <div className="flex items-center gap-2"><div className="w-3 h-3 rounded-full bg-amber-500" /><span className="text-xs font-bold text-slate-500">Moyenne (40-69%)</span></div>
                            <div className="flex items-center gap-2"><div className="w-3 h-3 rounded-full bg-slate-400" /><span className="text-xs font-bold text-slate-500">Faible (&lt;40%)</span></div>
                        </div>
                    </div>
                </div>
            )}

            {/* Toolbar */}
            <div className="bg-white border-b border-slate-200 p-6 flex items-center justify-between gap-4 shadow-sm z-20">
                <div className="flex items-center gap-3 flex-1 max-w-3xl">
                    <div className="relative flex-1 group">
                        <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400 group-hover:text-indigo-600 transition-colors" size={20} />
                        <input
                            type="text"
                            placeholder="Rechercher une procedure..."
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="w-full pl-12 pr-6 py-3 bg-slate-100 rounded-2xl border-transparent focus:bg-white focus:border-indigo-500 focus:ring-4 focus:ring-indigo-100 transition-all font-bold text-sm"
                        />
                    </div>

                    {/* Category filter */}
                    <div className="flex bg-slate-100 p-1 rounded-2xl">
                        <button onClick={() => setSelectedCategory('all')}
                            className={`px-3 py-2 rounded-xl text-[10px] font-black uppercase tracking-wider transition-all ${selectedCategory === 'all' ? 'bg-white shadow-sm text-slate-900' : 'text-slate-400 hover:text-slate-600'}`}>
                            Toutes ({matrixMetadata.length})
                        </button>
                        {CATEGORIES.map(cat => {
                            const count = globalStats.byCategory[cat.key] || 0;
                            return (
                                <button key={cat.key} onClick={() => setSelectedCategory(cat.key)}
                                    className={`px-3 py-2 rounded-xl text-[10px] font-black uppercase tracking-wider transition-all ${selectedCategory === cat.key ? 'bg-white shadow-sm text-slate-900' : 'text-slate-400 hover:text-slate-600'}`}>
                                    {cat.label.split(' ')[0]} {count}
                                </button>
                            );
                        })}
                    </div>
                </div>

                <div className="flex items-center gap-2">
                    <button onClick={() => setShowAlerts(true)}
                        className={`relative px-4 py-3 rounded-2xl font-black flex items-center gap-2 text-xs transition-all active:scale-95 ${iaScore.errors > 0 ? 'bg-red-50 text-red-600 hover:bg-red-100' : iaScore.warnings > 0 ? 'bg-amber-50 text-amber-600 hover:bg-amber-100' : 'bg-emerald-50 text-emerald-600 hover:bg-emerald-100'}`}>
                        <BrainCircuit size={18} /> Audit IA
                        {iaAlerts.length > 0 && (
                            <span className={`absolute -top-1 -right-1 w-5 h-5 rounded-full text-white text-[10px] font-black flex items-center justify-center ${iaScore.errors > 0 ? 'bg-red-500' : 'bg-amber-500'}`}>
                                {iaAlerts.length}
                            </span>
                        )}
                    </button>
                    <button onClick={exportToCSV}
                        className="px-4 py-3 rounded-2xl font-black flex items-center gap-2 bg-slate-100 text-slate-600 hover:bg-slate-200 transition-all active:scale-95 text-xs">
                        <Download size={18} /> CSV
                    </button>
                    <button onClick={() => setShowGraph(true)} disabled={!selectedProcedureId}
                        className={`px-4 py-3 rounded-2xl font-black flex items-center gap-2 transition-all shadow-lg active:scale-95 text-xs ${selectedProcedureId ? 'bg-slate-900 text-white shadow-slate-200' : 'bg-slate-100 text-slate-300 cursor-not-allowed shadow-none'}`}>
                        <Workflow size={18} /> Graphe
                    </button>
                </div>
            </div>

            <div className="flex-1 flex overflow-hidden">
                {/* MATRICE */}
                <div className="flex-1 overflow-auto relative p-6">
                    <div className="bg-white rounded-3xl border border-slate-200 shadow-xl shadow-slate-200/50 overflow-hidden flex flex-col min-h-full">
                        <div className="overflow-auto flex-1">
                            <table className="w-full border-separate border-spacing-0">
                                <thead className="sticky top-0 z-40">
                                    <tr>
                                        <th className="p-6 text-left bg-slate-800 text-white border-b border-white/10 sticky left-0 z-50 rounded-tl-3xl min-w-[280px]">
                                            <div className="flex items-center gap-3">
                                                <Grid3X3 size={22} className="text-indigo-400" />
                                                <div>
                                                    <p className="text-[10px] font-black uppercase tracking-widest opacity-60">Analyse IA</p>
                                                    <h3 className="text-lg font-black uppercase tracking-tighter">{filteredProcedures.length} procedures</h3>
                                                </div>
                                            </div>
                                        </th>
                                        {CRITERIA.map(c => (
                                            <th key={c.id}
                                                onMouseEnter={() => setHoveredCol(c.id)}
                                                onMouseLeave={() => setHoveredCol(null)}
                                                className={`p-4 text-center border-b border-slate-100 transition-all min-w-[100px] ${hoveredCol === c.id ? 'bg-indigo-600 text-white' : 'bg-slate-50 text-slate-900'}`}>
                                                <div className="space-y-1">
                                                    <span className={`block w-2 h-2 rounded-full mx-auto ${c.color} ${hoveredCol === c.id ? 'ring-4 ring-white/30' : ''}`} />
                                                    <span className="block text-[10px] font-black uppercase tracking-tighter">{c.label}</span>
                                                </div>
                                            </th>
                                        ))}
                                    </tr>
                                </thead>
                                <tbody>
                                    {filteredProcedures.map(proc => (
                                        <tr key={proc.id}
                                            onMouseEnter={() => setHoveredRow(proc.id)}
                                            onMouseLeave={() => setHoveredRow(null)}
                                            onClick={() => setSelectedProcedureId(proc.id)}
                                            className={`group transition-all cursor-pointer ${selectedProcedureId === proc.id ? 'bg-indigo-50' : hoveredRow === proc.id ? 'bg-slate-50' : 'bg-white'}`}>
                                            <td className={`p-4 sticky left-0 z-30 border-r border-slate-50 transition-all ${selectedProcedureId === proc.id ? 'bg-indigo-100/50' : hoveredRow === proc.id ? 'bg-slate-100/80' : 'bg-white'}`}>
                                                <div className="flex items-center gap-3">
                                                    <span className={`text-sm ${CATEGORIES.find(c => c.key === proc.category)?.color || 'bg-slate-300'} w-1 h-6 rounded-full`} />
                                                    <div>
                                                        <p className={`font-black uppercase tracking-tighter text-xs ${selectedProcedureId === proc.id ? 'text-indigo-600' : 'text-slate-800'}`}>
                                                            {proc.name}
                                                        </p>
                                                        <div className="flex items-center gap-2 mt-0.5">
                                                            <span className="text-[9px] font-mono text-slate-400">{proc.id}</span>
                                                            <span className={`text-[8px] font-black px-1.5 py-0.5 rounded-md uppercase ${proc.tier === 'PREMIUM' ? 'bg-amber-100 text-amber-700' : proc.tier === 'FALLBACK' ? 'bg-slate-100 text-slate-400' : 'bg-slate-50 text-slate-400'}`}>
                                                                {proc.tier}
                                                            </span>
                                                            {proc.conditionCount === 0 && (
                                                                <span className="text-[8px] font-black px-1.5 py-0.5 rounded-md bg-red-100 text-red-600">⚠ 0 COND</span>
                                                            )}
                                                        </div>
                                                    </div>
                                                </div>
                                            </td>

                                            {CRITERIA.map(c => {
                                                const isActive = proc.variables.has(c.id);
                                                const isCrossed = hoveredRow === proc.id || hoveredCol === c.id;
                                                return (
                                                    <td key={c.id}
                                                        onMouseEnter={() => setHoveredCol(c.id)}
                                                        className={`p-3 transition-all text-center relative ${isCrossed ? (isActive ? 'bg-indigo-100/40' : 'bg-slate-50/50') : ''}`}>
                                                        <div className="flex justify-center items-center">
                                                            {isActive ? (
                                                                <div className={`w-8 h-8 rounded-xl flex items-center justify-center transition-all shadow-sm ${selectedProcedureId === proc.id ? 'bg-indigo-600 text-white' : isCrossed ? 'bg-indigo-500 text-white scale-110' : 'bg-indigo-50 text-indigo-400'}`}>
                                                                    <CheckCircle size={16} />
                                                                </div>
                                                            ) : (
                                                                <div className={`w-1.5 h-1.5 rounded-full transition-all ${isCrossed ? 'bg-slate-300 scale-150' : 'bg-slate-100 opacity-50'}`} />
                                                            )}
                                                        </div>
                                                    </td>
                                                );
                                            })}
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>

                {/* SIDEBAR D'INTELLIGENCE */}
                <div className="w-[420px] border-l border-slate-200 bg-white flex flex-col p-6 overflow-y-auto space-y-8 shadow-2xl z-30">
                    {!selectedProcedureId ? (
                        <div className="flex flex-col h-full space-y-6 animate-in fade-in slide-in-from-right-4 duration-500">
                            <div className="text-center space-y-3">
                                <div className="w-16 h-16 bg-slate-50 rounded-3xl flex items-center justify-center text-slate-300 shadow-inner mx-auto">
                                    <Activity size={32} />
                                </div>
                                <h4 className="text-lg font-black uppercase text-slate-900">Vue D'ensemble</h4>
                                <p className="text-xs font-bold text-slate-400">{matrixMetadata.length} procedures analysees</p>
                            </div>

                            {/* IA Score compact */}
                            <div className={`p-5 rounded-3xl border ${iaScore.score >= 80 ? 'bg-emerald-50 border-emerald-200' : iaScore.score >= 50 ? 'bg-amber-50 border-amber-200' : 'bg-red-50 border-red-200'}`}>
                                <div className="flex items-center justify-between">
                                    <div className="flex items-center gap-3">
                                        <BrainCircuit size={24} className={iaScore.score >= 80 ? 'text-emerald-600' : iaScore.score >= 50 ? 'text-amber-600' : 'text-red-600'} />
                                        <div>
                                            <p className="text-[10px] font-black uppercase text-slate-400">Score IA</p>
                                            <p className={`text-2xl font-black ${iaScore.score >= 80 ? 'text-emerald-600' : iaScore.score >= 50 ? 'text-amber-600' : 'text-red-600'}`}>{iaScore.score}%</p>
                                        </div>
                                    </div>
                                    <button onClick={() => setShowAlerts(true)} className="text-[10px] font-black text-indigo-600 uppercase hover:underline">
                                        {iaAlerts.length} alertes →
                                    </button>
                                </div>
                            </div>

                            {/* Top Variables */}
                            <div className="space-y-3">
                                <h5 className="text-[10px] font-black uppercase tracking-widest text-slate-400 border-b border-slate-100 pb-2">Top Criteres Utilises</h5>
                                {globalStats.topVariables.map((v, i) => (
                                    <div key={v.id} className="flex items-center justify-between">
                                        <div className="flex items-center gap-2">
                                            <div className={`w-7 h-7 rounded-lg flex items-center justify-center text-[10px] font-black text-white ${v.color}`}>#{i + 1}</div>
                                            <div>
                                                <p className="text-xs font-black text-slate-700 uppercase">{v.label}</p>
                                                <p className="text-[9px] text-slate-400 font-mono">{v.id}</p>
                                            </div>
                                        </div>
                                        <span className="text-sm font-black text-indigo-600 bg-indigo-50 px-2 py-1 rounded-lg">{v.count}</span>
                                    </div>
                                ))}
                            </div>

                            {/* Stats by Category */}
                            <div className="space-y-3">
                                <h5 className="text-[10px] font-black uppercase tracking-widest text-slate-400 border-b border-slate-100 pb-2">Par categorie</h5>
                                <div className="grid grid-cols-2 gap-2">
                                    {CATEGORIES.map(cat => (
                                        <div key={cat.key} className="bg-slate-50 p-3 rounded-2xl border border-slate-100">
                                            <p className="text-[10px] font-black uppercase text-slate-400">{cat.label}</p>
                                            <p className="text-xl font-black text-slate-900">{globalStats.byCategory[cat.key] || 0}</p>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            {/* Engine Stats */}
                            <div className="bg-slate-900 rounded-3xl p-5 text-white">
                                <div className="flex items-center gap-2 mb-3">
                                    <Cpu size={16} className="text-indigo-400" />
                                    <p className="text-[10px] font-black uppercase tracking-widest text-slate-400">Moteur d'analyse</p>
                                </div>
                                <div className="grid grid-cols-3 gap-3">
                                    <div><p className="text-lg font-black">{globalStats.totalRules}</p><p className="text-[9px] text-slate-400 font-bold">Regles</p></div>
                                    <div><p className="text-lg font-black">{globalStats.avgConditions}</p><p className="text-[9px] text-slate-400 font-bold">Moy. cond.</p></div>
                                    <div><p className="text-lg font-black">{Object.keys(globalStats.variableUsage).length}</p><p className="text-[9px] text-slate-400 font-bold">Variables</p></div>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="space-y-6 animate-in slide-in-from-right-8 duration-500">
                            {/* Header */}
                            <div className="space-y-3">
                                <div className="flex items-center justify-between">
                                    <div className="flex items-center gap-2">
                                        <Layers size={20} className="text-indigo-600" />
                                        <p className="text-[10px] font-black uppercase text-indigo-500 tracking-widest">Focus</p>
                                    </div>
                                    <button onClick={() => setSelectedProcedureId(null)} className="text-[10px] font-black text-slate-400 hover:text-slate-600">✕ Fermer</button>
                                </div>
                                <div className="p-5 bg-slate-900 text-white rounded-2xl">
                                    <h5 className="font-black text-base uppercase">{matrixMetadata.find(m => m.id === selectedProcedureId)?.name}</h5>
                                    <p className="text-[10px] font-mono text-indigo-300 mt-1">{selectedProcedureId}</p>
                                    <div className="flex gap-3 mt-3">
                                        <span className="text-[10px] font-black px-2 py-1 bg-white/10 rounded-lg">
                                            {matrixMetadata.find(m => m.id === selectedProcedureId)?.conditionCount} conditions
                                        </span>
                                        <span className="text-[10px] font-black px-2 py-1 bg-white/10 rounded-lg">
                                            {matrixMetadata.find(m => m.id === selectedProcedureId)?.variables.size} variables
                                        </span>
                                    </div>
                                </div>
                            </div>

                            {/* ADN */}
                            <div className="space-y-3">
                                <h6 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                                    <Activity size={12} /> ADN ({matrixMetadata.find(m => m.id === selectedProcedureId)?.variables.size} criteres)
                                </h6>
                                <div className="flex flex-wrap gap-1.5">
                                    {Array.from(matrixMetadata.find(m => m.id === selectedProcedureId)?.variables || []).map(v => (
                                        <span key={v} className="px-2 py-1 bg-indigo-50 text-indigo-600 rounded-lg text-[9px] font-black uppercase border border-indigo-100">
                                            {v.split('.').pop()}
                                        </span>
                                    ))}
                                </div>
                            </div>

                            {/* Procedures Soeurs */}
                            <div className="space-y-3">
                                <h6 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Procedures Soeurs</h6>
                                {similarities.length > 0 ? similarities.map(sim => (
                                    <div key={sim.id} className="bg-white p-4 rounded-2xl border border-slate-100 hover:border-indigo-400 hover:shadow-lg transition-all cursor-pointer"
                                        onClick={() => setSelectedProcedureId(sim.id)}>
                                        <div className="flex items-center justify-between mb-2">
                                            <div className="flex items-center gap-2">
                                                <div className={`w-8 h-8 rounded-xl flex items-center justify-center text-[10px] font-black text-white ${sim.score >= 70 ? 'bg-emerald-500' : sim.score >= 40 ? 'bg-amber-500' : 'bg-slate-400'}`}>
                                                    {sim.score}%
                                                </div>
                                                <span className={`text-[8px] font-black uppercase px-1.5 py-0.5 rounded ${CATEGORIES.find(c => c.key === sim.category)?.color.replace('bg-', 'bg-') || 'bg-slate-100'} text-white`}>
                                                    {sim.category}
                                                </span>
                                            </div>
                                            <span className="text-[10px] text-slate-400">{sim.commonCount} commun(s)</span>
                                        </div>
                                        <p className="font-black text-slate-900 text-xs uppercase">{sim.name}</p>
                                    </div>
                                )) : (
                                    <div className="p-6 text-center bg-slate-50 rounded-2xl border border-dashed border-slate-200">
                                        <p className="text-xs font-bold text-slate-400 italic">Aucune correlation trouvee.</p>
                                    </div>
                                )}
                            </div>

                            {/* IA Alerts for this rule */}
                            {(() => {
                                const ruleAlerts = iaAlerts.filter(a => a.ruleId === selectedProcedureId);
                                if (ruleAlerts.length === 0) return null;
                                return (
                                    <div className="space-y-2 border-t border-slate-100 pt-4">
                                        <h6 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                                            <AlertTriangle size={12} /> Alertes IA ({ruleAlerts.length})
                                        </h6>
                                        {ruleAlerts.map((alert, i) => {
                                            const style = alertStyle(alert.type);
                                            return (
                                                <div key={i} className={`p-3 rounded-xl border ${style.bg} ${style.border}`}>
                                                    <p className={`text-xs font-bold ${style.text}`}>{alert.title}</p>
                                                    <p className="text-[10px] text-slate-500 mt-1">{alert.description}</p>
                                                </div>
                                            );
                                        })}
                                    </div>
                                );
                            })()}

                            {/* Documents */}
                            {(() => {
                                const proc = allProcedures.find(p => p.id === selectedProcedureId);
                                if (proc?.documents && proc.documents.length > 0) {
                                    return (
                                        <div className="space-y-3 border-t border-slate-100 pt-4">
                                            <h6 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                                                <FileText size={12} /> Pieces Justificatives ({proc.documents.length})
                                            </h6>
                                            <div className="space-y-1.5">
                                                {proc.documents.map((docId) => {
                                                    const doc = DOC_CATALOG[docId];
                                                    return (
                                                        <div key={docId} className="flex items-start gap-2 p-2 bg-slate-50 rounded-xl border border-slate-100">
                                                            <div className={`mt-1 min-w-[16px] h-[16px] rounded-full flex items-center justify-center text-[8px] font-bold text-white ${doc?.category === 'IDENTITY' ? 'bg-blue-400' : doc?.category === 'FINANCIAL' ? 'bg-amber-400' : doc?.category === 'CIVIL' ? 'bg-pink-400' : 'bg-emerald-400'}`}>
                                                                {docId.slice(0, 1)}
                                                            </div>
                                                            <div>
                                                                <p className="text-[10px] font-bold text-slate-800">{doc?.label || docId}</p>
                                                                {doc?.description && <p className="text-[9px] text-slate-400">{doc.description}</p>}
                                                            </div>
                                                        </div>
                                                    );
                                                })}
                                            </div>
                                        </div>
                                    );
                                }
                                return null;
                            })()}
                        </div>
                    )}
                </div>
            </div>

            {/* Footer */}
            <div className="bg-white border-t border-slate-200 px-6 py-2 flex items-center justify-between z-40">
                <div className="flex items-center gap-4">
                    {CRITERIA.slice(0, 6).map(c => (
                        <div key={c.id} className="flex items-center gap-1.5">
                            <span className={`w-2 h-2 rounded-full ${c.color}`} />
                            <span className="text-[9px] font-black uppercase text-slate-400 tracking-tighter">{c.label}</span>
                        </div>
                    ))}
                    <span className="text-[9px] font-black text-slate-300">+{CRITERIA.length - 6}</span>
                </div>
                <div className="flex items-center gap-2">
                    <span className={`text-[10px] font-black uppercase tracking-widest ${iaScore.score >= 80 ? 'text-emerald-500' : iaScore.score >= 50 ? 'text-amber-500' : 'text-red-500'}`}>
                        IA Score: {iaScore.score}%
                    </span>
                    <span className="text-[10px] font-bold text-slate-300">|</span>
                    <span className="text-[10px] font-bold text-slate-400">{matrixMetadata.length} regles · {Object.keys(globalStats.variableUsage).length} variables</span>
                </div>
            </div>
        </div>
    );
}
