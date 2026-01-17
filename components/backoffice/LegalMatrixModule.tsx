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
    ChevronDown,
    Activity
} from 'lucide-react';
import EligibilityStore from '../../services/EligibilityStore';
import { RuleCondition } from '../../types';

const CRITERIA = [
    { id: 'identity.age', label: 'Âge', group: 'Général', color: 'bg-blue-500' },
    { id: 'identity.nationality_group', label: 'Nat.', group: 'Général', color: 'bg-indigo-500' },
    { id: 'timeline.years_continuous_residence', label: 'Résidence', group: 'Parcours', color: 'bg-emerald-500' },
    { id: 'work.annual_gross_salary', label: 'Salaire', group: 'Finances', color: 'bg-amber-500' },
    { id: 'family.marriage_duration_years', label: 'Mariage', group: 'Famille', color: 'bg-pink-500' },
    { id: 'integration.french_level', label: 'Français', group: 'Intégration', color: 'bg-purple-500' },
    { id: 'education.diploma_level', label: 'Diplôme', group: 'Études', color: 'bg-cyan-500' },
    { id: 'civic.clean_criminal_record', label: 'Sécurité', group: 'Sécurité', color: 'bg-red-500' }
];

export default function LegalMatrixModule() {
    const [procedures] = useState(EligibilityStore.getRules('sejour'));
    const [searchQuery, setSearchQuery] = useState('');
    const [hoveredRow, setHoveredRow] = useState<string | null>(null);
    const [hoveredCol, setHoveredCol] = useState<string | null>(null);
    const [showGraph, setShowGraph] = useState(false);
    const [selectedProcedureId, setSelectedProcedureId] = useState<string | null>(null);

    // Extraction des variables
    const extractVariables = (condition: RuleCondition): Set<string> => {
        const vars = new Set<string>();
        if (!condition) return vars;
        if (condition.var) vars.add(condition.var);
        if (condition.AND) condition.AND.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        if (condition.OR) condition.OR.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        return vars;
    };

    const matrixMetadata = useMemo(() => {
        return procedures.map(proc => ({
            id: proc.id,
            name: proc.name,
            variables: extractVariables(proc.conditions),
            category: proc.id.includes('resident') ? 'Résidence' :
                proc.id.includes('famille') ? 'Famille' :
                    proc.id.includes('talent') ? 'Travail' : 'Autres'
        }));
    }, [procedures]);

    const filteredProcedures = useMemo(() => {
        return matrixMetadata.filter(p =>
            p.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
            p.id.toLowerCase().includes(searchQuery.toLowerCase())
        );
    }, [matrixMetadata, searchQuery]);

    const similarities = useMemo(() => {
        if (!selectedProcedureId) return [];
        const currentProc = matrixMetadata.find(m => m.id === selectedProcedureId);
        if (!currentProc) return [];

        return matrixMetadata
            .filter(m => m.id !== selectedProcedureId)
            .map(m => {
                const common = new Set([...currentProc.variables].filter(x => m.variables.has(x)));
                const score = Math.round((common.size / Math.max(currentProc.variables.size, 1)) * 100);
                return {
                    id: m.id,
                    name: m.name,
                    commonCount: common.size,
                    commonVars: Array.from(common),
                    score
                };
            })
            .filter(sim => sim.commonCount > 0)
            .sort((a, b) => b.score - a.score)
            .slice(0, 5);
    }, [selectedProcedureId, matrixMetadata]);

    return (
        <div className="flex flex-col h-full bg-slate-50 animate-in fade-in duration-500">
            {/* Toolbar Supérieure */}
            <div className="bg-white border-b border-slate-200 p-6 flex items-center justify-between gap-6 shadow-sm z-20">
                <div className="flex items-center gap-4 flex-1 max-w-2xl">
                    <div className="relative flex-1 group">
                        <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400 group-hover:text-indigo-600 transition-colors" size={20} />
                        <input
                            type="text"
                            placeholder="Rechercher une procédure (ex: Nationalité, Talent...)"
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="w-full pl-12 pr-6 py-4 bg-slate-100 rounded-[1.5rem] border-transparent focus:bg-white focus:border-indigo-500 focus:ring-4 focus:ring-indigo-100 transition-all font-black"
                        />
                    </div>
                    <div className="flex items-center gap-2 px-6 py-4 bg-slate-100 rounded-[1.5rem] text-slate-500 font-bold border border-transparent">
                        <Filter size={18} />
                        <span>Tous les critères</span>
                        <ChevronDown size={16} />
                    </div>
                </div>

                <div className="flex items-center gap-3">
                    <div className="text-right hidden md:block">
                        <p className="text-[10px] font-black uppercase text-slate-400">Analyse Structurelle</p>
                        <p className="text-xs font-black text-indigo-600 leading-none">37 procédures chargées</p>
                    </div>
                    <button
                        onClick={() => setShowGraph(true)}
                        disabled={!selectedProcedureId}
                        className={`px-8 py-4 rounded-2xl font-black flex items-center gap-2 transition-all shadow-xl active:scale-95 ${selectedProcedureId
                                ? 'bg-slate-900 text-white shadow-slate-200'
                                : 'bg-slate-100 text-slate-300 cursor-not-allowed shadow-none'
                            }`}
                    >
                        <Workflow size={20} /> Graphe
                    </button>
                </div>
            </div>

            <div className="flex-1 flex overflow-hidden">
                {/* LA MATRICE UX */}
                <div className="flex-1 overflow-auto relative p-6">
                    <div className="bg-white rounded-[2.5rem] border border-slate-200 shadow-xl shadow-slate-200/50 overflow-hidden flex flex-col min-h-full">
                        <div className="overflow-auto flex-1 scrollbar-hide">
                            <table className="w-full border-separate border-spacing-0">
                                <thead className="sticky top-0 z-40">
                                    <tr>
                                        <th className="p-8 text-left bg-slate-800 text-white border-b border-white/10 sticky left-0 z-50 rounded-tl-3xl min-w-[320px]">
                                            <div className="flex items-center gap-3">
                                                <Grid3X3 size={24} className="text-indigo-400" />
                                                <div>
                                                    <p className="text-xs font-black uppercase tracking-widest opacity-60 leading-none">Intelligence</p>
                                                    <h3 className="text-xl font-black uppercase tracking-tighter leading-none mt-1">Matrice Procédures</h3>
                                                </div>
                                            </div>
                                        </th>
                                        {CRITERIA.map(c => (
                                            <th
                                                key={c.id}
                                                onMouseEnter={() => setHoveredCol(c.id)}
                                                onMouseLeave={() => setHoveredCol(null)}
                                                className={`p-6 text-center border-b border-slate-100 transition-all min-w-[120px] ${hoveredCol === c.id ? 'bg-indigo-600 text-white' : 'bg-slate-50 text-slate-900'
                                                    }`}
                                            >
                                                <div className="space-y-1">
                                                    <span className={`block w-2 h-2 rounded-full mx-auto ${c.color} ${hoveredCol === c.id ? 'ring-4 ring-white/30' : ''}`} />
                                                    <span className="block text-[11px] font-black uppercase tracking-tighter">{c.label}</span>
                                                    <span className={`block text-[9px] font-mono opacity-40 lowercase ${hoveredCol === c.id ? 'text-white' : ''}`}>
                                                        {c.id.split('.')[0]}
                                                    </span>
                                                </div>
                                            </th>
                                        ))}
                                    </tr>
                                </thead>
                                <tbody>
                                    {filteredProcedures.map((proc, idx) => (
                                        <tr
                                            key={proc.id}
                                            onMouseEnter={() => setHoveredRow(proc.id)}
                                            onMouseLeave={() => setHoveredRow(null)}
                                            onClick={() => setSelectedProcedureId(proc.id)}
                                            className={`group transition-all cursor-pointer ${selectedProcedureId === proc.id ? 'bg-indigo-50' :
                                                    hoveredRow === proc.id ? 'bg-slate-50' : 'bg-white'
                                                }`}
                                        >
                                            <td className={`p-6 sticky left-0 z-30 border-r border-slate-50 transition-all ${selectedProcedureId === proc.id ? 'bg-indigo-100/50' :
                                                    hoveredRow === proc.id ? 'bg-slate-100/80 shadow-r-xl' : 'bg-white'
                                                }`}>
                                                <div className="flex items-center gap-4">
                                                    <span className={`w-1 h-8 rounded-full transition-all ${proc.category === 'Résidence' ? 'bg-emerald-500' :
                                                            proc.category === 'Famille' ? 'bg-pink-500' :
                                                                proc.category === 'Travail' ? 'bg-indigo-500' : 'bg-slate-300'
                                                        } ${hoveredRow === proc.id ? 'scale-y-125' : 'scale-y-100'}`} />
                                                    <div>
                                                        <p className={`font-black uppercase tracking-tighter text-sm transition-all ${selectedProcedureId === proc.id ? 'text-indigo-600' : 'text-slate-800'
                                                            }`}>
                                                            {proc.name}
                                                        </p>
                                                        <p className="text-[10px] font-mono text-slate-400">{proc.id}</p>
                                                    </div>
                                                </div>
                                            </td>

                                            {CRITERIA.map(c => {
                                                const isActive = proc.variables.has(c.id);
                                                const isCrossed = hoveredRow === proc.id || hoveredCol === c.id;

                                                return (
                                                    <td
                                                        key={c.id}
                                                        onMouseEnter={() => setHoveredCol(c.id)}
                                                        className={`p-4 transition-all text-center relative ${isCrossed ? (isActive ? 'bg-indigo-100/40' : 'bg-slate-50/50') : ''
                                                            }`}
                                                    >
                                                        <div className="flex justify-center items-center">
                                                            {isActive ? (
                                                                <div className={`w-10 h-10 rounded-2xl flex items-center justify-center transition-all shadow-sm ${selectedProcedureId === proc.id ? 'bg-indigo-600 text-white rotate-0' :
                                                                        isCrossed ? 'bg-indigo-500 text-white scale-110 shadow-indigo-200' : 'bg-indigo-50 text-indigo-400 group-hover:scale-90 group-hover:opacity-50'
                                                                    }`}>
                                                                    <CheckCircle size={18} />
                                                                </div>
                                                            ) : (
                                                                <div className={`w-1.5 h-1.5 rounded-full transition-all ${isCrossed ? 'bg-slate-300 scale-150' : 'bg-slate-100 opacity-50'}`} />
                                                            )}
                                                        </div>
                                                        {isActive && selectedProcedureId === proc.id && (
                                                            <div className="absolute top-0 right-0 w-2 h-2 bg-emerald-400 rounded-full animate-ping" />
                                                        )}
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

                {/* SIDEBAR D'INTELLIGENCE AUGMENTÉE */}
                <div className="w-[450px] border-l border-slate-200 bg-white flex flex-col p-8 overflow-y-auto space-y-10 shadow-2xl z-30">
                    {!selectedProcedureId ? (
                        <div className="flex flex-col items-center justify-center h-full text-center space-y-6">
                            <div className="w-24 h-24 bg-slate-50 rounded-[2.5rem] flex items-center justify-center text-slate-300 shadow-inner">
                                <Activity size={48} />
                            </div>
                            <div className="space-y-2">
                                <h4 className="text-xl font-black uppercase text-slate-900">Analyseur de Toile</h4>
                                <p className="text-sm font-medium text-slate-400 leading-relaxed max-w-[250px] mx-auto">
                                    Cliquez sur une procédure pour décoder son ADN juridique et voir ses corrélations.
                                </p>
                            </div>
                        </div>
                    ) : (
                        <div className="space-y-10 animate-in slide-in-from-right-8 duration-500">
                            {/* Header Focus */}
                            <div className="space-y-4">
                                <div className="flex items-center gap-3">
                                    <div className="w-12 h-12 bg-indigo-600 text-white rounded-2xl flex items-center justify-center shadow-xl shadow-indigo-100">
                                        <Layers size={24} />
                                    </div>
                                    <div>
                                        <p className="text-[10px] font-black uppercase text-indigo-500 tracking-widest">Focus Actif</p>
                                        <h4 className="text-xl font-black text-slate-900 leading-none">Corrélation</h4>
                                    </div>
                                </div>
                                <div className="p-6 bg-slate-900 text-white rounded-[2rem] shadow-2xl relative overflow-hidden group">
                                    <div className="absolute top-0 right-0 w-32 h-32 bg-indigo-500 rounded-full blur-[60px] opacity-20 -mr-16 -mt-16 group-hover:opacity-40 transition-opacity"></div>
                                    <h5 className="font-black text-lg leading-tight uppercase relative z-10">{matrixMetadata.find(m => m.id === selectedProcedureId)?.name}</h5>
                                    <p className="text-[10px] font-mono text-indigo-300 mt-2 relative z-10">{selectedProcedureId}</p>
                                </div>
                            </div>

                            {/* Résumé ADN */}
                            <div className="space-y-4">
                                <h6 className="text-xs font-black uppercase tracking-widest text-slate-400 px-2 flex items-center gap-2">
                                    <Activity size={14} /> ADN Structurel ({matrixMetadata.find(m => m.id === selectedProcedureId)?.variables.size} critères)
                                </h6>
                                <div className="flex flex-wrap gap-2">
                                    {Array.from(matrixMetadata.find(m => m.id === selectedProcedureId)?.variables || []).map(v => (
                                        <span key={v} className="px-3 py-1.5 bg-indigo-50 text-indigo-600 rounded-xl text-[10px] font-black uppercase tracking-tighter border border-indigo-100">
                                            {v.split('.').pop()}
                                        </span>
                                    ))}
                                </div>
                            </div>

                            {/* Procédures Sœurs avec Score */}
                            <div className="space-y-4">
                                <h6 className="text-xs font-black uppercase tracking-widest text-slate-400 px-2">Procédures Sœurs Détectées</h6>
                                <div className="space-y-4">
                                    {similarities.length > 0 ? similarities.map(sim => (
                                        <div key={sim.id} className="bg-white p-6 rounded-[2.5rem] border border-slate-100 hover:border-indigo-400 hover:shadow-2xl hover:shadow-indigo-500/10 transition-all group">
                                            <div className="flex items-center justify-between mb-4">
                                                <div className="flex items-center gap-2">
                                                    <div className="w-10 h-10 bg-slate-50 text-slate-900 rounded-2xl flex items-center justify-center font-black text-xs group-hover:bg-indigo-600 group-hover:text-white transition-all shadow-inner">
                                                        {sim.score}%
                                                    </div>
                                                    <span className="text-[10px] font-black uppercase text-slate-400 group-hover:text-indigo-600">Similitude</span>
                                                </div>
                                                <div className="flex items-center gap-1">
                                                    {[1, 2, 3, 4, 5].map(i => (
                                                        <div key={i} className={`w-1.5 h-3 rounded-full ${i <= (sim.score / 20) ? 'bg-emerald-400' : 'bg-slate-100'}`} />
                                                    ))}
                                                </div>
                                            </div>
                                            <p className="font-black text-slate-900 text-sm leading-snug mb-4 group-hover:text-indigo-600 transition-colors uppercase tracking-tight">{sim.name}</p>
                                            <div className="flex items-center justify-between">
                                                <div className="flex -space-x-2">
                                                    {sim.commonVars.slice(0, 3).map(v => {
                                                        const crit = CRITERIA.find(c => c.id === v);
                                                        return (
                                                            <div key={v} className={`w-6 h-6 rounded-lg border-2 border-white ${crit?.color || 'bg-slate-400'}`} title={v} />
                                                        );
                                                    })}
                                                    {sim.commonVars.length > 3 && (
                                                        <div className="w-6 h-6 rounded-lg bg-slate-200 border-2 border-white flex items-center justify-center text-[8px] font-black">+{sim.commonVars.length - 3}</div>
                                                    )}
                                                </div>
                                                <button className="text-[10px] font-black uppercase text-indigo-600 hover:underline">Comparer →</button>
                                            </div>
                                        </div>
                                    )) : (
                                        <div className="p-10 text-center bg-slate-50 rounded-3xl border border-dashed border-slate-200">
                                            <p className="text-xs font-bold text-slate-400 italic">Aucune corrélation trouvée.</p>
                                        </div>
                                    )}
                                </div>
                            </div>

                            {/* IA Alerts */}
                            <div className="bg-amber-50 rounded-[2.5rem] p-8 border border-amber-100 space-y-4 shadow-sm relative overflow-hidden group">
                                <div className="absolute top-0 right-0 w-24 h-24 bg-amber-500/10 rounded-full -mr-12 -mt-12 group-hover:scale-150 transition-transform"></div>
                                <div className="flex items-center gap-2 relative z-10">
                                    <div className="w-8 h-8 bg-amber-500 rounded-xl flex items-center justify-center text-white">
                                        <Zap size={18} />
                                    </div>
                                    <p className="text-xs font-black text-amber-700 uppercase tracking-widest">Audit IA</p>
                                </div>
                                <p className="text-xs font-medium text-amber-900 leading-relaxed italic relative z-10">
                                    "Attention : La procédure sélectionnée partage {matrixMetadata.find(m => m.id === selectedProcedureId)?.variables.size} critères avec le tronc commun CESEDA. Toute modification ici impactera la cohérence globale du moteur."
                                </p>
                            </div>
                        </div>
                    )}
                </div>
            </div>

            {/* Légende Bas de Page */}
            <div className="bg-white border-t border-slate-200 px-8 py-3 flex items-center justify-between z-40">
                <div className="flex items-center gap-6">
                    {CRITERIA.slice(0, 5).map(c => (
                        <div key={c.id} className="flex items-center gap-2">
                            <span className={`w-2 h-2 rounded-full ${c.color}`} />
                            <span className="text-[10px] font-black uppercase text-slate-400 tracking-tighter">{c.label}</span>
                        </div>
                    ))}
                    <span className="text-[10px] font-black text-slate-300">... +{CRITERIA.length - 5} autres</span>
                </div>
                <div className="flex items-center gap-3">
                    <span className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Mode Visualisation :</span>
                    <div className="flex bg-slate-100 p-1 rounded-xl">
                        <button className="px-3 py-1 bg-white rounded-lg text-[10px] font-black shadow-sm">GRILLE UX</button>
                        <button className="px-3 py-1 text-[10px] font-black text-slate-400">DETAIL</button>
                    </div>
                </div>
            </div>
        </div>
    );
}
