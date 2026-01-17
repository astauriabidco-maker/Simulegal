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
    Share2
} from 'lucide-react';
import EligibilityStore from '../../services/EligibilityStore';
import { RuleCondition } from '../../types';

const CRITERIA = [
    { id: 'identity.age', label: 'Âge', group: 'Général' },
    { id: 'identity.nationality_group', label: 'Nationalité', group: 'Général' },
    { id: 'timeline.years_continuous_residence', label: 'Résidence', group: 'Parcours' },
    { id: 'work.annual_gross_salary', label: 'Salaire', group: 'Finances' },
    { id: 'family.marriage_duration_years', label: 'Mariage', group: 'Famille' },
    { id: 'integration.french_level', label: 'Français', group: 'Intégration' },
    { id: 'education.diploma_level', label: 'Diplôme', group: 'Études' },
    { id: 'civic.clean_criminal_record', label: 'Casier', group: 'Sécurité' }
];

interface MatrixLink {
    procedureId: string;
    criterionId: string;
    isLinked: boolean;
    isSuggested?: boolean;
    reason?: string;
}

export default function LegalMatrixModule() {
    const [procedures] = useState(EligibilityStore.getRules('sejour'));
    const [showAll, setShowAll] = useState(false);
    const [showGraph, setShowGraph] = useState(false);
    const [suggestions, setSuggestions] = useState<MatrixLink[]>([
        {
            procedureId: 'passeport_talent_salarie_qualifie',
            criterionId: 'work.annual_gross_salary',
            isLinked: true,
            isSuggested: true,
            reason: "Le nouveau décret 2026 renforce le lien entre éligibilité et salaire brut annuel."
        },
        {
            procedureId: 'carte_resident_longue_duree_ue',
            criterionId: 'integration.french_level',
            isLinked: true,
            isSuggested: true,
            reason: "L'IA détecte une possible exigence de niveau B1 non encore encodée dans vos règles JSON."
        }
    ]);

    const [acceptedSuggestions, setAcceptedSuggestions] = useState<string[]>([]);
    const [selectedProcedureId, setSelectedProcedureId] = useState<string | null>(null);

    // Fonction récursive pour extraire toutes les variables d'une condition
    const extractVariables = (condition: RuleCondition): Set<string> => {
        const vars = new Set<string>();
        if (!condition) return vars;
        if (condition.var) vars.add(condition.var);
        if (condition.AND) condition.AND.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        if (condition.OR) condition.OR.forEach(c => extractVariables(c).forEach(v => vars.add(v)));
        return vars;
    };

    // Calcul de la matrice réelle
    const matrixMetadata = useMemo(() => {
        return procedures.map(proc => ({
            id: proc.id,
            name: proc.name,
            variables: extractVariables(proc.conditions)
        }));
    }, [procedures]);

    const displayProcedures = showAll ? matrixMetadata : matrixMetadata.slice(0, 12);

    // Calcul des similarités
    const similarities = useMemo(() => {
        if (!selectedProcedureId) return [];
        const currentProc = matrixMetadata.find(m => m.id === selectedProcedureId);
        if (!currentProc) return [];

        return matrixMetadata
            .filter(m => m.id !== selectedProcedureId)
            .map(m => {
                const common = new Set([...currentProc.variables].filter(x => m.variables.has(x)));
                return {
                    id: m.id,
                    name: m.name,
                    commonCount: common.size,
                    commonVars: Array.from(common)
                };
            })
            .filter(sim => sim.commonCount > 0)
            .sort((a, b) => b.commonCount - a.commonCount)
            .slice(0, 5);
    }, [selectedProcedureId, matrixMetadata]);

    const handleAccept = (suggestion: MatrixLink) => {
        setAcceptedSuggestions([...acceptedSuggestions, `${suggestion.procedureId}-${suggestion.criterionId}`]);
    };

    const renderGraphOverlay = () => (
        <div className="fixed inset-0 z-[100] bg-slate-900/90 backdrop-blur-xl flex items-center justify-center p-8 animate-in fade-in duration-300">
            <div className="bg-white w-full max-w-6xl h-full max-h-[800px] rounded-[3rem] shadow-2xl relative overflow-hidden flex flex-col">
                <button
                    onClick={() => setShowGraph(false)}
                    className="absolute top-8 right-8 w-12 h-12 bg-slate-100 rounded-full flex items-center justify-center text-slate-500 hover:bg-red-100 hover:text-red-500 transition-all z-20"
                >
                    <X size={24} />
                </button>

                <div className="p-10 border-b border-slate-100">
                    <h3 className="text-3xl font-black text-slate-900 flex items-center gap-4 uppercase tracking-tighter">
                        <Workflow className="text-indigo-600" size={32} /> Graphe de Dépendance Juridique
                    </h3>
                    <p className="text-slate-500 font-medium mt-2">Représentation visuelle des interconnexions entre les procédures via leurs critères communs (Vue IA).</p>
                </div>

                <div className="flex-1 relative bg-slate-50 p-10 overflow-hidden">
                    {/* Simulation de Graphe avec SVG et Divs */}
                    <div className="absolute inset-0 flex items-center justify-center">
                        <svg className="absolute inset-0 w-full h-full opacity-20">
                            <defs>
                                <linearGradient id="line-grad" x1="0%" y1="0%" x2="100%" y2="100%">
                                    <stop offset="0%" stopColor="#4f46e5" />
                                    <stop offset="100%" stopColor="#10b981" />
                                </linearGradient>
                            </defs>
                            <circle cx="50%" cy="50%" r="300" fill="none" stroke="url(#line-grad)" strokeWidth="1" strokeDasharray="10 10" />
                            <path d="M 50% 50% L 30% 30%" stroke="#4f46e5" strokeWidth="2" strokeDasharray="5 5" className="animate-pulse" />
                            <path d="M 50% 50% L 70% 30%" stroke="#4f46e5" strokeWidth="2" strokeDasharray="5 5" className="animate-pulse" />
                            <path d="M 50% 50% L 50% 80%" stroke="#4f46e5" strokeWidth="2" strokeDasharray="5 5" className="animate-pulse" />
                        </svg>

                        <div className="relative z-10 grid grid-cols-3 gap-32">
                            <div className="col-start-2 place-self-center">
                                <div className="p-6 bg-indigo-600 text-white rounded-[2rem] shadow-2xl shadow-indigo-200 border-4 border-white scale-110">
                                    <p className="text-[10px] font-black uppercase tracking-widest opacity-60">Focus</p>
                                    <p className="font-black text-center max-w-[150px] leading-tight">
                                        {selectedProcedureId ? matrixMetadata.find(m => m.id === selectedProcedureId)?.name : 'Sélectionnez une procédure'}
                                    </p>
                                </div>
                            </div>

                            {similarities.map((sim, idx) => {
                                const angles = [210, 330, 90, 45, 135];
                                const angle = angles[idx % angles.length];
                                const rad = (angle * Math.PI) / 180;
                                const x = Math.cos(rad) * 250;
                                const y = Math.sin(rad) * 250;

                                return (
                                    <div
                                        key={sim.id}
                                        style={{ transform: `translate(${x}px, ${y}px)` }}
                                        className="absolute p-4 bg-white border-2 border-slate-100 rounded-2xl shadow-lg hover:border-indigo-500 transition-all cursor-pointer group"
                                    >
                                        <div className="flex items-center gap-2 mb-1">
                                            <span className="w-5 h-5 bg-indigo-100 text-indigo-600 rounded-lg flex items-center justify-center text-[10px] font-black">
                                                {sim.commonCount}
                                            </span>
                                            <span className="text-[8px] font-black uppercase tracking-widest text-slate-400">Liens</span>
                                        </div>
                                        <p className="font-black text-slate-800 text-[10px] max-w-[120px] leading-tight uppercase group-hover:text-indigo-600">{sim.name}</p>
                                    </div>
                                );
                            })}
                        </div>
                    </div>

                    {/* Légende Graphe */}
                    <div className="absolute bottom-10 left-10 bg-white/80 backdrop-blur-md p-6 rounded-3xl border border-white shadow-xl space-y-3">
                        <div className="flex items-center gap-3">
                            <div className="w-3 h-3 bg-indigo-600 rounded-full" />
                            <span className="text-xs font-black text-slate-700 uppercase tracking-tighter">Procédures Sœurs</span>
                        </div>
                        <div className="flex items-center gap-3">
                            <div className="w-3 h-3 border-2 border-indigo-200 border-dashed rounded-full" />
                            <span className="text-xs font-black text-slate-700 uppercase tracking-tighter">Corrélation Critère</span>
                        </div>
                    </div>
                </div>

                <div className="p-8 bg-slate-900 text-white flex justify-between items-center">
                    <div className="flex items-center gap-4">
                        <Maximize2 size={24} className="text-indigo-400" />
                        <p className="text-sm font-medium">L'affichage montre les dépendances structurelles entre les règles du simulateur.</p>
                    </div>
                    <button className="px-6 py-3 bg-indigo-600 rounded-xl font-black flex items-center gap-2 hover:bg-indigo-700 transition-all">
                        <Share2 size={18} /> Exporter le Graphe
                    </button>
                </div>
            </div>
        </div>
    );

    return (
        <div className="h-full flex flex-col bg-slate-50 overflow-hidden relative">
            {showGraph && renderGraphOverlay()}

            {/* Header IA Pro */}
            <div className="p-6">
                <div className="bg-indigo-900 rounded-[2.5rem] p-8 text-white flex flex-col md:flex-row items-center gap-8 relative overflow-hidden shadow-2xl shadow-indigo-500/10">
                    <div className="absolute top-0 right-0 w-96 h-96 bg-indigo-500 rounded-full blur-[120px] opacity-20 -mr-48 -mt-48"></div>

                    <div className="w-20 h-20 bg-white/10 rounded-3xl flex items-center justify-center backdrop-blur-xl border border-white/20">
                        <BrainCircuit size={40} className="text-indigo-300" />
                    </div>

                    <div className="flex-1 space-y-2 text-center md:text-left">
                        <div className="flex items-center gap-2 justify-center md:justify-start">
                            <span className="px-2 py-0.5 bg-indigo-500 text-[10px] font-black uppercase tracking-widest rounded-md">Smart Auditor Engine</span>
                            <span className="flex items-center gap-1 text-[10px] text-emerald-400 font-bold uppercase tracking-widest">
                                <Zap size={10} /> Analyse structurelle active
                            </span>
                        </div>
                        <h2 className="text-3xl font-black tracking-tight uppercase">Matrice de Corrélation Avancée</h2>
                        <p className="text-indigo-200 font-medium max-w-2xl">
                            Visualisez les connexions entre critères et règles, et découvrez comment les procédures sont liées entre elles par des conditions communes.
                        </p>
                    </div>
                </div>
            </div>

            <div className="flex-1 overflow-auto px-6 pb-6">
                <div className="grid grid-cols-1 xl:grid-cols-4 gap-8">
                    {/* MATRICE PRINCIPALE */}
                    <div className="xl:col-span-3 space-y-4">
                        <div className="flex items-center justify-between">
                            <h3 className="text-xl font-black text-slate-900 flex items-center gap-2 uppercase tracking-tighter">
                                <Grid3X3 size={20} className="text-indigo-600" /> Cartographie des Liens
                            </h3>
                            <p className="text-xs font-bold text-slate-400 italic">Cliquez sur une procédure pour analyser ses corrélations</p>
                        </div>

                        <div className="bg-white rounded-[2rem] border border-slate-200 shadow-sm overflow-hidden flex flex-col h-[600px]">
                            <div className="flex-1 overflow-auto">
                                <table className="w-full border-collapse">
                                    <thead>
                                        <tr className="bg-slate-50/80 sticky top-0 z-30 shadow-sm">
                                            <th className="p-6 text-left text-xs font-black text-slate-400 uppercase tracking-widest border-b border-slate-100 sticky left-0 bg-slate-50 z-40">Procédures</th>
                                            {CRITERIA.map(c => (
                                                <th key={c.id} className="p-4 text-center text-[10px] font-black text-slate-900 uppercase tracking-tighter border-b border-slate-100 min-w-[100px] bg-slate-50">
                                                    {c.label}
                                                    <p className="text-[8px] text-slate-400 lowercase font-mono opacity-50">{c.id.split('.')[0]}</p>
                                                </th>
                                            ))}
                                        </tr>
                                    </thead>
                                    <tbody className="divide-y divide-slate-50">
                                        {displayProcedures.map(proc => (
                                            <tr
                                                key={proc.id}
                                                onClick={() => setSelectedProcedureId(proc.id)}
                                                className={`hover:bg-indigo-50/30 transition-all cursor-pointer group ${selectedProcedureId === proc.id ? 'bg-indigo-50/50' : ''}`}
                                            >
                                                <td className="p-6 sticky left-0 bg-white group-hover:bg-indigo-50/30 transition-colors z-20 border-r border-slate-50">
                                                    <p className="font-black text-slate-800 text-sm tracking-tight">{proc.name}</p>
                                                    <p className="text-[10px] font-mono text-slate-400">{proc.id}</p>
                                                </td>
                                                {CRITERIA.map(c => {
                                                    const isLinked = proc.variables.has(c.id);
                                                    const isSuggested = suggestions.find(s => s.procedureId === proc.id && s.criterionId === c.id);
                                                    const isAccepted = acceptedSuggestions.includes(`${proc.id}-${c.id}`);

                                                    return (
                                                        <td key={c.id} className="p-4 text-center">
                                                            <div className="flex justify-center">
                                                                {isSuggested && !isAccepted ? (
                                                                    <div className="w-6 h-6 rounded-full bg-amber-100 flex items-center justify-center text-amber-600 animate-pulse cursor-help group relative shadow-inner">
                                                                        <Zap size={14} />
                                                                        <div className="absolute bottom-full mb-3 left-1/2 -translate-x-1/2 w-64 p-4 bg-slate-900 text-white text-[10px] font-bold rounded-2xl opacity-0 group-hover:opacity-100 transition-all pointer-events-none z-50 shadow-2xl scale-95 group-hover:scale-100">
                                                                            <p className="text-amber-400 mb-1 flex items-center gap-1"><Cpu size={12} /> Suggestion IA</p>
                                                                            {isSuggested.reason}
                                                                        </div>
                                                                    </div>
                                                                ) : isLinked || isAccepted ? (
                                                                    <div className={`w-8 h-8 rounded-xl flex items-center justify-center transition-all ${isAccepted ? 'bg-emerald-500 text-white animate-bounce-short' : 'bg-indigo-100 text-indigo-600 rotate-0 group-hover:rotate-12 group-hover:scale-110 shadow-sm'}`}>
                                                                        <CheckCircle size={16} />
                                                                    </div>
                                                                ) : (
                                                                    <div className="w-1.5 h-1.5 rounded-full bg-slate-100 group-hover:bg-slate-200 transition-colors" />
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
                            <div className="p-6 bg-slate-50 border-t border-slate-100 flex justify-between items-center px-10">
                                <span className="text-xs font-bold text-slate-400 uppercase tracking-widest">{procedures.length} procédures analysées</span>
                                <button
                                    onClick={() => setShowAll(!showAll)}
                                    className="px-6 py-2 bg-white border border-slate-200 rounded-xl text-xs font-black text-indigo-600 hover:bg-slate-50 transition-colors uppercase tracking-widest shadow-sm"
                                >
                                    {showAll ? 'Réduire la liste' : 'Afficher l\'intégralité (37)'}
                                </button>
                            </div>
                        </div>
                    </div>

                    {/* PANNEAU DE CORRÉLATION TRANSVERSALE */}
                    <div className="space-y-6">
                        <h3 className="text-xl font-black text-slate-900 flex items-center gap-2 uppercase tracking-tighter">
                            <Layers size={20} className="text-indigo-600" /> Corrélations
                        </h3>

                        {!selectedProcedureId ? (
                            <div className="bg-slate-100 border-2 border-dashed border-slate-200 rounded-[2.5rem] p-12 text-center space-y-4">
                                <div className="w-16 h-16 bg-white rounded-full flex items-center justify-center mx-auto text-slate-300 shadow-sm">
                                    <Link2 size={32} />
                                </div>
                                <p className="text-xs font-bold text-slate-400 uppercase leading-relaxed max-w-[200px] mx-auto">
                                    Sélectionnez une procédure pour voir son impact réseau
                                </p>
                            </div>
                        ) : (
                            <div className="space-y-4 animate-in fade-in slide-in-from-right-4 duration-300">
                                <div className="bg-white p-6 rounded-[2rem] border-2 border-indigo-100 shadow-sm group">
                                    <p className="text-[10px] font-black text-indigo-400 uppercase tracking-widest mb-1">Focus Procédure</p>
                                    <h4 className="font-black text-slate-900 leading-tight text-lg">{matrixMetadata.find(m => m.id === selectedProcedureId)?.name}</h4>
                                </div>

                                <div className="space-y-3">
                                    <p className="text-xs font-black text-slate-400 uppercase tracking-widest px-2">Analyse de similitude structurelle :</p>
                                    {similarities.length > 0 ? similarities.map(sim => (
                                        <div key={sim.id} className="bg-white p-5 rounded-[2rem] border border-slate-200 group hover:border-indigo-400 hover:shadow-xl hover:shadow-indigo-500/5 transition-all">
                                            <div className="flex items-center justify-between mb-3">
                                                <div className="flex items-center gap-2">
                                                    <div className="w-8 h-8 bg-slate-50 text-slate-400 rounded-xl flex items-center justify-center font-black text-xs group-hover:bg-indigo-600 group-hover:text-white transition-all">
                                                        {sim.commonCount}
                                                    </div>
                                                    <span className="text-[10px] font-black uppercase text-indigo-600 tracking-tighter">Critères en commun</span>
                                                </div>
                                                <Workflow size={14} className="text-slate-200" />
                                            </div>
                                            <p className="font-bold text-slate-800 text-sm leading-snug mb-3 group-hover:text-indigo-600 transition-colors">{sim.name}</p>
                                            <div className="flex flex-wrap gap-1">
                                                {sim.commonVars.map(v => (
                                                    <span key={v} className="px-2 py-0.5 bg-slate-100 text-[8px] font-bold text-slate-500 rounded-md">
                                                        {v.split('.').pop()}
                                                    </span>
                                                ))}
                                            </div>
                                        </div>
                                    )) : (
                                        <div className="p-6 text-center text-[10px] font-bold text-slate-400 italic">
                                            Aucune similitude forte détectée.
                                        </div>
                                    )}
                                </div>

                                <button
                                    onClick={() => setShowGraph(true)}
                                    className="w-full py-5 bg-slate-900 text-white rounded-[1.5rem] font-black shadow-xl flex items-center justify-center gap-2 hover:bg-slate-800 transition-all active:scale-95 shadow-slate-200"
                                >
                                    <Workflow size={18} /> Voir le Graphe
                                </button>
                            </div>
                        )}

                        {/* SUGGESTIONS RAPIDES */}
                        <div className="bg-amber-50 rounded-[2.5rem] p-6 border border-amber-100 space-y-4 shadow-sm hover:translate-y-[-4px] transition-transform">
                            <div className="flex items-center gap-2">
                                <Zap size={18} className="text-amber-500" />
                                <p className="text-[10px] font-black text-amber-600 uppercase tracking-widest">IA Insight</p>
                            </div>
                            <p className="text-xs font-medium text-amber-900 leading-relaxed italic">
                                "La matrice révèle une corrélation de 85% entre vos procédures Talent et Startup. Pensez à unifier les critères de ressources."
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}
