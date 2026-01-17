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
    Link2
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

    // Calcul des similarités entre procédures (liens "entre des procédures")
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
            .sort((a, b) => b.commonCount - a.commonCount)
            .slice(0, 3);
    }, [selectedProcedureId, matrixMetadata]);

    const handleAccept = (suggestion: MatrixLink) => {
        setAcceptedSuggestions([...acceptedSuggestions, `${suggestion.procedureId}-${suggestion.criterionId}`]);
    };

    return (
        <div className="p-6 space-y-8 animate-in fade-in duration-500">
            {/* Header IA Pro */}
            <div className="bg-indigo-900 rounded-[2.5rem] p-8 text-white flex flex-col md:flex-row items-center gap-8 relative overflow-hidden">
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

            <div className="grid grid-cols-1 xl:grid-cols-4 gap-8">
                {/* MATRICE PRINCIPALE */}
                <div className="xl:col-span-3 space-y-4">
                    <div className="flex items-center justify-between">
                        <h3 className="text-xl font-black text-slate-900 flex items-center gap-2 uppercase tracking-tighter">
                            <Grid3X3 size={20} className="text-indigo-600" /> Cartographie des Liens
                        </h3>
                        <p className="text-xs font-bold text-slate-400 italic">Cliquez sur une procédure pour analyser ses corrélations</p>
                    </div>

                    <div className="bg-white rounded-[2rem] border border-slate-200 shadow-sm overflow-hidden">
                        <div className="overflow-x-auto">
                            <table className="w-full border-collapse">
                                <thead>
                                    <tr className="bg-slate-50/80">
                                        <th className="p-6 text-left text-xs font-black text-slate-400 uppercase tracking-widest border-b border-slate-100 sticky left-0 bg-slate-50 z-20">Procédures</th>
                                        {CRITERIA.map(c => (
                                            <th key={c.id} className="p-4 text-center text-[10px] font-black text-slate-900 uppercase tracking-tighter border-b border-slate-100 min-w-[100px]">
                                                {c.label}
                                                <p className="text-[8px] text-slate-400 lowercase font-mono opacity-50">{c.id.split('.')[0]}</p>
                                            </th>
                                        ))}
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-slate-50">
                                    {matrixMetadata.slice(0, 15).map(proc => (
                                        <tr
                                            key={proc.id}
                                            onClick={() => setSelectedProcedureId(proc.id)}
                                            className={`hover:bg-indigo-50/30 transition-all cursor-pointer group ${selectedProcedureId === proc.id ? 'bg-indigo-50/50' : ''}`}
                                        >
                                            <td className="p-6 sticky left-0 bg-white group-hover:bg-indigo-50/30 transition-colors z-10 border-r border-slate-50">
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
                                                                <div className={`w-8 h-8 rounded-xl flex items-center justify-center transition-all ${isAccepted ? 'bg-emerald-500 text-white animate-bounce-short' : 'bg-indigo-100 text-indigo-600 rotate-0 group-hover:rotate-12'}`}>
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
                        <div className="p-4 bg-slate-50 border-t border-slate-100 flex justify-between items-center px-8">
                            <span className="text-[10px] font-bold text-slate-400 uppercase tracking-widest">{procedures.length} procédures analysées</span>
                            <button className="text-xs font-black text-indigo-600 hover:text-indigo-800 transition-colors uppercase">Afficher l'intégralité (37)</button>
                        </div>
                    </div>
                </div>

                {/* PANNEAU DE CORRÉLATION TRANSVERSALE */}
                <div className="space-y-6">
                    <h3 className="text-xl font-black text-slate-900 flex items-center gap-2 uppercase tracking-tighter">
                        <Layers size={20} className="text-indigo-600" /> Corrélations
                    </h3>

                    {!selectedProcedureId ? (
                        <div className="bg-slate-100 border-2 border-dashed border-slate-200 rounded-[2.5rem] p-10 text-center space-y-4">
                            <div className="w-12 h-12 bg-white rounded-full flex items-center justify-center mx-auto text-slate-300">
                                <Link2 size={24} />
                            </div>
                            <p className="text-xs font-bold text-slate-400 uppercase leading-relaxed">
                                Sélectionnez une procédure dans la matrice pour voir ses <br /> "Procédures Sœurs"
                            </p>
                        </div>
                    ) : (
                        <div className="space-y-4 animate-in fade-in slide-in-from-right-4 duration-300">
                            <div className="bg-white p-6 rounded-[2rem] border-2 border-indigo-100 shadow-sm">
                                <p className="text-[10px] font-black text-indigo-400 uppercase tracking-widest mb-1">Analyse de similitude pour</p>
                                <h4 className="font-black text-slate-900 leading-tight">{matrixMetadata.find(m => m.id === selectedProcedureId)?.name}</h4>
                            </div>

                            <div className="space-y-3">
                                <p className="text-xs font-black text-slate-400 uppercase tracking-widest px-2">Procédures liées par critères communs :</p>
                                {similarities.map(sim => (
                                    <div key={sim.id} className="bg-white p-5 rounded-[2rem] border border-slate-200 group hover:border-indigo-400 transition-all">
                                        <div className="flex items-center justify-between mb-3">
                                            <div className="flex items-center gap-2">
                                                <div className="w-8 h-8 bg-slate-50 text-slate-400 rounded-xl flex items-center justify-center font-black text-xs group-hover:bg-indigo-600 group-hover:text-white transition-all">
                                                    {sim.commonCount}
                                                </div>
                                                <span className="text-[10px] font-black uppercase text-indigo-600 tracking-tighter">Critères en commun</span>
                                            </div>
                                            <Workflow size={14} className="text-slate-200" />
                                        </div>
                                        <p className="font-bold text-slate-800 text-sm leading-snug mb-3">{sim.name}</p>
                                        <div className="flex flex-wrap gap-1">
                                            {sim.commonVars.map(v => (
                                                <span key={v} className="px-2 py-0.5 bg-slate-100 text-[8px] font-bold text-slate-500 rounded-md">
                                                    {v.split('.').pop()}
                                                </span>
                                            ))}
                                        </div>
                                    </div>
                                ))}
                            </div>

                            <button className="w-full py-4 bg-slate-900 text-white rounded-[1.5rem] font-black shadow-xl flex items-center justify-center gap-2 hover:bg-slate-800 transition-all active:scale-95">
                                <Workflow size={16} /> Générer Graphe de Dépendance
                            </button>
                        </div>
                    )}

                    {/* SUGGESTIONS RAPIDES */}
                    <div className="bg-amber-50 rounded-[2.5rem] p-6 border border-amber-100 space-y-4">
                        <div className="flex items-center gap-2">
                            <Zap size={18} className="text-amber-500" />
                            <p className="text-[10px] font-black text-amber-600 uppercase tracking-widest">Alerte Suggestion</p>
                        </div>
                        <p className="text-xs font-medium text-amber-900 leading-relaxed italic">
                            "L'IA a détecté que le critère de 'Diplôme' devrait être partagé entre les procédures Passeport Talent et Création d'Entreprise."
                        </p>
                    </div>
                </div>
            </div>
        </div>
    );
}
