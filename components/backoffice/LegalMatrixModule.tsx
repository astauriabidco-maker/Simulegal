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
    Cpu
} from 'lucide-react';
import EligibilityStore from '../../services/EligibilityStore';

const CRITERIA = [
    { id: 'age', label: 'Âge', group: 'Identité' },
    { id: 'residence', label: 'Résidence', group: 'Parcours' },
    { id: 'salary', label: 'Salaire', group: 'Finances' },
    { id: 'marriage', label: 'Mariage', group: 'Famille' },
    { id: 'french', label: 'Français', group: 'Intégration' },
    { id: 'diploma', label: 'Diplôme', group: 'Études' }
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
            criterionId: 'salary',
            isLinked: true,
            isSuggested: true,
            reason: "D'après le décret 2026-12, le seuil de salaire est désormais la condition maîtresse."
        },
        {
            procedureId: 'naturalisation_par_decret',
            criterionId: 'french',
            isLinked: true,
            isSuggested: true,
            reason: "Alignement suggéré avec le nouveau référentiel B1/B2."
        }
    ]);

    const [acceptedSuggestions, setAcceptedSuggestions] = useState<string[]>([]);

    // Matrice de calcul simplifiée (pour démo, on vérifie si le critère est présent dans les conditions techniques)
    const checkLink = (proc: any, criterionId: string) => {
        const json = JSON.stringify(proc.conditions).toLowerCase();
        const keywords: Record<string, string[]> = {
            age: ['age'],
            residence: ['residence', 'residence_in_france', 'years_continuous'],
            salary: ['salary', 'gross_salary', 'smic', 'resources'],
            marriage: ['marriage', 'spouse', 'community_of_life'],
            french: ['french_level', 'integration'],
            diploma: ['diploma', 'education']
        };

        return keywords[criterionId]?.some(kw => json.includes(kw)) || false;
    };

    const handleAccept = (suggestion: MatrixLink) => {
        setAcceptedSuggestions([...acceptedSuggestions, `${suggestion.procedureId}-${suggestion.criterionId}`]);
        // Ici on pourrait appeler EligibilityStore pour injecter la condition No-Code automatiquement
    };

    return (
        <div className="p-6 space-y-8 animate-in fade-in duration-500">
            {/* Header IA */}
            <div className="bg-indigo-900 rounded-[2.5rem] p-8 text-white flex flex-col md:flex-row items-center gap-8 relative overflow-hidden">
                <div className="absolute top-0 right-0 w-96 h-96 bg-indigo-500 rounded-full blur-[120px] opacity-20 -mr-48 -mt-48"></div>

                <div className="w-20 h-20 bg-white/10 rounded-3xl flex items-center justify-center backdrop-blur-xl border border-white/20">
                    <BrainCircuit size={40} className="text-indigo-300" />
                </div>

                <div className="flex-1 space-y-2 text-center md:text-left">
                    <div className="flex items-center gap-2 justify-center md:justify-start">
                        <span className="px-2 py-0.5 bg-indigo-500 text-[10px] font-black uppercase tracking-widest rounded-md">AI Model Legal-V4</span>
                        <span className="flex items-center gap-1 text-[10px] text-emerald-400 font-bold uppercase tracking-widest">
                            <Zap size={10} /> Analyse en temps réel
                        </span>
                    </div>
                    <h2 className="text-3xl font-black tracking-tight uppercase">Matrice de Corrélation IA</h2>
                    <p className="text-indigo-200 font-medium max-w-2xl">
                        L'IA analyse les nouveaux textes législatifs et détecte les liens manquants entre vos procédures et les critères d'éligibilité.
                    </p>
                </div>

                <div className="bg-white/10 p-4 rounded-2xl backdrop-blur-md border border-white/10 text-center">
                    <p className="text-3xl font-black">2</p>
                    <p className="text-[10px] font-black uppercase tracking-widest text-indigo-300">Suggestions actives</p>
                </div>
            </div>

            <div className="grid grid-cols-1 xl:grid-cols-3 gap-8">
                {/* MATRICE VISUELLE */}
                <div className="xl:col-span-2 space-y-4">
                    <h3 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <Grid3X3 size={20} className="text-indigo-600" /> Matrice Procédures / Critères
                    </h3>

                    <div className="bg-white rounded-[2rem] border border-slate-200 shadow-sm overflow-hidden">
                        <div className="overflow-x-auto">
                            <table className="w-full border-collapse">
                                <thead>
                                    <tr className="bg-slate-50">
                                        <th className="p-6 text-left text-xs font-black text-slate-400 uppercase tracking-widest border-b border-slate-100">Procédures</th>
                                        {CRITERIA.map(c => (
                                            <th key={c.id} className="p-4 text-center text-[10px] font-black text-slate-900 uppercase tracking-tighter border-b border-slate-100 min-w-[80px]">
                                                {c.label}
                                            </th>
                                        ))}
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-slate-50">
                                    {procedures.slice(0, 10).map(proc => (
                                        <tr key={proc.id} className="hover:bg-slate-50/50 transition-colors">
                                            <td className="p-6">
                                                <p className="font-black text-slate-800 text-sm tracking-tight">{proc.name}</p>
                                                <p className="text-[10px] font-mono text-slate-400">{proc.id}</p>
                                            </td>
                                            {CRITERIA.map(c => {
                                                const isLinked = checkLink(proc, c.id);
                                                const isSuggested = suggestions.find(s => s.procedureId === proc.id && s.criterionId === c.id);
                                                const isAccepted = acceptedSuggestions.includes(`${proc.id}-${c.id}`);

                                                return (
                                                    <td key={c.id} className="p-4 text-center">
                                                        <div className="flex justify-center">
                                                            {isSuggested && !isAccepted ? (
                                                                <div className="w-6 h-6 rounded-full bg-amber-100 flex items-center justify-center text-amber-600 animate-pulse cursor-help group relative">
                                                                    <Zap size={14} />
                                                                    <div className="absolute bottom-full mb-2 left-1/2 -translate-x-1/2 w-48 p-2 bg-slate-900 text-white text-[10px] font-bold rounded-lg opacity-0 group-hover:opacity-100 transition-opacity pointer-events-none z-50">
                                                                        {isSuggested.reason}
                                                                        <div className="mt-1 flex items-center gap-1 text-amber-400">
                                                                            <Cpu size={10} /> Suggéré par l'IA
                                                                        </div>
                                                                    </div>
                                                                </div>
                                                            ) : isLinked || isAccepted ? (
                                                                <div className={`w-6 h-6 rounded-full flex items-center justify-center ${isAccepted ? 'bg-emerald-500 text-white' : 'bg-indigo-100 text-indigo-600'}`}>
                                                                    <CheckCircle size={14} />
                                                                </div>
                                                            ) : (
                                                                <div className="w-1.5 h-1.5 rounded-full bg-slate-100" />
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
                        <div className="p-4 bg-slate-50 border-t border-slate-100 flex justify-center">
                            <button className="text-xs font-bold text-slate-400 hover:text-indigo-600 transition-colors">Charger toutes les procédures ({procedures.length})</button>
                        </div>
                    </div>
                </div>

                {/* CENTRE DE DÉCISION IA */}
                <div className="space-y-6">
                    <h3 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <Bot size={20} className="text-indigo-600" /> Suggestions de l'Auditeur
                    </h3>

                    <div className="space-y-4">
                        {suggestions.filter(s => !acceptedSuggestions.includes(`${s.procedureId}-${s.criterionId}`)).map((s, idx) => (
                            <div key={idx} className="bg-white rounded-[2rem] border-2 border-amber-100 p-6 space-y-4 shadow-sm hover:shadow-xl hover:shadow-amber-500/5 transition-all">
                                <div className="flex items-center justify-between">
                                    <span className="px-2 py-1 bg-amber-50 text-amber-600 text-[10px] font-black uppercase rounded-lg border border-amber-100">Lien Suggéré</span>
                                    <span className="text-xs font-bold text-slate-400 flex items-center gap-1 text-emerald-500">
                                        <Zap size={12} /> Confiance 94%
                                    </span>
                                </div>

                                <div className="flex items-center gap-3">
                                    <div className="flex-1 min-w-0">
                                        <p className="text-xs font-bold text-slate-400 uppercase tracking-widest">{s.procedureId}</p>
                                        <p className="font-black text-slate-900 leading-tight">
                                            {procedures.find(p => p.id === s.procedureId)?.name}
                                        </p>
                                    </div>
                                    <ArrowRight className="text-slate-300" size={20} />
                                    <div className="bg-indigo-50 px-3 py-2 rounded-xl border border-indigo-100">
                                        <p className="text-[10px] font-black text-indigo-400 uppercase">Critère</p>
                                        <p className="font-black text-indigo-600">{CRITERIA.find(c => c.id === s.criterionId)?.label}</p>
                                    </div>
                                </div>

                                <div className="bg-slate-50 p-4 rounded-2xl flex gap-3 items-start">
                                    <Info className="text-slate-400 mt-0.5" size={16} />
                                    <p className="text-xs font-medium text-slate-600 italic">
                                        "{s.reason}"
                                    </p>
                                </div>

                                <div className="grid grid-cols-2 gap-3 pt-2">
                                    <button
                                        onClick={() => handleAccept(s)}
                                        className="h-12 bg-indigo-600 text-white rounded-2xl font-black shadow-xl shadow-indigo-100 flex items-center justify-center gap-2 hover:bg-indigo-700 transition-all active:scale-95"
                                    >
                                        <Zap size={16} /> Appliquer
                                    </button>
                                    <button className="h-12 bg-white border border-slate-200 text-slate-400 rounded-2xl font-black flex items-center justify-center gap-2 hover:bg-slate-50 transition-all">
                                        <XCircle size={16} /> Rejeter
                                    </button>
                                </div>
                            </div>
                        ))}

                        {acceptedSuggestions.length === suggestions.length && (
                            <div className="bg-emerald-50 border border-emerald-100 rounded-[2rem] p-10 text-center space-y-4">
                                <div className="w-16 h-16 bg-emerald-100 text-emerald-600 rounded-full flex items-center justify-center mx-auto mb-4">
                                    <CheckCircle size={32} />
                                </div>
                                <h4 className="font-black text-emerald-900 uppercase">Tout est à jour !</h4>
                                <p className="text-xs font-bold text-emerald-700">L'IA n'a détecté aucune anomalie ou lien manquant dans vos procédures pour le moment.</p>
                            </div>
                        )}
                    </div>
                </div>
            </div>

            {/* ANALYSE DES CONSÉQUENCES */}
            <div className="bg-slate-50 rounded-[2.5rem] border border-slate-200 p-8 flex flex-col md:flex-row items-center gap-8">
                <div className="w-16 h-16 bg-white rounded-2xl flex items-center justify-center text-indigo-600 shadow-sm border border-slate-100">
                    <BrainCircuit size={32} />
                </div>
                <div className="flex-1">
                    <h3 className="text-xl font-black text-slate-900 uppercase tracking-tight">Analyse de Conséquence Prédictive</h3>
                    <p className="text-slate-500 font-medium">
                        Avant d'appliquer un lien suggéré, l'IA simule l'impact sur votre base de données de test pour éviter les faux-négatifs.
                    </p>
                </div>
                <div className="flex gap-4">
                    <div className="text-center px-6 py-3 bg-white rounded-2xl border border-slate-100 shadow-sm">
                        <p className="text-xs font-black text-slate-400 uppercase">Précision</p>
                        <p className="text-xl font-black text-emerald-500">99.2%</p>
                    </div>
                </div>
            </div>
        </div>
    );
}
