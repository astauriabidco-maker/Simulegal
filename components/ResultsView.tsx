'use client';

import React, { useMemo } from 'react';
import { UserProfile, ProcedureRule } from '@/types';
import { evaluateRule } from '@/lib/engine';
import rulesSejour from '@/specs/rules_sejour.json';
import rulesNaturalisation from '@/specs/rules_naturalisation.json';

interface ResultsViewProps {
    userProfile: UserProfile;
    onReset: () => void;
}

export default function ResultsView({ userProfile, onReset }: ResultsViewProps) {
    const eligibleStays = useMemo(() => {
        let results = (rulesSejour as ProcedureRule[])
            .filter((rule) => evaluateRule(userProfile, rule.conditions));

        // Anti-Regression Filter: If user has RESIDENT_CARD, hide precarious permits
        if (userProfile.admin.current_visa_type === 'RESIDENT_CARD') {
            const allowedResidencyIds = ['carte_resident_longue_duree_ue', 'carte_resident_refugie_apatride', 'carte_resident_conjoint_francais', 'carte_resident_regroupement_familial'];
            results = results.filter(r => allowedResidencyIds.includes(r.id));
        }

        // Sort by priority descending
        results.sort((a, b) => b.priority - a.priority);

        // Noise Filtering: If we have a very strong option (> 60), hide noise (< 20)
        const hasStrongOption = results.some(r => r.priority >= 60);
        if (hasStrongOption) {
            results = results.filter(r => r.priority >= 20);
        }

        return results;
    }, [userProfile]);

    const eligibleNaturalization = useMemo(() => {
        return (rulesNaturalisation as ProcedureRule[])
            .filter((rule) => evaluateRule(userProfile, rule.conditions))
            .sort((a, b) => b.priority - a.priority);
    }, [userProfile]);

    const targetGoal = userProfile.project.target_goal || 'BOTH';
    const hasNatEligible = eligibleNaturalization.length > 0;
    const hasStayEligible = eligibleStays.length > 0;

    // Layout configuration based on intention
    const showNaturalizationFirst = targetGoal === 'NATURALIZATION' || targetGoal === 'BOTH';

    // Procedure Deduction Logic
    const getProcedureDetails = (currentVisa: string, resultId: string) => {
        if (resultId.startsWith('nat_')) {
            return { label: 'Acquisition de la Nationalité', color: 'bg-blue-600 text-white' };
        }
        if (currentVisa === 'NONE' || currentVisa === 'VISITOR') {
            return { label: 'Première Demande / Régularisation', color: 'bg-orange-500 text-white' };
        }
        if (currentVisa === 'STUDENT' && (resultId.includes('salarie') || resultId.includes('talent') || resultId.includes('rece'))) {
            return { label: 'Changement de Statut', color: 'bg-purple-600 text-white' };
        }
        if (resultId.includes('resident') && currentVisa !== 'RESIDENT_CARD') {
            return { label: 'Accès à la Résidence (10 ans)', color: 'bg-amber-500 text-white' };
        }

        // Renewal detection
        const isVPF = currentVisa === 'VPF' && resultId.includes('vpf');
        const isWorker = (currentVisa === 'WORKER' || currentVisa === 'VLS-TS') && (resultId.includes('salarie') || resultId.includes('temporaire'));
        const isStudent = currentVisa === 'STUDENT' && resultId.includes('etudiant');
        const isTalent = currentVisa === 'PASSEPORT_TALENT' && resultId.includes('talent');
        const isResident = currentVisa === 'RESIDENT_CARD' && resultId.includes('resident');

        if (isVPF || isWorker || isStudent || isTalent || isResident) {
            return { label: 'Renouvellement', color: 'bg-emerald-500 text-white' };
        }

        return { label: 'Nouvelle Procédure', color: 'bg-slate-500 text-white' };
    };

    return (
        <div className="p-8 flex-1 flex flex-col">
            <div className="flex justify-between items-center mb-8">
                <h2 className="text-3xl font-extrabold text-slate-900">Vos Résultats</h2>
                <button
                    onClick={onReset}
                    className="text-indigo-600 font-semibold hover:text-indigo-800 transition-colors"
                >
                    Recommencer
                </button>
            </div>

            {/* NATURALIZATION SECTION - THE "GRAAL" (GOLD STYLE) */}
            {hasNatEligible && (
                <div className={`mb-12 p-8 bg-gradient-to-br from-amber-400 via-amber-500 to-amber-600 rounded-3xl text-white shadow-2xl shadow-amber-200 border-4 border-white animate-in zoom-in-95 duration-700 relative overflow-hidden ${!showNaturalizationFirst ? 'order-2 opacity-90' : 'order-1'}`}>
                    {/* Decorative Background Elements */}
                    <div className="absolute top-0 right-0 -mr-8 -mt-8 w-32 h-32 bg-white/20 rounded-full blur-2xl" />
                    <div className="absolute bottom-0 left-0 -ml-8 -mb-8 w-32 h-32 bg-black/10 rounded-full blur-2xl" />

                    <div className="relative z-10">
                        <div className="flex items-center gap-2 mb-4">
                            <span className="bg-white/20 backdrop-blur-sm text-white text-[10px] font-black px-3 py-1 rounded-full uppercase tracking-[0.2em] border border-white/30">
                                LE GRAAL
                            </span>
                            <span className={`text-[10px] font-bold px-3 py-1 rounded-full border border-white/30 backdrop-blur-sm`}>
                                {getProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleNaturalization[0].id).label}
                            </span>
                        </div>

                        <div className="flex items-center gap-6">
                            <div className="w-16 h-16 bg-white/30 backdrop-blur-md rounded-2xl flex items-center justify-center text-3xl shadow-inner border border-white/40">
                                🇫🇷
                            </div>
                            <div className="flex-1">
                                <h3 className="text-2xl font-black tracking-tight mb-1 drop-shadow-sm">Nationalité Française</h3>
                                <p className="text-amber-50 font-medium opacity-90 leading-tight">
                                    Félicitations ! Vous semblez être éligible à la nationalité via <strong>{eligibleNaturalization[0].name}</strong>.
                                </p>
                            </div>
                            <button className="px-6 py-3 bg-white text-amber-600 font-black rounded-xl text-sm hover:bg-amber-50 hover:scale-105 active:scale-95 transition-all shadow-lg shadow-amber-700/20 uppercase tracking-wider">
                                Lancer la demande
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* RESIDENCY SECTION */}
            <div className={`space-y-12 flex flex-col ${showNaturalizationFirst ? 'order-2' : 'order-1'}`}>
                {hasStayEligible ? (
                    <section className="animate-in slide-in-from-bottom-8 duration-700">
                        <div className="flex items-center gap-3 mb-6">
                            <h3 className="text-xs font-black text-slate-400 uppercase tracking-[0.3em]">
                                {targetGoal === 'NATURALIZATION' ? 'Vos droits au séjour' : 'Meilleure Option de Séjour'}
                            </h3>
                            <div className="h-px bg-slate-100 flex-1" />
                        </div>

                        {/* HERO CARD - THE WINNER */}
                        <div className="relative group">
                            <div className="absolute top-4 right-4 z-20">
                                <span className="bg-indigo-600 text-white text-[10px] font-bold px-3 py-1 rounded-full shadow-lg shadow-indigo-200 uppercase tracking-wider">
                                    Meilleure Option
                                </span>
                            </div>
                            <div className="p-8 rounded-3xl relative overflow-hidden border-2 border-indigo-100 bg-indigo-50/30 shadow-2xl shadow-indigo-100/20 transition-all duration-300">
                                <div className="flex flex-col md:flex-row gap-8 items-start relative z-10">
                                    <div className="flex-1">
                                        <div className="flex flex-wrap items-center gap-3 mb-4">
                                            <span className={`text-[10px] font-bold px-3 py-1.5 rounded-lg uppercase tracking-wider shadow-sm ${getProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).color}`}>
                                                {getProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).label}
                                            </span>
                                        </div>

                                        <h4 className="text-2xl font-bold text-gray-900 mb-3 tracking-tight">
                                            {eligibleStays[0].name}
                                        </h4>
                                        <p className="text-gray-600 leading-relaxed max-w-2xl font-medium mb-8">
                                            {eligibleStays[0].description}
                                        </p>

                                        <div className="flex flex-wrap gap-4">
                                            <button className="px-6 py-3 bg-indigo-600 text-white font-semibold rounded-xl hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-200 py-3">
                                                Détails de la procédure
                                            </button>
                                            <button className="px-6 py-3 bg-white text-slate-600 font-semibold rounded-xl hover:bg-slate-50 transition-all border border-slate-200 shadow-sm">
                                                Liste des pièces
                                            </button>
                                        </div>
                                    </div>
                                    <div className="hidden lg:flex w-32 h-32 bg-white/50 backdrop-blur-sm rounded-2xl items-center justify-center text-5xl border border-white/50 shadow-inner">
                                        📄
                                    </div>
                                </div>

                                {/* Decorative Grid Pattern */}
                                <div className="absolute top-0 right-0 w-64 h-64 opacity-[0.03] pointer-events-none translate-x-1/2 -translate-y-1/2">
                                    <div className="w-full h-full bg-[radial-gradient(#000_1px,transparent_1px)] [background-size:20px_20px]" />
                                </div>
                            </div>
                        </div>

                        {/* CHALLENGERS - OTHER OPTIONS */}
                        {eligibleStays.length > 1 && (
                            <div className="mt-12">
                                <div className="flex items-center gap-3 mb-6">
                                    <h4 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.3em]">Autres alternatives possibles</h4>
                                    <div className="h-px bg-slate-100 flex-1" />
                                </div>
                                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                                    {eligibleStays.slice(1).map((option) => (
                                        <div key={option.id} className="group p-6 bg-white border border-gray-200 rounded-2xl hover:border-indigo-200 hover:shadow-xl hover:shadow-indigo-100/30 transition-all duration-300">
                                            <div className="mb-4">
                                                <span className={`text-[9px] font-bold px-2 py-1 rounded-md uppercase tracking-tighter ${getProcedureDetails(userProfile.admin.current_visa_type || 'NONE', option.id).color}`}>
                                                    {getProcedureDetails(userProfile.admin.current_visa_type || 'NONE', option.id).label}
                                                </span>
                                            </div>
                                            <h5 className="text-lg font-semibold text-gray-900 mb-2 group-hover:text-indigo-600 transition-colors">{option.name}</h5>
                                            <p className="text-sm text-gray-500 line-clamp-2 leading-snug">{option.description}</p>
                                            <div className="mt-4 pt-4 border-t border-slate-100 flex justify-end opacity-0 group-hover:opacity-100 transition-opacity">
                                                <span className="text-xs font-bold text-indigo-500 uppercase tracking-widest">Voir plus →</span>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        )}
                    </section>
                ) : (
                    // Only show stay permit error if Naturalization is also not found OR if stay permit WAS the goal
                    (!hasNatEligible || targetGoal !== 'NATURALIZATION') && (
                        <div className="p-12 text-center bg-slate-50 rounded-3xl border-2 border-dashed border-slate-200">
                            <div className="text-4xl mb-4">🔍</div>
                            <h3 className="text-xl font-bold text-slate-900 mb-2">Aucune éligibilité immédiate trouvée</h3>
                            <p className="text-slate-500 max-w-md mx-auto">
                                Nos algorithmes n'ont pas trouvé de titre de séjour correspondant exactement à votre profil actuel.
                            </p>
                        </div>
                    )
                )}
            </div>

            <div className="mt-12 pt-8 border-t border-slate-100 order-last">
                <p className="text-xs text-slate-400 italic text-center">
                    Ce simulateur est fourni à titre indicatif et ne remplace pas une décision préfectorale officielle.
                    Les données sont traitées localement et ne sont pas conservées.
                </p>
            </div>
        </div>
    );
}
