'use client';

import React from 'react';
import { UserProfile } from '@/types';

interface StepProps {
    data: UserProfile;
    update: (section: keyof UserProfile, data: any) => void;
    onNext: () => void;
    canNext: boolean;
}

export default function IdentityTimelineStep({ data, update, onNext, canNext }: StepProps) {
    return (
        <div className="p-8 flex-1 flex flex-col">
            <h2 className="text-2xl font-bold text-slate-900 mb-1">ÉTAPE 1 : IDENTITÉ & ORIGINE</h2>
            <p className="text-slate-500 mb-8">Le Socle : Comprendre qui vous êtes pour déterminer les règles applicables.</p>

            <div className="mb-8 p-6 bg-slate-50 rounded-2xl border border-slate-100">
                <label className="block text-sm font-bold text-slate-700 mb-4 uppercase tracking-widest">
                    Quel est votre objectif principal ? <span className="text-red-500">*</span>
                </label>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <button
                        onClick={() => update('project', { target_goal: 'NATURALIZATION' })}
                        className={`p-4 rounded-xl border-2 text-left transition-all duration-300 transform hover:scale-[1.02] active:scale-[0.98] ${data.project.target_goal === 'NATURALIZATION'
                            ? 'bg-indigo-50 border-indigo-600 ring-2 ring-indigo-600/10'
                            : 'bg-white border-slate-100 hover:border-slate-200 shadow-sm'
                            }`}
                    >
                        <div className="text-xl mb-1">🇫🇷</div>
                        <div className={`font-bold text-sm ${data.project.target_goal === 'NATURALIZATION' ? 'text-indigo-900' : 'text-slate-900'}`}>Nationalité</div>
                        <div className="text-[10px] text-slate-500 mt-1 leading-tight">Devenir Français (Naturalisation)</div>
                    </button>

                    <button
                        onClick={() => update('project', { target_goal: 'RESIDENCE_PERMIT' })}
                        className={`p-4 rounded-xl border-2 text-left transition-all duration-300 transform hover:scale-[1.02] active:scale-[0.98] ${data.project.target_goal === 'RESIDENCE_PERMIT'
                            ? 'bg-indigo-50 border-indigo-600 ring-2 ring-indigo-600/10'
                            : 'bg-white border-slate-100 hover:border-slate-200 shadow-sm'
                            }`}
                    >
                        <div className="text-xl mb-1">🪪</div>
                        <div className={`font-bold text-sm ${data.project.target_goal === 'RESIDENCE_PERMIT' ? 'text-indigo-900' : 'text-slate-900'}`}>Titre de Séjour</div>
                        <div className="text-[10px] text-slate-500 mt-1 leading-tight">Obtenir ou renouveler un titre</div>
                    </button>

                    <button
                        onClick={() => update('project', { target_goal: 'BOTH' })}
                        className={`p-4 rounded-xl border-2 text-left transition-all duration-300 transform hover:scale-[1.02] active:scale-[0.98] ${data.project.target_goal === 'BOTH'
                            ? 'bg-indigo-50 border-indigo-600 ring-2 ring-indigo-600/10'
                            : 'bg-white border-slate-100 hover:border-slate-200 shadow-sm'
                            }`}
                    >
                        <div className="text-xl mb-1">⚖️</div>
                        <div className={`font-bold text-sm ${data.project.target_goal === 'BOTH' ? 'text-indigo-900' : 'text-slate-900'}`}>Toutes les options</div>
                        <div className="text-[10px] text-slate-500 mt-1 leading-tight">Connaître tous mes droits</div>
                    </button>
                </div>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
                <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                        Groupe de nationalité <span className="text-red-500">*</span>
                    </label>
                    <select
                        value={data.identity.nationality_group}
                        onChange={(e) => update('identity', { nationality_group: e.target.value })}
                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                    >
                        <option value="">Sélectionnez votre nationalité...</option>
                        <option value="FRANCE">Française (Né en France / Double nationalité)</option>
                        <option value="TUNISIAN">Tunisienne</option>
                        <option value="ALGERIAN">Algérienne</option>
                        <option value="MOROCCAN">Marocaine</option>
                        <option value="EU">Ressortissant UE / EEE / Suisse</option>
                        <option value="REFUGEE">Bénéficiaire du statut de Réfugié / Protection Subsidiaire</option>
                        <option value="STATELESS">Apatride</option>
                        <option value="NON_EU">Autre (Hors Union Européenne)</option>
                    </select>
                </div>

                <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                        Âge <span className="text-red-500">*</span>
                    </label>
                    <input
                        type="number"
                        value={data.identity.age}
                        onFocus={(e) => e.target.select()}
                        onChange={(e) => update('identity', { age: Number(e.target.value) || 0 })}
                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                    />
                </div>
            </div>

            <div className="space-y-4 mb-8">
                <div className="flex items-center gap-3 p-4 bg-indigo-50 rounded-xl border border-indigo-100">
                    <input
                        type="checkbox"
                        id="born_in_france"
                        checked={data.identity.born_in_france}
                        onChange={(e) => update('identity', { born_in_france: e.target.checked })}
                        className="w-5 h-5 text-indigo-600 rounded"
                    />
                    <label htmlFor="born_in_france" className="text-sm font-medium text-indigo-900">
                        Je suis né(e) en France
                    </label>
                </div>
            </div>

            <div className="mt-auto flex justify-end">
                <button
                    onClick={onNext}
                    disabled={!canNext}
                    className={`w-full md:w-auto px-8 py-3 font-semibold rounded-lg transition-all duration-200 ${canNext
                        ? 'bg-indigo-600 text-white hover:bg-indigo-700 shadow-lg shadow-indigo-200'
                        : 'bg-slate-200 text-slate-400 cursor-not-allowed'
                        }`}
                >
                    Suivant
                </button>
            </div>
        </div>
    );
}
