'use client';

import React from 'react';
import { UserProfile } from '@/types';

interface StepProps {
    data: UserProfile;
    update: (section: keyof UserProfile, data: any) => void;
    onNext: () => void;
    onBack: () => void;
    canNext: boolean;
}

export default function MigratoryHistoryStep({ data, update, onNext, onBack, canNext }: StepProps) {
    const arrivalAge = data.identity.age - data.timeline.years_continuous_residence;

    return (
        <div className="p-8 flex-1 flex flex-col">
            <h2 className="text-2xl font-bold text-slate-900 mb-1">ÉTAPE 2 : HISTOIRE MIGRATOIRE</h2>
            <p className="text-slate-500 mb-8">Le Statut : Comprendre votre situation légale actuelle et votre historique en France.</p>

            <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
                <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                        Date d'entrée en France <span className="text-red-500">*</span>
                    </label>
                    <input
                        type="date"
                        value={data.timeline.entry_date}
                        onChange={(e) => update('timeline', { entry_date: e.target.value })}
                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                    />
                    <p className="mt-2 text-xs text-slate-400 italic">
                        Si vous ne connaissez pas la date exacte, mettez le 1er janvier de l'année d'arrivée.
                    </p>
                </div>

                <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                        Titre de séjour actuel <span className="text-red-500">*</span>
                    </label>
                    <select
                        value={data.admin.current_visa_type || 'NONE'}
                        onChange={(e) => update('admin', { current_visa_type: e.target.value })}
                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                    >
                        <option value="NONE">Aucun (Sans-papiers)</option>
                        <option value="VLS-TS">Visa Long Séjour valant Titre de Séjour (VLS-TS)</option>
                        <option value="STUDENT">Titre de Séjour - Étudiant</option>
                        <option value="WORKER">Titre de Séjour - Salarié / Travailleur Temporaire</option>
                        <option value="VPF">Titre de Séjour - Vie Privée et Familiale (VPF)</option>
                        <option value="PASSEPORT_TALENT">Passeport Talent (Tout type)</option>
                        <option value="VISITOR">Titre de Séjour - Visiteur</option>
                        <option value="RESIDENT_CARD">Carte de Résident (10 ans)</option>
                        <option value="RECEIPISSE">Récépissé de demande / APS</option>
                    </select>
                </div>


                {data.admin.current_visa_type !== 'NONE' && arrivalAge < 18 && (
                    <div className="animate-in fade-in slide-in-from-top-4 duration-300">
                        <label className="block text-sm font-medium text-gray-700 mb-1">Mode d'entrée en France</label>
                        <select
                            value={data.admin.entry_mode || 'STANDARD'}
                            onChange={(e) => update('admin', { entry_mode: e.target.value })}
                            className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                        >
                            <option value="STANDARD">Entrée standard / Visa touristique</option>
                            <option value="FAMILY_REUNIFICATION">Regroupement Familial</option>
                        </select>
                    </div>
                )}
            </div>

            {/* Ordre Public Section */}
            <div className="mb-8 p-6 bg-slate-50 rounded-2xl border border-slate-100">
                <h3 className="text-sm font-bold text-slate-400 uppercase tracking-widest mb-4">Ordre Public & Sécurité</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div className="flex flex-col gap-3">
                        <label className="text-sm font-semibold text-slate-700">
                            Casier Judiciaire <span className="text-red-500">*</span>
                        </label>
                        <div className="flex gap-2">
                            <button
                                onClick={() => update('civic', { clean_criminal_record: true })}
                                className={`flex-1 py-2 px-4 rounded-lg font-medium transition-all ${data.civic.clean_criminal_record
                                    ? 'bg-emerald-100 text-emerald-700 border-2 border-emerald-200'
                                    : 'bg-white text-slate-500 border border-slate-200 hover:border-slate-300'
                                    }`}
                            >
                                Néant (B2 vierge)
                            </button>
                            <button
                                onClick={() => update('civic', { clean_criminal_record: false })}
                                className={`flex-1 py-2 px-4 rounded-lg font-medium transition-all ${!data.civic.clean_criminal_record
                                    ? 'bg-red-100 text-red-700 border-2 border-red-200'
                                    : 'bg-white text-slate-500 border border-slate-200 hover:border-slate-300'
                                    }`}
                            >
                                Condamnation
                            </button>
                        </div>
                        <p className="text-[10px] text-slate-400 italic">
                            Avez-vous des condamnations inscrites au bulletin n°2 ?
                        </p>
                    </div>

                    <div className="flex flex-col gap-3">
                        <label className="text-sm font-semibold text-slate-700">
                            Mesures d'Éloignement <span className="text-red-500">*</span>
                        </label>
                        <div className="flex gap-2">
                            <button
                                onClick={() => update('civic', { no_expulsion_order: true })}
                                className={`flex-1 py-2 px-4 rounded-lg font-medium transition-all ${data.civic.no_expulsion_order
                                    ? 'bg-emerald-100 text-emerald-700 border-2 border-emerald-200'
                                    : 'bg-white text-slate-500 border border-slate-200 hover:border-slate-300'
                                    }`}
                            >
                                Aucune (Situation RAS)
                            </button>
                            <button
                                onClick={() => update('civic', { no_expulsion_order: false })}
                                className={`flex-1 py-2 px-4 rounded-lg font-medium transition-all ${!data.civic.no_expulsion_order
                                    ? 'bg-red-100 text-red-700 border-2 border-red-200'
                                    : 'bg-white text-slate-500 border border-slate-200 hover:border-slate-300'
                                    }`}
                            >
                                OQTF / IRTF
                            </button>
                        </div>
                        <p className="text-[10px] text-slate-400 italic">
                            Faites-vous l'objet d'une OQTF ou interdiction de territoire ?
                        </p>
                    </div>
                </div>
            </div>

            <div className="mt-auto flex justify-between">
                <button
                    onClick={onBack}
                    className="px-6 py-3 text-gray-500 hover:text-gray-700 hover:bg-gray-100 font-semibold rounded-lg transition-all duration-200"
                >
                    Précédent
                </button>
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
