'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import { ArrowRight, ArrowLeft, MapPin, Calendar, UserCheck, AlertCircle } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface RdvPrefectureStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

type RdvSubStep = 1 | 2;

export default function RdvPrefectureStep({ userProfile, updateProfile, onNext }: RdvPrefectureStepProps) {
    const [subStep, setSubStep] = useState<RdvSubStep>(1);

    const handleUpdate = (updates: Partial<UserProfile['rdv_prefecture']>) => {
        updateProfile({
            rdv_prefecture: {
                ...userProfile.rdv_prefecture,
                ...updates
            }
        });
    };

    const isAnefCase = userProfile.rdv_prefecture.rdv_reason === 'renouvellement_anef';

    const nextSubStep = () => {
        setSubStep((s) => (s + 1) as RdvSubStep);
    };

    const prevSubStep = () => setSubStep((s) => (s - 1) as RdvSubStep);

    const isSubStepValid = () => {
        const { rdv_prefecture } = userProfile;
        if (subStep === 1) return !!rdv_prefecture.prefecture_dept && /^\d{5}$/.test(rdv_prefecture.prefecture_dept);
        if (subStep === 2) return !!rdv_prefecture.rdv_reason;
        return true;
    };

    const renderProgressDots = () => {
        return (
            <div className="flex gap-2 mb-8 justify-center">
                {[1, 2].map((i) => (
                    <div
                        key={i}
                        className={cn(
                            "h-1.5 rounded-full transition-all duration-300",
                            subStep === i ? "w-8 bg-indigo-600" : "w-2 bg-slate-200"
                        )}
                    />
                ))}
            </div>
        );
    };

    return (
        <div className="flex-1 flex flex-col p-8 md:p-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {renderProgressDots()}

            {subStep === 1 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-indigo-50 text-indigo-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <MapPin className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Préfecture cible</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Vérifions les guichets disponibles dans votre département.</p>
                    </div>

                    <div className="max-w-md mx-auto w-full space-y-4">
                        <label className="block text-sm font-black text-slate-400 uppercase tracking-widest ml-1">Code Postal</label>
                        <input
                            type="text"
                            placeholder="Ex: 92000"
                            maxLength={5}
                            value={userProfile.rdv_prefecture.prefecture_dept || ''}
                            onChange={(e) => handleUpdate({ prefecture_dept: e.target.value.replace(/\D/g, '') })}
                            className="w-full p-6 bg-slate-50 border-2 border-slate-100 rounded-2xl text-2xl font-bold text-slate-900 focus:border-indigo-600 focus:ring-4 focus:ring-indigo-50 outline-none transition-all placeholder:text-slate-300 text-center tracking-[0.2em]"
                        />
                    </div>
                </div>
            )}

            {subStep === 2 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-blue-50 text-blue-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Calendar className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Motif EXACT du rendez-vous</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Vérifions la faisabilité de votre demande.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {[
                            { id: 'retrait_titre', label: 'Retrait de titre de séjour', desc: 'Suite à SMS ou Courrier reçu' },
                            { id: 'premiere_demande_papier', label: 'Première demande (Papier)', desc: 'Régularisation / AES / Guichet physique' },
                            { id: 'commission_medicale', label: 'Commission médicale', desc: 'Permis de conduire / Aptitude' },
                            { id: 'renouvellement_hors_ligne', label: 'Renouvellement (Hors ligne)', desc: 'Titres non gérés par l\'ANEF' },
                            { id: 'naturalisation', label: 'Dépôt Naturalisation', desc: 'Entretien de nationalité' },
                            { id: 'echange_permis', label: 'Échange Permis', desc: 'Permis étranger vers permis français' },
                            { id: 'renouvellement_anef', label: 'Renouvellement (En ligne - ANEF)', desc: 'Étudiant / Passeport Talent / Visiteur' }
                        ].map((opt) => (
                            <button
                                key={opt.id}
                                onClick={() => handleUpdate({ rdv_reason: opt.id as any })}
                                className={cn(
                                    "flex flex-col p-5 rounded-2xl border-2 transition-all text-left",
                                    userProfile.rdv_prefecture.rdv_reason === opt.id
                                        ? "border-indigo-600 bg-indigo-50/50 ring-4 ring-indigo-50"
                                        : "border-slate-100 hover:border-indigo-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-lg font-bold transition-colors", userProfile.rdv_prefecture.rdv_reason === opt.id ? "text-indigo-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                                <span className="text-xs text-slate-500 font-medium">{opt.desc}</span>
                            </button>
                        ))}
                    </div>

                    {isAnefCase && (
                        <div className="max-w-lg mx-auto p-6 bg-rose-50 border-2 border-rose-100 rounded-[2rem] flex gap-4 items-start animate-in slide-in-from-top-2">
                            <AlertCircle className="w-6 h-6 text-rose-500 shrink-0 mt-1" />
                            <div className="space-y-2">
                                <p className="text-rose-900 font-black text-sm uppercase tracking-tight">Vente impossible pour ce motif</p>
                                <p className="text-rose-700 text-sm font-medium leading-relaxed">
                                    Pour ce motif, il n'existe plus de guichet physique. La procédure est 100% en ligne. Nous ne pouvons pas vous vendre de rendez-vous.
                                </p>
                                <button className="text-rose-600 font-bold text-sm underline hover:text-rose-800 transition-colors">
                                    Voir notre offre 'Accompagnement Dossier ANEF'
                                </button>
                            </div>
                        </div>
                    )}
                </div>
            )}

            <div className="mt-auto pt-10 flex items-center justify-between border-t border-slate-100">
                {subStep > 1 ? (
                    <button
                        onClick={prevSubStep}
                        className="flex items-center gap-3 px-8 py-4 text-slate-400 font-black hover:text-slate-600 transition-colors"
                    >
                        <ArrowLeft className="w-5 h-5" />
                        Précédent
                    </button>
                ) : (
                    <div />
                )}

                {!isAnefCase && (
                    <button
                        onClick={subStep === 2 ? onNext : nextSubStep}
                        disabled={!isSubStepValid()}
                        className={cn(
                            "flex items-center gap-4 px-12 py-5 rounded-2xl font-black text-lg transition-all shadow-xl",
                            isSubStepValid()
                                ? "bg-slate-900 text-white hover:bg-slate-800 shadow-slate-200"
                                : "bg-slate-100 text-slate-300 cursor-not-allowed shadow-none"
                        )}
                    >
                        {subStep === 2 ? "VOIR LE DEVIS / COMMANDER" : "Continuer"}
                        <ArrowRight className="w-5 h-5" />
                    </button>
                )}
            </div>
        </div>
    );
}
