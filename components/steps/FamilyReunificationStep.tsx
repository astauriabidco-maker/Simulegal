'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import { User, Clock, Heart, Wallet, Home, Navigation, AlertCircle, CheckCircle2, ArrowRight, ArrowLeft } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface FamilyReunificationStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

export default function FamilyReunificationStep({ userProfile, updateProfile, onNext }: FamilyReunificationStepProps) {
    const [subStep, setSubStep] = useState(1);
    const family = userProfile.family;

    const handleUpdate = (updates: Partial<UserProfile['family']>) => {
        updateProfile({ family: { ...family, ...updates } });
    };

    const isSubStepValid = (step: number) => {
        switch (step) {
            case 1:
                return !!family.sponsor_nationality && !!family.presence_duration;
            case 2:
                if (family.has_handicap_allowance === true) return true;
                if (family.has_handicap_allowance === false) {
                    const hasSalary = (userProfile.work.salary_monthly_gross || 0) > 0;
                    const hasSource = !!family.income_source;
                    return hasSalary && hasSource;
                }
                return false;
            case 3:
                return !!family.housing_status;
            default:
                return false;
        }
    };

    const nextSubStep = () => {
        if (subStep < 3) setSubStep(subStep + 1);
        else onNext();
    };

    const prevSubStep = () => {
        if (subStep > 1) setSubStep(subStep - 1);
    };

    return (
        <div className="max-w-3xl mx-auto space-y-12 py-12 px-6 animate-in fade-in slide-in-from-bottom-6 duration-700">
            {/* Progress Mini Bar */}
            <div className="flex gap-2 max-w-xs mx-auto mb-8">
                {[1, 2, 3].map((s) => (
                    <div
                        key={s}
                        className={cn(
                            "h-1.5 flex-1 rounded-full transition-all duration-500",
                            s <= subStep ? "bg-indigo-600 shadow-[0_0_10px_rgba(79,70,229,0.4)]" : "bg-slate-100"
                        )}
                    />
                ))}
            </div>

            {/* SCREEN 1: LE TEMPS */}
            {subStep === 1 && (
                <div className="space-y-10 animate-in fade-in slide-in-from-right-8 duration-500">
                    <div className="text-center space-y-4">
                        <div className="inline-flex p-4 bg-blue-50 text-blue-600 rounded-3xl mb-2">
                            <Clock className="w-10 h-10" />
                        </div>
                        <h2 className="text-4xl font-black text-slate-900 tracking-tight">Le Temps</h2>
                        <p className="text-slate-500 text-lg font-medium max-w-md mx-auto">
                            Commençons par votre situation de résidence en France.
                        </p>
                    </div>

                    <div className="space-y-8">
                        {/* Nationality */}
                        <div className="space-y-4">
                            <label className="block text-xl font-bold text-slate-800">Quelle est votre nationalité ?</label>
                            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                                {[
                                    { label: "Algérienne", value: "ALGERIAN", desc: "Accord franco-algérien" },
                                    { label: "Autre", value: "OTHER", desc: "Tous les autres pays" }
                                ].map((opt) => (
                                    <button
                                        key={opt.value}
                                        onClick={() => handleUpdate({ sponsor_nationality: opt.value as any })}
                                        className={cn(
                                            "p-6 rounded-[2rem] border-2 text-left transition-all group",
                                            family.sponsor_nationality === opt.value
                                                ? "bg-slate-900 border-slate-900 text-white shadow-xl"
                                                : "bg-white border-slate-100 text-slate-600 hover:border-blue-200"
                                        )}
                                    >
                                        <div className="font-black text-lg mb-1">{opt.label}</div>
                                        <div className={cn("text-xs font-bold uppercase tracking-widest", family.sponsor_nationality === opt.value ? "text-slate-400" : "text-slate-400")}>{opt.desc}</div>
                                    </button>
                                ))}
                            </div>
                        </div>

                        {/* Duration */}
                        <div className="space-y-4">
                            <label className="block text-xl font-bold text-slate-800">Depuis combien de temps résidez-vous régulièrement en France ?</label>
                            <div className="grid grid-cols-1 gap-3">
                                {[
                                    { label: "Moins de 12 mois", value: "LESS_12" },
                                    { label: "12 à 18 mois", value: "12_18" },
                                    { label: "Plus de 18 mois", value: "MORE_18" }
                                ].map((opt) => (
                                    <button
                                        key={opt.value}
                                        onClick={() => handleUpdate({ presence_duration: opt.value as any })}
                                        className={cn(
                                            "p-5 rounded-2xl border-2 text-left transition-all font-black text-lg",
                                            family.presence_duration === opt.value
                                                ? "bg-blue-600 border-blue-600 text-white shadow-lg"
                                                : "bg-white border-slate-100 text-slate-600 hover:border-blue-200"
                                        )}
                                    >
                                        {opt.label}
                                    </button>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* SCREEN 2: LES RESSOURCES */}
            {subStep === 2 && (
                <div className="space-y-10 animate-in fade-in slide-in-from-right-8 duration-500">
                    <div className="text-center space-y-4">
                        <div className="inline-flex p-4 bg-emerald-50 text-emerald-600 rounded-3xl mb-2">
                            <Wallet className="w-10 h-10" />
                        </div>
                        <h2 className="text-4xl font-black text-slate-900 tracking-tight">Les Ressources</h2>
                        <p className="text-slate-500 text-lg font-medium max-w-md mx-auto">
                            L'administration vérifie la stabilité et le montant de vos revenus.
                        </p>
                    </div>

                    <div className="space-y-10">
                        {/* Handicap */}
                        <div className="space-y-4">
                            <label className="block text-xl font-bold text-slate-800">Percevez-vous l'AAH ou l'ASI ?</label>
                            <p className="text-slate-400 text-xs font-bold uppercase tracking-wider -mt-2 mb-4">
                                L'Allocation aux Adultes Handicapés (AAH) ou l'Allocation Supplémentaire d'Invalidité sont des aides spécifiques.
                            </p>
                            <div className="grid grid-cols-2 gap-4">
                                {[
                                    { label: 'Oui', value: true },
                                    { label: 'Non', value: false }
                                ].map((opt) => (
                                    <button
                                        key={opt.label}
                                        onClick={() => handleUpdate({ has_handicap_allowance: opt.value })}
                                        className={cn(
                                            "p-6 rounded-3xl font-black text-xl transition-all border-2",
                                            family.has_handicap_allowance === opt.value
                                                ? "bg-emerald-600 border-emerald-600 text-white shadow-lg shadow-emerald-100"
                                                : "bg-slate-50 border-slate-100 text-slate-600 hover:border-emerald-200"
                                        )}
                                    >
                                        {opt.label}
                                    </button>
                                ))}
                            </div>
                        </div>

                        {family.has_handicap_allowance === false && (
                            <div className="space-y-10 p-8 bg-slate-50 rounded-[3rem] border-2 border-slate-100 animate-in zoom-in-95 duration-500">
                                {/* Income Source */}
                                <div className="space-y-4">
                                    <label className="block text-lg font-bold text-slate-800">Quelle est la source principale de vos revenus ?</label>
                                    <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
                                        {[
                                            { label: "Travail (Salarié/Indépendant)", value: "SALARY" },
                                            { label: "Retraite", value: "PENSION" },
                                            { label: "Chômage", value: "OTHER" },
                                            { label: "RSA ou Aides sociales", value: "RSA_ALOWANCE" }
                                        ].map((opt) => (
                                            <button
                                                key={opt.value}
                                                onClick={() => handleUpdate({ income_source: opt.value as any })}
                                                className={cn(
                                                    "p-4 rounded-xl border-2 text-left transition-all font-bold text-sm",
                                                    family.income_source === opt.value
                                                        ? "bg-white border-emerald-500 text-emerald-700 shadow-sm"
                                                        : "bg-white border-transparent text-slate-500 hover:border-slate-200"
                                                )}
                                            >
                                                {opt.label}
                                            </button>
                                        ))}
                                    </div>
                                </div>

                                {/* Amount */}
                                <div className="space-y-4">
                                    <label className="block text-lg font-bold text-slate-800">Moyenne mensuelle nette sur les 12 derniers mois ?</label>
                                    <p className="text-slate-400 text-xs font-bold italic mb-4">
                                        Prenez le montant Net Payé avant impôt. Excluez les aides au logement.
                                    </p>
                                    <div className="relative">
                                        <input
                                            type="number"
                                            value={userProfile.work.salary_monthly_gross || ''}
                                            onChange={(e) => updateProfile({ work: { ...userProfile.work, salary_monthly_gross: parseInt(e.target.value) || 0 } })}
                                            className="w-full p-6 bg-white border-2 border-slate-200 rounded-2xl focus:border-emerald-500 focus:ring-4 focus:ring-emerald-50 transition-all outline-none font-black text-3xl text-slate-900"
                                            placeholder="0"
                                        />
                                        <span className="absolute right-8 top-1/2 -translate-y-1/2 text-3xl font-black text-slate-400">€</span>
                                    </div>
                                </div>
                            </div>
                        )}
                    </div>
                </div>
            )}

            {/* SCREEN 3: LE LOGEMENT */}
            {subStep === 3 && (
                <div className="space-y-10 animate-in fade-in slide-in-from-right-8 duration-500">
                    <div className="text-center space-y-4">
                        <div className="inline-flex p-4 bg-indigo-50 text-indigo-600 rounded-3xl mb-2">
                            <Home className="w-10 h-10" />
                        </div>
                        <h2 className="text-4xl font-black text-slate-900 tracking-tight">Le Logement</h2>
                        <p className="text-slate-500 text-lg font-medium max-w-md mx-auto">
                            Dernière étape : les conditions d'accueil de votre famille.
                        </p>
                    </div>

                    <div className="space-y-6">
                        <label className="block text-xl font-bold text-slate-800 text-center mb-8">Avez-vous un logement à votre nom pour accueillir votre famille ?</label>
                        <div className="flex flex-col gap-4">
                            {[
                                { label: "Oui", value: 'OWNED_RENTED' },
                                { label: "Non", value: 'SEARCHING' },
                                { label: "Je ne sais pas", value: 'UNKNOWN' }
                            ].map((opt) => (
                                <button
                                    key={opt.value}
                                    onClick={() => handleUpdate({ housing_status: opt.value as any })}
                                    className={cn(
                                        "p-8 rounded-[2.5rem] border-2 text-center transition-all font-black text-2xl group relative overflow-hidden",
                                        family.housing_status === opt.value
                                            ? "bg-indigo-600 border-indigo-600 text-white shadow-2xl shadow-indigo-100"
                                            : "bg-white border-slate-100 text-slate-600 hover:border-indigo-200"
                                    )}
                                >
                                    {opt.label}
                                    {family.housing_status === opt.value && (
                                        <div className="absolute top-0 right-0 p-4 opacity-20">
                                            <CheckCircle2 className="w-8 h-8" />
                                        </div>
                                    )}
                                </button>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {/* NAVIGATION FOOTER */}
            <div className="pt-12 border-t border-slate-100 flex flex-col sm:flex-row items-center justify-between gap-6">
                <button
                    onClick={prevSubStep}
                    disabled={subStep === 1}
                    className={cn(
                        "flex items-center gap-2 px-8 py-4 font-bold rounded-2xl transition-all",
                        subStep === 1
                            ? "opacity-0 pointer-events-none"
                            : "text-slate-400 hover:text-slate-600 hover:bg-slate-50"
                    )}
                >
                    <ArrowLeft className="w-5 h-5" />
                    Précédent
                </button>

                <button
                    disabled={!isSubStepValid(subStep)}
                    onClick={nextSubStep}
                    className={cn(
                        "group w-full sm:w-auto px-12 py-6 rounded-[2rem] font-black text-xl transition-all flex items-center justify-center gap-4 shadow-2xl",
                        isSubStepValid(subStep)
                            ? "bg-indigo-600 text-white hover:bg-indigo-700 hover:scale-[1.02] active:scale-[0.98] shadow-indigo-200"
                            : "bg-slate-200 text-slate-400 cursor-not-allowed shadow-none"
                    )}
                >
                    {subStep === 3 ? "VOIR MON ÉLIGIBILITÉ" : "Continuer"}
                    <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                </button>
            </div>
        </div>
    );
}
