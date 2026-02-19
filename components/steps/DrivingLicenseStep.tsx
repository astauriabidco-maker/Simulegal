'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import { ArrowRight, ArrowLeft, Car, Globe, Calendar, CheckCircle2, AlertCircle } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface DrivingLicenseStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

type DrivingSubStep = 1 | 2 | 3 | 4;

export default function DrivingLicenseStep({ userProfile, updateProfile, onNext }: DrivingLicenseStepProps) {
    const [subStep, setSubStep] = useState<DrivingSubStep>(1);

    const handleUpdate = (updates: Partial<UserProfile['driving']>) => {
        updateProfile({
            driving: {
                ...userProfile.driving,
                ...updates
            }
        });
    };

    const nextSubStep = () => setSubStep((s) => (s + 1) as DrivingSubStep);
    const prevSubStep = () => setSubStep((s) => (s - 1) as DrivingSubStep);

    const parseDate = (d: string) => {
        const [m, y] = d.split('/');
        return new Date(parseInt(y), parseInt(m) - 1);
    };

    const isSubStepValid = () => {
        const { driving } = userProfile;
        if (subStep === 1) return !!driving.status;
        if (subStep === 2) return !!driving.license_country;
        if (subStep === 3) {
            if (driving.status === 'STUDENT') return true;
            return !!driving.residence_start_date && /^\d{2}\/\d{4}$/.test(driving.residence_start_date || '');
        }
        if (subStep === 4) {
            return !!driving.license_issue_date && /^\d{2}\/\d{4}$/.test(driving.license_issue_date || '');
        }
        return true;
    };

    const isPostResidenceIssue = () => {
        const { residence_start_date, license_issue_date } = userProfile.driving;
        if (!residence_start_date || !license_issue_date) return false;
        // Si étudiant, la date de résidence n'est pas pertinente de la même façon, mais affichons quand même l'alerte si post-arrivée
        // Cependant, le formulaire demande la date de résidence uniquement si NON étudiant.
        // Si étudiant, residence_start_date peut être vide.
        if (userProfile.driving.status === 'STUDENT') return false;

        try {
            return parseDate(license_issue_date) > parseDate(residence_start_date);
        } catch (e) {
            return false;
        }
    };

    const renderProgressDots = () => (
        <div className="flex gap-2 mb-8 justify-center">
            {[1, 2, 3, 4].map((i) => (
                <div
                    key={i}
                    className={cn(
                        "h-1.5 rounded-full transition-all duration-300",
                        subStep === i ? "w-8 bg-blue-600" : "w-2 bg-slate-200"
                    )}
                />
            ))}
        </div>
    );

    return (
        <div className="flex-1 flex flex-col p-8 md:p-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {renderProgressDots()}

            {subStep === 1 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-blue-50 text-blue-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Car className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Votre statut actuel</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Pour l'échange, votre titre de séjour définit les règles applicables.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {[
                            { id: 'STUDENT', label: 'Étudiant', desc: 'VLS-TS Étudiant ou Carte Étudiant' },
                            { id: 'TOURIST', label: 'Touriste', desc: 'Court séjour / Visiteur temporaire' },
                            { id: 'WORKER_VP', label: 'Salarié / Vie Privée / Autre', desc: 'Titre de séjour résidentiel' },
                            { id: 'EU_NATIONAL', label: 'Ressortissant UE', desc: 'Union Européenne / EEE / Suisse' }
                        ].map((opt) => (
                            <button
                                key={opt.id}
                                onClick={() => handleUpdate({ status: opt.id as any })}
                                className={cn(
                                    "flex flex-col p-6 rounded-2xl border-2 transition-all text-left group",
                                    userProfile.driving.status === opt.id
                                        ? "border-blue-600 bg-blue-50/50 ring-4 ring-blue-50"
                                        : "border-slate-100 hover:border-blue-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-lg font-bold mb-1 transition-colors", userProfile.driving.status === opt.id ? "text-blue-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                                <span className="text-sm text-slate-500 font-medium">{opt.desc}</span>
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 2 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-indigo-50 text-indigo-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Globe className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Pays de délivrance</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">L'échange dépend des accords bilatéraux entre la France et ce pays.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {[
                            { id: 'MAGHREB', label: 'Maroc / Algérie / Tunisie', desc: 'Accords historiques d\'échange direct' },
                            { id: 'ACCORD', label: 'Autre pays avec accord', desc: 'Sénégal, Côte d\'ivoire, etc.' },
                            { id: 'NO_ACCORD', label: 'Pays sans accord', desc: 'Inde, Chine, USA (certains états), etc.' },
                            { id: 'USA_CANADA', label: 'USA / Canada', desc: 'Vérification spécifique par État/Province' }
                        ].map((opt) => (
                            <button
                                key={opt.id}
                                onClick={() => handleUpdate({ license_country: opt.id as any })}
                                className={cn(
                                    "flex flex-col p-6 rounded-2xl border-2 transition-all text-left",
                                    userProfile.driving.license_country === opt.id
                                        ? "border-indigo-600 bg-indigo-50/50 ring-4 ring-indigo-50"
                                        : "border-slate-100 hover:border-indigo-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-lg font-bold mb-1", userProfile.driving.license_country === opt.id ? "text-indigo-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                                <span className="text-sm text-slate-500 font-medium">{opt.desc}</span>
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 3 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-emerald-50 text-emerald-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Calendar className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Début de résidence</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Date de votre premier titre de séjour (hors période étudiante).</p>
                    </div>

                    <div className="max-w-lg mx-auto w-full space-y-6">
                        {userProfile.driving.status === 'STUDENT' ? (
                            <div className="p-8 bg-blue-50 rounded-3xl border-2 border-blue-100 flex gap-6 items-start">
                                <AlertCircle className="w-8 h-8 text-blue-600 shrink-0" />
                                <div className="space-y-2">
                                    <h4 className="font-bold text-blue-900">Information Statut Étudiant</h4>
                                    <p className="text-blue-700 leading-relaxed font-medium">
                                        En tant qu'étudiant, vous n'êtes pas considéré comme "résident normal" au sens du permis de conduire. Vous pouvez conduire avec votre permis étranger valide pendant toute la durée de vos études.
                                    </p>
                                </div>
                            </div>
                        ) : (
                            <div className="space-y-4">
                                <label className="block text-sm font-black text-slate-400 uppercase tracking-widest ml-1">Mois / Année d'arrivée (Titre de séjour)</label>
                                <input
                                    type="text"
                                    placeholder="MM/YYYY"
                                    value={userProfile.driving.residence_start_date || ''}
                                    onChange={(e) => handleUpdate({ residence_start_date: e.target.value })}
                                    className="w-full p-6 bg-slate-50 border-2 border-slate-100 rounded-2xl text-xl font-bold text-slate-900 focus:border-blue-600 focus:ring-4 focus:ring-blue-50 outline-none transition-all placeholder:text-slate-300"
                                />
                                <div className="p-6 bg-amber-50 rounded-2xl border border-amber-100 flex gap-4">
                                    <AlertCircle className="w-5 h-5 text-amber-600 shrink-0 mt-0.5" />
                                    <p className="text-sm text-amber-800 font-medium leading-relaxed">
                                        Date de votre PREMIER titre de séjour (ou validation VLS-TS). Si vous étiez étudiant avant, mettez la date de votre changement de statut vers salarié/vie privée.
                                    </p>
                                </div>
                            </div>
                        )}
                    </div>
                </div>
            )}

            {subStep === 4 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-rose-50 text-rose-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Car className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Date d'obtention du permis</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Cette date détermine si votre permis est échangeable.</p>
                    </div>

                    <div className="max-w-lg mx-auto w-full space-y-6">
                        <div className="space-y-4">
                            <label className="block text-sm font-black text-slate-400 uppercase tracking-widest ml-1">Mois / Année d'obtention</label>
                            <input
                                type="text"
                                placeholder="MM/YYYY"
                                value={userProfile.driving.license_issue_date || ''}
                                onChange={(e) => handleUpdate({ license_issue_date: e.target.value })}
                                className="w-full p-6 bg-slate-50 border-2 border-slate-100 rounded-2xl text-xl font-bold text-slate-900 focus:border-rose-600 focus:ring-4 focus:ring-rose-50 outline-none transition-all placeholder:text-slate-300"
                            />

                            <div className="p-6 bg-slate-50 rounded-2xl border border-slate-100 flex gap-4">
                                <AlertCircle className="w-5 h-5 text-slate-400 shrink-0 mt-0.5" />
                                <p className="text-sm text-slate-500 font-medium leading-relaxed">
                                    Le permis doit avoir été obtenu <strong>avant</strong> votre résidence normale en France.
                                </p>
                            </div>

                            {isPostResidenceIssue() && (
                                <div className="p-6 bg-rose-50 rounded-2xl border border-rose-100 flex gap-4 animate-in fade-in slide-in-from-top-2">
                                    <AlertCircle className="w-6 h-6 text-rose-600 shrink-0 mt-0.5" />
                                    <div className="space-y-1">
                                        <p className="text-sm font-bold text-rose-800">Attention : Échange probablement impossible</p>
                                        <p className="text-sm text-rose-800 font-medium leading-relaxed">
                                            Votre permis a été obtenu <strong>après</strong> votre date d'installation en France. Sauf exception très rare, il ne sera pas échangeable et vous devrez repasser le permis français.
                                        </p>
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>
                </div>
            )}

            <div className="mt-auto pt-12 flex items-center justify-between border-t border-slate-100">
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

                <button
                    onClick={subStep === 4 ? onNext : nextSubStep}
                    disabled={!isSubStepValid()}
                    className={cn(
                        "flex items-center gap-4 px-12 py-5 rounded-2xl font-black text-lg transition-all shadow-xl",
                        isSubStepValid()
                            ? "bg-slate-900 text-white hover:bg-slate-800 shadow-slate-200"
                            : "bg-slate-100 text-slate-300 cursor-not-allowed shadow-none"
                    )}
                >
                    {subStep === 4 ? "VOIR MON ÉLIGIBILITÉ" : "Continuer"}
                    <ArrowRight className="w-5 h-5" />
                </button>
            </div>
        </div>
    );
}
