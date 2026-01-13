'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import { ArrowRight, ArrowLeft, GraduationCap, MapPin, CheckCircle2, History, Scale } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';
import { CIVIC_QUESTIONS } from '@/data/modules/civic';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface CivicExamStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

type CivicSubStep = 1 | 2 | 3;

export default function CivicExamStep({ userProfile, updateProfile, onNext }: CivicExamStepProps) {
    const [subStep, setSubStep] = useState<CivicSubStep>(1);

    const handleUpdate = (updates: Partial<UserProfile['civic_exam']>) => {
        updateProfile({
            civic_exam: {
                ...userProfile.civic_exam,
                ...updates
            }
        });
    };

    const nextSubStep = () => {
        setSubStep((s) => (s + 1) as CivicSubStep);
    };

    const prevSubStep = () => setSubStep((s) => (s - 1) as CivicSubStep);

    const isSubStepValid = () => {
        const { civic_exam } = userProfile;
        if (subStep === 1) return !!civic_exam?.civic_goal;
        if (subStep === 2) return !!civic_exam?.knowledge_level;
        if (subStep === 3) return !!civic_exam?.location_zip && /^\d{5}$/.test(civic_exam.location_zip);
        return true;
    };

    const renderProgressDots = () => {
        return (
            <div className="flex gap-2 mb-8 justify-center">
                {[1, 2, 3].map((i) => (
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

    const q1 = CIVIC_QUESTIONS[0];
    const q2 = CIVIC_QUESTIONS[1];
    const q3 = CIVIC_QUESTIONS[2];

    return (
        <div className="flex-1 flex flex-col p-8 md:p-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {renderProgressDots()}

            {subStep === 1 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-indigo-50 text-indigo-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <GraduationCap className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">{q1.label}</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Votre préparation sera adaptée selon l'échéance administrative.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {q1?.options?.map((opt) => (
                            <button
                                key={opt.value}
                                onClick={() => handleUpdate({ civic_goal: opt.value as any })}
                                className={cn(
                                    "flex flex-col p-6 rounded-3xl border-2 transition-all text-left",
                                    userProfile.civic_exam?.civic_goal === opt.value
                                        ? "border-indigo-600 bg-indigo-50/50 ring-4 ring-indigo-100"
                                        : "border-slate-100 hover:border-indigo-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-xl font-bold transition-colors", userProfile.civic_exam?.civic_goal === opt.value ? "text-indigo-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                                {('description' in opt) && opt.description && (
                                    <span className="text-sm text-slate-500 mt-1">{opt.description}</span>
                                )}
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 2 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-blue-50 text-blue-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <History className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">{q2.label}</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Cela nous permet d'évaluer le volume de révisions nécessaire.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {q2?.options?.map((opt) => (
                            <button
                                key={opt.value}
                                onClick={() => handleUpdate({ knowledge_level: opt.value as any })}
                                className={cn(
                                    "flex items-center justify-between p-6 rounded-[2.5rem] border-2 transition-all text-left group",
                                    userProfile.civic_exam?.knowledge_level === opt.value
                                        ? "border-blue-600 bg-blue-50/50 ring-4 ring-blue-100"
                                        : "border-slate-100 hover:border-blue-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-xl font-bold transition-colors", userProfile.civic_exam?.knowledge_level === opt.value ? "text-blue-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                                {userProfile.civic_exam?.knowledge_level === opt.value && (
                                    <CheckCircle2 className="w-6 h-6 text-blue-600" />
                                )}
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 3 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-emerald-50 text-emerald-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <MapPin className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">{q3.label}</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">{q3.description}</p>
                    </div>

                    <div className="max-w-md mx-auto w-full">
                        <div className="relative group">
                            <input
                                type="text"
                                maxLength={5}
                                value={userProfile.civic_exam?.location_zip || ''}
                                onChange={(e) => handleUpdate({ location_zip: e.target.value.replace(/\D/g, '') })}
                                placeholder={q3.placeholder}
                                className="w-full bg-slate-50 border-2 border-slate-100 rounded-2xl px-8 py-6 text-3xl font-black text-slate-900 placeholder:text-slate-300 focus:outline-none focus:border-emerald-600 focus:bg-white focus:ring-8 focus:ring-emerald-50 transition-all text-center tracking-[0.5em]"
                            />
                        </div>
                    </div>
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

                <button
                    onClick={subStep === 3 ? onNext : nextSubStep}
                    disabled={!isSubStepValid()}
                    className={cn(
                        "flex items-center gap-4 px-12 py-5 rounded-2xl font-black text-lg transition-all shadow-xl",
                        isSubStepValid()
                            ? "bg-slate-900 text-white hover:bg-slate-800 shadow-slate-200"
                            : "bg-slate-100 text-slate-300 cursor-not-allowed shadow-none"
                    )}
                >
                    {subStep === 3 ? "VOIR LES FORMATEURS" : "Continuer"}
                    <ArrowRight className="w-5 h-5" />
                </button>
            </div>
        </div>
    );
}
