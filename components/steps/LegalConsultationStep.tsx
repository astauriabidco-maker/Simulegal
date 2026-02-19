'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import { ArrowRight, ArrowLeft, Gavel, Monitor, MapPin, Calendar as CalendarIcon } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';
import { LEGAL_QUESTIONS } from '@/data/modules/legal';
import BookingCalendar from '../BookingCalendar';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface LegalConsultationStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

type LegalSubStep = 1 | 2 | 3;

export default function LegalConsultationStep({ userProfile, updateProfile, onNext }: LegalConsultationStepProps) {
    const [subStep, setSubStep] = useState<LegalSubStep>(1);

    const handleUpdate = (updates: Partial<UserProfile['rdv_juriste']>) => {
        updateProfile({
            rdv_juriste: {
                ...userProfile.rdv_juriste,
                ...updates
            }
        });
    };

    const nextSubStep = () => {
        setSubStep((s) => (s + 1) as LegalSubStep);
    };

    const prevSubStep = () => setSubStep((s) => (s - 1) as LegalSubStep);

    const isSubStepValid = () => {
        const { rdv_juriste } = userProfile;
        if (subStep === 1) return !!rdv_juriste.subject;
        if (subStep === 2) return !!rdv_juriste.mode;
        if (subStep === 3) return !!rdv_juriste.slotId; // Check slot
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

    const question1 = LEGAL_QUESTIONS[0];
    const question2 = LEGAL_QUESTIONS[1];

    return (
        <div className="flex-1 flex flex-col p-8 md:p-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {renderProgressDots()}

            {subStep === 1 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-indigo-50 text-indigo-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Gavel className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">{question1.label}</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Choisissez le thème qui correspond le mieux à vos besoins.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-4 max-w-lg mx-auto">
                        {question1.options.map((opt) => (
                            <button
                                key={opt.value}
                                onClick={() => handleUpdate({ subject: opt.value as any })}
                                className={cn(
                                    "flex flex-col p-5 rounded-2xl border-2 transition-all text-left",
                                    userProfile.rdv_juriste.subject === opt.value
                                        ? "border-indigo-600 bg-indigo-50/50 ring-4 ring-indigo-100"
                                        : "border-slate-100 hover:border-indigo-200 hover:bg-slate-50"
                                )}
                            >
                                <span className={cn("text-lg font-bold transition-colors", userProfile.rdv_juriste.subject === opt.value ? "text-indigo-900" : "text-slate-900")}>
                                    {opt.label}
                                </span>
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 2 && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center">
                        <div className="w-16 h-16 bg-blue-50 text-blue-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <Monitor className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">{question2.label}</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">Choisissez la modalité qui vous convient le mieux.</p>
                    </div>

                    <div className="grid grid-cols-1 gap-6 max-w-lg mx-auto">
                        {question2.options.map((opt: any) => (
                            <button
                                key={opt.value}
                                onClick={() => handleUpdate({ mode: opt.value as any })}
                                className={cn(
                                    "flex items-start gap-6 p-8 rounded-3xl border-2 transition-all text-left group",
                                    userProfile.rdv_juriste.mode === opt.value
                                        ? "border-indigo-600 bg-indigo-50/50 ring-8 ring-indigo-50"
                                        : "border-slate-100 hover:border-indigo-200 hover:bg-slate-50"
                                )}
                            >
                                <div className={cn("w-12 h-12 rounded-2xl flex items-center justify-center shrink-0 transition-colors", userProfile.rdv_juriste.mode === opt.value ? "bg-indigo-600 text-white" : "bg-slate-100 text-slate-400 group-hover:bg-indigo-100 group-hover:text-indigo-600")}>
                                    {opt.value === 'remote' ? <Monitor className="w-6 h-6" /> : <MapPin className="w-6 h-6" />}
                                </div>
                                <div className="space-y-1">
                                    <span className={cn("text-xl font-black block transition-colors", userProfile.rdv_juriste.mode === opt.value ? "text-indigo-900" : "text-slate-900")}>
                                        {opt.label}
                                    </span>
                                    {opt.description && (
                                        <span className="text-slate-500 font-medium block">
                                            {opt.description}
                                        </span>
                                    )}
                                </div>
                            </button>
                        ))}
                    </div>
                </div>
            )}

            {subStep === 3 && (
                <div className="flex-1 flex flex-col animate-in fade-in zoom-in-95 duration-300">
                    <div className="space-y-3 text-center mb-8">
                        <div className="w-16 h-16 bg-emerald-50 text-emerald-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                            <CalendarIcon className="w-8 h-8" />
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Choisissez votre créneau</h2>
                        <p className="text-slate-500 font-medium max-w-md mx-auto">
                            Sélectionnez une date et une heure pour votre consultation {userProfile.rdv_juriste.mode === 'remote' ? 'à distance' : 'en cabinet'}.
                        </p>
                    </div>

                    <div className="flex-1 overflow-hidden">
                        <BookingCalendar
                            onSelectSlot={(slot) => {
                                handleUpdate({ slotId: slot?.id, slotDate: slot?.start });
                            }}
                            selectedSlot={userProfile.rdv_juriste.slotId ? { id: userProfile.rdv_juriste.slotId } as any : null}
                        />
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
                    {subStep === 3 ? "VOIR LE TARIF / RÉSERVER" : "Continuer"}
                    <ArrowRight className="w-5 h-5" />
                </button>
            </div>
        </div>
    );
}
