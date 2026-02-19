'use client';

import React from 'react';
import { UserProfile } from '@/types';
import { ArrowRight, Phone, MessageSquare, MapPin } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface CallbackStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

export default function CallbackStep({ userProfile, updateProfile, onNext }: CallbackStepProps) {
    const handleUpdate = (section: 'callback' | 'identity', updates: any) => {
        updateProfile({ [section]: { ...(userProfile[section] as any), ...updates } });
    };

    const isValid = () => {
        const { identity, callback } = userProfile;
        return !!identity.phone && !!identity.name && !!callback?.callback_subject && /^\d{5}$/.test(callback?.location_zip || '');
    };

    return (
        <div className="flex-1 flex flex-col p-8 md:p-12 animate-in fade-in slide-in-from-bottom-4 duration-500">
            <div className="space-y-6 animate-in fade-in zoom-in-95 duration-300">
                <div className="space-y-3 text-center mb-6">
                    <div className="w-16 h-16 bg-emerald-50 text-emerald-600 rounded-2xl flex items-center justify-center mx-auto mb-6">
                        <Phone className="w-8 h-8" />
                    </div>
                    <h2 className="text-3xl font-black text-slate-900 tracking-tight">On vous rappelle !</h2>
                    <p className="text-slate-500 font-medium max-w-md mx-auto">
                        Laissez-nous vos coordonnées, un conseiller SimuLegal vous recontacte rapidement.
                    </p>
                </div>

                <div className="max-w-md mx-auto w-full space-y-4">
                    {/* Nom */}
                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-2">Votre Nom</label>
                        <input
                            type="text"
                            value={userProfile.identity.name || ''}
                            onChange={(e) => handleUpdate('identity', { name: e.target.value })}
                            placeholder="Jean Dupont"
                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-semibold text-slate-900 focus:outline-none focus:border-emerald-600 focus:bg-white transition-all"
                        />
                    </div>

                    {/* Téléphone */}
                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-2">Votre Téléphone (Mobile)</label>
                        <input
                            type="tel"
                            value={userProfile.identity.phone || ''}
                            onChange={(e) => handleUpdate('identity', { phone: e.target.value })}
                            placeholder="06 12 34 56 78"
                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-semibold text-slate-900 focus:outline-none focus:border-emerald-600 focus:bg-white transition-all"
                        />
                    </div>

                    {/* Localisation (Code Postal) */}
                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-2 flex items-center gap-2">
                            <MapPin className="w-4 h-4 text-slate-400" /> Code Postal
                        </label>
                        <input
                            type="text"
                            maxLength={5}
                            value={userProfile.callback?.location_zip || ''}
                            onChange={(e) => handleUpdate('callback', { location_zip: e.target.value.replace(/\D/g, '') })}
                            placeholder="75000"
                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-semibold text-slate-900 focus:outline-none focus:border-emerald-600 focus:bg-white transition-all"
                        />
                    </div>

                    {/* Sujet */}
                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-2 flex items-center gap-2">
                            <MessageSquare className="w-4 h-4 text-slate-400" /> Motif de l'appel
                        </label>
                        <select
                            value={userProfile.callback?.callback_subject || ''}
                            onChange={(e) => handleUpdate('callback', { callback_subject: e.target.value })}
                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-semibold text-slate-900 focus:outline-none focus:border-emerald-600 focus:bg-white transition-all appearance-none"
                        >
                            <option value="" disabled>Sélectionnez un motif...</option>
                            <option value="INFO">Information générale</option>
                            <option value="URGENT_LEGAL">Problème Urgent</option>
                            <option value="FOLLOW_UP">Suivi de dossier</option>
                            <option value="BLOCKED">Je suis bloqué</option>
                        </select>
                    </div>
                </div>
            </div>

            <div className="mt-8 pt-6 border-t border-slate-100 text-center">
                <button
                    onClick={onNext}
                    disabled={!isValid()}
                    className={cn(
                        "w-full max-w-md mx-auto flex items-center justify-center gap-4 px-8 py-5 rounded-2xl font-black text-lg transition-all shadow-xl",
                        isValid()
                            ? "bg-slate-900 text-white hover:bg-slate-800 shadow-slate-200 hover:scale-[1.02]"
                            : "bg-slate-100 text-slate-300 cursor-not-allowed shadow-none"
                    )}
                >
                    ÊTRE RAPPELÉ GRATUITEMENT
                    <ArrowRight className="w-5 h-5" />
                </button>
                <p className="mt-4 text-xs text-slate-400 font-medium">
                    En cliquant, vous acceptez que SimuLegal vous contacte dans le cadre de votre démarche.
                </p>
            </div>
        </div>
    );
}
