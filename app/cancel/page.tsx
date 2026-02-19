'use client';

import React from 'react';
import Link from 'next/link';
import { XCircle, ArrowLeft, HelpCircle } from 'lucide-react';

export default function CancelPage() {
    return (
        <div className="min-h-screen bg-slate-50 flex flex-col items-center justify-center p-6">
            <div className="max-w-2xl w-full bg-white rounded-[3rem] shadow-2xl p-12 md:p-16 text-center relative overflow-hidden animate-in fade-in zoom-in-95 duration-500">
                <div className="absolute top-0 right-0 -mr-20 -mt-20 w-80 h-80 bg-rose-50 rounded-full blur-3xl opacity-50" />

                <div className="relative z-10 space-y-8">
                    <div className="w-24 h-24 bg-rose-50 text-rose-500 rounded-full flex items-center justify-center mx-auto shadow-lg shadow-rose-100 animate-pulse">
                        <XCircle className="w-12 h-12" />
                    </div>

                    <div className="space-y-4">
                        <h1 className="text-4xl font-black text-slate-900 tracking-tight">
                            Paiement annulé
                        </h1>
                        <p className="text-xl text-slate-500 font-medium max-w-md mx-auto">
                            Vous avez interrompu la procédure de paiement. Aucune somme n'a été débitée de votre compte.
                        </p>
                    </div>

                    <div className="bg-slate-50 border border-slate-100 rounded-3xl p-6 text-left space-y-4">
                        <div className="flex items-start gap-4">
                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-slate-400 shrink-0">
                                <HelpCircle className="w-6 h-6" />
                            </div>
                            <div>
                                <h4 className="font-bold text-slate-900">Un problème technique ?</h4>
                                <p className="text-sm text-slate-500">
                                    Si votre carte a été refusée, vérifiez le solde ou contactez votre banque. Vous pouvez réessayer avec une autre carte.
                                </p>
                            </div>
                        </div>
                    </div>

                    <div className="flex flex-col sm:flex-row gap-4 justify-center pt-4">
                        <Link
                            href="/"
                            className="px-8 py-4 bg-slate-900 text-white rounded-2xl font-black hover:bg-black transition-all shadow-xl flex items-center gap-2 justify-center"
                        >
                            <ArrowLeft className="w-5 h-5" />
                            Réessayer
                        </Link>

                        <a
                            href="mailto:support@simulegal.fr"
                            className="px-8 py-4 bg-white border-2 border-slate-100 text-slate-600 rounded-2xl font-bold hover:bg-slate-50 transition-all flex items-center gap-2 justify-center"
                        >
                            Contacter le support
                        </a>
                    </div>
                </div>
            </div>
        </div>
    );
}
