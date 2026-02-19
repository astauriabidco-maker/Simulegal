'use client';

import React from 'react';
import Link from 'next/link';
import { useSearchParams } from 'next/navigation';
import { CheckCircle2, ArrowRight, Download, Calendar, Home } from 'lucide-react';

export default function SuccessPage() {
    const searchParams = useSearchParams();
    const sessionId = searchParams.get('session_id');
    const isMock = searchParams.get('mock_payment') === 'true';

    return (
        <div className="min-h-screen bg-slate-50 flex flex-col items-center justify-center p-6">
            <div className="max-w-3xl w-full bg-white rounded-[3rem] shadow-2xl p-12 md:p-20 text-center relative overflow-hidden animate-in fade-in zoom-in-95 duration-700">
                <div className="absolute top-0 right-0 -mr-20 -mt-20 w-96 h-96 bg-emerald-50 rounded-full blur-3xl opacity-50" />
                <div className="absolute bottom-0 left-0 -ml-20 -mb-20 w-96 h-96 bg-indigo-50 rounded-full blur-3xl opacity-50" />

                <div className="relative z-10 space-y-10">
                    <div className="w-24 h-24 bg-emerald-100 text-emerald-600 rounded-full flex items-center justify-center mx-auto shadow-lg shadow-emerald-200 animate-bounce">
                        <CheckCircle2 className="w-12 h-12" />
                    </div>

                    <div className="space-y-4">
                        <h1 className="text-4xl md:text-5xl font-black text-slate-900 tracking-tight">
                            Paiement validé !
                        </h1>
                        <p className="text-xl text-slate-500 font-medium max-w-xl mx-auto">
                            Merci de votre confiance. Votre dossier a bien été enregistré et va être traité par nos équipes.
                        </p>
                    </div>

                    <div className="bg-slate-50 border border-slate-100 rounded-3xl p-8 max-w-lg mx-auto space-y-4">
                        <div className="flex justify-between items-center text-sm font-bold text-slate-400 uppercase tracking-widest border-b border-slate-200 pb-4">
                            <span>Référence transaction</span>
                            <span className="text-slate-900">{sessionId ? sessionId.slice(-8).toUpperCase() : 'PENDING'}</span>
                        </div>
                        {isMock && (
                            <div className="text-xs text-amber-500 font-bold bg-amber-50 p-2 rounded-lg">
                                ⚠️ Mode Simulation (Aucun débit réel)
                            </div>
                        )}
                        <p className="text-sm text-slate-500 leading-relaxed">
                            Un email de confirmation contenant votre facture et votre reçu de paiement vient de vous être envoyé.
                        </p>
                    </div>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4 max-w-2xl mx-auto">
                        <Link href="/mon-espace" className="group p-6 bg-indigo-600 text-white rounded-3xl hover:bg-slate-900 transition-all shadow-xl flex flex-col items-center justify-center gap-3">
                            <Calendar className="w-8 h-8 group-hover:scale-110 transition-transform" />
                            <span className="font-bold text-lg">Suivre mon dossier</span>
                            <span className="text-xs opacity-70">Accéder à mon espace client</span>
                        </Link>

                        <button onClick={() => window.print()} className="group p-6 bg-white border-2 border-slate-100 text-slate-600 rounded-3xl hover:border-slate-300 transition-all flex flex-col items-center justify-center gap-3">
                            <Download className="w-8 h-8 group-hover:scale-110 transition-transform" />
                            <span className="font-bold text-lg">Télécharger le reçu</span>
                            <span className="text-xs opacity-70">Format PDF pour vos archives</span>
                        </button>
                    </div>

                    <div className="pt-8">
                        <Link href="/" className="inline-flex items-center gap-2 text-slate-400 hover:text-slate-600 font-bold transition-colors">
                            <Home className="w-4 h-4" />
                            Retour à l'accueil
                        </Link>
                    </div>
                </div>
            </div>

            <p className="mt-8 text-center text-slate-400 text-sm font-medium">
                Besoin d'aide ? <a href="mailto:support@simulegal.fr" className="text-indigo-600 hover:underline">support@simulegal.fr</a> • +33 1 23 45 67 89
            </p>
        </div>
    );
}
