'use client';

import React from 'react';
import { ShieldAlert, Home, ArrowLeft } from 'lucide-react';
import { useRouter } from 'next/navigation';

export default function ForbiddenPage() {
    const router = useRouter();

    return (
        <div className="min-h-screen bg-slate-50 flex items-center justify-center p-6">
            <div className="max-w-md w-full bg-white rounded-[3rem] shadow-2xl p-12 text-center space-y-8 animate-in fade-in zoom-in duration-500">
                <div className="w-24 h-24 bg-red-100 rounded-full flex items-center justify-center mx-auto text-red-600 shadow-inner">
                    <ShieldAlert size={48} />
                </div>

                <div className="space-y-2">
                    <h1 className="text-3xl font-black text-slate-900 uppercase tracking-tighter">Accès Restreint</h1>
                    <p className="text-slate-500 font-medium leading-relaxed">
                        Désolé, vous n'avez pas les permissions nécessaires pour accéder à cette section.
                        Cette tentative a été enregistrée pour des raisons de sécurité.
                    </p>
                </div>

                <div className="pt-4 flex flex-col gap-3">
                    <button
                        onClick={() => router.push('/')}
                        className="w-full h-14 bg-slate-900 text-white rounded-2xl font-black flex items-center justify-center gap-2 hover:bg-slate-800 transition-all shadow-xl"
                    >
                        <Home size={20} /> Retour à l'accueil
                    </button>
                    <button
                        onClick={() => router.back()}
                        className="w-full h-14 bg-white border-2 border-slate-100 text-slate-400 rounded-2xl font-black flex items-center justify-center gap-2 hover:bg-slate-50 transition-all"
                    >
                        <ArrowLeft size={20} /> Page précédente
                    </button>
                </div>

                <div className="pt-8 border-t border-slate-50">
                    <p className="text-[10px] font-black text-slate-300 uppercase tracking-widest">Code Erreur : 403 - FORBIDDEN</p>
                </div>
            </div>
        </div>
    );
}
