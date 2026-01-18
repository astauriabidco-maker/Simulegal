'use client';

import React, { useState } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import {
    Mail,
    ArrowRight,
    CheckCircle,
    AlertCircle,
    Lock,
    Sparkles,
    ExternalLink
} from 'lucide-react';

type LoginState = 'INPUT' | 'SENT' | 'ERROR';

export default function MagicLinkLogin() {
    const [email, setEmail] = useState('');
    const [state, setState] = useState<LoginState>('INPUT');
    const [isLoading, setIsLoading] = useState(false);
    const [errorMessage, setErrorMessage] = useState('');
    const [foundLeads, setFoundLeads] = useState<Lead[]>([]);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();

        if (!email.trim()) {
            setErrorMessage('Veuillez entrer votre email');
            setState('ERROR');
            return;
        }

        setIsLoading(true);

        // Simule un délai réseau
        await new Promise(resolve => setTimeout(resolve, 1000));

        // Recherche dans le CRM via le store (qui gère le mode démo/mock)
        const leads = await CRM.getAllLeadsByEmail(email);

        if (leads.length === 0) {
            setErrorMessage('Aucun dossier trouvé pour cet email. Avez-vous déjà commandé un service ?');
            setState('ERROR');
            setIsLoading(false);
            return;
        }

        // Simule l'envoi du Magic Link
        console.log(`[MAGIC LINK] 📧 Envoi du lien de connexion à ${email}`);
        console.log(`[MAGIC LINK] 🔗 Lien: https://simulegal.fr/login?token=${btoa(email)}&t=${Date.now()}`);

        setFoundLeads(leads);
        setState('SENT');
        setIsLoading(false);
    };

    const handleDevAccess = (leadId: string) => {
        window.location.href = `/espace-client?id=${leadId}`;
    };

    return (
        <div className="min-h-screen bg-gradient-to-br from-indigo-600 via-purple-600 to-pink-500 flex items-center justify-center p-4">
            <div className="w-full max-w-md">
                {/* Logo */}
                <div className="text-center mb-8">
                    <div className="w-16 h-16 bg-white rounded-2xl flex items-center justify-center mx-auto mb-4 shadow-2xl">
                        <span className="text-3xl font-black text-indigo-600">S</span>
                    </div>
                    <h1 className="text-white text-2xl font-black">SimuLegal</h1>
                </div>

                {/* Card */}
                <div className="bg-white rounded-3xl shadow-2xl overflow-hidden">
                    {/* Header */}
                    <div className="p-8 pb-0">
                        <div className="w-14 h-14 bg-indigo-100 rounded-2xl flex items-center justify-center mb-4">
                            <Lock className="text-indigo-600" size={28} />
                        </div>
                        <h2 className="text-2xl font-black text-slate-900 mb-2">
                            Accéder à mon espace
                        </h2>
                        <p className="text-slate-500 text-sm">
                            Entrez votre email pour recevoir un lien de connexion sécurisé.
                        </p>
                    </div>

                    {/* Content */}
                    <div className="p-8">
                        {state === 'INPUT' && (
                            <form onSubmit={handleSubmit} className="space-y-4">
                                <div>
                                    <label className="block text-sm font-bold text-slate-700 mb-2">
                                        Votre adresse email
                                    </label>
                                    <div className="relative">
                                        <Mail className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={20} />
                                        <input
                                            type="email"
                                            value={email}
                                            onChange={(e) => setEmail(e.target.value)}
                                            placeholder="exemple@email.com"
                                            className="w-full pl-12 pr-4 py-4 border border-slate-200 rounded-xl text-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                            autoFocus
                                        />
                                    </div>
                                </div>

                                <button
                                    type="submit"
                                    disabled={isLoading}
                                    className="w-full bg-gradient-to-r from-indigo-600 to-purple-600 hover:from-indigo-700 hover:to-purple-700 text-white py-4 rounded-xl font-black text-lg flex items-center justify-center gap-2 transition-all disabled:opacity-50 shadow-lg shadow-indigo-200"
                                >
                                    {isLoading ? (
                                        <div className="w-6 h-6 border-3 border-white border-t-transparent rounded-full animate-spin" />
                                    ) : (
                                        <>
                                            Recevoir mon lien de connexion
                                            <ArrowRight size={20} />
                                        </>
                                    )}
                                </button>
                            </form>
                        )}

                        {/* Demo Mode for Candidates */}
                        {state === 'INPUT' && (
                            <div className="mt-6 pt-6 border-t border-slate-100">
                                <div className="flex items-center gap-2 mb-4">
                                    <Sparkles className="text-amber-500" size={16} />
                                    <span className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Accès Démo par Service</span>
                                </div>
                                <div className="grid grid-cols-1 gap-2">
                                    {[
                                        { email: 'candidat@demo.fr', name: 'Jean', label: 'Naturalisation' },
                                        { email: 'marie@demo.fr', name: 'Marie', label: 'Titre de Séjour (VPF)' },
                                        { email: 'paul@demo.fr', name: 'Paul', label: 'Salarié' },
                                        { email: 'lea@demo.fr', name: 'Léa', label: 'Étudiant' },
                                        { email: 'alex@demo.fr', name: 'Alex', label: 'Passeport Talent' },
                                        { email: 'marc@demo.fr', name: 'Marc', label: 'Échange Permis' },
                                    ].map((demo) => (
                                        <button
                                            key={demo.email}
                                            onClick={async () => {
                                                setIsLoading(true);
                                                const lead = await CRM.demoLogin(demo.email);
                                                if (lead) {
                                                    window.location.href = `/espace-client?id=${lead.id}`;
                                                } else {
                                                    setErrorMessage('Échec de la connexion démo. Vérifiez que le backend est lancé.');
                                                    setState('ERROR');
                                                    setIsLoading(false);
                                                }
                                            }}
                                            className="w-full h-11 bg-slate-50 hover:bg-indigo-50 text-slate-700 hover:text-indigo-700 rounded-xl text-xs font-bold border border-slate-100 hover:border-indigo-100 transition-all flex items-center justify-between px-4 group"
                                        >
                                            <span className="flex items-center gap-2">
                                                <span className="w-6 h-6 bg-white rounded-lg flex items-center justify-center text-[10px] shadow-sm group-hover:scale-110 transition-transform">
                                                    {demo.name.charAt(0)}
                                                </span>
                                                {demo.label}
                                            </span>
                                            <ArrowRight size={14} className="opacity-0 group-hover:opacity-100 transition-opacity" />
                                        </button>
                                    ))}
                                </div>
                            </div>
                        )}

                        {state === 'SENT' && (
                            <div className="text-center space-y-6 animate-in fade-in zoom-in-95 duration-300">
                                <div className="w-20 h-20 bg-emerald-100 rounded-full flex items-center justify-center mx-auto">
                                    <CheckCircle className="text-emerald-600" size={40} />
                                </div>
                                <div>
                                    <h3 className="text-xl font-black text-slate-900 mb-2">Lien envoyé !</h3>
                                    <p className="text-slate-500 text-sm">
                                        Un lien de connexion a été envoyé à<br />
                                        <span className="font-bold text-slate-900">{email}</span>
                                    </p>
                                </div>

                                <div className="bg-slate-50 rounded-xl p-4 text-left">
                                    <p className="text-xs text-slate-400 mb-2">📬 Vérifiez votre boîte mail et cliquez sur le lien pour accéder à votre espace.</p>
                                </div>

                                {/* DEV MODE: Accès direct */}
                                <div className="border-t border-slate-200 pt-6">
                                    <div className="flex items-center gap-2 justify-center text-xs text-amber-600 mb-3">
                                        <Sparkles size={14} />
                                        <span className="font-bold">MODE DEV : Accès direct</span>
                                    </div>
                                    <div className="space-y-2">
                                        {foundLeads.map((lead) => (
                                            <button
                                                key={lead.id}
                                                onClick={() => handleDevAccess(lead.id)}
                                                className="w-full flex items-center justify-between bg-indigo-50 hover:bg-indigo-100 border border-indigo-200 rounded-xl p-3 transition-colors"
                                            >
                                                <div className="text-left">
                                                    <p className="font-bold text-indigo-900 text-sm">{lead.serviceName}</p>
                                                    <p className="text-xs text-indigo-500 font-mono">{lead.id}</p>
                                                </div>
                                                <ExternalLink className="text-indigo-600" size={16} />
                                            </button>
                                        ))}
                                    </div>
                                </div>

                                <button
                                    onClick={() => { setState('INPUT'); setEmail(''); }}
                                    className="text-slate-400 text-sm font-bold hover:text-slate-600 transition-colors"
                                >
                                    Utiliser un autre email
                                </button>
                            </div>
                        )}

                        {state === 'ERROR' && (
                            <div className="text-center space-y-6 animate-in fade-in zoom-in-95 duration-300">
                                <div className="w-20 h-20 bg-red-100 rounded-full flex items-center justify-center mx-auto">
                                    <AlertCircle className="text-red-600" size={40} />
                                </div>
                                <div>
                                    <h3 className="text-xl font-black text-slate-900 mb-2">Oups !</h3>
                                    <p className="text-slate-500 text-sm">{errorMessage}</p>
                                </div>
                                <button
                                    onClick={() => { setState('INPUT'); setErrorMessage(''); }}
                                    className="w-full bg-slate-900 hover:bg-slate-800 text-white py-3 rounded-xl font-bold transition-colors"
                                >
                                    Réessayer
                                </button>
                            </div>
                        )}
                    </div>
                </div>

                {/* Footer */}
                <p className="text-center text-white/60 text-xs mt-6">
                    🔒 Connexion sécurisée sans mot de passe
                </p>
            </div>
        </div>
    );
}
