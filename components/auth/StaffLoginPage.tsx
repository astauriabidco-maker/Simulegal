'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { AuthStore, AdminUser, UserRole } from '../../services/authStore';
import {
    Lock,
    Mail,
    Eye,
    EyeOff,
    Building2,
    Shield,
    ArrowRight,
    AlertCircle,
    Loader2,
    Users
} from 'lucide-react';

export default function StaffLoginPage() {
    const router = useRouter();
    const [email, setEmail] = useState('');
    const [password, setPassword] = useState('');
    const [showPassword, setShowPassword] = useState(false);
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState('');

    const handleQuickLogin = async (roleEmail: string, rolePass: string) => {
        setEmail(roleEmail);
        setPassword(rolePass);

        // Petit délai pour l'effet visuel avant soumission
        setTimeout(async () => {
            setError('');
            setIsLoading(true);
            try {
                const result = await AuthStore.login(roleEmail, rolePass);
                if (result.success && result.user) {
                    router.push('/admin');
                } else {
                    setError(result.error || 'Identifiants démo invalides');
                }
            } catch (err) {
                setError('Erreur lors du login démo');
            } finally {
                setIsLoading(false);
            }
        }, 100);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError('');
        setIsLoading(true);

        try {
            const result = await AuthStore.login(email, password);

            if (result.success && result.user) {
                console.log(`[STAFF-AUTH] Valid login for ${result.user.name}`);
                // Redirection selon le rôle
                if (result.user.role === 'AGENCY') {
                    router.push('/admin'); // Ou une route spécifique agence si elle existait séparément
                } else {
                    router.push('/admin');
                }
            } else {
                setError(result.error || 'Identifiants professionnels invalides');
            }
        } catch (err) {
            setError('Une erreur est survenue lors de la connexion');
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className="min-h-screen bg-[#0f172a] flex flex-col md:flex-row overflow-hidden">
            {/* Left Side: Branding / Info (Hidden on mobile) */}
            <div className="hidden md:flex md:w-1/2 bg-indigo-900 relative items-center justify-center p-12 overflow-hidden">
                <div className="absolute top-0 right-0 w-96 h-96 bg-indigo-500 rounded-full blur-[120px] opacity-20 -mr-48 -mt-48"></div>
                <div className="absolute bottom-0 left-0 w-96 h-96 bg-emerald-500 rounded-full blur-[120px] opacity-10 -ml-48 -mb-48"></div>

                <div className="relative z-10 space-y-8 max-w-lg">
                    <div className="w-20 h-20 bg-white/10 rounded-3xl flex items-center justify-center border border-white/20 backdrop-blur-xl">
                        <Shield className="text-white" size={40} />
                    </div>
                    <div className="space-y-4">
                        <h1 className="text-5xl font-black text-white tracking-tighter uppercase leading-none">
                            Portail <br /> <span className="text-indigo-400">Professionnel</span>
                        </h1>
                        <p className="text-indigo-100 text-lg font-medium leading-relaxed">
                            Espace sécurisé réservé aux agences partenaires et à l'administration du siège SimuLegal.
                        </p>
                    </div>
                    <div className="grid grid-cols-2 gap-4">
                        <div className="p-4 bg-white/5 border border-white/10 rounded-2xl">
                            <Building2 className="text-indigo-300 mb-2" size={24} />
                            <p className="text-sm font-black text-white uppercase tracking-widest">Siège HQ</p>
                            <p className="text-xs text-white/40">Gestion globale & Audit</p>
                        </div>
                        <div className="p-4 bg-white/5 border border-white/10 rounded-2xl">
                            <Users className="text-indigo-300 mb-2" size={24} />
                            <p className="text-sm font-black text-white uppercase tracking-widest">Agences</p>
                            <p className="text-xs text-white/40">Suivi dossiers locaux</p>
                        </div>
                    </div>
                </div>
            </div>

            {/* Right Side: Login Form */}
            <div className="flex-1 flex items-center justify-center p-6 md:p-12 bg-white md:rounded-l-[3rem]">
                <div className="w-full max-w-sm space-y-8 animate-in slide-in-from-right-8 duration-500">
                    <div className="md:hidden text-center mb-8">
                        <div className="w-12 h-12 bg-indigo-600 rounded-xl flex items-center justify-center mx-auto mb-4">
                            <Shield className="text-white" size={24} />
                        </div>
                        <h2 className="text-2xl font-black text-slate-900">SimuLegal Pro</h2>
                    </div>

                    <div className="space-y-2">
                        <h2 className="text-3xl font-black text-slate-900 tracking-tight">Connexion Staff</h2>
                        <p className="text-slate-500 font-medium">Entrez vos identifiants professionnels pour continuer.</p>
                    </div>

                    <form onSubmit={handleSubmit} className="space-y-6">
                        <div className="space-y-4">
                            <div>
                                <label className="block text-xs font-black text-slate-400 uppercase tracking-widest mb-2">Email Professionnel</label>
                                <div className="relative">
                                    <Mail className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={20} />
                                    <input
                                        type="email"
                                        value={email}
                                        onChange={(e) => setEmail(e.target.value)}
                                        placeholder="admin@simulegal.fr"
                                        className="w-full h-14 pl-12 pr-4 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:bg-white focus:border-indigo-600 focus:ring-4 focus:ring-indigo-100 outline-none transition-all font-bold"
                                        required
                                    />
                                </div>
                            </div>

                            <div>
                                <label className="block text-xs font-black text-slate-400 uppercase tracking-widest mb-2">Mot de passe</label>
                                <div className="relative">
                                    <Lock className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={20} />
                                    <input
                                        type={showPassword ? 'text' : 'password'}
                                        value={password}
                                        onChange={(e) => setPassword(e.target.value)}
                                        placeholder="••••••••"
                                        className="w-full h-14 pl-12 pr-12 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:bg-white focus:border-indigo-600 focus:ring-4 focus:ring-indigo-100 outline-none transition-all font-bold"
                                        required
                                    />
                                    <button
                                        type="button"
                                        onClick={() => setShowPassword(!showPassword)}
                                        className="absolute right-4 top-1/2 -translate-y-1/2 text-slate-400 hover:text-indigo-600 transition-colors"
                                    >
                                        {showPassword ? <EyeOff size={20} /> : <Eye size={20} />}
                                    </button>
                                </div>
                            </div>
                        </div>

                        {error && (
                            <div className="p-4 bg-red-50 border border-red-100 rounded-2xl flex items-center gap-3 text-red-600 animate-in fade-in duration-300">
                                <AlertCircle size={20} />
                                <p className="text-sm font-bold">{error}</p>
                            </div>
                        )}

                        <button
                            type="submit"
                            disabled={isLoading}
                            className="w-full h-16 bg-slate-900 text-white rounded-2xl font-black text-lg shadow-xl shadow-slate-200 flex items-center justify-center gap-3 hover:bg-slate-800 disabled:bg-slate-300 transition-all active:scale-95"
                        >
                            {isLoading ? (
                                <Loader2 className="animate-spin" size={24} />
                            ) : (
                                <>
                                    Authentification
                                    <ArrowRight size={20} />
                                </>
                            )}
                        </button>
                    </form>

                    {/* Quick Access (Demo Mode) */}
                    <div className="space-y-4">
                        <div className="flex items-center gap-2">
                            <div className="h-px flex-1 bg-slate-100"></div>
                            <span className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em]">Accès Rapide (Demo)</span>
                            <div className="h-px flex-1 bg-slate-100"></div>
                        </div>

                        <div className="grid grid-cols-1 gap-2">
                            <button
                                onClick={() => handleQuickLogin('super.admin@simulegal.fr', 'demo')}
                                className="w-full h-12 bg-white border-2 border-indigo-100 hover:border-indigo-600 hover:bg-indigo-50 text-indigo-900 rounded-xl text-xs font-black uppercase tracking-wider transition-all flex items-center justify-between px-4 group"
                            >
                                <span>Super Admin</span>
                                <ArrowRight size={14} className="group-hover:translate-x-1 transition-transform" />
                            </button>

                            <button
                                onClick={() => handleQuickLogin('hq.admin@simulegal.fr', 'demo')}
                                className="w-full h-12 bg-white border-2 border-emerald-100 hover:border-emerald-600 hover:bg-emerald-50 text-emerald-900 rounded-xl text-xs font-black uppercase tracking-wider transition-all flex items-center justify-between px-4 group"
                            >
                                <span>Admin Siège (HQ)</span>
                                <ArrowRight size={14} className="group-hover:translate-x-1 transition-transform" />
                            </button>

                            <button
                                onClick={() => handleQuickLogin('agency.paris@simulegal.fr', 'demo')}
                                className="w-full h-12 bg-white border-2 border-slate-100 hover:border-slate-600 hover:bg-slate-50 text-slate-900 rounded-xl text-xs font-black uppercase tracking-wider transition-all flex items-center justify-between px-4 group"
                            >
                                <span>Manager Agence (Paris)</span>
                                <ArrowRight size={14} className="group-hover:translate-x-1 transition-transform" />
                            </button>

                            <button
                                onClick={() => handleQuickLogin('relay.bordeaux@simulegal.fr', 'demo')}
                                className="w-full h-12 bg-white border-2 border-amber-100 hover:border-amber-600 hover:bg-amber-50 text-amber-900 rounded-xl text-xs font-black uppercase tracking-wider transition-all flex items-center justify-between px-4 group"
                            >
                                <span>Point Relais (Bordeaux)</span>
                                <ArrowRight size={14} className="group-hover:translate-x-1 transition-transform" />
                            </button>
                        </div>
                    </div>

                    <div className="pt-8 border-t border-slate-100 flex items-center justify-between">
                        <button
                            onClick={() => router.push('/')}
                            className="text-xs font-black text-slate-400 uppercase tracking-widest hover:text-indigo-600 transition-colors"
                        >
                            ← Retour au site
                        </button>
                        <p className="text-[10px] font-bold text-slate-300 uppercase tracking-tighter">SimuLegal Security v2.4</p>
                    </div>
                </div>
            </div>
        </div>
    );
}
