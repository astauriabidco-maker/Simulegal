'use client';

import React, { useState } from 'react';
import { AuthStore, AdminUser, UserRole } from '../../services/authStore';
import {
    Lock,
    Mail,
    Eye,
    EyeOff,
    Building2,
    Users,
    Crown,
    ArrowRight,
    AlertCircle,
    Loader2
} from 'lucide-react';

interface AdminLoginProps {
    onLoginSuccess: (user: AdminUser) => void;
}

const getRoleConfig = (roleId: string) => {
    const icons: Record<string, React.ReactNode> = {
        'SUPER_ADMIN': <Crown size={14} />, 'SUPERADMIN': <Crown size={14} />,
        'HQ_ADMIN': <Building2 size={14} />, 'HQ': <Building2 size={14} />,
        'CASE_WORKER': <Users size={14} />,
        'AGENCY_MANAGER': <Users size={14} />, 'AGENCY': <Users size={14} />,
        'SALES': <Users size={14} />,
        'KIOSK_AGENT': <Users size={14} />,
    };
    const colors: Record<string, string> = {
        'SUPER_ADMIN': 'bg-red-100 text-red-700', 'SUPERADMIN': 'bg-red-100 text-red-700',
        'HQ_ADMIN': 'bg-amber-100 text-amber-700', 'HQ': 'bg-amber-100 text-amber-700',
        'CASE_WORKER': 'bg-blue-100 text-blue-700',
        'AGENCY_MANAGER': 'bg-emerald-100 text-emerald-700', 'AGENCY': 'bg-emerald-100 text-emerald-700',
        'SALES': 'bg-violet-100 text-violet-700',
        'KIOSK_AGENT': 'bg-purple-100 text-purple-700',
    };
    const labels: Record<string, string> = {
        'SUPER_ADMIN': 'Super Admin', 'SUPERADMIN': 'Super Admin',
        'HQ_ADMIN': 'Siège', 'HQ': 'Siège',
        'CASE_WORKER': 'Juriste', 'AGENCY_MANAGER': 'Agence', 'AGENCY': 'Agence',
        'SALES': 'Commercial', 'KIOSK_AGENT': 'Kiosque',
    };
    return {
        icon: icons[roleId] || <Users size={14} />,
        color: colors[roleId] || 'bg-slate-100 text-slate-700',
        label: labels[roleId] || roleId,
    };
};

export default function AdminLogin({ onLoginSuccess }: AdminLoginProps) {
    const [email, setEmail] = useState('');
    const [password, setPassword] = useState('');
    const [showPassword, setShowPassword] = useState(false);
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState('');
    const [showDemoAccounts, setShowDemoAccounts] = useState(false);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError('');
        setIsLoading(true);

        // Simule un délai réseau
        await new Promise(r => setTimeout(r, 800));

        const result = await AuthStore.login(email, password);

        if (result.success && result.user) {
            onLoginSuccess(result.user);
        } else {
            setError(result.error || 'Erreur de connexion');
        }

        setIsLoading(false);
    };

    const handleDemoLogin = (demoEmail: string, demoPassword: string) => {
        setEmail(demoEmail);
        setPassword(demoPassword);
        setShowDemoAccounts(false);
    };

    const demoCredentials = AuthStore.getDemoCredentials();

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-indigo-900 to-purple-900 flex items-center justify-center p-4">
            <div className="w-full max-w-md">
                {/* Logo */}
                <div className="text-center mb-8">
                    <div className="w-16 h-16 bg-white rounded-2xl flex items-center justify-center mx-auto mb-4 shadow-2xl">
                        <span className="text-3xl font-black text-indigo-600">S</span>
                    </div>
                    <h1 className="text-white text-2xl font-black">SimuLegal</h1>
                    <p className="text-white/60 text-sm mt-1">Back-Office Administration</p>
                </div>

                {/* Card Login */}
                <div className="bg-white rounded-3xl shadow-2xl overflow-hidden">
                    {/* Header */}
                    <div className="p-8 pb-0">
                        <div className="w-14 h-14 bg-slate-100 rounded-2xl flex items-center justify-center mb-4">
                            <Lock className="text-slate-600" size={28} />
                        </div>
                        <h2 className="text-2xl font-black text-slate-900 mb-2">
                            Connexion
                        </h2>
                        <p className="text-slate-500 text-sm">
                            Accédez à votre espace d'administration.
                        </p>
                    </div>

                    {/* Form */}
                    <form onSubmit={handleSubmit} className="p-8 space-y-4">
                        {/* Email */}
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-2">
                                Email professionnel
                            </label>
                            <div className="relative">
                                <Mail className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={20} />
                                <input
                                    type="email"
                                    value={email}
                                    onChange={(e) => setEmail(e.target.value)}
                                    placeholder="votre.email@simulegal.fr"
                                    className="w-full pl-12 pr-4 py-4 border border-slate-200 rounded-xl text-base focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                    required
                                />
                            </div>
                        </div>

                        {/* Password */}
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-2">
                                Mot de passe
                            </label>
                            <div className="relative">
                                <Lock className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={20} />
                                <input
                                    type={showPassword ? 'text' : 'password'}
                                    value={password}
                                    onChange={(e) => setPassword(e.target.value)}
                                    placeholder="••••••••"
                                    className="w-full pl-12 pr-12 py-4 border border-slate-200 rounded-xl text-base focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                    required
                                />
                                <button
                                    type="button"
                                    onClick={() => setShowPassword(!showPassword)}
                                    className="absolute right-4 top-1/2 -translate-y-1/2 text-slate-400 hover:text-slate-600"
                                >
                                    {showPassword ? <EyeOff size={20} /> : <Eye size={20} />}
                                </button>
                            </div>
                        </div>

                        {/* Error */}
                        {error && (
                            <div className="flex items-center gap-2 text-red-600 bg-red-50 px-4 py-3 rounded-xl text-sm animate-in fade-in duration-200">
                                <AlertCircle size={18} />
                                {error}
                            </div>
                        )}

                        {/* Submit */}
                        <button
                            type="submit"
                            disabled={isLoading}
                            className="w-full bg-slate-900 hover:bg-slate-800 disabled:bg-slate-400 text-white py-4 rounded-xl font-black text-lg flex items-center justify-center gap-2 transition-all shadow-lg"
                        >
                            {isLoading ? (
                                <Loader2 className="animate-spin" size={24} />
                            ) : (
                                <>
                                    Se connecter
                                    <ArrowRight size={20} />
                                </>
                            )}
                        </button>
                    </form>

                    {/* Demo Accounts Accordion */}
                    <div className="border-t border-slate-100">
                        <button
                            onClick={() => setShowDemoAccounts(!showDemoAccounts)}
                            className="w-full p-4 text-center text-sm text-slate-500 hover:bg-slate-50 transition-colors"
                        >
                            🧪 Comptes de démonstration {showDemoAccounts ? '▲' : '▼'}
                        </button>

                        {showDemoAccounts && (
                            <div className="px-4 pb-4 space-y-2 animate-in slide-in-from-top-2 duration-200">
                                {demoCredentials.map((cred) => {
                                    const roleConfig = getRoleConfig(cred.role);
                                    return (
                                        <button
                                            key={cred.email}
                                            onClick={() => handleDemoLogin(cred.email, cred.password)}
                                            className="w-full flex items-center justify-between p-3 bg-slate-50 hover:bg-slate-100 rounded-xl transition-colors text-left"
                                        >
                                            <div>
                                                <p className="font-bold text-slate-900 text-sm">{cred.name}</p>
                                                <p className="text-xs text-slate-500 font-mono">{cred.email}</p>
                                            </div>
                                            <span className={`flex items-center gap-1 text-[10px] font-bold px-2 py-1 rounded-full ${roleConfig.color}`}>
                                                {roleConfig.icon}
                                                {roleConfig.label}
                                            </span>
                                        </button>
                                    );
                                })}
                                <p className="text-[10px] text-slate-400 text-center mt-2">
                                    Mot de passe : <code className="bg-slate-200 px-1 rounded">demo123</code>
                                </p>
                            </div>
                        )}
                    </div>
                </div>

                {/* Footer */}
                <p className="text-center text-white/40 text-xs mt-6">
                    🔒 Accès réservé au personnel autorisé
                </p>
            </div>
        </div>
    );
}
