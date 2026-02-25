'use client';

import React, { useState, useEffect } from 'react';
import { CheckCircle, Send, Phone, Mail, MapPin, User, ChevronDown, Loader2, Shield, Clock, Award } from 'lucide-react';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

interface ServiceOption {
    id: string;
    label: string;
    pole: string;
}

export default function ContactPage() {
    const [services, setServices] = useState<ServiceOption[]>([]);
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [isSuccess, setIsSuccess] = useState(false);
    const [error, setError] = useState('');

    const [form, setForm] = useState({
        firstName: '',
        lastName: '',
        phone: '',
        email: '',
        city: '',
        zipCode: '',
        interestServiceId: '',
        source: 'WEBSITE',
        website: '', // Honeypot
    });

    useEffect(() => {
        fetch(`${API_URL}/lead-capture/services`)
            .then(r => r.json())
            .then(setServices)
            .catch(() => { });
    }, []);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError('');
        setIsSubmitting(true);

        try {
            const response = await fetch(`${API_URL}/lead-capture`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    ...form,
                    utmSource: new URLSearchParams(window.location.search).get('utm_source') || undefined,
                    utmMedium: new URLSearchParams(window.location.search).get('utm_medium') || undefined,
                    utmCampaign: new URLSearchParams(window.location.search).get('utm_campaign') || undefined,
                }),
            });

            const data = await response.json();

            if (data.success) {
                setIsSuccess(true);
            } else {
                setError(data.message || 'Une erreur est survenue. Veuillez réessayer.');
            }
        } catch {
            setError('Erreur de connexion. Vérifiez votre connexion internet et réessayez.');
        } finally {
            setIsSubmitting(false);
        }
    };

    if (isSuccess) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-950 via-indigo-950 to-slate-900 flex items-center justify-center p-4">
                <div className="max-w-md w-full text-center">
                    <div className="w-20 h-20 mx-auto mb-6 rounded-3xl bg-gradient-to-br from-emerald-400 to-emerald-600 flex items-center justify-center shadow-xl shadow-emerald-500/30 animate-bounce">
                        <CheckCircle size={40} className="text-white" />
                    </div>
                    <h1 className="text-3xl font-black text-white mb-3">Demande envoyée !</h1>
                    <p className="text-lg text-slate-300 mb-8 leading-relaxed">
                        Merci pour votre confiance. Un conseiller SimuLegal vous contactera
                        <strong className="text-white"> dans les plus brefs délais</strong>.
                    </p>
                    <div className="flex flex-col gap-3">
                        <a
                            href="/"
                            className="px-6 py-3 bg-white/10 text-white rounded-2xl font-bold hover:bg-white/20 transition-all backdrop-blur-sm border border-white/10"
                        >
                            Retour à l'accueil
                        </a>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-950 via-indigo-950 to-slate-900">
            {/* Hero Section */}
            <div className="max-w-6xl mx-auto px-4 pt-16 pb-8">
                <div className="text-center mb-12">
                    <div className="inline-flex items-center gap-2 px-4 py-2 bg-indigo-500/20 border border-indigo-400/30 rounded-full mb-6 backdrop-blur-sm">
                        <Shield size={14} className="text-indigo-400" />
                        <span className="text-sm font-bold text-indigo-300">Consultation gratuite • Sans engagement</span>
                    </div>
                    <h1 className="text-4xl md:text-5xl font-black text-white mb-4 leading-tight">
                        Besoin d'aide pour vos<br />
                        <span className="bg-gradient-to-r from-indigo-400 to-violet-400 bg-clip-text text-transparent">
                            démarches juridiques ?
                        </span>
                    </h1>
                    <p className="text-lg text-slate-400 max-w-2xl mx-auto leading-relaxed">
                        Remplissez ce formulaire et un conseiller spécialisé vous rappelle
                        pour une évaluation gratuite de votre situation.
                    </p>
                </div>
            </div>

            {/* Form + Trust signals */}
            <div className="max-w-6xl mx-auto px-4 pb-24">
                <div className="grid md:grid-cols-5 gap-8">
                    {/* Trust Signals */}
                    <div className="md:col-span-2 space-y-6">
                        {[
                            { icon: Clock, title: 'Réponse sous 24h', desc: 'Un conseiller vous rappelle rapidement' },
                            { icon: Shield, title: 'Données protégées', desc: 'Vos informations sont sécurisées (RGPD)' },
                            { icon: Award, title: '+2000 dossiers traités', desc: 'Expertise reconnue en droit des étrangers' },
                            { icon: Phone, title: 'Suivi personnalisé', desc: 'Un interlocuteur dédié pour votre dossier' },
                        ].map((item, i) => (
                            <div key={i} className="flex items-start gap-4 p-4 rounded-2xl bg-white/5 border border-white/10 backdrop-blur-sm">
                                <div className="w-10 h-10 rounded-xl bg-indigo-500/20 flex items-center justify-center flex-shrink-0">
                                    <item.icon size={18} className="text-indigo-400" />
                                </div>
                                <div>
                                    <h3 className="text-sm font-bold text-white">{item.title}</h3>
                                    <p className="text-xs text-slate-400 mt-0.5">{item.desc}</p>
                                </div>
                            </div>
                        ))}
                    </div>

                    {/* Form */}
                    <div className="md:col-span-3">
                        <form onSubmit={handleSubmit} className="bg-white/[0.08] backdrop-blur-xl border border-white/10 rounded-3xl p-8 shadow-2xl">
                            <h2 className="text-xl font-black text-white mb-6">📋 Vos informations</h2>

                            {error && (
                                <div className="mb-6 p-4 bg-red-500/20 border border-red-400/30 rounded-2xl text-sm text-red-300">
                                    {error}
                                </div>
                            )}

                            {/* Honeypot — hidden */}
                            <input
                                type="text"
                                name="website"
                                value={form.website}
                                onChange={e => setForm(prev => ({ ...prev, website: e.target.value }))}
                                style={{ position: 'absolute', left: '-9999px', opacity: 0 }}
                                tabIndex={-1}
                                autoComplete="off"
                            />

                            <div className="space-y-4">
                                {/* Nom / Prénom */}
                                <div className="grid grid-cols-2 gap-3">
                                    <div>
                                        <label className="block text-xs font-bold text-slate-400 mb-1.5">Prénom *</label>
                                        <div className="relative">
                                            <User size={15} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500" />
                                            <input
                                                type="text"
                                                required
                                                value={form.firstName}
                                                onChange={e => setForm(prev => ({ ...prev, firstName: e.target.value }))}
                                                className="w-full pl-10 pr-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                                placeholder="Jean"
                                            />
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-400 mb-1.5">Nom *</label>
                                        <div className="relative">
                                            <User size={15} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500" />
                                            <input
                                                type="text"
                                                required
                                                value={form.lastName}
                                                onChange={e => setForm(prev => ({ ...prev, lastName: e.target.value }))}
                                                className="w-full pl-10 pr-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                                placeholder="Dupont"
                                            />
                                        </div>
                                    </div>
                                </div>

                                {/* Téléphone */}
                                <div>
                                    <label className="block text-xs font-bold text-slate-400 mb-1.5">Téléphone *</label>
                                    <div className="relative">
                                        <Phone size={15} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500" />
                                        <input
                                            type="tel"
                                            required
                                            value={form.phone}
                                            onChange={e => setForm(prev => ({ ...prev, phone: e.target.value }))}
                                            className="w-full pl-10 pr-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                            placeholder="06 12 34 56 78"
                                        />
                                    </div>
                                </div>

                                {/* Email */}
                                <div>
                                    <label className="block text-xs font-bold text-slate-400 mb-1.5">Email</label>
                                    <div className="relative">
                                        <Mail size={15} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500" />
                                        <input
                                            type="email"
                                            value={form.email}
                                            onChange={e => setForm(prev => ({ ...prev, email: e.target.value }))}
                                            className="w-full pl-10 pr-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                            placeholder="jean.dupont@email.com"
                                        />
                                    </div>
                                </div>

                                {/* Ville / CP */}
                                <div className="grid grid-cols-3 gap-3">
                                    <div className="col-span-2">
                                        <label className="block text-xs font-bold text-slate-400 mb-1.5">Ville</label>
                                        <div className="relative">
                                            <MapPin size={15} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500" />
                                            <input
                                                type="text"
                                                value={form.city}
                                                onChange={e => setForm(prev => ({ ...prev, city: e.target.value }))}
                                                className="w-full pl-10 pr-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                                placeholder="Paris"
                                            />
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-400 mb-1.5">Code postal</label>
                                        <input
                                            type="text"
                                            value={form.zipCode}
                                            onChange={e => setForm(prev => ({ ...prev, zipCode: e.target.value }))}
                                            className="w-full px-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all"
                                            placeholder="75001"
                                        />
                                    </div>
                                </div>

                                {/* Service */}
                                <div>
                                    <label className="block text-xs font-bold text-slate-400 mb-1.5">Service qui vous intéresse</label>
                                    <div className="relative">
                                        <ChevronDown size={15} className="absolute right-3 top-1/2 -translate-y-1/2 text-slate-500 pointer-events-none" />
                                        <select
                                            value={form.interestServiceId}
                                            onChange={e => setForm(prev => ({ ...prev, interestServiceId: e.target.value }))}
                                            className="w-full px-4 py-3 bg-white/10 border border-white/10 rounded-xl text-white text-sm appearance-none focus:outline-none focus:ring-2 focus:ring-indigo-500/50 focus:border-indigo-500/50 transition-all [&>option]:bg-slate-900 [&>option]:text-white"
                                        >
                                            <option value="">— Sélectionnez un service —</option>
                                            {services.map(s => (
                                                <option key={s.id} value={s.id}>{s.label}</option>
                                            ))}
                                        </select>
                                    </div>
                                </div>
                            </div>

                            {/* Submit */}
                            <button
                                type="submit"
                                disabled={isSubmitting}
                                className="w-full mt-8 py-4 bg-gradient-to-r from-indigo-600 to-violet-600 text-white rounded-2xl font-black text-lg hover:from-indigo-700 hover:to-violet-700 transition-all shadow-lg shadow-indigo-500/30 active:scale-[0.98] disabled:opacity-50 flex items-center justify-center gap-3"
                            >
                                {isSubmitting ? (
                                    <>
                                        <Loader2 size={20} className="animate-spin" />
                                        Envoi en cours...
                                    </>
                                ) : (
                                    <>
                                        <Send size={18} />
                                        Être rappelé gratuitement
                                    </>
                                )}
                            </button>

                            <p className="text-[10px] text-slate-500 text-center mt-4 leading-relaxed">
                                En soumettant ce formulaire, vous acceptez d'être contacté par SimuLegal.
                                Vos données sont protégées conformément au RGPD.
                                <a href="/mentions-legales" className="underline hover:text-slate-400 ml-1">Politique de confidentialité</a>
                            </p>
                        </form>
                    </div>
                </div>
            </div>
        </div>
    );
}
