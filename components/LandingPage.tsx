'use client';

import React, { useState } from 'react';
import {
    ArrowRight,
    Flag,
    FileText,
    ShieldCheck,
    GraduationCap,
    Languages,
    BookOpen,
    Scale,
    Users,
    Briefcase,
    CheckCircle2,
    Lock,
    Zap,
    X,
    Car,
    Calendar,
    Gavel
} from 'lucide-react';
import { SERVICES_CATALOG, Service } from '../data/services';

const iconMap: Record<string, any> = {
    Flag,
    FileText,
    ShieldCheck,
    GraduationCap,
    Languages,
    BookOpen,
    Scale,
    Users,
    Briefcase,
    Car,
    Calendar,
    Gavel
};

interface LandingPageProps {
    onStartSimulator: (serviceId?: string) => void;
}

export default function LandingPage({ onStartSimulator }: LandingPageProps) {
    const [showContactModal, setShowContactModal] = useState(false);
    const [selectedServiceForContact, setSelectedServiceForContact] = useState<Service | null>(null);

    const handleServiceClick = (service: Service) => {
        if (service.isSimulatable) {
            onStartSimulator(service.id);
        } else {
            setSelectedServiceForContact(service);
            setShowContactModal(true);
        }
    };

    const poles = [
        { id: 'PROCEDURES', label: '🚀 PÔLE PROCÉDURES', color: 'text-blue-600', bgColor: 'bg-blue-50' },
        { id: 'INTEGRATION', label: '🎓 PÔLE INTÉGRATION', color: 'text-indigo-600', bgColor: 'bg-indigo-50' },
        { id: 'EXPERTISE', label: '⚖️ PÔLE EXPERTISE', color: 'text-purple-600', bgColor: 'bg-purple-50' }
    ];

    return (
        <div className="min-h-screen bg-white font-sans text-slate-900 selection:bg-indigo-100 selection:text-indigo-900">
            {/* Hero Section */}
            <section className="relative pt-32 pb-32 overflow-hidden bg-gradient-to-br from-blue-900 via-blue-800 to-indigo-900">
                <div className="absolute top-0 right-0 w-1/2 h-full opacity-10 pointer-events-none">
                    <div className="absolute top-0 right-0 w-96 h-96 bg-white rounded-full blur-[100px]"></div>
                    <div className="absolute bottom-0 right-0 w-64 h-64 bg-blue-400 rounded-full blur-[80px]"></div>
                </div>

                <div className="max-w-7xl mx-auto px-6 relative z-10 text-center">
                    <div className="inline-flex items-center gap-2 px-4 py-2 bg-white/10 backdrop-blur-md text-blue-100 rounded-full text-xs font-bold uppercase tracking-widest mb-10 border border-white/20">
                        <span className="flex h-2 w-2 rounded-full bg-blue-400 animate-pulse"></span>
                        Plateforme Certifiée 2026
                    </div>

                    <h1 className="text-5xl md:text-7xl font-black leading-[1.1] tracking-tight mb-8 text-white">
                        Votre Avenir en France, <br />
                        <span className="text-transparent bg-clip-text bg-gradient-to-r from-blue-200 to-indigo-100 italic">Sécurisé et Simplifié.</span>
                    </h1>

                    <p className="text-xl md:text-2xl text-blue-100/80 leading-relaxed mb-12 max-w-3xl mx-auto font-medium">
                        De l'analyse d'éligibilité jusqu'à l'obtention de vos papiers. <br className="hidden md:block" />
                        La plateforme tout-en-un pour les étrangers en France.
                    </p>

                    <button
                        onClick={() => onStartSimulator()}
                        className="group relative inline-flex items-center gap-4 px-12 py-7 bg-orange-500 text-white text-xl font-black rounded-2xl hover:bg-orange-600 transition-all shadow-[0_20px_50px_rgba(249,115,22,0.4)] hover:shadow-[0_25px_60px_rgba(249,115,22,0.5)] hover:-translate-y-1 active:scale-95"
                    >
                        COMMENCER MON DIAGNOSTIC GRATUIT
                        <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                    </button>
                    <p className="mt-5 text-blue-200/60 text-sm font-semibold tracking-wide">
                        ⏱ ~3 minutes • 100% gratuit • Résultats instantanés
                    </p>

                    <div className="mt-20 flex flex-wrap items-center justify-center gap-10 opacity-40 grayscale contrast-125">
                        <span className="text-sm font-black text-white uppercase tracking-[0.2em] border-r border-white/20 pr-10 last:border-0 last:pr-0">CESEDA Compliance</span>
                        <span className="text-sm font-black text-white uppercase tracking-[0.2em] border-r border-white/20 pr-10 last:border-0 last:pr-0">ISO 27001 Data Protection</span>
                        <span className="text-sm font-black text-white uppercase tracking-[0.2em]">Expertise IA Juridique</span>
                    </div>
                </div>
            </section>

            {/* Services Grid Section */}
            <section className="py-32 bg-slate-50">
                <div className="max-w-7xl mx-auto px-6">
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-16">
                        {poles.map((pole) => (
                            <div key={pole.id} className="space-y-10">
                                <div className="flex items-center gap-4 border-b-2 border-slate-200 pb-6">
                                    <h2 className={`text-base font-black uppercase tracking-[0.2em] ${pole.color}`}>
                                        {pole.label}
                                    </h2>
                                </div>

                                <div className="space-y-8">
                                    {SERVICES_CATALOG.filter(s => s.pole === pole.id).map((service) => {
                                        const Icon = iconMap[service.iconName] || FileText;
                                        return (
                                            <div
                                                key={service.id}
                                                onClick={() => handleServiceClick(service)}
                                                className="group relative bg-white p-8 rounded-[2.5rem] border border-slate-100 shadow-sm hover:shadow-2xl transition-all duration-500 cursor-pointer hover:-translate-y-2 overflow-hidden"
                                            >
                                                {service.badge && (
                                                    <div className="absolute top-6 right-6 bg-orange-500 text-white text-[10px] font-black px-3 py-1.5 rounded-full uppercase tracking-widest shadow-lg shadow-orange-100">
                                                        {service.badge}
                                                    </div>
                                                )}

                                                <div className={`w-14 h-14 rounded-2xl flex items-center justify-center mb-8 transition-all duration-300 ${pole.id === 'PROCEDURES' ? 'bg-blue-50 text-blue-600 group-hover:bg-blue-600 group-hover:text-white' :
                                                    pole.id === 'INTEGRATION' ? 'bg-indigo-50 text-indigo-600 group-hover:bg-indigo-600 group-hover:text-white' :
                                                        'bg-purple-50 text-purple-600 group-hover:bg-purple-600 group-hover:text-white'
                                                    }`}>
                                                    <Icon className="w-7 h-7" />
                                                </div>

                                                <h3 className="text-xl font-bold text-slate-900 mb-4 leading-tight group-hover:text-indigo-600 transition-colors">{service.title}</h3>
                                                <p className="text-base text-slate-500 leading-relaxed font-medium mb-8">
                                                    {service.description}
                                                </p>

                                                <button className="w-full py-4 px-6 rounded-2xl border-2 border-slate-100 text-slate-600 font-bold text-sm uppercase tracking-widest group-hover:bg-slate-900 group-hover:border-slate-900 group-hover:text-white transition-all flex items-center justify-center gap-2">
                                                    {service.isSimulatable ? 'Commencer' : 'Voir l\'offre'}
                                                    <ArrowRight className="w-4 h-4 opacity-50 group-hover:opacity-100 group-hover:translate-x-1 transition-all" />
                                                </button>
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </section>

            {/* Social Proof Section */}
            <section className="bg-white py-24 border-t border-slate-100">
                <div className="max-w-7xl mx-auto px-6">
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-16 items-center text-center">
                        <div className="flex flex-col items-center gap-6 group">
                            <div className="w-16 h-16 bg-emerald-50 text-emerald-600 rounded-3xl flex items-center justify-center shadow-inner group-hover:scale-110 transition-transform duration-300">
                                <ShieldCheck className="w-8 h-8" />
                            </div>
                            <div>
                                <h4 className="font-black text-slate-900 uppercase tracking-[0.2em] text-sm mb-2">Expertise Juridique & IA</h4>
                                <p className="text-sm text-slate-400 font-bold uppercase tracking-widest">Algorithmes basés sur le CESEDA</p>
                            </div>
                        </div>

                        <div className="flex flex-col items-center gap-6 group md:border-x-2 md:border-slate-50 md:px-10">
                            <div className="w-16 h-16 bg-blue-50 text-blue-600 rounded-3xl flex items-center justify-center shadow-inner group-hover:scale-110 transition-transform duration-300">
                                <Zap className="w-8 h-8" />
                            </div>
                            <div>
                                <h4 className="font-black text-slate-900 uppercase tracking-[0.2em] text-sm mb-2">Mise à jour Loi 2026</h4>
                                <p className="text-sm text-slate-400 font-bold uppercase tracking-widest">Dernières réformes intégrées</p>
                            </div>
                        </div>

                        <div className="flex flex-col items-center gap-6 group">
                            <div className="w-16 h-16 bg-indigo-50 text-indigo-600 rounded-3xl flex items-center justify-center shadow-inner group-hover:scale-110 transition-transform duration-300">
                                <Lock className="w-8 h-8" />
                            </div>
                            <div>
                                <h4 className="font-black text-slate-900 uppercase tracking-[0.2em] text-sm mb-2">Confidentialité Totale</h4>
                                <p className="text-sm text-slate-400 font-bold uppercase tracking-widest">Données traitées localement</p>
                            </div>
                        </div>
                    </div>
                </div>
            </section>

            {/* Footer */}
            <footer className="bg-slate-900 py-20 text-white/50 text-center text-sm font-bold uppercase tracking-[0.3em]">
                <div className="max-w-7xl mx-auto px-6">
                    <div className="flex justify-center items-center gap-4 mb-10 grayscale opacity-40 hover:opacity-100 transition-opacity cursor-pointer">
                        <div className="w-10 h-10 bg-white/10 rounded-xl flex items-center justify-center text-white text-xl">S</div>
                        <span className="text-white">SimuLegal</span>
                    </div>
                    <p className="mb-4">© 2026 SimuLegal Platform. Tous droits réservés.</p>
                    <p className="text-[10px] opacity-30">Expertise non-officielle à titre indicatif • CESEDA Compliant</p>
                </div>
            </footer>

            {/* Contact Modal */}
            {showContactModal && selectedServiceForContact && (
                <div className="fixed inset-0 z-[100] flex items-center justify-center p-6 backdrop-blur-xl bg-slate-900/40 transition-all duration-500">
                    <div className="bg-white w-full max-w-xl rounded-[3.5rem] shadow-[0_50px_100px_rgba(15,23,42,0.2)] relative overflow-hidden animate-in fade-in zoom-in-95 slide-in-from-bottom-10 duration-500">
                        <button
                            onClick={() => setShowContactModal(false)}
                            className="absolute top-10 right-10 p-3 hover:bg-slate-100 rounded-full transition-colors text-slate-400 hover:text-slate-900"
                        >
                            <X className="w-6 h-6" />
                        </button>

                        <div className="p-16">
                            <div className="w-20 h-20 bg-indigo-50 text-indigo-600 rounded-[2rem] flex items-center justify-center mb-10 shadow-inner">
                                {selectedServiceForContact && React.createElement(iconMap[selectedServiceForContact.iconName] || FileText, { className: 'w-10 h-10' })}
                            </div>

                            <h2 className="text-4xl font-black text-slate-900 mb-6 leading-tight">{selectedServiceForContact.title}</h2>
                            <p className="text-xl text-slate-500 font-medium leading-relaxed mb-12">
                                Ce service nécessite une étude personnalisée par nos experts juridiques. Prévoyez un accompagnement sur mesure.
                            </p>

                            <div className="space-y-4">
                                <button className="w-full py-6 bg-indigo-600 text-white font-black rounded-[1.5rem] hover:bg-indigo-700 transition-all shadow-2xl shadow-indigo-100 flex items-center justify-center gap-4 text-lg">
                                    Prendre Rendez-vous
                                    <ArrowRight className="w-6 h-6" />
                                </button>
                                <button className="w-full py-6 bg-white text-slate-700 font-black rounded-[1.5rem] border-2 border-slate-100 hover:border-slate-900 transition-all text-lg">
                                    Être rappelé gratuitement
                                </button>
                            </div>
                        </div>

                        <div className="bg-slate-50 py-8 text-center text-[10px] font-black text-slate-400 uppercase tracking-[0.4em]">
                            Assistance Prioritaire SimuLegal Premium
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
