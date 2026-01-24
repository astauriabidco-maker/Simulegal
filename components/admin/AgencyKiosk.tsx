'use client';

import React, { useState, useEffect } from 'react';
import { SERVICES_CATALOG, Service } from '../../data/services';
import { AgencyExt } from '../../services/AgencyStore';
import { CRM } from '../../services/crmStore';
import {
    Flag, FileText, Users, Languages, GraduationCap,
    Phone, Car, Gavel, Calendar, ArrowLeft, Send, CheckCircle
} from 'lucide-react';
import SimulatorWrapper from '../SimulatorWrapper';

const ICON_MAP: Record<string, any> = {
    Flag, FileText, Users, Languages, GraduationCap, Phone, Car, Gavel, Calendar
};

interface AgencyKioskProps {
    agency: AgencyExt;
    onReset: () => void;
}

export default function AgencyKiosk({ agency, onReset }: AgencyKioskProps) {
    const [step, setStep] = useState<'CHOICE' | 'FORM' | 'SIMULATION' | 'SUCCESS'>('CHOICE');
    const [selectedService, setSelectedService] = useState<Service | null>(null);
    const [formData, setFormData] = useState({ name: '', email: '', phone: '' });

    const handleSelectService = (service: Service) => {
        setSelectedService(service);
        if (service.isSimulatable) {
            setStep('SIMULATION');
        } else {
            setStep('FORM');
        }
    };

    const handleSubmitForm = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!selectedService) return;

        // Création du lead direct
        await CRM.saveLead({
            ...formData,
            serviceId: selectedService.id,
            serviceName: selectedService.title,
            originAgencyId: agency.id,
            status: 'NEW'
        });

        setStep('SUCCESS');
    };

    if (step === 'SIMULATION' && selectedService) {
        return (
            <div className="fixed inset-0 bg-white z-[100] overflow-y-auto">
                <div className="p-4 border-b border-slate-100 flex items-center justify-between sticky top-0 bg-white/80 backdrop-blur-md">
                    <button onClick={() => setStep('CHOICE')} className="flex items-center gap-2 text-slate-500 font-bold">
                        <ArrowLeft size={20} /> Retour
                    </button>
                    <div className="text-center">
                        <p className="text-xs font-black text-indigo-600 uppercase italic">Borne Interactive {agency.name}</p>
                    </div>
                    <div className="w-20" />
                </div>
                <SimulatorWrapper
                    initialServiceId={selectedService.id}
                    forceAgencyId={agency.id}
                    onComplete={() => setStep('SUCCESS')}
                />
            </div>
        );
    }

    return (
        <div className="min-h-screen bg-slate-50 flex flex-col">
            {/* Header Kiosk */}
            <div className="bg-white p-8 border-b border-slate-200 text-center">
                <h1 className="text-4xl font-black text-slate-900 mb-2">Bienvenue chez SimuLegal</h1>
                <p className="text-slate-500 font-medium">Agence {agency.name} • {agency.id.slice(0, 8)}</p>
            </div>

            <div className="flex-1 p-8 max-w-6xl mx-auto w-full">
                {step === 'CHOICE' && (
                    <div className="grid grid-cols-3 gap-6">
                        {SERVICES_CATALOG.filter(s => s.id !== 'rappel').map(service => {
                            const Icon = ICON_MAP[service.iconName] || FileText;
                            return (
                                <button
                                    key={service.id}
                                    onClick={() => handleSelectService(service)}
                                    className="bg-white p-8 rounded-3xl border border-slate-200 shadow-sm hover:shadow-xl hover:border-indigo-300 transition-all text-left flex flex-col gap-4 group"
                                >
                                    <div className="w-16 h-16 bg-indigo-50 rounded-2xl flex items-center justify-center text-indigo-600 group-hover:scale-110 transition-transform">
                                        <Icon size={32} />
                                    </div>
                                    <div>
                                        <h3 className="text-xl font-black text-slate-900 mb-2">{service.title}</h3>
                                        <p className="text-sm text-slate-500 line-clamp-2 font-medium">{service.description}</p>
                                    </div>
                                    <div className="mt-auto pt-4 flex items-center justify-between">
                                        <span className="text-xs font-black text-indigo-600 border border-indigo-100 px-3 py-1 rounded-full uppercase tracking-tighter">
                                            {service.type === 'SIMULATION' ? 'Test éligibilité' : 'RDV Direct'}
                                        </span>
                                        <span className="text-slate-300 font-bold">→</span>
                                    </div>
                                </button>
                            );
                        })}
                    </div>
                )}

                {step === 'FORM' && (
                    <div className="max-w-md mx-auto bg-white p-10 rounded-[40px] shadow-2xl border border-slate-100">
                        <button onClick={() => setStep('CHOICE')} className="mb-6 text-slate-400 hover:text-slate-600 flex items-center gap-2 font-bold text-sm">
                            <ArrowLeft size={16} /> Retour
                        </button>
                        <h2 className="text-3xl font-black text-slate-900 mb-6">Vos coordonnées</h2>
                        <form onSubmit={handleSubmitForm} className="space-y-4">
                            <input
                                required
                                type="text"
                                placeholder="Nom complet"
                                value={formData.name}
                                onChange={e => setFormData({ ...formData, name: e.target.value })}
                                className="w-full p-4 bg-slate-50 border-none rounded-2xl outline-none ring-2 ring-transparent focus:ring-indigo-500 transition-all font-bold"
                            />
                            <input
                                required
                                type="email"
                                placeholder="Adresse email"
                                value={formData.email}
                                onChange={e => setFormData({ ...formData, email: e.target.value })}
                                className="w-full p-4 bg-slate-50 border-none rounded-2xl outline-none ring-2 ring-transparent focus:ring-indigo-500 transition-all font-bold"
                            />
                            <input
                                required
                                type="tel"
                                placeholder="Téléphone"
                                value={formData.phone}
                                onChange={e => setFormData({ ...formData, phone: e.target.value })}
                                className="w-full p-4 bg-slate-50 border-none rounded-2xl outline-none ring-2 ring-transparent focus:ring-indigo-500 transition-all font-bold"
                            />
                            <button
                                type="submit"
                                className="w-full p-5 bg-indigo-600 text-white rounded-2xl font-black text-lg shadow-xl shadow-indigo-200 hover:bg-indigo-700 hover:-translate-y-1 transition-all flex items-center justify-center gap-3 mt-4"
                            >
                                <Send size={24} /> Valider l'inscription
                            </button>
                        </form>
                    </div>
                )}

                {step === 'SUCCESS' && (
                    <div className="max-w-md mx-auto text-center py-20 px-10 bg-white rounded-[60px] shadow-2xl border border-slate-50">
                        <div className="w-24 h-24 bg-emerald-100 text-emerald-600 rounded-full flex items-center justify-center mx-auto mb-8 animate-bounce">
                            <CheckCircle size={48} />
                        </div>
                        <h2 className="text-4xl font-black text-slate-900 mb-4">C'est prêt !</h2>
                        <p className="text-slate-500 font-bold mb-10 leading-relaxed">
                            Votre demande a été enregistrée avec succès. Un conseiller de l'agence <span className="text-indigo-600">{agency.name}</span> va vous recevoir dans quelques instants.
                        </p>
                        <button
                            onClick={() => { setStep('CHOICE'); setFormData({ name: '', email: '', phone: '' }); }}
                            className="px-10 py-5 bg-slate-100 text-slate-700 rounded-2xl font-black hover:bg-slate-200 transition-all"
                        >
                            Terminer
                        </button>
                    </div>
                )}
            </div>

            {/* Footer Kiosk */}
            <div className="p-8 text-center text-slate-300 font-bold text-xs uppercase tracking-[0.2em]">
                SimuLegal Kiosk v2.5.0 • © {new Date().getFullYear()} Simulegal SAS
            </div>
        </div>
    );
}
