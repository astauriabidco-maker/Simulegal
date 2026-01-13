'use client';

import React, { useState } from 'react';
import { X, Lock, CheckCircle, FileText, CreditCard, Download, MessageCircle, ArrowRight } from 'lucide-react';
import { CRM } from '../services/crmStore';

interface CheckoutFlowProps {
    isOpen: boolean;
    onClose: () => void;
    serviceId: string;
    serviceName: string;
    price: number; // En centimes
}

export default function CheckoutFlow({ isOpen, onClose, serviceId, serviceName, price }: CheckoutFlowProps) {
    if (!isOpen) return null;

    const [step, setStep] = useState<1 | 2 | 3 | 4>(1);
    const [formData, setFormData] = useState({ name: '', email: '', phone: '' });
    const [isProcessing, setIsProcessing] = useState(false);
    const [hasAgreed, setHasAgreed] = useState(false);

    const handleNext = () => setStep((s) => (s + 1) as any);

    const handlePayment = () => {
        setIsProcessing(true);
        setTimeout(() => {
            // Simulation appel API Stripe + CRM
            CRM.saveLead({
                name: formData.name,
                email: formData.email,
                phone: formData.phone,
                serviceId: serviceId,
                amountPaid: price,
                contractSignedAt: new Date().toISOString()
            });
            setIsProcessing(false);
            setStep(4);
        }, 2000);
    };

    return (
        <div className="fixed inset-0 bg-slate-900/70 backdrop-blur-sm flex items-center justify-center z-50 p-4 animate-in fade-in duration-300">
            <div className="bg-white rounded-[2rem] shadow-2xl w-full max-w-lg overflow-hidden flex flex-col max-h-[90vh] animate-in zoom-in-95 duration-300">

                {/* Header avec Stepper */}
                <div className="bg-slate-50 p-6 border-b border-slate-100">
                    <div className="flex justify-between items-center mb-6">
                        <div>
                            <h3 className="font-black text-slate-900 text-lg uppercase tracking-tight">Finalisation de commande</h3>
                            <p className="text-xs text-slate-500 font-bold">{serviceName}</p>
                        </div>
                        <button onClick={onClose} className="w-10 h-10 bg-white rounded-full flex items-center justify-center text-slate-400 hover:text-slate-600 hover:rotate-90 transition-all border border-slate-100 shadow-sm">
                            <X size={20} />
                        </button>
                    </div>

                    <div className="flex gap-2">
                        {[1, 2, 3, 4].map(s => (
                            <div key={s} className="flex-1 h-1.5 rounded-full bg-slate-200 relative overflow-hidden">
                                <div
                                    className={`absolute inset-0 bg-indigo-600 transition-transform duration-500 ease-out origin-left ${s <= step ? 'scale-x-100' : 'scale-x-0'}`}
                                />
                            </div>
                        ))}
                    </div>
                </div>

                {/* Contenu Scrollable */}
                <div className="p-8 overflow-y-auto custom-scrollbar">

                    {/* STEP 1: IDENTITÉ */}
                    {step === 1 && (
                        <div className="space-y-6 animate-in fade-in slide-in-from-bottom-4 duration-500">
                            <div className="space-y-2">
                                <h2 className="text-2xl font-black text-slate-900">Qui êtes-vous ?</h2>
                                <p className="text-slate-500 font-medium">Ces informations serviront à établir votre contrat légal.</p>
                            </div>

                            <div className="space-y-4">
                                <div className="space-y-1.5 focus-within:translate-x-1 transition-transform">
                                    <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Nom complet</label>
                                    <input
                                        type="text" placeholder="Ex: Jean Dupont"
                                        className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:border-indigo-600 focus:bg-white outline-none transition-all font-bold"
                                        value={formData.name} onChange={e => setFormData({ ...formData, name: e.target.value })}
                                    />
                                </div>
                                <div className="space-y-1.5 focus-within:translate-x-1 transition-transform">
                                    <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Email</label>
                                    <input
                                        type="email" placeholder="Ex: jean@email.com"
                                        className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:border-indigo-600 focus:bg-white outline-none transition-all font-bold"
                                        value={formData.email} onChange={e => setFormData({ ...formData, email: e.target.value })}
                                    />
                                </div>
                                <div className="space-y-1.5 focus-within:translate-x-1 transition-transform">
                                    <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Téléphone (Mobile)</label>
                                    <input
                                        type="tel" placeholder="Ex: 06 12 34 56 78"
                                        className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:border-indigo-600 focus:bg-white outline-none transition-all font-bold"
                                        value={formData.phone} onChange={e => setFormData({ ...formData, phone: e.target.value })}
                                    />
                                </div>
                            </div>

                            <button
                                disabled={!formData.name || !formData.email || !formData.phone}
                                onClick={handleNext}
                                className="w-full bg-slate-900 text-white p-5 rounded-2xl font-black text-lg hover:bg-black transition-all shadow-xl shadow-slate-200 mt-4 disabled:opacity-30 disabled:shadow-none flex items-center justify-center gap-3 group"
                            >
                                Continuer
                                <ArrowRight size={20} className="group-hover:translate-x-1 transition-transform" />
                            </button>
                        </div>
                    )}

                    {/* STEP 2: CONTRAT */}
                    {step === 2 && (
                        <div className="space-y-6 animate-in fade-in slide-in-from-right-4 duration-500">
                            <div className="space-y-2">
                                <h2 className="text-2xl font-black text-slate-900 flex items-center gap-3">
                                    <FileText className="text-indigo-600" size={28} /> Signature du Mandat
                                </h2>
                                <p className="text-slate-500 font-medium">Mandat exclusif d'accompagnement juridique.</p>
                            </div>

                            <div className="bg-slate-50 p-6 rounded-2xl border-2 border-slate-100 text-[11px] text-slate-600 h-48 overflow-y-auto leading-relaxed custom-scrollbar bg-opacity-50">
                                <p className="font-extrabold text-slate-900 mb-4 underline uppercase">MANDAT DE PRESTATION DE SERVICE</p>
                                <p>Je soussigné(e), <strong>{formData.name}</strong>, domicilié(e) au numéro de téléphone <strong>{formData.phone}</strong>, mandate expressément la plateforme <strong>SimuLegal</strong> pour m'accompagner dans mes démarches administratives et juridiques relatives au service : <strong>{serviceName}</strong>.</p>

                                <p className="mt-4 font-bold text-slate-800">Ce mandat inclut :</p>
                                <ul className="list-disc pl-4 mt-2 space-y-1">
                                    <li>L'analyse approfondie des pièces justificatives fournies.</li>
                                    <li>La préparation du dossier technique conforme aux exigences préfectorales.</li>
                                    <li>L'assistance juridique par nos experts tout au long de la procédure.</li>
                                    <li>Le suivi et la relance auprès des autorités compétentes si nécessaire.</li>
                                </ul>

                                <p className="mt-4 italic">Le présent mandat est conclu pour une durée nécessaire à l'accomplissement des prestations. Le client reconnaît que SimuLegal agit en tant qu'assistant aux formalités.</p>

                                <p className="mt-6 font-bold text-slate-900 uppercase">Fait à Paris, le {new Date().toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' })}.</p>
                            </div>

                            <label className="flex items-start gap-4 p-5 border-2 border-slate-100 rounded-2xl cursor-pointer hover:bg-slate-50 transition-colors group">
                                <input
                                    type="checkbox"
                                    checked={hasAgreed}
                                    onChange={(e) => setHasAgreed(e.target.checked)}
                                    className="mt-1 w-6 h-6 rounded-lg text-indigo-600 border-slate-300 focus:ring-indigo-600 transition-all cursor-pointer"
                                />
                                <span className="text-xs font-bold text-slate-600 leading-snug group-hover:text-slate-900 transition-colors">
                                    Je reconnais avoir lu et accepté les <strong>CGV</strong> et je procède à la signature électronique de ce mandat.
                                </span>
                            </label>

                            <button
                                disabled={!hasAgreed}
                                onClick={handleNext}
                                className="w-full bg-slate-900 text-white p-5 rounded-2xl font-black text-lg hover:bg-black transition-all shadow-xl shadow-slate-200 flex justify-center items-center gap-3 disabled:opacity-30 disabled:shadow-none"
                            >
                                ✍️ Signer & Continuer
                            </button>
                        </div>
                    )}

                    {/* STEP 3: PAIEMENT */}
                    {step === 3 && (
                        <div className="space-y-8 animate-in fade-in slide-in-from-right-4 duration-500">
                            <div className="text-center space-y-2">
                                <p className="text-[10px] font-black tracking-widest text-slate-400 uppercase">Montant à régler</p>
                                <p className="text-6xl font-black text-slate-900">{(price / 100).toFixed(2)}<span className="text-2xl ml-1">€</span></p>
                            </div>

                            <div className="border-2 border-slate-100 rounded-3xl p-6 space-y-4 relative bg-slate-50/30 overflow-hidden">
                                <div className="absolute top-0 right-0 bg-emerald-100 text-emerald-700 text-[10px] px-3 py-1.5 rounded-bl-xl font-black flex items-center gap-1.5 uppercase tracking-wider">
                                    <Lock size={12} /> Paiement Sécurisé
                                </div>

                                <div className="space-y-4 pt-4">
                                    <div className="space-y-1.5">
                                        <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Numéro de carte</label>
                                        <div className="relative">
                                            <input type="text" placeholder="xxxx xxxx xxxx xxxx" className="w-full p-4 bg-white border-2 border-slate-100 rounded-xl font-mono text-lg outline-none focus:border-emerald-500 transition-all shadow-sm" />
                                            <div className="absolute right-4 top-1/2 -translate-y-1/2 flex gap-1">
                                                <CreditCard size={24} className="text-slate-300" />
                                            </div>
                                        </div>
                                    </div>

                                    <div className="flex gap-4">
                                        <div className="flex-1 space-y-1.5">
                                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Expiration</label>
                                            <input type="text" placeholder="MM/AA" className="w-full p-4 bg-white border-2 border-slate-100 rounded-xl font-mono text-lg outline-none focus:border-emerald-500 transition-all shadow-sm" />
                                        </div>
                                        <div className="flex-1 space-y-1.5">
                                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">CVC</label>
                                            <input type="text" placeholder="123" className="w-full p-4 bg-white border-2 border-slate-100 rounded-xl font-mono text-lg outline-none focus:border-emerald-500 transition-all shadow-sm" />
                                        </div>
                                    </div>
                                </div>
                            </div>

                            <div className="space-y-6">
                                <button
                                    onClick={handlePayment}
                                    disabled={isProcessing}
                                    className="w-full bg-emerald-600 text-white p-6 rounded-2xl font-black text-xl hover:bg-emerald-700 transition-all flex justify-center items-center gap-3 shadow-xl shadow-emerald-100 disabled:opacity-50"
                                >
                                    {isProcessing ? (
                                        <>
                                            <div className="w-6 h-6 border-4 border-white border-t-transparent rounded-full animate-spin" />
                                            Validation...
                                        </>
                                    ) : (
                                        `Payer ${(price / 100).toFixed(2)} €`
                                    )}
                                </button>
                                <p className="text-center text-slate-400 font-bold text-[10px] uppercase tracking-wider">Transaction CRYPTÉE 256 bits via Stripe</p>
                            </div>
                        </div>
                    )}

                    {/* STEP 4: SUCCÈS */}
                    {step === 4 && (
                        <div className="text-center space-y-8 py-4 animate-in fade-in zoom-in-95 duration-700">
                            <div className="relative">
                                <div className="absolute inset-0 bg-green-200 rounded-full blur-3xl opacity-20 animate-pulse" />
                                <div className="w-24 h-24 bg-green-100 text-green-600 rounded-full flex items-center justify-center mx-auto relative z-10 border-4 border-white shadow-2xl">
                                    <CheckCircle size={48} />
                                </div>
                            </div>

                            <div className="space-y-2">
                                <h2 className="text-3xl font-black text-slate-900">Félicitations !</h2>
                                <p className="text-slate-500 font-medium">Votre dossier numéro <span className="text-indigo-600 font-black">#SL-{Math.floor(Math.random() * 90000 + 10000)}</span> est maintenant ouvert.</p>
                            </div>

                            <div className="bg-slate-900 rounded-[2.5rem] p-8 text-left relative overflow-hidden shadow-2xl">
                                <div className="absolute top-0 right-0 p-8 opacity-10 text-white">
                                    <FileText size={120} />
                                </div>

                                <h4 className="font-black text-white text-xl mb-2 relative z-10">Prochaine étape : Checklist</h4>
                                <p className="text-slate-400 text-sm font-medium mb-8 relative z-10">Nous avons généré votre liste de documents personnalisée prête à l'emploi.</p>

                                <div className="space-y-4 relative z-10">
                                    <button className="w-full bg-white text-slate-900 p-5 rounded-2xl font-black flex items-center justify-center gap-3 hover:bg-slate-50 transition-all shadow-lg active:scale-95">
                                        <Download size={20} /> Télécharger (PDF)
                                    </button>
                                    <button className="w-full bg-[#25D366] text-white p-5 rounded-2xl font-black flex items-center justify-center gap-3 hover:bg-[#20bd5a] transition-all shadow-lg active:scale-95">
                                        <MessageCircle size={20} /> Recevoir sur WhatsApp
                                    </button>
                                </div>
                            </div>

                            <button
                                onClick={onClose}
                                className="text-slate-400 font-black text-xs uppercase tracking-widest hover:text-slate-600 transition-colors"
                            >
                                Fermer la fenêtre
                            </button>
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
}
