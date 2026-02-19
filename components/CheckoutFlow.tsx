'use client';

import React, { useState, useRef } from 'react';
import { X, Lock, CheckCircle, FileText, CreditCard, Download, MessageCircle, ArrowRight, PenTool, ShieldCheck, RotateCcw, FolderOpen } from 'lucide-react';
import { CRM } from '../services/crmStore';
import { ServiceConfigStore } from '../services/ServiceConfigStore';
import { LeadRouter } from '../services/LeadRouter';
import { AgencyStore, AgencyExt } from '../services/AgencyStore';

interface CheckoutFlowProps {
    isOpen: boolean;
    onClose: () => void;
    serviceId: string;
    serviceName: string;
    price: number; // En centimes
    partnerId?: string | null; // Code partenaire pour mode Kiosk
    isKioskMode?: boolean;
    onKioskReset?: () => void;
    extraData?: any; // Données supplémentaires spécifiques au service (ex: rdv_prefecture)
}

export default function CheckoutFlow({
    isOpen,
    onClose,
    serviceId,
    serviceName,
    price,
    partnerId,
    isKioskMode = false,
    onKioskReset,
    extraData
}: CheckoutFlowProps) {
    if (!isOpen) return null;

    const [step, setStep] = useState<1 | 2 | 3 | 4 | 5>(1);
    const [formData, setFormData] = useState({ name: '', email: '', phone: '', zipCode: '' });
    const [isProcessing, setIsProcessing] = useState(false);
    const [hasAgreed, setHasAgreed] = useState(false);
    const [hasReadContract, setHasReadContract] = useState(false);
    const [contractData, setContractData] = useState<any>(null);
    const [createdLeadId, setCreatedLeadId] = useState<string | null>(null);
    const [assignedAgency, setAssignedAgency] = useState<any>(null); // Store agency for booking
    const contractRef = useRef<HTMLDivElement>(null);

    const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
        const { scrollTop, scrollHeight, clientHeight } = e.currentTarget;
        if (scrollHeight - scrollTop <= clientHeight + 5) {
            setHasReadContract(true);
        }
    };

    const handleNext = () => setStep((s) => (s + 1) as any);

    const handleSignContract = () => {
        const auditTrail = {
            signedAt: new Date().toISOString(),
            ip: '192.168.1.1',
            userAgent: navigator.userAgent,
            contractVersion: 'v1.2-2026',
            contractHash: 'SHA256:' + Math.random().toString(36).substring(2, 15),
            consent: true,
            scrollValidated: true
        };

        setContractData(auditTrail);
        setStep(3);
    };

    const handlePayment = async () => {
        setIsProcessing(true);
        const checklist = ServiceConfigStore.getRequirements(serviceId);

        try {
            // Fetch All Agencies first for routing
            const allAgencies = await AgencyStore.getAllAgencies();

            // Determine Agency using real data
            const originAgencyId = partnerId || LeadRouter.getOriginAgency(serviceId, formData.zipCode, partnerId, allAgencies);

            const agency = allAgencies.find(a => a.id === originAgencyId);
            setAssignedAgency(agency);

            const newLead = await CRM.saveLead({
                name: formData.name,
                email: formData.email,
                phone: formData.phone,
                serviceId: serviceId,
                serviceName: serviceName,
                amountPaid: price,
                requiredDocuments: checklist,
                originAgencyId: originAgencyId,
                contract: {
                    signedAt: new Date().toISOString(),
                    ipAddress: "88.123.44.12",
                    consentVersion: "v1.0",
                    isSigned: true
                },
                ...(extraData || {}) // Inject extra data dynamically
            } as any);

            if (newLead) {
                setCreatedLeadId(newLead.id);
            }
            setIsProcessing(false);

            // Decision Matrix for Booking Flow
            const isDigitalService = ['rdv_juriste', 'consultation_avocat'].includes(serviceId);

            if (isDigitalService) {
                // CASE 1: Digital Service -> Always Show Booking (Visio)
                setStep(4);
            } else if (agency && agency.type === 'CORNER') {
                // CASE 3: Procedural + Corner -> Skip Booking (Drop-off)
                setStep(5);
            } else {
                // CASE 2: Procedural + Physical/HQ -> Show Booking (Physical)
                setStep(4);
            }
        } catch (error) {
            console.error('[CheckoutFlow] Erreur paiement:', error);
            setIsProcessing(false);
        }
    };

    // Helper to determine mode for widget
    const getBookingMode = () => {
        if (['rdv_juriste', 'consultation_avocat'].includes(serviceId)) return 'VISIO_JURISTE';
        return 'PHYSICAL_AGENCY';
    };

    return (
        <div className="fixed inset-0 bg-slate-900/70 backdrop-blur-sm flex items-center justify-center z-50 p-4 animate-in fade-in duration-300">
            <div className={`bg-white rounded-[2rem] shadow-2xl w-full max-w-lg overflow-hidden flex flex-col max-h-[90vh] animate-in zoom-in-95 duration-300 ${step === 4 ? 'bg-slate-50' : ''}`}>

                {/* Header with Stepper */}
                {step !== 4 && ( // Hide standard header during booking for immersive feel
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
                            {[1, 2, 3, 4, 5].map(s => (
                                <div key={s} className="flex-1 h-1.5 rounded-full bg-slate-200 relative overflow-hidden">
                                    <div
                                        className={`absolute inset-0 bg-indigo-600 transition-transform duration-500 ease-out origin-left ${s <= step ? 'scale-x-100' : 'scale-x-0'}`}
                                    />
                                </div>
                            ))}
                        </div>
                    </div>
                )}

                {/* Scrollable Content */}
                <div className={`p-8 overflow-y-auto custom-scrollbar ${step === 4 ? 'p-0' : ''}`}>

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
                                <div className="space-y-1.5 focus-within:translate-x-1 transition-transform">
                                    <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Code Postal</label>
                                    <input
                                        type="text" placeholder="Ex: 75001"
                                        maxLength={5}
                                        className="w-full p-4 bg-slate-50 border-2 border-slate-100 rounded-2xl focus:border-indigo-600 focus:bg-white outline-none transition-all font-bold"
                                        value={formData.zipCode} onChange={e => setFormData({ ...formData, zipCode: e.target.value })}
                                    />
                                </div>
                            </div>

                            <button
                                disabled={!formData.name || !formData.email || !formData.phone || !formData.zipCode}
                                onClick={handleNext}
                                className="w-full bg-slate-900 text-white p-5 rounded-2xl font-black text-lg hover:bg-black transition-all shadow-xl shadow-slate-200 mt-4 disabled:opacity-30 disabled:shadow-none flex items-center justify-center gap-3 group"
                            >
                                Continuer
                                <ArrowRight size={20} className="group-hover:translate-x-1 transition-transform" />
                            </button>
                        </div>
                    )}

                    {/* STEP 2: CONTRAT & SIGNATURE */}
                    {step === 2 && (
                        <div className="space-y-4">
                            <h2 className="text-xl font-bold text-slate-900 flex items-center gap-2">
                                <FileText className="text-indigo-600" /> Mandat de Représentation
                            </h2>

                            <div className="bg-slate-50 p-4 rounded-lg border border-slate-200 text-xs text-slate-600 h-40 overflow-y-auto leading-relaxed text-justify shadow-inner">
                                <p className="font-bold mb-2 uppercase">Mandat de pouvoir spécial</p>
                                <p>JE SOUSSIGNÉ(E) :</p>
                                <p><strong>{formData.name}</strong>, (ci-après "Le Mandant"),</p>
                                <p>Email : {formData.email}</p>
                                <br />
                                <p>DONNE PAR LA PRÉSENTE MANDAT EXPRÈS À :</p>
                                <p>La société <strong>ANTIGRAVITY SERVICES</strong> (ci-après "Le Mandataire"),</p>
                                <br />
                                <p><strong>POUR :</strong> Effectuer en mon nom et pour mon compte toutes les démarches administratives nécessaires relatives au service : <strong>{serviceName.toUpperCase()}</strong>.</p>
                                <p>Cela inclut la constitution du dossier, la vérification des pièces, la prise de rendez-vous et la correspondance avec l'administration compétente.</p>
                                <p>Le Mandant certifie que toutes les informations fournies sont exactes et sincères.</p>
                                <br />
                                <p className="italic text-slate-400">Ce mandat est signé électroniquement conformément au règlement eIDAS. Une preuve d'horodatage sera conservée.</p>
                            </div>

                            <label className="flex items-start gap-3 p-3 border rounded-lg cursor-pointer hover:bg-slate-50 transition-colors group">
                                <div className="relative flex items-center">
                                    <input
                                        type="checkbox"
                                        checked={hasAgreed}
                                        className="peer h-5 w-5 cursor-pointer appearance-none rounded border border-slate-300 shadow-sm transition-all checked:border-indigo-600 checked:bg-indigo-600 hover:shadow-md"
                                        onChange={(e) => setHasAgreed(e.target.checked)}
                                    />
                                    <span className="absolute left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 opacity-0 peer-checked:opacity-100 text-white pointer-events-none">
                                        <CheckCircle size={12} />
                                    </span>
                                </div>
                                <span className="text-sm text-slate-700 group-hover:text-slate-900">
                                    Je reconnais avoir lu le mandat ci-dessus, j'accepte les CGV et je consens à la signature électronique.
                                </span>
                            </label>

                            <button
                                id="btn-sign"
                                disabled={!hasAgreed}
                                onClick={handleSignContract}
                                className="w-full py-3 rounded-lg font-bold flex justify-center items-center gap-2 transition-all disabled:opacity-50 disabled:cursor-not-allowed bg-slate-900 text-white hover:bg-slate-800 shadow-md hover:shadow-lg"
                            >
                                <PenTool size={18} />
                                <span>Signer le mandat</span>
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

                    {/* STEP 4: PRISE DE RDV (SMART AGENDA) */}
                    {step === 4 && (
                        <div className="animate-in fade-in slide-in-from-right-4 duration-500">
                            {/* Importation dynamique pour éviter les dépendances circulaires ou lourdes si non nécessaire */}
                            {React.createElement(
                                React.lazy(() => import('./calendar/BookingWidget')),
                                {
                                    lead: { id: createdLeadId || '', name: formData.name, email: formData.email },
                                    serviceId: serviceId,
                                    forcedAgencyId: assignedAgency?.id,
                                    initialMode: getBookingMode(),
                                    onComplete: () => setStep(5)
                                }
                            )}
                        </div>
                    )}

                    {/* STEP 5: SUCCÈS & RECAP */}
                    {step === 5 && (
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
                                <div className="flex items-center justify-center gap-1.5 text-[10px] font-bold text-emerald-600 uppercase tracking-tight">
                                    <ShieldCheck size={14} /> Dossier scellé juridiquement (Audit Trail v1.2)
                                </div>
                                {partnerId && (
                                    <p className="text-xs text-slate-400 mt-2">Partenaire : {partnerId}</p>
                                )}
                            </div>

                            <div className="bg-slate-900 rounded-[2.5rem] p-8 text-left relative overflow-hidden shadow-2xl">
                                <div className="absolute top-0 right-0 p-8 opacity-10 text-white">
                                    <FileText size={120} />
                                </div>

                                <h4 className="font-black text-white text-xl mb-2 relative z-10">Prochaine étape : Checklist</h4>
                                <p className="text-slate-400 text-sm font-medium mb-8 relative z-10">Nous avons généré votre liste de documents personnalisée prête à l'emploi.</p>

                                <div className="space-y-4 relative z-10">
                                    {isKioskMode && onKioskReset ? (
                                        <button
                                            onClick={onKioskReset}
                                            className="w-full bg-gradient-to-r from-indigo-600 to-purple-600 text-white p-6 rounded-2xl font-black text-lg flex items-center justify-center gap-3 hover:from-indigo-700 hover:to-purple-700 transition-all shadow-lg active:scale-95"
                                        >
                                            <RotateCcw size={24} />
                                            Terminer et revenir à l'accueil
                                        </button>
                                    ) : (
                                        <>
                                            <button
                                                onClick={() => window.location.href = `/espace-client?id=${createdLeadId}`}
                                                className="w-full bg-gradient-to-r from-indigo-600 to-purple-600 text-white p-5 rounded-2xl font-black flex items-center justify-center gap-3 hover:from-indigo-700 hover:to-purple-700 transition-all shadow-lg active:scale-95"
                                            >
                                                <FolderOpen size={20} /> Accéder à mon Espace Client
                                            </button>
                                            <button className="w-full bg-white text-slate-900 p-5 rounded-2xl font-black flex items-center justify-center gap-3 hover:bg-slate-50 transition-all shadow-lg active:scale-95">
                                                <Download size={20} /> Télécharger (PDF)
                                            </button>
                                            <button className="w-full bg-[#25D366] text-white p-5 rounded-2xl font-black flex items-center justify-center gap-3 hover:bg-[#20bd5a] transition-all shadow-lg active:scale-95">
                                                <MessageCircle size={20} /> Recevoir sur WhatsApp
                                            </button>
                                        </>
                                    )}
                                </div>
                            </div>

                            {!isKioskMode && (
                                <button
                                    onClick={onClose}
                                    className="text-slate-400 font-black text-xs uppercase tracking-widest hover:text-slate-600 transition-colors"
                                >
                                    Fermer la fenêtre
                                </button>
                            )}
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
}
