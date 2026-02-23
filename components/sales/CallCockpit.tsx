'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Device, Call } from '@twilio/voice-sdk';
import {
    Phone,
    PhoneOff,
    Mic,
    MicOff,
    User,
    Circle,
    X,
    MessageSquare,
    Clock,
    CheckCircle,
    MapPin,
    Briefcase,
    Send,
    Calendar,
    ChevronDown,
    ChevronUp,
    Flame,
    AlertTriangle,
    Link2,
    ClipboardCheck
} from 'lucide-react';
import { SalesStore, Prospect } from '../../services/SalesStore';
import { AuthStore } from '../../services/authStore';
import { SERVICES_CATALOG } from '../../data/services';

// ─── Checklist de qualification ──────────────────────────────────
interface QualificationStep {
    id: string;
    label: string;
    description: string;
    icon: string;
    category: 'IDENTITY' | 'NEED' | 'ELIGIBILITY' | 'LOCATION' | 'CLOSING';
}

const QUALIFICATION_STEPS: QualificationStep[] = [
    { id: 'confirm_identity', label: 'Confirmer l\'identité', description: 'Prénom, nom, numéro de tél correct', icon: '👤', category: 'IDENTITY' },
    { id: 'confirm_interest', label: 'Valider le besoin', description: 'Quel service l\'intéresse ? (Nationalité, séjour, permis…)', icon: '🎯', category: 'NEED' },
    { id: 'situation_overview', label: 'Situation générale', description: 'Nationalité, durée en France, situation familiale', icon: '📋', category: 'ELIGIBILITY' },
    { id: 'documents_status', label: 'État des documents', description: 'A-t-il déjà des documents ? Passeport, titre en cours ?', icon: '📁', category: 'ELIGIBILITY' },
    { id: 'collect_address', label: 'Collecter l\'adresse', description: 'Code postal → routage vers la bonne agence', icon: '📍', category: 'LOCATION' },
    { id: 'availability', label: 'Disponibilité RDV', description: 'Quand est-il disponible pour venir en agence ?', icon: '📅', category: 'CLOSING' },
    { id: 'objections', label: 'Traiter les objections', description: 'Prix, délais, concurrence, hésitations…', icon: '💬', category: 'CLOSING' },
];

// ─── Outcome d'appel ─────────────────────────────────────────────
type CallOutcome = 'QUALIFIED' | 'CALLBACK' | 'NOT_INTERESTED' | 'NO_ANSWER' | 'WRONG_NUMBER';

const CALL_OUTCOMES: { id: CallOutcome; label: string; icon: string; color: string }[] = [
    { id: 'QUALIFIED', label: 'Qualifié — Prêt pour RDV', icon: '✅', color: 'bg-emerald-50 border-emerald-200 text-emerald-700' },
    { id: 'CALLBACK', label: 'À rappeler', icon: '🔄', color: 'bg-blue-50 border-blue-200 text-blue-700' },
    { id: 'NOT_INTERESTED', label: 'Pas intéressé', icon: '❌', color: 'bg-red-50 border-red-200 text-red-700' },
    { id: 'NO_ANSWER', label: 'Pas de réponse', icon: '📵', color: 'bg-amber-50 border-amber-200 text-amber-700' },
    { id: 'WRONG_NUMBER', label: 'Mauvais numéro', icon: '⚠️', color: 'bg-slate-50 border-slate-200 text-slate-700' },
];

interface CallCockpitProps {
    prospect: Prospect;
    onClose: () => void;
    onSaveNote: (note: string) => void;
}

export default function CallCockpit({ prospect, onClose, onSaveNote }: CallCockpitProps) {
    // ─── Call State ──────────────────────────────────────────────────
    const [status, setStatus] = useState<'IDLE' | 'CALLING' | 'CONNECTED' | 'ENDED' | 'ERROR'>('IDLE');
    const [duration, setDuration] = useState(0);
    const [isMuted, setIsMuted] = useState(false);
    const [device, setDevice] = useState<Device | null>(null);
    const [activeConnection, setActiveConnection] = useState<Call | null>(null);
    const [errorMessage, setErrorMessage] = useState('');
    const [callLogId, setCallLogId] = useState<string | null>(null);

    // ─── Qualification State ────────────────────────────────────────
    const [checkedSteps, setCheckedSteps] = useState<Set<string>>(new Set());
    const [note, setNote] = useState('');
    const [selectedService, setSelectedService] = useState(prospect.interestServiceId || '');
    const [callOutcome, setCallOutcome] = useState<CallOutcome | null>(null);
    const [activeTab, setActiveTab] = useState<'CHECKLIST' | 'ADDRESS' | 'NOTES'>('CHECKLIST');

    // ─── Address Collection State ───────────────────────────────────
    const [addressForm, setAddressForm] = useState({
        address: prospect.address || '',
        zipCode: prospect.zipCode || '',
        city: prospect.city || '',
        country: prospect.country || 'France'
    });

    // ─── Computed Score ──────────────────────────────────────────────
    const qualificationProgress = Math.round((checkedSteps.size / QUALIFICATION_STEPS.length) * 100);
    const computedScore = Math.min(100, Math.round(
        (checkedSteps.size / QUALIFICATION_STEPS.length) * 60 +
        (selectedService ? 15 : 0) +
        (addressForm.zipCode ? 15 : 0) +
        (callOutcome === 'QUALIFIED' ? 10 : 0)
    ));

    // Timer ref
    const timerRef = useRef<NodeJS.Timeout | null>(null);

    // Initialisation Twilio Device
    useEffect(() => {
        const initDevice = async () => {
            try {
                const tokenResponse = await fetch('http://localhost:5000/voice/token', {
                    headers: { 'Authorization': `Bearer ${AuthStore.getToken()}` }
                });
                if (!tokenResponse.ok) throw new Error('Impossible de récupérer le token Twilio');
                const { token } = await tokenResponse.json();
                const newDevice = new Device(token, { logLevel: 1, codecPreferences: ['opus', 'pcmu'] as any });
                newDevice.on('ready', () => console.log('Twilio Device Ready'));
                newDevice.on('error', (error) => {
                    console.error('Twilio Error:', error);
                    setErrorMessage(error.message);
                    setStatus('ERROR');
                });
                setDevice(newDevice);
            } catch (err: any) {
                console.error('Setup failed', err);
                setErrorMessage(err.message);
                setStatus('ERROR');
            }
        };
        if (!prospect.phone) {
            setErrorMessage("Le prospect n'a pas de numéro de téléphone.");
            setStatus('ERROR');
        } else {
            initDevice();
        }
        return () => {
            if (device) device.destroy();
            if (activeConnection) activeConnection.disconnect();
            if (timerRef.current) clearInterval(timerRef.current);
        };
    }, [prospect.phone]);

    // Timer
    useEffect(() => {
        if (status === 'CONNECTED') {
            timerRef.current = setInterval(() => setDuration(prev => prev + 1), 1000);
        } else {
            if (timerRef.current) clearInterval(timerRef.current);
        }
    }, [status]);

    const formatDuration = (s: number) => {
        const mins = Math.floor(s / 60);
        const secs = s % 60;
        return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    };

    const handleStartCall = async () => {
        if (!device) return;
        setStatus('CALLING');
        try {
            const callLog = await SalesStore.startCall(prospect.id);
            if (callLog) setCallLogId(callLog.id);
            const params = { To: prospect.phone };
            const call = await device.connect({ params });
            call.on('accept', () => { setStatus('CONNECTED'); setActiveConnection(call); });
            call.on('disconnect', () => { setStatus('ENDED'); setActiveConnection(null); });
            call.on('error', (err) => { setErrorMessage(err.message || 'Erreur'); setStatus('ERROR'); });
        } catch (err: any) {
            setErrorMessage(err.message);
            setStatus('ERROR');
        }
    };

    const handleEndCall = () => {
        if (activeConnection) activeConnection.disconnect();
        setStatus('ENDED');
    };

    const toggleMute = () => {
        if (activeConnection) {
            const newMuted = !isMuted;
            activeConnection.mute(newMuted);
            setIsMuted(newMuted);
        }
    };

    const toggleStep = (stepId: string) => {
        setCheckedSteps(prev => {
            const next = new Set(prev);
            if (next.has(stepId)) next.delete(stepId);
            else next.add(stepId);
            return next;
        });
    };

    const handleSaveAndClose = async () => {
        // 1. Save call log
        if (callLogId) {
            await SalesStore.endCall(callLogId, duration, note, status === 'ENDED' ? 'COMPLETED' : 'FAILED');
        }

        // 2. Build update payload
        const updates: Partial<Prospect> = {};

        // Address
        if (addressForm.zipCode || addressForm.city || addressForm.address) {
            updates.address = addressForm.address || undefined;
            updates.zipCode = addressForm.zipCode || undefined;
            updates.city = addressForm.city || undefined;
            updates.country = addressForm.country || 'France';
        }

        // Service d'intérêt
        if (selectedService) {
            updates.interestServiceId = selectedService;
        }

        // Score
        updates.score = computedScore;

        // Status change based on outcome
        if (callOutcome === 'QUALIFIED') {
            updates.status = 'IN_DISCUSSION' as any;
        } else if (callOutcome === 'NOT_INTERESTED' || callOutcome === 'WRONG_NUMBER') {
            updates.status = 'LOST' as any;
        }

        // Last contact
        updates.lastContactAt = new Date().toISOString();

        // 3. Persist
        if (Object.keys(updates).length > 0) {
            await SalesStore.updateProspect(prospect.id, updates);
        }

        // 4. Save note
        if (note.trim()) {
            onSaveNote(note);
        }

        onClose();
    };

    const handleSendSimulatorLink = async () => {
        if (!prospect.phone) return;
        await SalesStore.sendSimulationLink(prospect.id, prospect.phone, prospect.firstName, 'SMS');
        setCheckedSteps(prev => new Set([...prev, 'simulator_sent']));
    };

    // ─── Services filtrés (pas le callback) ─────────────────────────
    const services = SERVICES_CATALOG.filter(s => s.id !== 'rappel_echeances');

    return (
        <div className="fixed inset-0 z-[100] flex items-center justify-center p-4 bg-slate-900/60 backdrop-blur-md animate-in fade-in duration-300">
            <div className="bg-white w-full max-w-5xl rounded-3xl shadow-2xl overflow-hidden flex flex-col lg:flex-row max-h-[90vh] animate-in zoom-in-95 duration-300">

                {/* ══════════════════════════════════════════════════
                    LEFT: Interface d'appel + Infos prospect
                   ══════════════════════════════════════════════════ */}
                <div className="w-full lg:w-[320px] bg-gradient-to-b from-slate-900 to-slate-800 p-6 flex flex-col relative flex-shrink-0">
                    <button onClick={onClose} className="absolute top-4 right-4 p-2 text-slate-400 hover:text-white transition-colors rounded-lg hover:bg-white/10">
                        <X size={18} />
                    </button>

                    {/* Prospect Info */}
                    <div className="text-center mb-6">
                        <div className="w-20 h-20 bg-gradient-to-br from-indigo-500 to-violet-600 rounded-2xl flex items-center justify-center mx-auto mb-3 text-white font-black text-2xl shadow-lg shadow-indigo-500/30">
                            {prospect.firstName[0]}{prospect.lastName[0]}
                        </div>
                        <h2 className="text-xl font-bold text-white">{prospect.firstName} {prospect.lastName}</h2>
                        <p className="text-slate-400 text-sm font-medium">{prospect.phone}</p>
                        {prospect.email && <p className="text-slate-500 text-xs truncate">{prospect.email}</p>}
                        {(prospect.city || prospect.zipCode) && (
                            <p className="text-slate-500 text-xs mt-1 flex items-center justify-center gap-1">
                                <MapPin size={10} /> {prospect.zipCode} {prospect.city}
                            </p>
                        )}
                    </div>

                    {/* Call Status & Controls */}
                    <div className="flex-1 flex flex-col items-center justify-center">
                        <div className="mb-6 text-center h-12">
                            {status === 'IDLE' && <span className="text-slate-500 text-sm italic">Prêt pour l'appel</span>}
                            {status === 'CALLING' && (
                                <span className="flex items-center gap-2 text-indigo-400 font-semibold animate-pulse text-sm">
                                    <Circle size={8} fill="currentColor" /> Appel en cours...
                                </span>
                            )}
                            {status === 'CONNECTED' && (
                                <div className="flex flex-col items-center">
                                    <span className="text-emerald-400 font-bold text-sm mb-1 flex items-center gap-2">
                                        <div className="w-2 h-2 rounded-full bg-emerald-400 animate-ping" /> Connecté
                                    </span>
                                    <span className="text-3xl font-mono text-white font-light">{formatDuration(duration)}</span>
                                </div>
                            )}
                            {status === 'ENDED' && <span className="text-slate-400 font-bold text-sm">Appel terminé — {formatDuration(duration)}</span>}
                            {status === 'ERROR' && <span className="text-red-400 font-bold text-xs max-w-[200px] truncate">{errorMessage || 'Erreur'}</span>}
                        </div>

                        <div className="flex items-center gap-4">
                            {(status === 'IDLE' || status === 'ERROR') ? (
                                <button onClick={handleStartCall} disabled={status === 'ERROR' && !device}
                                    className="w-14 h-14 bg-emerald-500 text-white rounded-full flex items-center justify-center shadow-lg shadow-emerald-500/30 hover:bg-emerald-600 transition-all hover:scale-110 active:scale-95 disabled:opacity-50">
                                    <Phone size={24} />
                                </button>
                            ) : (status === 'CONNECTED' || status === 'CALLING') ? (
                                <>
                                    <button onClick={toggleMute}
                                        className={`w-10 h-10 rounded-full flex items-center justify-center transition-all ${isMuted ? 'bg-red-500/20 text-red-400' : 'bg-white/10 text-white hover:bg-white/20'}`}>
                                        {isMuted ? <MicOff size={18} /> : <Mic size={18} />}
                                    </button>
                                    <button onClick={handleEndCall}
                                        className="w-14 h-14 bg-red-500 text-white rounded-full flex items-center justify-center shadow-lg shadow-red-500/30 hover:bg-red-600 transition-all hover:scale-110 active:scale-95">
                                        <PhoneOff size={24} />
                                    </button>
                                </>
                            ) : null}
                        </div>
                    </div>

                    {/* Score en temps réel */}
                    <div className="mt-4 bg-white/5 rounded-2xl p-4">
                        <div className="flex items-center justify-between mb-2">
                            <span className="text-xs font-bold text-slate-400 uppercase tracking-wider">Score Qualification</span>
                            <span className={`text-lg font-black ${computedScore >= 70 ? 'text-emerald-400' : computedScore >= 40 ? 'text-amber-400' : 'text-red-400'}`}>
                                {computedScore}
                            </span>
                        </div>
                        <div className="w-full h-2 bg-white/10 rounded-full overflow-hidden">
                            <div
                                className={`h-full rounded-full transition-all duration-500 ${computedScore >= 70 ? 'bg-emerald-400' : computedScore >= 40 ? 'bg-amber-400' : 'bg-red-400'}`}
                                style={{ width: `${computedScore}%` }}
                            />
                        </div>
                        <p className="text-[10px] text-slate-500 mt-2 text-center">
                            {checkedSteps.size}/{QUALIFICATION_STEPS.length} étapes • {selectedService ? '✓ Service' : '○ Service'} • {addressForm.zipCode ? '✓ CP' : '○ CP'}
                        </p>
                    </div>
                </div>

                {/* ══════════════════════════════════════════════════
                    RIGHT: Qualification Panels
                   ══════════════════════════════════════════════════ */}
                <div className="flex-1 flex flex-col min-h-0">

                    {/* Tab Bar */}
                    <div className="flex border-b border-slate-100 bg-slate-50/50">
                        {([
                            { id: 'CHECKLIST', label: 'Checklist', icon: <ClipboardCheck size={14} /> },
                            { id: 'ADDRESS', label: 'Adresse & Service', icon: <MapPin size={14} /> },
                            { id: 'NOTES', label: 'Notes & Résultat', icon: <MessageSquare size={14} /> },
                        ] as const).map(tab => (
                            <button
                                key={tab.id}
                                onClick={() => setActiveTab(tab.id)}
                                className={`flex-1 px-4 py-3 text-sm font-bold flex items-center justify-center gap-2 transition-all border-b-2 ${activeTab === tab.id
                                        ? 'border-indigo-500 text-indigo-600 bg-white'
                                        : 'border-transparent text-slate-400 hover:text-slate-600 hover:bg-white/50'
                                    }`}
                            >
                                {tab.icon} {tab.label}
                            </button>
                        ))}
                    </div>

                    {/* Tab Content */}
                    <div className="flex-1 overflow-y-auto p-5">

                        {/* ═══ TAB CHECKLIST ═══ */}
                        {activeTab === 'CHECKLIST' && (
                            <div className="space-y-2">
                                <div className="flex items-center justify-between mb-4">
                                    <h3 className="text-sm font-black text-slate-900">Checklist de qualification</h3>
                                    <span className="text-xs font-bold text-slate-400 bg-slate-100 px-2 py-1 rounded-full">
                                        {checkedSteps.size}/{QUALIFICATION_STEPS.length}
                                    </span>
                                </div>

                                {/* Progress bar */}
                                <div className="w-full h-1.5 bg-slate-100 rounded-full overflow-hidden mb-4">
                                    <div
                                        className="h-full bg-indigo-500 rounded-full transition-all duration-500"
                                        style={{ width: `${qualificationProgress}%` }}
                                    />
                                </div>

                                {['IDENTITY', 'NEED', 'ELIGIBILITY', 'LOCATION', 'CLOSING'].map(category => {
                                    const categorySteps = QUALIFICATION_STEPS.filter(s => s.category === category);
                                    const categoryLabels = {
                                        'IDENTITY': '👤 Identité',
                                        'NEED': '🎯 Besoin',
                                        'ELIGIBILITY': '📋 Éligibilité',
                                        'LOCATION': '📍 Localisation',
                                        'CLOSING': '🤝 Closing'
                                    };
                                    return (
                                        <div key={category} className="mb-3">
                                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5">
                                                {categoryLabels[category as keyof typeof categoryLabels]}
                                            </p>
                                            <div className="space-y-1">
                                                {categorySteps.map(step => (
                                                    <button
                                                        key={step.id}
                                                        onClick={() => toggleStep(step.id)}
                                                        className={`w-full text-left p-3 rounded-xl border transition-all flex items-start gap-3 group ${checkedSteps.has(step.id)
                                                                ? 'bg-emerald-50 border-emerald-200'
                                                                : 'bg-white border-slate-100 hover:border-slate-200 hover:bg-slate-50'
                                                            }`}
                                                    >
                                                        <div className={`w-5 h-5 rounded-md border-2 flex items-center justify-center flex-shrink-0 mt-0.5 transition-all ${checkedSteps.has(step.id)
                                                                ? 'bg-emerald-500 border-emerald-500 text-white'
                                                                : 'border-slate-300 group-hover:border-indigo-300'
                                                            }`}>
                                                            {checkedSteps.has(step.id) && <CheckCircle size={12} />}
                                                        </div>
                                                        <div className="flex-1 min-w-0">
                                                            <p className={`text-sm font-bold ${checkedSteps.has(step.id) ? 'text-emerald-700 line-through' : 'text-slate-700'}`}>
                                                                {step.icon} {step.label}
                                                            </p>
                                                            <p className={`text-xs ${checkedSteps.has(step.id) ? 'text-emerald-500' : 'text-slate-400'}`}>
                                                                {step.description}
                                                            </p>
                                                        </div>
                                                    </button>
                                                ))}
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>
                        )}

                        {/* ═══ TAB ADDRESS & SERVICE ═══ */}
                        {activeTab === 'ADDRESS' && (
                            <div className="space-y-6">
                                {/* Service d'intérêt */}
                                <div>
                                    <h3 className="text-sm font-black text-slate-900 mb-3 flex items-center gap-2">
                                        <Briefcase size={16} className="text-indigo-500" /> Service d'intérêt
                                    </h3>
                                    <div className="grid grid-cols-2 gap-2">
                                        {services.map(service => (
                                            <button
                                                key={service.id}
                                                onClick={() => setSelectedService(service.id)}
                                                className={`text-left p-3 rounded-xl border transition-all ${selectedService === service.id
                                                        ? 'bg-indigo-50 border-indigo-300 ring-1 ring-indigo-200'
                                                        : 'bg-white border-slate-100 hover:border-slate-200'
                                                    }`}
                                            >
                                                <p className={`text-xs font-bold truncate ${selectedService === service.id ? 'text-indigo-700' : 'text-slate-700'}`}>
                                                    {service.title}
                                                </p>
                                                {service.price !== undefined && service.price > 0 && (
                                                    <p className="text-[10px] text-slate-400 mt-0.5">{(service.price / 100).toFixed(0)}€</p>
                                                )}
                                            </button>
                                        ))}
                                    </div>
                                </div>

                                {/* Collecte d'adresse */}
                                <div>
                                    <h3 className="text-sm font-black text-slate-900 mb-3 flex items-center gap-2">
                                        <MapPin size={16} className="text-indigo-500" /> Adresse du prospect
                                    </h3>
                                    <div className="space-y-3 bg-slate-50 rounded-2xl p-4 border border-slate-100">
                                        <input
                                            type="text"
                                            value={addressForm.address}
                                            onChange={e => setAddressForm(prev => ({ ...prev, address: e.target.value }))}
                                            className="w-full px-4 py-2.5 bg-white border border-slate-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-indigo-500 text-sm"
                                            placeholder="Adresse (numéro + rue)"
                                        />
                                        <div className="grid grid-cols-3 gap-3">
                                            <input
                                                type="text"
                                                value={addressForm.zipCode}
                                                onChange={e => setAddressForm(prev => ({ ...prev, zipCode: e.target.value }))}
                                                className="px-4 py-2.5 bg-white border border-slate-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-indigo-500 text-sm font-mono"
                                                placeholder="Code postal"
                                            />
                                            <input
                                                type="text"
                                                value={addressForm.city}
                                                onChange={e => setAddressForm(prev => ({ ...prev, city: e.target.value }))}
                                                className="col-span-2 px-4 py-2.5 bg-white border border-slate-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-indigo-500 text-sm"
                                                placeholder="Ville"
                                            />
                                        </div>
                                        <input
                                            type="text"
                                            value={addressForm.country}
                                            onChange={e => setAddressForm(prev => ({ ...prev, country: e.target.value }))}
                                            className="w-full px-4 py-2.5 bg-white border border-slate-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-indigo-500 text-sm"
                                            placeholder="Pays"
                                        />
                                        {addressForm.zipCode && (
                                            <div className="flex items-center gap-2 p-3 bg-emerald-50 rounded-xl border border-emerald-100">
                                                <CheckCircle size={14} className="text-emerald-500" />
                                                <span className="text-xs font-bold text-emerald-700">
                                                    CP {addressForm.zipCode} → Routage agence possible
                                                </span>
                                            </div>
                                        )}
                                    </div>
                                </div>

                                {/* Actions rapides */}
                                <div>
                                    <h3 className="text-sm font-black text-slate-900 mb-3 flex items-center gap-2">
                                        <Send size={16} className="text-indigo-500" /> Actions rapides
                                    </h3>
                                    <div className="space-y-2">
                                        <button
                                            onClick={handleSendSimulatorLink}
                                            className="w-full text-left p-3 rounded-xl border border-slate-100 hover:border-indigo-200 hover:bg-indigo-50 transition-all flex items-center gap-3 group"
                                        >
                                            <div className="w-9 h-9 rounded-lg bg-indigo-100 flex items-center justify-center group-hover:bg-indigo-200 transition-colors">
                                                <Link2 size={16} className="text-indigo-600" />
                                            </div>
                                            <div>
                                                <p className="text-sm font-bold text-slate-700">Envoyer le lien Simulateur</p>
                                                <p className="text-xs text-slate-400">SMS au {prospect.phone}</p>
                                            </div>
                                        </button>
                                    </div>
                                </div>
                            </div>
                        )}

                        {/* ═══ TAB NOTES & RÉSULTAT ═══ */}
                        {activeTab === 'NOTES' && (
                            <div className="space-y-5">
                                {/* Compte-rendu */}
                                <div>
                                    <h3 className="text-sm font-black text-slate-900 mb-3 flex items-center gap-2">
                                        <MessageSquare size={16} className="text-indigo-500" /> Compte-rendu d'appel
                                    </h3>
                                    <textarea
                                        value={note}
                                        onChange={e => setNote(e.target.value)}
                                        placeholder="Points clés abordés, situation du prospect, objections, prochaines étapes..."
                                        className="w-full h-40 p-4 bg-slate-50 border border-slate-200 rounded-2xl resize-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all outline-none text-sm text-slate-700 placeholder:text-slate-400"
                                    />
                                </div>

                                {/* Résultat de l'appel */}
                                <div>
                                    <h3 className="text-sm font-black text-slate-900 mb-3">Résultat de l'appel</h3>
                                    <div className="grid grid-cols-1 gap-2">
                                        {CALL_OUTCOMES.map(outcome => (
                                            <button
                                                key={outcome.id}
                                                onClick={() => setCallOutcome(outcome.id)}
                                                className={`text-left p-3 rounded-xl border transition-all flex items-center gap-3 ${callOutcome === outcome.id
                                                        ? `${outcome.color} ring-1 ring-current/20`
                                                        : 'bg-white border-slate-100 hover:border-slate-200 text-slate-600'
                                                    }`}
                                            >
                                                <span className="text-lg">{outcome.icon}</span>
                                                <span className="text-sm font-bold">{outcome.label}</span>
                                                {callOutcome === outcome.id && (
                                                    <CheckCircle size={16} className="ml-auto" />
                                                )}
                                            </button>
                                        ))}
                                    </div>
                                </div>
                            </div>
                        )}
                    </div>

                    {/* ─── Bottom Action Bar ─── */}
                    <div className="border-t border-slate-100 p-4 bg-white flex items-center gap-3">
                        <div className="flex-1 flex items-center gap-2 text-xs text-slate-400">
                            {duration > 0 && (
                                <span className="flex items-center gap-1">
                                    <Clock size={12} /> {formatDuration(duration)}
                                </span>
                            )}
                            {callOutcome && (
                                <span className="flex items-center gap-1">
                                    • {CALL_OUTCOMES.find(o => o.id === callOutcome)?.icon} {CALL_OUTCOMES.find(o => o.id === callOutcome)?.label}
                                </span>
                            )}
                        </div>
                        <button
                            onClick={onClose}
                            className="px-4 py-2 text-slate-500 hover:bg-slate-100 rounded-lg font-medium transition-colors text-sm"
                        >
                            Annuler
                        </button>
                        <button
                            onClick={handleSaveAndClose}
                            className="px-6 py-2.5 bg-indigo-600 text-white rounded-xl font-bold hover:bg-indigo-700 transition-colors shadow-sm text-sm flex items-center gap-2"
                        >
                            <CheckCircle size={16} />
                            Enregistrer & Fermer
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}
