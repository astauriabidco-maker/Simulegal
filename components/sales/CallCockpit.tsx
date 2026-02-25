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
import { SalesStore, Prospect, AppointmentInfo } from '../../services/SalesStore';
import { AuthStore } from '../../services/authStore';
import { SERVICES_CATALOG } from '../../data/services';
import BookAppointmentModal from './BookAppointmentModal';

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
type CallOutcome = 'INTERESTED' | 'CALLBACK' | 'NOT_INTERESTED' | 'NO_ANSWER' | 'WRONG_NUMBER';

const CALL_OUTCOMES: { id: CallOutcome; label: string; icon: string; color: string }[] = [
    { id: 'INTERESTED', label: 'Intéressé — À fixer en RDV', icon: '✅', color: 'bg-emerald-50 border-emerald-200 text-emerald-700' },
    { id: 'CALLBACK', label: 'À rappeler', icon: '🔄', color: 'bg-blue-50 border-blue-200 text-blue-700' },
    { id: 'NOT_INTERESTED', label: 'Pas intéressé', icon: '❌', color: 'bg-red-50 border-red-200 text-red-700' },
    { id: 'NO_ANSWER', label: 'Pas de réponse', icon: '📵', color: 'bg-amber-50 border-amber-200 text-amber-700' },
    { id: 'WRONG_NUMBER', label: 'Mauvais numéro', icon: '⚠️', color: 'bg-slate-50 border-slate-200 text-slate-700' },
];


interface CallCockpitProps {
    prospect: Prospect;
    onClose: () => void;
    onSaveNote: (note: string) => void;
    onBookAppointment?: (appointment: AppointmentInfo) => void;
}

export default function CallCockpit({ prospect, onClose, onSaveNote, onBookAppointment }: CallCockpitProps) {
    // ─── Booking State (intégré dans le cockpit) ─────────────────
    const [showBooking, setShowBooking] = useState(false);
    // ─── Call State ──────────────────────────────────────────────────
    const [status, setStatus] = useState<'IDLE' | 'CALLING' | 'CONNECTED' | 'ENDED' | 'ERROR'>('IDLE');
    const [duration, setDuration] = useState(0);
    const [isMuted, setIsMuted] = useState(false);
    const [device, setDevice] = useState<Device | null>(null);
    const [activeConnection, setActiveConnection] = useState<Call | null>(null);
    const [errorMessage, setErrorMessage] = useState('');
    const [callLogId, setCallLogId] = useState<string | null>(null);
    const [isMockMode, setIsMockMode] = useState(false);

    // ─── Qualification State ────────────────────────────────────────
    const [checkedSteps, setCheckedSteps] = useState<Set<string>>(new Set());
    const [note, setNote] = useState('');
    const [selectedService, setSelectedService] = useState(prospect.interestServiceId || '');
    const [callOutcome, setCallOutcome] = useState<CallOutcome | null>(null);

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
        (callOutcome === 'INTERESTED' ? 10 : 0)
    ));

    // Timer ref
    const timerRef = useRef<NodeJS.Timeout | null>(null);

    // Initialisation Twilio Device
    useEffect(() => {
        const initDevice = async () => {
            try {
                const tokenResponse = await fetch('http://localhost:4000/voice/token', {
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
                console.warn('[CallCockpit] Twilio setup failed, enabling mock mode for testing.', err.message);
                setIsMockMode(true);
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
        setStatus('CALLING');
        try {
            const callLog = await SalesStore.startCall(prospect.id).catch(() => null);
            if (callLog && callLog.id) setCallLogId(callLog.id);

            if (isMockMode || !device) {
                setTimeout(() => {
                    setStatus('CONNECTED');
                }, 1500);
                return;
            }

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
        if (isMockMode || !device) {
            setIsMuted(!isMuted);
            return;
        }
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

    // ─── Règles métier d'abandon ──────────────────────────────────
    const MAX_NO_ANSWER = 5;
    const MAX_CALLBACKS = 3;

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

        // ─── Inférence du résultat d'appel ───────────────────────
        let finalOutcome = callOutcome;

        if (!finalOutcome) {
            if (checkedSteps.size >= QUALIFICATION_STEPS.length - 1) {
                finalOutcome = 'INTERESTED';
            } else if (checkedSteps.size > 0 || computedScore > 20) {
                finalOutcome = 'CALLBACK';
            }
        }

        // ─── Incrémentation des compteurs ────────────────────────
        const newCallAttempts = (prospect.callAttempts || 0) + 1;
        let newNoAnswerCount = prospect.noAnswerCount || 0;
        let newCallbackCount = prospect.callbackCount || 0;

        updates.callAttempts = newCallAttempts;
        updates.lastCallOutcome = finalOutcome || 'NONE';

        if (finalOutcome === 'NO_ANSWER') {
            newNoAnswerCount++;
            updates.noAnswerCount = newNoAnswerCount;
        } else {
            updates.noAnswerCount = 0;
            newNoAnswerCount = 0;
        }

        if (finalOutcome === 'CALLBACK') {
            newCallbackCount++;
            updates.callbackCount = newCallbackCount;
            updates.callbackRequestedAt = new Date().toISOString();
        }

        // ─── Détermination du statut ─────────────────────────────
        let autoLostReason = '';

        if (finalOutcome === 'NOT_INTERESTED' || finalOutcome === 'WRONG_NUMBER') {
            updates.status = 'LOST' as any;
            autoLostReason = finalOutcome === 'NOT_INTERESTED' ? 'Pas intéressé' : 'Mauvais numéro';
        } else if (newNoAnswerCount >= MAX_NO_ANSWER) {
            updates.status = 'LOST' as any;
            autoLostReason = `${newNoAnswerCount} appels sans réponse consécutifs`;
        } else if (newCallbackCount >= MAX_CALLBACKS) {
            updates.status = 'LOST' as any;
            autoLostReason = `${newCallbackCount} demandes de rappel sans suite`;
        } else if (finalOutcome === 'INTERESTED') {
            updates.status = 'CONTACTED' as any;
        } else if (finalOutcome === 'CALLBACK' || finalOutcome === 'NO_ANSWER') {
            updates.status = 'CONTACTED' as any;
        }

        // Last contact
        updates.lastContactAt = new Date().toISOString();

        // 3. Persist
        if (Object.keys(updates).length > 0) {
            await SalesStore.updateProspect(prospect.id, updates);
        }

        // 4. Save note
        let finalNote = note.trim();
        const outcomeLabel = CALL_OUTCOMES.find(o => o.id === finalOutcome)?.label || finalOutcome;

        finalNote += `\n\n--- Rapport d'Appel (tentative #${newCallAttempts}) ---`;
        if (finalOutcome) finalNote += `\nConclusion : ${outcomeLabel}`;
        if (checkedSteps.size > 0) finalNote += `\n☑️ Qualification : ${Math.round((checkedSteps.size / QUALIFICATION_STEPS.length) * 100)}% complétée.`;
        if (autoLostReason) finalNote += `\n⚠️ Auto-classé PERDU : ${autoLostReason}`;
        if (finalOutcome === 'NO_ANSWER') finalNote += `\n📵 Sans réponse (${newNoAnswerCount}/${MAX_NO_ANSWER} avant classement perdu)`;
        if (finalOutcome === 'CALLBACK') finalNote += `\n🔄 Rappel demandé (${newCallbackCount}/${MAX_CALLBACKS} avant classement perdu)`;

        if (autoLostReason) {
            alert(`⚠️ Ce prospect a été automatiquement classé PERDU.\n\nRaison : ${autoLostReason}\n\nVous pouvez le réactiver manuellement si nécessaire.`);
        }

        if (finalNote.trim()) {
            onSaveNote(finalNote.trim());
        } else {
            onSaveNote("");
        }

        onClose();
    };

    const handleSendSimulatorLink = async () => {
        if (!prospect.phone) return;
        await SalesStore.sendSimulationLink(prospect.id, prospect.phone, prospect.firstName, 'SMS');
        setCheckedSteps(prev => new Set([...prev, 'simulator_sent']));
    };

    // ─── Services filtrés ────────────────────────────────────────────
    const services = SERVICES_CATALOG.filter(s => s.id !== 'rappel_echeances');

    // ─── Category labels ─────────────────────────────────────────────
    const categoryLabels: Record<string, string> = {
        'IDENTITY': '👤 Identité',
        'NEED': '🎯 Besoin',
        'ELIGIBILITY': '📋 Éligibilité',
        'LOCATION': '📍 Localisation',
        'CLOSING': '🤝 Closing'
    };

    return (
        <div className="fixed inset-0 z-[100] flex items-center justify-center p-4 bg-slate-900/60 backdrop-blur-md animate-in fade-in duration-300">
            <div className="bg-white w-full max-w-6xl rounded-3xl shadow-2xl overflow-hidden flex flex-col lg:flex-row max-h-[92vh] animate-in zoom-in-95 duration-300">

                {/* ══════════════════════════════════════════════════
                    LEFT: Interface d'appel + Infos prospect
                   ══════════════════════════════════════════════════ */}
                <div className="w-full lg:w-[300px] bg-gradient-to-b from-slate-900 to-slate-800 p-5 flex flex-col relative flex-shrink-0">
                    <button onClick={onClose} className="absolute top-3 right-3 p-2 text-slate-400 hover:text-white transition-colors rounded-lg hover:bg-white/10">
                        <X size={18} />
                    </button>

                    {/* Prospect Info */}
                    <div className="text-center mb-4">
                        <div className="w-16 h-16 bg-gradient-to-br from-indigo-500 to-violet-600 rounded-2xl flex items-center justify-center mx-auto mb-2 text-white font-black text-xl shadow-lg shadow-indigo-500/30">
                            {prospect.firstName[0]}{prospect.lastName[0]}
                        </div>
                        <h2 className="text-lg font-bold text-white">{prospect.firstName} {prospect.lastName}</h2>
                        <p className="text-slate-400 text-sm font-medium">{prospect.phone}</p>
                        {prospect.email && <p className="text-slate-500 text-xs truncate">{prospect.email}</p>}

                        {/* Badges tentatives */}
                        <div className="flex items-center justify-center gap-1.5 mt-2 flex-wrap">
                            <span className="px-2 py-0.5 bg-white/10 rounded-full text-[10px] text-slate-300">
                                📞 {prospect.callAttempts || 0}
                            </span>
                            {(prospect.noAnswerCount || 0) > 0 && (
                                <span className={`px-2 py-0.5 rounded-full text-[10px] ${(prospect.noAnswerCount || 0) >= 3 ? 'bg-red-500/20 text-red-300' : 'bg-amber-500/20 text-amber-300'}`}>
                                    📵 {prospect.noAnswerCount}/{MAX_NO_ANSWER}
                                </span>
                            )}
                            {(prospect.callbackCount || 0) > 0 && (
                                <span className={`px-2 py-0.5 rounded-full text-[10px] ${(prospect.callbackCount || 0) >= 2 ? 'bg-red-500/20 text-red-300' : 'bg-blue-500/20 text-blue-300'}`}>
                                    🔄 {prospect.callbackCount}/{MAX_CALLBACKS}
                                </span>
                            )}
                        </div>
                    </div>

                    {/* Call Status & Controls */}
                    <div className="flex-1 flex flex-col items-center justify-center">
                        <div className="mb-4 text-center h-12">
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
                            {status === 'ENDED' && <span className="text-slate-400 font-bold text-sm">Terminé — {formatDuration(duration)}</span>}
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
                    <div className="mt-3 bg-white/5 rounded-2xl p-3">
                        <div className="flex items-center justify-between mb-1.5">
                            <span className="text-[10px] font-bold text-slate-400 uppercase tracking-wider">Score</span>
                            <span className={`text-lg font-black ${computedScore >= 70 ? 'text-emerald-400' : computedScore >= 40 ? 'text-amber-400' : 'text-red-400'}`}>
                                {computedScore}
                            </span>
                        </div>
                        <div className="w-full h-1.5 bg-white/10 rounded-full overflow-hidden">
                            <div
                                className={`h-full rounded-full transition-all duration-500 ${computedScore >= 70 ? 'bg-emerald-400' : computedScore >= 40 ? 'bg-amber-400' : 'bg-red-400'}`}
                                style={{ width: `${computedScore}%` }}
                            />
                        </div>
                        <p className="text-[9px] text-slate-500 mt-1.5 text-center">
                            {checkedSteps.size}/{QUALIFICATION_STEPS.length} étapes • {selectedService ? '✓' : '○'} Service • {addressForm.zipCode ? '✓' : '○'} CP
                        </p>
                    </div>
                </div>

                {/* ══════════════════════════════════════════════════
                    RIGHT: Single-flow (plus de tabs !)
                   ══════════════════════════════════════════════════ */}
                <div className="flex-1 flex flex-col min-h-0">

                    {/* ─── QUICK ACTIONS BAR ─── Toujours visible en haut */}
                    <div className="border-b border-slate-100 bg-gradient-to-r from-slate-50 to-white p-4">
                        <div className="flex items-center gap-2 flex-wrap">
                            {/* Call Outcomes — pilules cliquables */}
                            {CALL_OUTCOMES.map(outcome => (
                                <button
                                    key={outcome.id}
                                    onClick={() => setCallOutcome(callOutcome === outcome.id ? null : outcome.id)}
                                    className={`px-3 py-2 rounded-xl border text-xs font-bold transition-all active:scale-95 flex items-center gap-1.5 ${callOutcome === outcome.id
                                        ? `${outcome.color} ring-2 ring-current/20 shadow-sm`
                                        : 'bg-white border-slate-200 text-slate-500 hover:border-slate-300 hover:text-slate-700'
                                        }`}
                                >
                                    <span>{outcome.icon}</span>
                                    <span className="hidden sm:inline">{outcome.label}</span>
                                </button>
                            ))}

                            {/* Separator */}
                            <div className="w-px h-8 bg-slate-200 mx-1" />

                            {/* Book Appointment — s'ouvre PAR-DESSUS le cockpit */}
                            <button
                                onClick={() => setShowBooking(true)}
                                className="px-4 py-2 rounded-xl bg-indigo-600 text-white text-xs font-bold hover:bg-indigo-700 transition-all active:scale-95 flex items-center gap-1.5 shadow-sm shadow-indigo-200"
                            >
                                <Calendar size={14} />
                                Fixer RDV
                            </button>

                            {/* Send simulator link */}
                            <button
                                onClick={handleSendSimulatorLink}
                                className="px-3 py-2 rounded-xl bg-white border border-slate-200 text-slate-500 text-xs font-bold hover:border-indigo-300 hover:text-indigo-600 transition-all active:scale-95 flex items-center gap-1.5"
                                title="Envoyer le lien simulateur par SMS"
                            >
                                <Link2 size={14} />
                                <span className="hidden md:inline">Simulateur</span>
                            </button>
                        </div>

                        {/* Outcome selected feedback */}
                        {callOutcome && (
                            <div className="mt-2 flex items-center gap-2">
                                <span className="text-[10px] text-slate-400">Issue choisie :</span>
                                <span className="text-xs font-bold text-slate-700">
                                    {CALL_OUTCOMES.find(o => o.id === callOutcome)?.icon} {CALL_OUTCOMES.find(o => o.id === callOutcome)?.label}
                                </span>
                                <button onClick={() => setCallOutcome(null)} className="text-slate-400 hover:text-red-500 transition-colors">
                                    <X size={12} />
                                </button>
                            </div>
                        )}
                    </div>

                    {/* ─── SCROLLABLE CONTENT ─── */}
                    <div className="flex-1 overflow-y-auto">

                        {/* ═══ QUALIFICATION FLOW ═══ */}
                        <div className="p-5">
                            <div className="flex items-center justify-between mb-3">
                                <h3 className="text-sm font-black text-slate-900 flex items-center gap-2">
                                    <ClipboardCheck size={16} className="text-indigo-500" />
                                    Qualification
                                </h3>
                                <span className="text-xs font-bold text-slate-400 bg-slate-100 px-2.5 py-1 rounded-full">
                                    {checkedSteps.size}/{QUALIFICATION_STEPS.length}
                                </span>
                            </div>

                            {/* Progress bar */}
                            <div className="w-full h-1.5 bg-slate-100 rounded-full overflow-hidden mb-4">
                                <div
                                    className={`h-full rounded-full transition-all duration-500 ${qualificationProgress >= 85 ? 'bg-emerald-500' : qualificationProgress >= 50 ? 'bg-amber-500' : 'bg-indigo-500'}`}
                                    style={{ width: `${qualificationProgress}%` }}
                                />
                            </div>

                            {/* Steps by category — with inline fields */}
                            <div className="space-y-4">
                                {['IDENTITY', 'NEED', 'ELIGIBILITY', 'LOCATION', 'CLOSING'].map(category => {
                                    const categorySteps = QUALIFICATION_STEPS.filter(s => s.category === category);
                                    return (
                                        <div key={category}>
                                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5">
                                                {categoryLabels[category]}
                                            </p>
                                            <div className="space-y-1">
                                                {categorySteps.map(step => (
                                                    <div key={step.id}>
                                                        <button
                                                            onClick={() => toggleStep(step.id)}
                                                            className={`w-full text-left px-3 py-2.5 rounded-xl border transition-all flex items-center gap-3 group ${checkedSteps.has(step.id)
                                                                ? 'bg-emerald-50 border-emerald-200'
                                                                : 'bg-white border-slate-100 hover:border-slate-200 hover:bg-slate-50'
                                                                }`}
                                                        >
                                                            <div className={`w-5 h-5 rounded-md border-2 flex items-center justify-center flex-shrink-0 transition-all ${checkedSteps.has(step.id)
                                                                ? 'bg-emerald-500 border-emerald-500 text-white'
                                                                : 'border-slate-300 group-hover:border-indigo-300'
                                                                }`}>
                                                                {checkedSteps.has(step.id) && <CheckCircle size={12} />}
                                                            </div>
                                                            <div className="flex-1 min-w-0">
                                                                <p className={`text-sm font-semibold ${checkedSteps.has(step.id) ? 'text-emerald-700' : 'text-slate-700'}`}>
                                                                    {step.icon} {step.label}
                                                                </p>
                                                                <p className={`text-[11px] ${checkedSteps.has(step.id) ? 'text-emerald-500' : 'text-slate-400'}`}>
                                                                    {step.description}
                                                                </p>
                                                            </div>
                                                        </button>

                                                        {/* ── Inline: Sélecteur de service (apparaît quand "Valider le besoin" est coché) ── */}
                                                        {step.id === 'confirm_interest' && checkedSteps.has('confirm_interest') && (
                                                            <div className="ml-8 mt-1.5 mb-2 p-3 bg-indigo-50/50 rounded-xl border border-indigo-100 animate-in slide-in-from-top-2 duration-200">
                                                                <p className="text-[10px] font-bold text-indigo-600 uppercase tracking-wider mb-2">Service d'intérêt</p>
                                                                <div className="grid grid-cols-2 gap-1.5">
                                                                    {services.map(service => (
                                                                        <button
                                                                            key={service.id}
                                                                            onClick={() => setSelectedService(service.id)}
                                                                            className={`text-left px-3 py-2 rounded-lg border text-xs transition-all ${selectedService === service.id
                                                                                ? 'bg-indigo-100 border-indigo-300 text-indigo-800 font-bold'
                                                                                : 'bg-white border-slate-100 text-slate-600 hover:border-indigo-200'
                                                                                }`}
                                                                        >
                                                                            {service.title}
                                                                        </button>
                                                                    ))}
                                                                </div>
                                                            </div>
                                                        )}

                                                        {/* ── Inline: Champs d'adresse (apparaît quand "Collecter l'adresse" est coché) ── */}
                                                        {step.id === 'collect_address' && checkedSteps.has('collect_address') && (
                                                            <div className="ml-8 mt-1.5 mb-2 p-3 bg-violet-50/50 rounded-xl border border-violet-100 animate-in slide-in-from-top-2 duration-200">
                                                                <p className="text-[10px] font-bold text-violet-600 uppercase tracking-wider mb-2">Adresse du prospect</p>
                                                                <div className="space-y-2">
                                                                    <input
                                                                        type="text"
                                                                        value={addressForm.address}
                                                                        onChange={e => setAddressForm(prev => ({ ...prev, address: e.target.value }))}
                                                                        className="w-full px-3 py-2 bg-white border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-violet-400 text-sm"
                                                                        placeholder="Adresse (numéro + rue)"
                                                                    />
                                                                    <div className="grid grid-cols-3 gap-2">
                                                                        <input
                                                                            type="text"
                                                                            value={addressForm.zipCode}
                                                                            onChange={e => setAddressForm(prev => ({ ...prev, zipCode: e.target.value }))}
                                                                            className="px-3 py-2 bg-white border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-violet-400 text-sm font-mono"
                                                                            placeholder="Code postal"
                                                                        />
                                                                        <input
                                                                            type="text"
                                                                            value={addressForm.city}
                                                                            onChange={e => setAddressForm(prev => ({ ...prev, city: e.target.value }))}
                                                                            className="col-span-2 px-3 py-2 bg-white border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-violet-400 text-sm"
                                                                            placeholder="Ville"
                                                                        />
                                                                    </div>
                                                                    {addressForm.zipCode && (
                                                                        <div className="flex items-center gap-2 p-2 bg-emerald-50 rounded-lg border border-emerald-100">
                                                                            <CheckCircle size={12} className="text-emerald-500" />
                                                                            <span className="text-[11px] font-bold text-emerald-700">
                                                                                CP {addressForm.zipCode} → Routage agence OK
                                                                            </span>
                                                                        </div>
                                                                    )}
                                                                </div>
                                                            </div>
                                                        )}
                                                    </div>
                                                ))}
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* ═══ NOTES — Toujours visible ═══ */}
                        <div className="px-5 pb-5">
                            <div className="border-t border-slate-100 pt-4">
                                <h3 className="text-sm font-black text-slate-900 mb-2 flex items-center gap-2">
                                    <MessageSquare size={14} className="text-indigo-500" />
                                    Notes
                                </h3>
                                <textarea
                                    value={note}
                                    onChange={e => setNote(e.target.value)}
                                    placeholder="Points clés, objections, prochaines étapes..."
                                    className="w-full h-24 p-3 bg-slate-50 border border-slate-200 rounded-xl resize-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all outline-none text-sm text-slate-700 placeholder:text-slate-400"
                                />
                            </div>
                        </div>
                    </div>

                    {/* ─── Bottom Action Bar ─── */}
                    <div className="border-t border-slate-100 px-4 py-3 bg-white flex items-center gap-3">
                        <div className="flex-1 flex items-center gap-2 text-xs text-slate-400">
                            {duration > 0 && (
                                <span className="flex items-center gap-1">
                                    <Clock size={12} /> {formatDuration(duration)}
                                </span>
                            )}
                            {callOutcome && (
                                <span className="flex items-center gap-1 px-2 py-0.5 bg-slate-100 rounded-full">
                                    {CALL_OUTCOMES.find(o => o.id === callOutcome)?.icon} {CALL_OUTCOMES.find(o => o.id === callOutcome)?.label}
                                </span>
                            )}
                            {checkedSteps.size > 0 && (
                                <span className="text-slate-300">• {checkedSteps.size} étapes</span>
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

            {/* ═══ Booking Modal PAR-DESSUS le cockpit ═══ */}
            {showBooking && (
                <div className="fixed inset-0 z-[110]">
                    <BookAppointmentModal
                        prospectName={`${prospect.firstName} ${prospect.lastName}`}
                        prospectPhone={prospect.phone}
                        defaultServiceId={selectedService || prospect.interestServiceId}
                        existingAppointment={prospect.appointment}
                        onBook={(appointment) => {
                            if (onBookAppointment) {
                                onBookAppointment(appointment);
                            }
                            setShowBooking(false);
                        }}
                        onClose={() => setShowBooking(false)}
                    />
                </div>
            )}
        </div>
    );
}

