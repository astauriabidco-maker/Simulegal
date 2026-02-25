'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { Calendar, MapPin, Clock, X, Send, CheckCircle, User, RefreshCw, AlertTriangle } from 'lucide-react';
import { AppointmentInfo } from '../../services/SalesStore';
import { AuthStore } from '../../services/authStore';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

interface BookAppointmentModalProps {
    prospectName: string;
    prospectPhone: string;
    defaultServiceId?: string;
    existingAppointment?: AppointmentInfo;
    onBook: (appointment: AppointmentInfo) => void;
    onClose: () => void;
}

interface AgencyOption {
    id: string;
    name: string;
    city: string;
    region: string;
}

export default function BookAppointmentModal({
    prospectName,
    prospectPhone,
    defaultServiceId,
    existingAppointment,
    onBook,
    onClose
}: BookAppointmentModalProps) {
    // ─── State ───
    const [agencies, setAgencies] = useState<AgencyOption[]>([]);
    const [agencyId, setAgencyId] = useState(existingAppointment?.agencyId || '');
    const [selectedDate, setSelectedDate] = useState('');
    const [availableSlots, setAvailableSlots] = useState<string[]>([]);
    const [selectedSlot, setSelectedSlot] = useState('');
    const [sendConfirmation, setSendConfirmation] = useState(true);
    const [confirmationChannel, setConfirmationChannel] = useState<'SMS' | 'WHATSAPP'>('SMS');
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [isLoadingSlots, setIsLoadingSlots] = useState(false);
    const [isLoadingAgencies, setIsLoadingAgencies] = useState(true);

    // Success screen
    const [showSuccess, setShowSuccess] = useState(false);
    const [bookedAppointment, setBookedAppointment] = useState<AppointmentInfo | null>(null);
    const [assignedHost, setAssignedHost] = useState<{ id: string; name: string } | null>(null);
    const [validationError, setValidationError] = useState('');
    const [bookingError, setBookingError] = useState('');

    const selectedAgency = agencies.find(a => a.id === agencyId);

    // ─── Fetch agencies from backend ───
    useEffect(() => {
        const fetchAgencies = async () => {
            try {
                const token = AuthStore.getToken();
                const res = await fetch(`${API_URL}/agencies`, {
                    headers: { 'Authorization': `Bearer ${token}` }
                });
                if (res.ok) {
                    const data = await res.json();
                    setAgencies(data.map((a: any) => ({
                        id: a.id,
                        name: a.name,
                        city: a.city || '',
                        region: a.region || '',
                    })));
                }
            } catch (err) {
                console.error('[BookAppointment] Erreur chargement agences:', err);
            } finally {
                setIsLoadingAgencies(false);
            }
        };
        fetchAgencies();
    }, []);

    // ─── Fetch available slots when agency + date change ───
    const fetchSlots = useCallback(async () => {
        if (!agencyId || !selectedDate) return;

        setIsLoadingSlots(true);
        setAvailableSlots([]);
        setSelectedSlot('');
        setBookingError('');

        try {
            const params = new URLSearchParams({ date: selectedDate });
            if (agencyId) params.append('agencyId', agencyId);
            if (defaultServiceId) params.append('serviceId', defaultServiceId);

            const res = await fetch(`${API_URL}/appointments/slots?${params.toString()}`);
            if (res.ok) {
                const slots: string[] = await res.json();
                // Only keep future slots
                const now = new Date();
                setAvailableSlots(slots.filter(s => new Date(s) > now));
            }
        } catch (err) {
            console.error('[BookAppointment] Erreur chargement créneaux:', err);
        } finally {
            setIsLoadingSlots(false);
        }
    }, [agencyId, selectedDate, defaultServiceId]);

    useEffect(() => {
        fetchSlots();
    }, [fetchSlots]);

    // ─── Generate next 14 days ───
    const getAvailableDates = () => {
        const dates: string[] = [];
        const start = new Date();
        start.setDate(start.getDate() + 1);
        for (let i = 0; i < 14; i++) {
            const d = new Date(start);
            d.setDate(d.getDate() + i);
            const day = d.getDay();
            if (day !== 0 && day !== 6) { // Exclude weekend
                dates.push(d.toISOString().split('T')[0]);
            }
        }
        return dates;
    };

    const availableDates = getAvailableDates();

    // ─── Submit ───
    const handleSubmit = async () => {
        if (!selectedSlot || !agencyId) {
            setValidationError('Veuillez sélectionner une agence et un créneau.');
            setTimeout(() => setValidationError(''), 3000);
            return;
        }

        setIsSubmitting(true);
        setBookingError('');

        const appointment: AppointmentInfo = {
            date: selectedSlot,
            agencyId,
            agencyName: selectedAgency?.name || agencyId,
            serviceId: defaultServiceId,
            confirmed: sendConfirmation,
            confirmationSentVia: sendConfirmation ? confirmationChannel : undefined,
        };

        try {
            // Call parent handler (which calls SalesStore.bookAppointment)
            // But we need the response, so we'll call the API directly to get assignedHost
            const token = AuthStore.getToken();
            const prospectIdMatch = window.location.pathname.match(/prospect[s]?\/(PROSPECT-[A-Z0-9-]+)/i);

            // Let the parent handler do the API call
            onBook(appointment);

            setBookedAppointment(appointment);
            setShowSuccess(true);
        } catch (err: any) {
            // Handle slot-no-longer-available error
            const msg = err?.message || 'Erreur lors de la réservation';
            if (msg.includes('créneau') || msg.includes('disponible')) {
                setBookingError(msg);
                // Refresh slots
                fetchSlots();
            } else {
                setBookingError(msg);
            }
        } finally {
            setIsSubmitting(false);
        }
    };

    // Auto-close after success
    useEffect(() => {
        if (showSuccess) {
            const timer = setTimeout(() => onClose(), 3500);
            return () => clearTimeout(timer);
        }
    }, [showSuccess, onClose]);

    // ─── Format helpers ───
    const formatSlotTime = (iso: string) => {
        const d = new Date(iso);
        return d.toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' });
    };

    const formatDateLabel = (dateStr: string) => {
        const d = new Date(dateStr + 'T12:00:00');
        return d.toLocaleDateString('fr-FR', { weekday: 'short', day: 'numeric', month: 'short' });
    };

    // ─── Group slots by morning / afternoon ───
    const morningSlots = availableSlots.filter(s => new Date(s).getHours() < 12);
    const afternoonSlots = availableSlots.filter(s => new Date(s).getHours() >= 12);

    return (
        <div className="fixed inset-0 z-[60] flex items-center justify-center">
            <div className="absolute inset-0 bg-black/40 backdrop-blur-sm" onClick={showSuccess ? undefined : onClose} />
            <div className="relative w-[560px] max-w-[95vw] max-h-[90vh] flex flex-col bg-white rounded-3xl shadow-2xl overflow-hidden animate-in zoom-in-95 duration-200">

                {/* ─── SUCCESS SCREEN ─── */}
                {showSuccess && bookedAppointment ? (
                    <div className="p-8 text-center flex-1 overflow-y-auto">
                        <div className="relative mx-auto w-20 h-20 mb-6">
                            <div className="absolute inset-0 bg-emerald-100 rounded-full animate-ping opacity-20" />
                            <div className="relative w-20 h-20 bg-gradient-to-br from-emerald-400 to-emerald-600 rounded-full flex items-center justify-center shadow-lg shadow-emerald-200 animate-in zoom-in-50 duration-500">
                                <CheckCircle size={36} className="text-white" strokeWidth={2.5} />
                            </div>
                        </div>

                        <h2 className="text-xl font-black text-slate-900 mb-1">
                            {existingAppointment ? 'RDV modifié !' : 'RDV confirmé !'}
                        </h2>
                        <p className="text-sm text-slate-500 mb-6">
                            Le rendez-vous a été {existingAppointment ? 'mis à jour' : 'enregistré'} avec succès
                        </p>

                        <div className="bg-gradient-to-br from-indigo-50 to-purple-50 rounded-2xl p-5 border border-indigo-100 text-left mb-6">
                            <div className="flex items-start gap-3 mb-3">
                                <div className="w-10 h-10 rounded-xl bg-indigo-100 flex items-center justify-center flex-shrink-0">
                                    <Calendar size={20} className="text-indigo-600" />
                                </div>
                                <div>
                                    <p className="font-bold text-slate-900">{prospectName}</p>
                                    <p className="text-xs text-slate-500">{prospectPhone}</p>
                                </div>
                            </div>
                            <div className="space-y-2 text-sm">
                                <div className="flex items-center gap-2 text-slate-700">
                                    <MapPin size={14} className="text-indigo-400" />
                                    <span className="font-semibold">{bookedAppointment.agencyName}</span>
                                </div>
                                <div className="flex items-center gap-2 text-slate-700">
                                    <Clock size={14} className="text-indigo-400" />
                                    <span>
                                        {new Date(bookedAppointment.date).toLocaleDateString('fr-FR', { weekday: 'long', day: 'numeric', month: 'long' })}
                                        {' à '}
                                        {new Date(bookedAppointment.date).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                    </span>
                                </div>
                                {assignedHost && (
                                    <div className="flex items-center gap-2 text-slate-700">
                                        <User size={14} className="text-indigo-400" />
                                        <span>Juriste : <strong>{assignedHost.name}</strong></span>
                                    </div>
                                )}
                            </div>
                            {sendConfirmation && (
                                <div className="mt-3 pt-3 border-t border-indigo-100 flex items-center gap-2">
                                    <Send size={12} className="text-emerald-500" />
                                    <span className="text-xs text-emerald-600 font-semibold">
                                        Confirmation envoyée par {confirmationChannel === 'SMS' ? '📱 SMS' : '💬 WhatsApp'}
                                    </span>
                                </div>
                            )}
                        </div>

                        <div className="w-full h-1 bg-slate-100 rounded-full overflow-hidden mb-3">
                            <div
                                className="h-full bg-gradient-to-r from-emerald-400 to-emerald-600 rounded-full"
                                style={{ animation: 'shrink 3.5s linear forwards', width: '100%' }}
                            />
                        </div>
                        <p className="text-[11px] text-slate-400">Fermeture automatique...</p>
                        <button onClick={onClose} className="mt-4 px-6 py-2.5 rounded-xl bg-slate-100 text-slate-600 text-sm font-bold hover:bg-slate-200 transition-all">
                            Fermer maintenant
                        </button>
                        <style jsx>{`@keyframes shrink { from { width: 100%; } to { width: 0%; } }`}</style>
                    </div>
                ) : (
                    /* ─── BOOKING FORM ─── */
                    <>
                        {/* Header */}
                        <div className="relative flex-shrink-0 bg-gradient-to-br from-indigo-600 via-indigo-700 to-purple-700 px-6 py-5">
                            <button onClick={onClose} className="absolute top-4 right-4 p-1.5 hover:bg-white/10 rounded-full text-white/60 hover:text-white transition-colors">
                                <X size={18} />
                            </button>
                            <div className="flex items-center gap-3">
                                <div className="w-11 h-11 rounded-xl bg-white/15 flex items-center justify-center flex-shrink-0">
                                    <Calendar size={22} className="text-white" />
                                </div>
                                <div className="min-w-0 pr-6">
                                    <h2 className="text-lg font-bold text-white truncate">
                                        {existingAppointment ? 'Modifier le RDV' : 'Fixer un RDV en agence'}
                                    </h2>
                                    <p className="text-indigo-200 text-sm truncate">{prospectName} • {prospectPhone}</p>
                                </div>
                            </div>
                            {/* Live indicator */}
                            <div className="absolute top-4 left-6 flex items-center gap-1.5">
                                <div className="w-2 h-2 bg-emerald-400 rounded-full animate-pulse" />
                                <span className="text-[10px] text-emerald-300 font-bold uppercase tracking-wider">Temps réel</span>
                            </div>
                        </div>

                        {/* Form */}
                        <div className="p-6 space-y-5 flex-1 overflow-y-auto">
                            {/* Errors */}
                            {validationError && (
                                <div className="p-3 mb-2 bg-red-50 border border-red-100 rounded-xl text-sm text-red-700 font-medium flex items-center gap-2 animate-in slide-in-from-top duration-200">
                                    ⚠️ {validationError}
                                </div>
                            )}
                            {bookingError && (
                                <div className="p-3 mb-2 bg-amber-50 border border-amber-200 rounded-xl text-sm text-amber-800 font-medium flex items-center gap-2 animate-in slide-in-from-top duration-200">
                                    <AlertTriangle size={16} className="text-amber-500 flex-shrink-0" />
                                    <span>{bookingError}</span>
                                    <button onClick={fetchSlots} className="ml-auto text-amber-600 hover:text-amber-800 transition-colors">
                                        <RefreshCw size={14} />
                                    </button>
                                </div>
                            )}

                            {/* ─── Step 1: Agence ─── */}
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                    <MapPin size={13} className="inline mr-1.5 -mt-0.5" />
                                    Agence
                                </label>
                                {isLoadingAgencies ? (
                                    <div className="flex items-center gap-3 p-4 bg-slate-50 rounded-xl">
                                        <RefreshCw size={16} className="text-slate-400 animate-spin" />
                                        <span className="text-sm text-slate-500">Chargement des agences...</span>
                                    </div>
                                ) : (
                                    <div className="grid grid-cols-2 gap-2 max-h-40 overflow-y-auto">
                                        {agencies.map((agency) => (
                                            <button
                                                key={agency.id}
                                                onClick={() => { setAgencyId(agency.id); setSelectedDate(''); setSelectedSlot(''); }}
                                                className={`text-left px-3 py-2.5 rounded-xl border-2 transition-all ${agencyId === agency.id
                                                    ? 'border-indigo-500 bg-indigo-50 shadow-sm'
                                                    : 'border-slate-100 bg-white hover:border-slate-200'
                                                    }`}
                                            >
                                                <p className={`text-xs font-bold ${agencyId === agency.id ? 'text-indigo-700' : 'text-slate-900'}`}>
                                                    {agency.name}
                                                </p>
                                                <p className="text-[10px] text-slate-400 mt-0.5">{agency.city} • {agency.region}</p>
                                            </button>
                                        ))}
                                    </div>
                                )}
                            </div>

                            {/* ─── Step 2: Date ─── */}
                            {agencyId && (
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        <Calendar size={13} className="inline mr-1.5 -mt-0.5" />
                                        Date du RDV
                                    </label>
                                    <div className="flex gap-2 overflow-x-auto pb-2 scrollbar-thin">
                                        {availableDates.map(d => (
                                            <button
                                                key={d}
                                                onClick={() => { setSelectedDate(d); setSelectedSlot(''); }}
                                                className={`flex-shrink-0 px-3 py-2 rounded-xl border-2 transition-all text-center ${selectedDate === d
                                                    ? 'border-indigo-500 bg-indigo-50 shadow-sm'
                                                    : 'border-slate-100 bg-white hover:border-slate-200'
                                                    }`}
                                            >
                                                <p className={`text-xs font-bold ${selectedDate === d ? 'text-indigo-700' : 'text-slate-700'}`}>
                                                    {formatDateLabel(d)}
                                                </p>
                                            </button>
                                        ))}
                                    </div>
                                </div>
                            )}

                            {/* ─── Step 3: Créneaux temps réel ─── */}
                            {agencyId && selectedDate && (
                                <div>
                                    <div className="flex items-center justify-between mb-2">
                                        <label className="text-xs font-bold text-slate-500 uppercase tracking-wider flex items-center gap-1.5">
                                            <Clock size={13} />
                                            Créneaux disponibles
                                            {!isLoadingSlots && (
                                                <span className="text-[10px] text-emerald-500 font-bold normal-case">
                                                    ({availableSlots.length} dispo{availableSlots.length > 1 ? 's' : ''})
                                                </span>
                                            )}
                                        </label>
                                        <button onClick={fetchSlots} className="text-xs text-indigo-500 hover:text-indigo-700 font-bold flex items-center gap-1 transition-colors">
                                            <RefreshCw size={12} className={isLoadingSlots ? 'animate-spin' : ''} />
                                            Rafraîchir
                                        </button>
                                    </div>

                                    {isLoadingSlots ? (
                                        <div className="flex flex-col items-center gap-3 py-8">
                                            <RefreshCw size={24} className="text-indigo-400 animate-spin" />
                                            <p className="text-sm text-slate-500 font-medium">Interrogation de l'agenda en temps réel...</p>
                                        </div>
                                    ) : availableSlots.length === 0 ? (
                                        <div className="text-center py-6 bg-slate-50 rounded-xl border border-slate-100">
                                            <Calendar size={32} className="mx-auto text-slate-300 mb-2" />
                                            <p className="text-sm text-slate-500 font-medium">Aucun créneau disponible ce jour</p>
                                            <p className="text-xs text-slate-400 mt-1">Tous les juristes sont occupés. Essayez une autre date.</p>
                                        </div>
                                    ) : (
                                        <div className="space-y-3">
                                            {/* Morning */}
                                            {morningSlots.length > 0 && (
                                                <div>
                                                    <p className="text-[10px] font-bold text-amber-500 uppercase tracking-wider mb-1.5">☀️ Matin</p>
                                                    <div className="grid grid-cols-4 gap-1.5">
                                                        {morningSlots.map(slot => (
                                                            <button
                                                                key={slot}
                                                                onClick={() => setSelectedSlot(slot)}
                                                                className={`py-2.5 rounded-xl border-2 text-sm font-bold transition-all ${selectedSlot === slot
                                                                    ? 'border-indigo-500 bg-indigo-600 text-white shadow-md shadow-indigo-200'
                                                                    : 'border-slate-100 bg-white text-slate-700 hover:border-indigo-200 hover:bg-indigo-50'
                                                                    }`}
                                                            >
                                                                {formatSlotTime(slot)}
                                                            </button>
                                                        ))}
                                                    </div>
                                                </div>
                                            )}
                                            {/* Afternoon */}
                                            {afternoonSlots.length > 0 && (
                                                <div>
                                                    <p className="text-[10px] font-bold text-blue-500 uppercase tracking-wider mb-1.5">🌙 Après-midi</p>
                                                    <div className="grid grid-cols-4 gap-1.5">
                                                        {afternoonSlots.map(slot => (
                                                            <button
                                                                key={slot}
                                                                onClick={() => setSelectedSlot(slot)}
                                                                className={`py-2.5 rounded-xl border-2 text-sm font-bold transition-all ${selectedSlot === slot
                                                                    ? 'border-indigo-500 bg-indigo-600 text-white shadow-md shadow-indigo-200'
                                                                    : 'border-slate-100 bg-white text-slate-700 hover:border-indigo-200 hover:bg-indigo-50'
                                                                    }`}
                                                            >
                                                                {formatSlotTime(slot)}
                                                            </button>
                                                        ))}
                                                    </div>
                                                </div>
                                            )}
                                        </div>
                                    )}
                                </div>
                            )}

                            {/* ─── Step 4: Confirmation ─── */}
                            {selectedSlot && (
                                <>
                                    <div className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                                        <div className="flex items-center justify-between mb-3">
                                            <label className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                                <Send size={14} />
                                                Envoyer confirmation au lead
                                            </label>
                                            <button
                                                onClick={() => setSendConfirmation(!sendConfirmation)}
                                                className={`relative w-11 h-6 rounded-full transition-colors ${sendConfirmation ? 'bg-indigo-600' : 'bg-slate-300'}`}
                                            >
                                                <div className={`absolute top-0.5 w-5 h-5 bg-white rounded-full shadow transition-transform ${sendConfirmation ? 'translate-x-5' : 'translate-x-0.5'}`} />
                                            </button>
                                        </div>
                                        {sendConfirmation && (
                                            <div className="flex gap-2">
                                                <button
                                                    onClick={() => setConfirmationChannel('SMS')}
                                                    className={`flex-1 py-2 rounded-lg text-xs font-bold transition-all ${confirmationChannel === 'SMS'
                                                        ? 'bg-indigo-600 text-white'
                                                        : 'bg-white border border-slate-200 text-slate-500 hover:border-slate-300'
                                                        }`}
                                                >
                                                    📱 SMS
                                                </button>
                                                <button
                                                    onClick={() => setConfirmationChannel('WHATSAPP')}
                                                    className={`flex-1 py-2 rounded-lg text-xs font-bold transition-all ${confirmationChannel === 'WHATSAPP'
                                                        ? 'bg-emerald-600 text-white'
                                                        : 'bg-white border border-slate-200 text-slate-500 hover:border-slate-300'
                                                        }`}
                                                >
                                                    💬 WhatsApp
                                                </button>
                                            </div>
                                        )}
                                    </div>

                                    {/* Recap */}
                                    <div className="bg-indigo-50 rounded-xl p-4 border border-indigo-100">
                                        <p className="text-xs font-black text-indigo-600 uppercase tracking-wider mb-2">Récapitulatif</p>
                                        <div className="space-y-1.5 text-sm text-slate-700">
                                            <p>📍 <strong>{selectedAgency?.name}</strong></p>
                                            <p>🗓 {new Date(selectedSlot).toLocaleDateString('fr-FR', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' })}</p>
                                            <p>🕐 {formatSlotTime(selectedSlot)}</p>
                                            {defaultServiceId && <p>🎯 Service : {defaultServiceId}</p>}
                                        </div>
                                    </div>
                                </>
                            )}
                        </div>

                        {/* Footer */}
                        <div className="px-6 py-4 bg-slate-50 border-t border-slate-100 flex gap-3">
                            <button
                                onClick={onClose}
                                className="flex-1 py-3 rounded-xl border border-slate-200 text-slate-600 font-bold text-sm hover:bg-white transition-all"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleSubmit}
                                disabled={!selectedSlot || !agencyId || isSubmitting}
                                className="flex-1 py-3 rounded-xl bg-indigo-600 text-white font-bold text-sm hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                            >
                                {isSubmitting ? (
                                    <RefreshCw size={16} className="animate-spin" />
                                ) : (
                                    <>
                                        <CheckCircle size={16} />
                                        {existingAppointment ? 'Modifier' : 'Confirmer le RDV'}
                                    </>
                                )}
                            </button>
                        </div>
                    </>
                )}
            </div>
        </div>
    );
}
