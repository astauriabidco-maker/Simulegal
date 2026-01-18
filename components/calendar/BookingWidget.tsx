'use client';

import React, { useState, useEffect } from 'react';
import { Calendar, Clock, MapPin, Video, CheckCircle2, ChevronRight, ChevronLeft } from 'lucide-react';
import { CalendarStore, AppointmentType } from '../../services/CalendarStore';
import { AgencyStore } from '../../services/AgencyStore';
import { Agency } from '../../types/backoffice';
import { format, addMonths, subMonths, startOfMonth, endOfMonth, eachDayOfInterval, isSameMonth, isSameDay, isToday } from 'date-fns';
import { fr } from 'date-fns/locale';

interface BookingWidgetProps {
    lead: { id: string; name: string; email: string };
    forcedAgencyId?: string;
    initialMode?: AppointmentType;
    onComplete: () => void;
}

export default function BookingWidget({ lead, forcedAgencyId, initialMode, onComplete }: BookingWidgetProps) {
    // State
    const [step, setStep] = useState<'MODE' | 'AGENCY' | 'DATE' | 'SLOT' | 'CONFIRM'>('MODE');
    const [mode, setMode] = useState<AppointmentType | null>(initialMode || null);
    const [selectedAgency, setSelectedAgency] = useState<Agency | null>(null);
    const [viewDate, setViewDate] = useState(new Date());
    const [selectedDate, setSelectedDate] = useState<Date | null>(null);
    const [availableSlots, setAvailableSlots] = useState<string[]>([]);
    const [selectedSlot, setSelectedSlot] = useState<string | null>(null);
    const [loading, setLoading] = useState(false);

    // Data
    const [agencies, setAgencies] = useState<Agency[]>([]);

    useEffect(() => {
        loadAgencies();

        // Handling Initial Logic
        if (initialMode) {
            setMode(initialMode);
            if (initialMode === 'VISIO_JURISTE') {
                setStep('DATE'); // Skip Mode & Agency
            } else if (forcedAgencyId) {
                // If Physical + Forced Agency -> Skip Mode & Agency Selection
                handleForcedAgency(forcedAgencyId);
            } else {
                // If Physical but no forced agency -> Go to Agency Selection
                setStep('AGENCY');
            }
        } else if (forcedAgencyId) {
            handleForcedAgency(forcedAgencyId);
        }
    }, [forcedAgencyId, initialMode]);

    const loadAgencies = async () => {
        const all = await AgencyStore.getAllAgencies();
        // Filter: Allow HQ, OWNED, FRANCHISE.
        setAgencies(all.filter(a => ['HQ', 'OWNED', 'FRANCHISE'].includes(a.type as any)) as any);
    };

    const handleForcedAgency = async (id: string) => {
        const all = await AgencyStore.getAllAgencies();
        const agency = all.find(a => a.id === id);
        if (agency) {
            setSelectedAgency(agency as any);
            // Only force Physical mode if not explicitly Visio
            if (!initialMode || initialMode === 'PHYSICAL_AGENCY') {
                setMode('PHYSICAL_AGENCY');
                setStep('DATE');
            }
        }
    };

    const handleModeSelect = (type: AppointmentType) => {
        setMode(type);
        if (type === 'VISIO_JURISTE') {
            setStep('DATE');
        } else {
            setStep('AGENCY');
        }
    };

    const handleDateSelect = async (date: Date) => {
        setSelectedDate(date);
        setLoading(true);
        try {
            // CRITICAL: If Visio, ignore agencyId constraints
            const agencyContext = mode === 'VISIO_JURISTE' ? undefined : selectedAgency?.id;
            const slots = await CalendarStore.getAvailableSlots(date.toISOString(), agencyContext);
            setAvailableSlots(slots);
            setStep('SLOT');
        } finally {
            setLoading(false);
        }
    };

    const handleConfirm = async () => {
        if (!selectedSlot || !mode) return;
        setLoading(true);
        try {
            await CalendarStore.bookAppointment(selectedSlot, lead, mode, selectedAgency?.id);
            onComplete();
        } catch (error) {
            console.error('Booking failed', error);
        } finally {
            setLoading(false);
        }
    };

    // --- Render Steps ---

    const renderModeSelection = () => (
        <div className="space-y-4">
            <h3 className="text-xl font-black text-slate-900 mb-6">Comment souhaitez-vous rencontrer votre expert ?</h3>

            <button
                onClick={() => handleModeSelect('VISIO_JURISTE')}
                className="w-full p-6 bg-indigo-50 hover:bg-indigo-100 border-2 border-indigo-100 hover:border-indigo-300 rounded-2xl flex items-center gap-4 transition-all group text-left"
            >
                <div className="w-12 h-12 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm group-hover:scale-110 transition-transform">
                    <Video size={24} />
                </div>
                <div>
                    <h4 className="font-bold text-slate-900">En Visio-Conférence</h4>
                    <p className="text-sm text-slate-500">Depuis chez vous, avec un expert spécialisé.</p>
                </div>
                <ChevronRight className="ml-auto text-indigo-300" />
            </button>

            <button
                onClick={() => handleModeSelect('PHYSICAL_AGENCY')}
                className="w-full p-6 bg-emerald-50 hover:bg-emerald-100 border-2 border-emerald-100 hover:border-emerald-300 rounded-2xl flex items-center gap-4 transition-all group text-left"
            >
                <div className="w-12 h-12 bg-white rounded-xl flex items-center justify-center text-emerald-600 shadow-sm group-hover:scale-110 transition-transform">
                    <MapPin size={24} />
                </div>
                <div>
                    <h4 className="font-bold text-slate-900">En Agence Physique</h4>
                    <p className="text-sm text-slate-500">Rencontrez votre conseiller local près de chez vous.</p>
                </div>
                <ChevronRight className="ml-auto text-emerald-300" />
            </button>
        </div>
    );

    const renderAgencySelection = () => (
        <div className="space-y-4">
            <div className="flex items-center gap-2 mb-6">
                <button onClick={() => setStep('MODE')} className="p-2 hover:bg-slate-100 rounded-lg text-slate-400">
                    <ChevronLeft size={20} />
                </button>
                <h3 className="text-xl font-black text-slate-900">Choisissez votre agence</h3>
            </div>

            <div className="grid gap-3 max-h-96 overflow-y-auto">
                {agencies.map(agency => (
                    <button
                        key={agency.id}
                        onClick={() => { setSelectedAgency(agency as any); setStep('DATE'); }}
                        className="p-4 bg-white border border-slate-200 rounded-xl hover:border-indigo-500 hover:shadow-md transition-all text-left flex items-start gap-3"
                    >
                        <MapPin className="text-slate-400 mt-1" size={18} />
                        <div>
                            <p className="font-bold text-slate-900">{agency.name}</p>
                            <p className="text-xs text-slate-500">{agency.address} - {agency.region}</p>
                        </div>
                    </button>
                ))}
            </div>
        </div>
    );

    const renderCalendar = () => {
        const days = eachDayOfInterval({
            start: startOfMonth(viewDate),
            end: endOfMonth(viewDate),
        });

        return (
            <div className="space-y-6">
                <div className="flex items-center gap-2 mb-2">
                    <button onClick={() => setStep(mode === 'VISIO_JURISTE' ? 'MODE' : 'AGENCY')} className="p-2 hover:bg-slate-100 rounded-lg text-slate-400">
                        <ChevronLeft size={20} />
                    </button>
                    <div>
                        <h3 className="text-xl font-black text-slate-900">Choisissez une date</h3>
                        <p className="text-sm text-slate-500">
                            {mode === 'VISIO_JURISTE' ? 'Disponibilités Visio' : `Disponibilités à ${selectedAgency?.name}`}
                        </p>
                    </div>
                </div>

                <div className="bg-white rounded-2xl border border-slate-200 p-4">
                    <div className="flex justify-between items-center mb-4">
                        <button onClick={() => setViewDate(subMonths(viewDate, 1))} className="p-1 hover:bg-slate-100 rounded"><ChevronLeft size={20} /></button>
                        <span className="font-bold capitalize">{format(viewDate, 'MMMM yyyy', { locale: fr })}</span>
                        <button onClick={() => setViewDate(addMonths(viewDate, 1))} className="p-1 hover:bg-slate-100 rounded"><ChevronRight size={20} /></button>
                    </div>

                    <div className="grid grid-cols-7 text-center text-xs font-bold text-slate-400 mb-2">
                        {['Lu', 'Ma', 'Me', 'Je', 'Ve', 'Sa', 'Di'].map(d => <div key={d}>{d}</div>)}
                    </div>

                    <div className="grid grid-cols-7 gap-1">
                        {days.map((day: Date) => {
                            const isPast = day < new Date() && !isToday(day);
                            const isWeekend = day.getDay() === 0 || day.getDay() === 6;
                            const disabled = isPast || isWeekend;

                            return (
                                <button
                                    key={day.toISOString()}
                                    disabled={disabled}
                                    onClick={() => handleDateSelect(day)}
                                    className={`
                                        h-10 rounded-lg text-sm font-medium transition-all
                                        ${disabled ? 'text-slate-300 cursor-not-allowed' : 'hover:bg-indigo-50 text-slate-700'}
                                        ${selectedDate && isSameDay(day, selectedDate) ? 'bg-indigo-600 text-white hover:bg-indigo-700' : ''}
                                        ${isToday(day) && !selectedDate ? 'text-indigo-600 font-bold' : ''}
                                    `}
                                >
                                    {format(day, 'd')}
                                </button>
                            );
                        })}
                    </div>
                </div>
            </div>
        );
    };

    const renderSlots = () => (
        <div className="space-y-6">
            <div className="flex items-center gap-2 mb-2">
                <button onClick={() => { setStep('DATE'); setSelectedSlot(null); }} className="p-2 hover:bg-slate-100 rounded-lg text-slate-400">
                    <ChevronLeft size={20} />
                </button>
                <div>
                    <h3 className="text-xl font-black text-slate-900">Choisissez l'heure</h3>
                    <p className="text-sm text-slate-500 capitalize">{selectedDate && format(selectedDate, 'EEEE d MMMM', { locale: fr })}</p>
                </div>
            </div>

            <div className="grid grid-cols-3 gap-3 max-h-80 overflow-y-auto p-1">
                {availableSlots.length > 0 ? availableSlots.map(slot => {
                    const timeStr = format(new Date(slot), 'HH:mm');
                    return (
                        <button
                            key={slot}
                            onClick={() => { setSelectedSlot(slot); setStep('CONFIRM'); }}
                            className={`
                                py-3 px-4 rounded-xl text-sm font-bold border transition-all
                                ${selectedSlot === slot
                                    ? 'bg-indigo-600 text-white border-indigo-600 shadow-lg scale-105'
                                    : 'bg-white text-slate-700 border-slate-200 hover:border-indigo-300 hover:shadow-md'}
                            `}
                        >
                            {timeStr}
                        </button>
                    );
                }) : (
                    <div className="col-span-3 text-center py-8 text-slate-500">
                        Aucun créneau disponible ce jour.
                    </div>
                )}
            </div>
        </div>
    );

    const renderConfirmation = () => (
        <div className="text-center py-8 animate-in fade-in zoom-in duration-300">
            <div className="w-20 h-20 bg-emerald-100 text-emerald-600 rounded-full flex items-center justify-center mx-auto mb-6">
                <Calendar size={40} />
            </div>

            <h3 className="text-2xl font-black text-slate-900 mb-2">Confirmer ce rendez-vous ?</h3>
            <p className="text-slate-500 mb-8 max-w-sm mx-auto">
                Un expert vous attendra pour finaliser votre dossier.
            </p>

            <div className="bg-slate-50 rounded-2xl p-6 mb-8 text-left border border-slate-100">
                <div className="flex items-start gap-4 mb-4">
                    <Clock className="text-indigo-500 mt-1" size={20} />
                    <div>
                        <p className="font-bold text-slate-900 capitalize">
                            {selectedSlot && format(new Date(selectedSlot), 'EEEE d MMMM yyyy', { locale: fr })}
                        </p>
                        <p className="text-slate-500">
                            à {selectedSlot && format(new Date(selectedSlot), 'HH:mm')} (30 min)
                        </p>
                    </div>
                </div>

                <div className="flex items-start gap-4">
                    {mode === 'VISIO_JURISTE' ? <Video className="text-indigo-500 mt-1" size={20} /> : <MapPin className="text-indigo-500 mt-1" size={20} />}
                    <div>
                        <p className="font-bold text-slate-900">
                            {mode === 'VISIO_JURISTE' ? 'Visio-Conférence' : 'Rendez-vous en Agence'}
                        </p>
                        <p className="text-sm text-slate-500">
                            {mode === 'VISIO_JURISTE' ? 'Lien Google Meet envoyé par email' : selectedAgency?.name}
                        </p>
                    </div>
                </div>
            </div>

            <div className="flex gap-4">
                <button
                    onClick={() => setStep('SLOT')}
                    className="flex-1 py-4 font-bold text-slate-400 hover:text-slate-600 transition-colors"
                >
                    Modifier
                </button>
                <button
                    onClick={handleConfirm}
                    disabled={loading}
                    className="flex-1 bg-slate-900 text-white py-4 rounded-2xl font-black shadow-xl hover:bg-slate-800 hover:scale-105 transition-all flex items-center justify-center gap-2"
                >
                    {loading ? 'Validation...' : 'Confirmer le RDV'}
                    {!loading && <CheckCircle2 size={20} />}
                </button>
            </div>
        </div>
    );

    return (
        <div className="bg-white rounded-[32px] shadow-xl border border-slate-100 overflow-hidden max-w-md w-full mx-auto">
            <div className="p-8">
                {step === 'MODE' && renderModeSelection()}
                {step === 'AGENCY' && renderAgencySelection()}
                {step === 'DATE' && renderCalendar()}
                {step === 'SLOT' && renderSlots()}
                {step === 'CONFIRM' && renderConfirmation()}
            </div>
        </div>
    );
}
