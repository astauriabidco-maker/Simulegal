'use client';

import React, { useState, useEffect } from 'react';
import { Calendar as CalendarIcon, Clock, User, MapPin, Video, Monitor, ChevronLeft, ChevronRight, Filter, CheckCircle2 } from 'lucide-react';
import { CalendarStore, Appointment } from '../../../services/CalendarStore';
import { AgencyStore } from '../../../services/AgencyStore';
import { format, startOfWeek, endOfWeek, addWeeks, subWeeks, eachDayOfInterval, isSameDay, isToday, addMinutes, startOfDay, endOfDay, isAfter, isBefore, addDays } from 'date-fns';
import { fr } from 'date-fns/locale';
import { CRM, Lead } from '../../../services/crmStore';
import { ServiceConfigStore } from '../../../services/ServiceConfigStore';
import { UserStore } from '../../../services/UserStore';

interface StaffCalendarViewProps {
    currentUserRole: 'HQ_ADMIN' | 'AGENCY_MANAGER' | 'CASE_WORKER';
    currentUserAgencyId?: string; // If null/HQ -> View all or filter
}

export default function StaffCalendarView({ currentUserRole, currentUserAgencyId }: StaffCalendarViewProps) {
    const [viewDate, setViewDate] = useState(new Date());
    const [appointments, setAppointments] = useState<Appointment[]>([]);
    const [absences, setAbsences] = useState<any[]>([]);
    const [selectedAppointment, setSelectedAppointment] = useState<Appointment | null>(null);
    const [selectedAbsence, setSelectedAbsence] = useState<any | null>(null);
    const [loading, setLoading] = useState(false);

    // Filters
    const [filterType, setFilterType] = useState<'ALL' | 'VISIO' | 'AGENCY'>('ALL');
    const [filterAgencyId, setFilterAgencyId] = useState<string>('ALL');
    const [agencies, setAgencies] = useState<any[]>([]);
    const [specialFilters, setSpecialFilters] = useState<string[]>([]); // 'MISSING_DOCS' | 'UNCONFIRMED' | 'CONFLICTS'

    // Manual Booking
    const [slotToBook, setSlotToBook] = useState<Date | null>(null);
    const [isBooking, setIsBooking] = useState(false);
    const [leads, setLeads] = useState<Lead[]>([]);
    const [searchLead, setSearchLead] = useState('');
    const [selectedLead, setSelectedLead] = useState<Lead | null>(null);
    const [bookingType, setBookingType] = useState<'VISIO_JURISTE' | 'PHYSICAL_AGENCY'>('VISIO_JURISTE');
    const [bookingService, setBookingService] = useState('naturalisation');
    const [allStaff, setAllStaff] = useState<any[]>([]);
    const [bookingAgencyId, setBookingAgencyId] = useState<string>('');
    const [bookingHostUserId, setBookingHostUserId] = useState<string>('');
    const [preselectedLead, setPreselectedLead] = useState<Lead | null>(null);

    useEffect(() => {
        // Read URL params for preselected lead (from Sales Hub/Pipeline)
        if (typeof window !== 'undefined') {
            const params = new URLSearchParams(window.location.search);
            const leadId = params.get('leadId');
            if (leadId) {
                const lead: Partial<Lead> = {
                    id: leadId,
                    name: params.get('name') || '',
                    email: params.get('email') || '',
                    serviceId: params.get('service') || ''
                };
                setPreselectedLead(lead as Lead);
                setSelectedLead(lead as Lead);
                if (lead.serviceId) {
                    setBookingService(lead.serviceId);
                }
            }
        }
    }, []);

    useEffect(() => {
        loadData();
    }, [viewDate, filterAgencyId, filterType, specialFilters]);

    useEffect(() => {
        loadAgencies();
        loadLeads();
        loadStaff();
    }, []);

    const loadStaff = async () => {
        const users = await UserStore.getAllUsers();
        setAllStaff(users);
    };

    const loadLeads = async () => {
        const all = await CRM.getAllLeads();
        setLeads(all);
    };

    const loadAgencies = async () => {
        if (currentUserRole === 'HQ_ADMIN') {
            const all = await AgencyStore.getAllAgencies();
            setAgencies(all);
        }
    };

    const loadData = async () => {
        setLoading(true);
        try {
            const start = startOfWeek(viewDate, { weekStartsOn: 1 }).toISOString();
            const end = endOfWeek(viewDate, { weekStartsOn: 1 }).toISOString();

            const [allApts, allAbs] = await Promise.all([
                CalendarStore.getAllAppointments({ start, end }),
                CalendarStore.getAllAbsences({ start, end })
            ]);

            // Apply Filters to Appointments
            let filteredApts = allApts;

            if (currentUserRole === 'AGENCY_MANAGER' || currentUserRole === 'CASE_WORKER') {
                if (currentUserAgencyId) {
                    filteredApts = filteredApts.filter(a => a.agencyId === currentUserAgencyId);
                }
            }

            if (filterType === 'VISIO') {
                filteredApts = filteredApts.filter(a => a.type === 'VISIO_JURISTE');
            } else if (filterType === 'AGENCY') {
                filteredApts = filteredApts.filter(a => a.type === 'PHYSICAL_AGENCY');
            }

            if (filterAgencyId !== 'ALL') {
                filteredApts = filteredApts.filter(a => a.agencyId === filterAgencyId);
            }

            // Apply Operational Filters
            if (specialFilters.length > 0) {
                // Pre-calculate conflicts if needed
                const conflictIds = new Set<string>();
                if (specialFilters.includes('CONFLICTS')) {
                    const sorted = [...filteredApts].sort((a, b) => new Date(a.start).getTime() - new Date(b.start).getTime());
                    for (let i = 0; i < sorted.length - 1; i++) {
                        const current = sorted[i];
                        const next = sorted[i + 1];
                        if (current.hostUserId === next.hostUserId && isAfter(new Date(current.end), new Date(next.start))) {
                            conflictIds.add(current.id);
                            conflictIds.add(next.id);
                        }
                    }
                }

                filteredApts = filteredApts.filter(a => {
                    let match = true;
                    if (specialFilters.includes('MISSING_DOCS')) {
                        match = match && (a.dossierStatus === 'INCOMPLETE' || a.dossierStatus === 'PARTIAL');
                    }
                    if (specialFilters.includes('UNCONFIRMED')) {
                        const start = new Date(a.start);
                        const tomorrow = endOfDay(addDays(new Date(), 1));
                        match = match && isAfter(start, new Date()) && isBefore(start, tomorrow) && a.status === 'SCHEDULED';
                    }
                    if (specialFilters.includes('CONFLICTS')) {
                        match = match && conflictIds.has(a.id);
                    }
                    return match;
                });
            }

            setAppointments(filteredApts);

            // Filter Absences (scope)
            let filteredAbs = allAbs;
            if (currentUserRole !== 'HQ_ADMIN' && currentUserAgencyId) {
                // Keep absences of users in the same agency or the specific user
                // For simplicity, we just show all for now since they are few, 
                // but we could filter by user homeAgencyId if we had it in the response.
            }

            setAbsences(filteredAbs);
        } finally {
            setLoading(false);
        }
    };

    const days = eachDayOfInterval({
        start: startOfWeek(viewDate, { weekStartsOn: 1 }),
        end: endOfWeek(viewDate, { weekStartsOn: 1 })
    });

    const hours = Array.from({ length: 19 - 8 }, (_, i) => i + 8); // 08:00 to 18:00

    const getAppointmentsForSlot = (day: Date, hour: number) => {
        return appointments.filter(apt => {
            const start = new Date(apt.start);
            return isSameDay(start, day) && start.getHours() === hour;
        });
    };

    const getAbsencesForSlot = (day: Date, hour: number) => {
        return absences.filter(abs => {
            const start = new Date(abs.start);
            return isSameDay(start, day) && start.getHours() === hour;
        });
    };

    const [modalMode, setModalMode] = useState<'BOOKING' | 'ABSENCE'>('BOOKING');
    const [absenceReason, setAbsenceReason] = useState('');

    // Drag and Drop State
    const [draggedAppointment, setDraggedAppointment] = useState<Appointment | null>(null);

    const handleDragStart = (e: React.DragEvent, apt: Appointment) => {
        setDraggedAppointment(apt);
        e.dataTransfer.effectAllowed = 'move';
    };

    const handleDragOver = (e: React.DragEvent) => {
        e.preventDefault();
        e.dataTransfer.dropEffect = 'move';
    };

    const handleDrop = async (e: React.DragEvent, day: Date, hour: number, minutes: number) => {
        e.preventDefault();
        if (!draggedAppointment) return;

        const originalDuration = (new Date(draggedAppointment.end).getTime() - new Date(draggedAppointment.start).getTime());

        const newStart = new Date(day);
        newStart.setHours(hour, minutes, 0, 0); // Set to drop slot time

        const newEnd = new Date(newStart.getTime() + originalDuration);

        setLoading(true);
        try {
            await CalendarStore.updateAppointment(draggedAppointment.id, { start: newStart.toISOString(), end: newEnd.toISOString() });
            loadData();
        } catch (error) {
            alert('Impossible de déplacer le rendez-vous : ' + (error as any).message);
        } finally {
            setLoading(false);
            setDraggedAppointment(null);
        }
    };

    const handleSlotClick = (day: Date, hour: number, minutes: number) => {
        const slot = new Date(day);
        slot.setHours(hour, minutes, 0, 0);
        setSelectedLead(preselectedLead);
        setBookingHostUserId('');
        setBookingAgencyId(currentUserAgencyId || '');
        setSlotToBook(slot);
        setModalMode('BOOKING');
        setIsBooking(true);
    };

    const handleManualBook = async () => {
        if (!selectedLead || !slotToBook) return;

        setLoading(true);
        try {
            await CalendarStore.bookAppointment(
                slotToBook.toISOString(),
                { id: selectedLead.id, name: selectedLead.name, email: selectedLead.email },
                bookingType,
                bookingAgencyId || undefined,
                bookingService,
                bookingHostUserId || undefined
            );
            setIsBooking(false);
            setSlotToBook(null);
            setSelectedLead(preselectedLead);
            setBookingHostUserId('');
            loadData();
        } catch (error) {
            alert('Erreur lors de la réservation : ' + (error as any).message);
        } finally {
            setLoading(false);
        }
    };

    const handleCreateAbsence = async () => {
        if (!slotToBook || !bookingHostUserId) {
            alert('Veuillez sélectionner un agent.');
            return;
        }

        setLoading(true);
        try {
            const end = addMinutes(slotToBook, 30).toISOString();
            await CalendarStore.createAbsence({
                userId: bookingHostUserId,
                start: slotToBook.toISOString(),
                end,
                reason: absenceReason
            });
            setIsBooking(false);
            setSlotToBook(null);
            setAbsenceReason('');
            loadData();
        } catch (error) {
            alert('Erreur lors du blocage : ' + (error as any).message);
        } finally {
            setLoading(false);
        }
    };

    const [cancellationReason, setCancellationReason] = useState('');
    const [isCancelling, setIsCancelling] = useState(false);

    const handleCancelAppointment = async () => {
        if (!selectedAppointment || !cancellationReason) {
            alert('Veuillez fournir un motif d\'annulation.');
            return;
        }

        if (!confirm('Êtes-vous sûr de vouloir annuler ce rendez-vous ?')) return;

        setLoading(true);
        try {
            await CalendarStore.cancelAppointment(selectedAppointment.id, cancellationReason);
            setSelectedAppointment(null);
            setCancellationReason('');
            setIsCancelling(false);
            loadData();
        } catch (error) {
            alert('Erreur lors de l\'annulation : ' + (error as any).message);
        } finally {
            setLoading(false);
        }
    };

    const handleDeleteAbsence = async (id: string) => {
        if (!confirm('Voulez-vous vraiment supprimer ce blocage ?')) return;
        setLoading(true);
        try {
            await CalendarStore.deleteAbsence(id);
            setSelectedAbsence(null);
            loadData();
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="h-full flex flex-col bg-white rounded-3xl shadow-sm border border-slate-100 overflow-hidden relative">
            {/* Context Banner */}
            {preselectedLead && (
                <div className="bg-indigo-600 text-white px-6 py-2 flex justify-between items-center shadow-md z-10">
                    <div className="flex items-center gap-3">
                        <div className="bg-white/20 p-1 rounded-full"><User size={16} /></div>
                        <span className="text-sm font-bold">Sélection d'un créneau pour : {preselectedLead.name}</span>
                    </div>
                    <button onClick={() => {
                        setPreselectedLead(null);
                        setSelectedLead(null);
                        const newUrl = window.location.protocol + "//" + window.location.host + window.location.pathname;
                        window.history.pushState({ path: newUrl }, '', newUrl);
                    }} className="text-xs bg-white/10 hover:bg-white/20 px-3 py-1 rounded-lg font-bold transition-colors">
                        Quitter
                    </button>
                </div>
            )}

            {/* Header */}
            <div className="p-6 border-b border-slate-100 flex justify-between items-center bg-slate-50/50">
                <div className="flex items-center gap-4">
                    <div className="flex items-center bg-white rounded-xl border border-slate-200 p-1 shadow-sm">
                        <button onClick={() => setViewDate(subWeeks(viewDate, 1))} className="p-2 hover:bg-slate-100 rounded-lg text-slate-500"><ChevronLeft size={20} /></button>
                        <button onClick={() => setViewDate(new Date())} className="px-4 py-2 font-black text-slate-700 text-sm hover:bg-slate-50 rounded-lg">Aujourd'hui</button>
                        <button onClick={() => setViewDate(addWeeks(viewDate, 1))} className="p-2 hover:bg-slate-100 rounded-lg text-slate-500"><ChevronRight size={20} /></button>
                    </div>
                    <span className="text-xl font-black text-slate-900 capitalize px-2">
                        {format(days[0], 'MMMM yyyy', { locale: fr })}
                    </span>
                    {loading && <span className="text-xs text-indigo-500 font-bold animate-pulse">Chargement...</span>}
                </div>

                {/* Filters (HQ Only) */}
                {currentUserRole === 'HQ_ADMIN' && (
                    <div className="flex items-center gap-3">
                        <div className="flex items-center gap-2 px-3 py-2 bg-white border border-slate-200 rounded-xl">
                            <Filter size={16} className="text-slate-400" />
                            <select
                                value={filterType}
                                onChange={(e) => setFilterType(e.target.value as any)}
                                className="text-sm font-bold text-slate-700 bg-transparent outline-none cursor-pointer"
                            >
                                <option value="ALL">Tout voir</option>
                                <option value="VISIO">Visio (Juristes)</option>
                                <option value="AGENCY">Rendez-vous Agence</option>
                            </select>
                        </div>

                        <div className="flex items-center gap-2 px-3 py-2 bg-white border border-slate-200 rounded-xl">
                            <MapPin size={16} className="text-slate-400" />
                            <select
                                value={filterAgencyId}
                                onChange={(e) => setFilterAgencyId(e.target.value)}
                                className="text-sm font-bold text-slate-700 bg-transparent outline-none cursor-pointer max-w-[150px]"
                            >
                                <option value="ALL">Toutes les agences</option>
                                {agencies.map(a => (
                                    <option key={a.id} value={a.id}>{a.name}</option>
                                ))}
                            </select>
                        </div>
                    </div>
                )}

                {/* Operational Filters (Everyone) */}
                <div className="flex items-center gap-2 ml-4">
                    <button
                        onClick={() => setSpecialFilters(prev => prev.includes('MISSING_DOCS') ? prev.filter(f => f !== 'MISSING_DOCS') : [...prev, 'MISSING_DOCS'])}
                        className={`px-3 py-2 rounded-xl border text-[10px] font-bold uppercase tracking-wider transition-all flex items-center gap-2 ${specialFilters.includes('MISSING_DOCS') ? 'bg-amber-50 border-amber-200 text-amber-700' : 'bg-white border-slate-200 text-slate-400 hover:border-slate-300'}`}
                        title="Dossiers Incomplets"
                    >
                        ⚠️ <span className="hidden xl:inline">Incomplets</span>
                    </button>
                    <button
                        onClick={() => setSpecialFilters(prev => prev.includes('UNCONFIRMED') ? prev.filter(f => f !== 'UNCONFIRMED') : [...prev, 'UNCONFIRMED'])}
                        className={`px-3 py-2 rounded-xl border text-[10px] font-bold uppercase tracking-wider transition-all flex items-center gap-2 ${specialFilters.includes('UNCONFIRMED') ? 'bg-purple-50 border-purple-200 text-purple-700' : 'bg-white border-slate-200 text-slate-400 hover:border-slate-300'}`}
                        title="Rendez-vous à confirmer pour demain"
                    >
                        📅 <span className="hidden xl:inline">A Confirmer</span>
                    </button>
                    <button
                        onClick={() => setSpecialFilters(prev => prev.includes('CONFLICTS') ? prev.filter(f => f !== 'CONFLICTS') : [...prev, 'CONFLICTS'])}
                        className={`px-3 py-2 rounded-xl border text-[10px] font-bold uppercase tracking-wider transition-all flex items-center gap-2 ${specialFilters.includes('CONFLICTS') ? 'bg-rose-50 border-rose-200 text-rose-700' : 'bg-white border-slate-200 text-slate-400 hover:border-slate-300'}`}
                        title="Conflits de planning"
                    >
                        🛑 <span className="hidden xl:inline">Conflits</span>
                    </button>
                </div>
            </div>

            {/* Calendar Grid */}
            <div className="flex-1 overflow-y-auto custom-scrollbar flex">

                {/* Time Column */}
                <div className="w-20 flex-shrink-0 border-r border-slate-100 bg-slate-50/30">
                    <div className="h-14 border-b border-slate-100" /> {/* Header spacer */}
                    {hours.map(hour => (
                        <div key={hour} className="h-28 border-b border-slate-100 text-xs font-bold text-slate-400 flex items-start justify-center pt-2">
                            {hour}:00
                        </div>
                    ))}
                </div>

                {/* Days Columns */}
                <div className="flex-1 grid grid-cols-7 min-w-[800px]">
                    {days.map((day: Date) => (
                        <div key={day.toISOString()} className={`flex-shrink-0 border-r border-slate-100 ${isToday(day) ? 'bg-indigo-50/10' : ''}`}>
                            {/* Day Header */}
                            <div className={`h-14 border-b border-slate-100 flex flex-col items-center justify-center ${isToday(day) ? 'bg-indigo-50 text-indigo-600' : 'bg-slate-50/50 text-slate-500'}`}>
                                <span className="text-[10px] font-black uppercase tracking-widest">{format(day, 'EEE', { locale: fr })}</span>
                                <span className={`text-lg font-black ${isToday(day) ? 'text-indigo-600' : 'text-slate-900'}`}>{format(day, 'd')}</span>
                            </div>

                            {/* Time Slots */}
                            <div className="relative">
                                {hours.map(hour => {
                                    const slotApts = getAppointmentsForSlot(day, hour);
                                    const slotAbsences = getAbsencesForSlot(day, hour);

                                    return (
                                        <div key={`${day}-${hour}`} className="h-28 border-b border-slate-50 relative group">
                                            {/* Grid line helper */}
                                            <div className="absolute inset-x-0 top-1/2 border-t border-slate-50 border-dashed opacity-0 group-hover:opacity-50" />

                                            {/* Clickable areas for empty slots (30 min each) */}
                                            <button
                                                onDragOver={handleDragOver}
                                                onDrop={(e) => handleDrop(e, day, hour, 0)}
                                                onClick={() => handleSlotClick(day, hour, 0)}
                                                className="absolute inset-x-0 top-0 h-1/2 z-0 hover:bg-indigo-50/30 transition-colors cursor-pointer border-r border-slate-100/50"
                                                title="Réserver 00"
                                            />
                                            <button
                                                onDragOver={handleDragOver}
                                                onDrop={(e) => handleDrop(e, day, hour, 30)}
                                                onClick={() => handleSlotClick(day, hour, 30)}
                                                className="absolute inset-x-0 bottom-0 h-1/2 z-0 hover:bg-indigo-50/30 transition-colors cursor-pointer border-r border-slate-100/50"
                                                title="Réserver 30"
                                            />

                                            {/* Render Absences First (Background layer) */}
                                            {slotAbsences.map(abs => {
                                                const startMin = new Date(abs.start).getMinutes();
                                                const duration = (new Date(abs.end).getTime() - new Date(abs.start).getTime()) / (1000 * 60);
                                                const height = (duration / 60) * 112;
                                                const top = (startMin / 60) * 112;

                                                return (
                                                    <button
                                                        key={abs.id}
                                                        onClick={(e) => {
                                                            e.stopPropagation();
                                                            setSelectedAbsence(abs);
                                                        }}
                                                        className="absolute left-0 right-0 z-10 bg-slate-200/50 border-l-4 border-slate-400 text-slate-500 text-[10px] font-bold px-2 py-1 flex items-center gap-1 overflow-hidden hover:bg-slate-300/50 transition-colors"
                                                        style={{ top: `${top}px`, height: `${height}px` }}
                                                    >
                                                        <Clock size={10} />
                                                        <span className="truncate">ABS: {abs.user?.name || 'Staff'} {abs.reason && `(${abs.reason})`}</span>
                                                    </button>
                                                );
                                            })}

                                            {slotApts.map(apt => {
                                                const startMin = new Date(apt.start).getMinutes();
                                                const duration = (new Date(apt.end).getTime() - new Date(apt.start).getTime()) / (1000 * 60);
                                                const height = (duration / 60) * 112;
                                                const top = (startMin / 60) * 112;

                                                const isVisio = apt.type === 'VISIO_JURISTE';

                                                return (
                                                    <button
                                                        key={apt.id}
                                                        draggable
                                                        onDragStart={(e) => handleDragStart(e, apt)}
                                                        onClick={(e) => {
                                                            e.stopPropagation();
                                                            setSelectedAppointment(apt);
                                                            setIsCancelling(false);
                                                            setCancellationReason('');
                                                        }}
                                                        className={`
                                                            absolute left-1 right-1 rounded-lg px-2 py-1 text-left text-xs transition-all hover:scale-[1.02] hover:z-20 shadow-sm border-l-4 cursor-grab active:cursor-grabbing
                                                            ${isVisio
                                                                ? 'bg-indigo-100 border-indigo-500 text-indigo-800'
                                                                : 'bg-emerald-100 border-emerald-500 text-emerald-800'
                                                            }
                                                        `}
                                                        style={{ top: `${top}px`, height: `${height}px` }}
                                                    >
                                                        <div className="flex justify-between items-start gap-1">
                                                            <div className="font-bold truncate">{apt.leadName}</div>
                                                            {apt.dossierStatus && (
                                                                <div className={`w-2 h-2 rounded-full mt-1 shrink-0 ${apt.dossierStatus === 'COMPLETE' ? 'bg-emerald-500 shadow-[0_0_8px_rgba(16,185,129,0.5)]' :
                                                                    apt.dossierStatus === 'PARTIAL' ? 'bg-orange-400' :
                                                                        'bg-rose-500'
                                                                    }`} title={`Dossier ${apt.dossierStatus}`} />
                                                            )}
                                                        </div>
                                                        <div className="flex items-center gap-1 opacity-80 scale-90 origin-left">
                                                            {isVisio ? <Video size={10} /> : <MapPin size={10} />}
                                                            <span>{format(new Date(apt.start), 'HH:mm')}</span>
                                                        </div>
                                                    </button>
                                                );
                                            })}
                                        </div>
                                    );
                                })}
                            </div>
                        </div>
                    ))}
                </div>
            </div>

            {/* Modal Detail */}
            {selectedAppointment && (
                <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-slate-900/40 backdrop-blur-sm shadow-2xl">
                    <div className="bg-white rounded-3xl shadow-2xl p-8 max-w-sm w-full animate-in zoom-in-95">
                        <div className="flex justify-between items-start mb-6">
                            <div>
                                <h3 className="text-xl font-black text-slate-900">Détails du Rendez-vous</h3>
                                <p className="text-sm text-slate-500 font-bold">{selectedAppointment.type === 'VISIO_JURISTE' ? 'Visio-Conférence Experte' : 'Rendez-vous Agence'}</p>
                            </div>
                            <button onClick={() => setSelectedAppointment(null)} className="p-2 hover:bg-slate-100 rounded-full text-slate-400 hover:rotate-90 transition-all">
                                <span className="sr-only">Fermer</span>
                                <svg className="w-6 h-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" /></svg>
                            </button>
                        </div>

                        <div className="space-y-4 mb-8">
                            <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100">
                                <div className="w-12 h-12 bg-white rounded-full flex items-center justify-center shadow-sm text-slate-900 font-black text-lg">
                                    {selectedAppointment.leadName.charAt(0)}
                                </div>
                                <div>
                                    <p className="font-bold text-slate-900">{selectedAppointment.leadName}</p>
                                    <p className="text-xs text-slate-400 font-medium">Client #SL-{selectedAppointment.leadId.substring(0, 4)}</p>
                                </div>
                            </div>

                            {selectedAppointment.dossierStatus && (
                                <div className={`p-4 rounded-2xl border flex items-center justify-between ${selectedAppointment.dossierStatus === 'COMPLETE' ? 'bg-emerald-50 border-emerald-100 text-emerald-700' :
                                    selectedAppointment.dossierStatus === 'PARTIAL' ? 'bg-orange-50 border-orange-100 text-orange-700' :
                                        'bg-rose-50 border-rose-100 text-rose-700'
                                    }`}>
                                    <div className="flex items-center gap-2">
                                        <CheckCircle2 size={16} />
                                        <span className="text-xs font-black uppercase tracking-wider">
                                            Dossier {selectedAppointment.dossierStatus === 'COMPLETE' ? 'Complet' :
                                                selectedAppointment.dossierStatus === 'PARTIAL' ? 'Incomplet' : 'Vide'}
                                        </span>
                                    </div>
                                    <span className="text-[10px] font-bold">
                                        {selectedAppointment.missingDocsCount === 0 ? '✓ Prêt pour RDV' : `${selectedAppointment.missingDocsCount} pièces manquantes`}
                                    </span>
                                </div>
                            )}

                            <div className="space-y-3">
                                <div className="flex items-center gap-3 text-slate-600 border-b border-slate-50 pb-2">
                                    <Clock size={16} className="text-indigo-500" />
                                    <div className="text-xs font-bold leading-tight">
                                        <p>{format(new Date(selectedAppointment.start), 'EEEE d MMMM', { locale: fr })}</p>
                                        <p className="text-slate-400">{format(new Date(selectedAppointment.start), 'HH:mm')} - {format(new Date(selectedAppointment.end), 'HH:mm')}</p>
                                    </div>
                                </div>

                                {selectedAppointment.hostUser && (
                                    <div className="flex items-center gap-3 text-slate-600">
                                        <User size={16} className="text-indigo-500" />
                                        <span className="text-xs font-bold truncate">Juriste: {(selectedAppointment.hostUser as any).name}</span>
                                    </div>
                                )}

                                {selectedAppointment.type === 'VISIO_JURISTE' && selectedAppointment.meetingLink && (
                                    <a href={selectedAppointment.meetingLink} target="_blank" rel="noopener noreferrer" className="flex items-center gap-3 text-indigo-600 hover:text-indigo-700 transition-colors p-3 bg-indigo-50 rounded-xl border border-indigo-100">
                                        <Monitor size={18} />
                                        <span className="text-xs font-black">Rejoindre la visio (Meet)</span>
                                    </a>
                                )}
                                {selectedAppointment.type === 'PHYSICAL_AGENCY' && (
                                    <div className="flex items-center gap-3 text-slate-600">
                                        <MapPin size={18} className="text-emerald-500" />
                                        <span className="text-xs font-bold">Agence Physique</span>
                                    </div>
                                )}
                            </div>
                        </div>

                        {isCancelling ? (
                            <div className="mt-6 p-4 bg-rose-50 rounded-2xl border border-rose-100 animate-in slide-in-from-top-2">
                                <label className="text-xs font-black uppercase text-rose-500 mb-2 block">Motif d'annulation</label>
                                <textarea
                                    className="w-full p-3 rounded-xl border border-rose-200 text-sm focus:ring-2 focus:ring-rose-500 outline-none mb-3"
                                    placeholder="Expliquez pourquoi..."
                                    rows={3}
                                    value={cancellationReason}
                                    onChange={(e) => setCancellationReason(e.target.value)}
                                />
                                <div className="flex gap-2">
                                    <button onClick={() => setIsCancelling(false)} className="px-4 py-2 text-rose-400 font-bold hover:bg-rose-100 rounded-lg text-xs">Retour</button>
                                    <button onClick={handleCancelAppointment} className="flex-1 px-4 py-2 bg-rose-500 text-white font-bold rounded-lg text-xs shadow-md hover:bg-rose-600 transition-colors">Confirmer l'annulation</button>
                                </div>
                            </div>
                        ) : (
                            <div className="flex gap-3 mt-8">
                                <button onClick={() => setIsCancelling(true)} className="flex-1 py-3 text-rose-500 font-bold hover:bg-rose-50 rounded-xl transition-colors">Annuler le RDV</button>
                                <button onClick={() => setSelectedAppointment(null)} className="flex-1 bg-slate-900 text-white py-3 rounded-xl font-black shadow-lg hover:bg-slate-800 transition-all">
                                    Fermer
                                </button>
                            </div>
                        )}
                    </div>
                </div>
            )}

            {/* Absence Detail Modal */}
            {selectedAbsence && (
                <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-slate-900/40 backdrop-blur-sm shadow-2xl">
                    <div className="bg-white rounded-3xl shadow-2xl p-8 max-w-sm w-full animate-in zoom-in-95">
                        <div className="flex justify-between items-start mb-6">
                            <div>
                                <h3 className="text-xl font-black text-slate-900">Blocage Créneau</h3>
                                <p className="text-sm text-slate-500 font-bold">Indisponibilité Staff</p>
                            </div>
                            <button onClick={() => setSelectedAbsence(null)} className="p-2 hover:bg-slate-100 rounded-full text-slate-400">
                                <svg className="w-6 h-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" /></svg>
                            </button>
                        </div>

                        <div className="space-y-4 mb-8">
                            <div className="flex items-center gap-3 text-slate-600">
                                <User size={18} className="text-slate-400" />
                                <span className="font-bold">{selectedAbsence.user?.name || 'Staff'}</span>
                            </div>
                            <div className="flex items-center gap-3 text-slate-600">
                                <Clock size={18} className="text-slate-400" />
                                <span className="font-bold">
                                    {format(new Date(selectedAbsence.start), 'HH:mm')} - {format(new Date(selectedAbsence.end), 'HH:mm')}
                                </span>
                            </div>
                            {selectedAbsence.reason && (
                                <div className="p-4 bg-slate-50 rounded-2xl border border-slate-100 text-xs font-medium text-slate-600 italic">
                                    "{selectedAbsence.reason}"
                                </div>
                            )}
                        </div>

                        <div className="flex gap-3">
                            <button
                                onClick={() => handleDeleteAbsence(selectedAbsence.id)}
                                className="flex-1 py-3 text-rose-500 font-bold hover:bg-rose-50 rounded-xl transition-colors"
                            >
                                Débloquer
                            </button>
                            <button onClick={() => setSelectedAbsence(null)} className="flex-1 bg-slate-900 text-white py-3 rounded-xl font-black shadow-lg hover:bg-slate-800 transition-all">
                                Fermer
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* Manual Booking / Absence Modal */}
            {isBooking && slotToBook && (
                <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-slate-900/40 backdrop-blur-sm shadow-2xl">
                    <div className="bg-white rounded-3xl shadow-2xl p-8 max-w-md w-full animate-in zoom-in-95">
                        <div className="flex justify-between items-start mb-6">
                            <div>
                                <h3 className="text-xl font-black text-slate-900">Planification</h3>
                                <p className="text-sm text-indigo-600 font-bold">
                                    {format(slotToBook, 'EEEE d MMMM à HH:mm', { locale: fr })}
                                </p>
                            </div>
                            <button onClick={() => { setIsBooking(false); setSelectedLead(preselectedLead); }} className="p-2 hover:bg-slate-100 rounded-full text-slate-400">
                                <svg className="w-6 h-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" /></svg>
                            </button>
                        </div>

                        {/* Mode Selector */}
                        <div className="flex bg-slate-100 p-1 rounded-2xl mb-6">
                            <button
                                onClick={() => setModalMode('BOOKING')}
                                className={`flex-1 py-2 text-xs font-black rounded-xl transition-all ${modalMode === 'BOOKING' ? 'bg-white shadow-sm text-indigo-600' : 'text-slate-500 hover:text-slate-700'}`}
                            >
                                RÉSERVATION CLIENT
                            </button>
                            <button
                                onClick={() => setModalMode('ABSENCE')}
                                className={`flex-1 py-2 text-xs font-black rounded-xl transition-all ${modalMode === 'ABSENCE' ? 'bg-white shadow-sm text-neutral-600' : 'text-slate-500 hover:text-slate-700'}`}
                            >
                                BLOQUER CRÉNEAU
                            </button>
                        </div>

                        <div className="space-y-6">
                            {modalMode === 'BOOKING' ? (
                                <>
                                    {/* Lead Selection */}
                                    <div className="space-y-2">
                                        <label className="text-xs font-black uppercase tracking-wider text-slate-400">Client</label>
                                        <div className="relative">
                                            <div className="absolute inset-y-0 left-4 flex items-center pointer-events-none text-slate-400">
                                                <User size={18} />
                                            </div>
                                            <input
                                                type="text"
                                                placeholder="Rechercher un client..."
                                                className="w-full pl-11 pr-4 py-3 bg-slate-50 border border-slate-200 rounded-2xl text-sm font-bold text-slate-900 focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                                                value={selectedLead ? selectedLead.name : searchLead}
                                                onChange={(e) => {
                                                    setSearchLead(e.target.value);
                                                    if (selectedLead) setSelectedLead(null);
                                                }}
                                                disabled={!!selectedLead}
                                            />
                                            {selectedLead && (
                                                <button onClick={() => setSelectedLead(null)} className="absolute inset-y-0 right-4 px-2 text-[10px] font-black uppercase text-indigo-500">Changer</button>
                                            )}
                                        </div>

                                        {!selectedLead && searchLead.length > 1 && (
                                            <div className="bg-white border border-slate-200 rounded-2xl shadow-xl overflow-hidden mt-2 max-h-48 overflow-y-auto">
                                                {leads.filter(l => l.name.toLowerCase().includes(searchLead.toLowerCase()) || l.email.toLowerCase().includes(searchLead.toLowerCase())).map(lead => (
                                                    <button key={lead.id} onClick={() => { setSelectedLead(lead); setSearchLead(''); }} className="w-full px-4 py-3 flex items-center justify-between hover:bg-slate-50 border-b border-slate-50 last:border-0">
                                                        <div className="text-left">
                                                            <p className="text-sm font-bold text-slate-900">{lead.name}</p>
                                                            <p className="text-[10px] text-slate-400 font-medium">{lead.email}</p>
                                                        </div>
                                                        <div className="text-[10px] bg-indigo-50 text-indigo-600 px-2 py-1 rounded-lg font-black uppercase">{lead.serviceId}</div>
                                                    </button>
                                                ))}
                                            </div>
                                        )}
                                    </div>

                                    <div className="space-y-2">
                                        <label className="text-xs font-black uppercase tracking-wider text-slate-400">Service</label>
                                        <select className="w-full p-3 bg-slate-50 border border-slate-200 rounded-2xl text-sm font-bold text-slate-900 outline-none" value={bookingService} onChange={(e) => setBookingService(e.target.value)}>
                                            {ServiceConfigStore.getAllServices().map(s => <option key={s.id} value={s.id}>{s.name}</option>)}
                                        </select>
                                    </div>
                                </>
                            ) : (
                                <div className="space-y-4">
                                    <div className="space-y-2">
                                        <label className="text-xs font-black uppercase tracking-wider text-slate-400">Motif de l'absence</label>
                                        <input
                                            type="text"
                                            placeholder="Ex: Réunion, Déjeuner, Congé..."
                                            className="w-full p-3 bg-slate-50 border border-slate-200 rounded-2xl text-sm font-bold text-slate-900 outline-none focus:ring-2 focus:ring-slate-500"
                                            value={absenceReason}
                                            onChange={(e) => setAbsenceReason(e.target.value)}
                                        />
                                    </div>
                                </div>
                            )}

                            {/* Agent Selection - Shared for both modes */}
                            <div className="grid grid-cols-1 gap-4">
                                <div className="space-y-2">
                                    <label className="text-xs font-black uppercase tracking-wider text-slate-400">
                                        {modalMode === 'BOOKING' ? 'Agent / Juriste' : 'Sélectionner le Staff'}
                                    </label>
                                    <select
                                        className="w-full p-3 bg-slate-50 border border-slate-200 rounded-2xl text-sm font-bold text-slate-900 outline-none"
                                        value={bookingHostUserId}
                                        onChange={(e) => setBookingHostUserId(e.target.value)}
                                    >
                                        <option value="">{modalMode === 'BOOKING' ? 'Sélection Intelligente...' : 'Choisir...'}</option>
                                        {allStaff.map(s => {
                                            const isExpert = modalMode === 'BOOKING' && s.expertises?.includes(bookingService);
                                            return (
                                                <option key={s.id} value={s.id}>
                                                    {isExpert ? '🛡️ ' : ''}{s.name} {isExpert ? '(Expert)' : ''}
                                                </option>
                                            );
                                        })}
                                    </select>
                                </div>
                            </div>

                            {modalMode === 'BOOKING' && (
                                <div className="space-y-2">
                                    <label className="text-xs font-black uppercase tracking-wider text-slate-400">Format</label>
                                    <div className="grid grid-cols-2 gap-3">
                                        <button onClick={() => setBookingType('VISIO_JURISTE')} className={`p-4 rounded-2xl border-2 flex flex-col items-center gap-2 transition-all ${bookingType === 'VISIO_JURISTE' ? 'bg-indigo-50 border-indigo-500' : 'bg-white border-slate-100'}`}><Video size={24} className={bookingType === 'VISIO_JURISTE' ? 'text-indigo-600' : 'text-slate-400'} /><span className={`text-xs font-black ${bookingType === 'VISIO_JURISTE' ? 'text-indigo-900' : 'text-slate-400'}`}>Visio</span></button>
                                        <button onClick={() => setBookingType('PHYSICAL_AGENCY')} className={`p-4 rounded-2xl border-2 flex flex-col items-center gap-2 transition-all ${bookingType === 'PHYSICAL_AGENCY' ? 'bg-emerald-50 border-emerald-500' : 'bg-white border-slate-100'}`}><MapPin size={24} className={bookingType === 'PHYSICAL_AGENCY' ? 'text-emerald-600' : 'text-slate-400'} /><span className={`text-xs font-black ${bookingType === 'PHYSICAL_AGENCY' ? 'text-emerald-900' : 'text-slate-400'}`}>Agence</span></button>
                                    </div>
                                </div>
                            )}

                            <button
                                onClick={modalMode === 'BOOKING' ? handleManualBook : handleCreateAbsence}
                                disabled={loading || (modalMode === 'BOOKING' && !selectedLead) || (modalMode === 'ABSENCE' && !bookingHostUserId)}
                                className={`w-full py-4 rounded-2xl font-black shadow-lg transition-all ${loading ? 'opacity-50 cursor-not-allowed' : 'bg-slate-900 text-white hover:bg-slate-800'}`}
                            >
                                {loading ? 'Traitement...' : modalMode === 'BOOKING' ? 'Confirmer la Réservation' : 'Bloquer le Créneau'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
