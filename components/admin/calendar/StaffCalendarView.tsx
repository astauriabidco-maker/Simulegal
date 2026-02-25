'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { Calendar as CalendarIcon, Clock, User, MapPin, Video, Monitor, ChevronLeft, ChevronRight, Filter, CheckCircle2, Phone, MessageSquare, Send, ExternalLink, Zap, Copy, Check, Edit3, PhoneCall, PhoneOff, Mic, MicOff } from 'lucide-react';
import { CalendarStore, Appointment } from '../../../services/CalendarStore';
import { AgencyStore } from '../../../services/AgencyStore';
import { format, startOfWeek, endOfWeek, addWeeks, subWeeks, eachDayOfInterval, isSameDay, isToday, addMinutes, startOfDay, endOfDay, isAfter, isBefore, addDays } from 'date-fns';
import { fr } from 'date-fns/locale';
import { CRM, Lead } from '../../../services/crmStore';
import { ServiceConfigStore } from '../../../services/ServiceConfigStore';
import { UserStore } from '../../../services/UserStore';
import { WhatsAppWidget } from '../../backoffice/WhatsAppWidget';
import { SERVICES_CATALOG } from '../../../data/services';

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

    // Toast & Quick Actions
    const [toastMessage, setToastMessage] = useState('');
    const [showQuickMessage, setShowQuickMessage] = useState(false);
    const [quickMessageText, setQuickMessageText] = useState('');
    const [quickMessageChannel, setQuickMessageChannel] = useState<'sms' | 'whatsapp' | 'email'>('whatsapp');

    // Panels inside RDV modal
    const [activePanel, setActivePanel] = useState<'none' | 'call' | 'whatsapp' | 'message'>('none');
    const [isInCall, setIsInCall] = useState(false);
    const [callDuration, setCallDuration] = useState(0);
    const [callNote, setCallNote] = useState('');
    const [isMuted, setIsMuted] = useState(false);

    // Editable fields
    const [editingService, setEditingService] = useState(false);
    const [editingStaff, setEditingStaff] = useState(false);
    const [editServiceId, setEditServiceId] = useState('');
    const [editStaffId, setEditStaffId] = useState('');
    const [isSavingField, setIsSavingField] = useState(false);

    const showToast = useCallback((msg: string) => {
        setToastMessage(msg);
        setTimeout(() => setToastMessage(''), 2500);
    }, []);

    const copyToClipboard = useCallback((text: string, label: string) => {
        navigator.clipboard.writeText(text).then(() => {
            showToast(`✅ ${label} copié !`);
        }).catch(() => {
            showToast(`📋 ${text}`);
        });
    }, [showToast]);

    // Call timer
    useEffect(() => {
        let timer: NodeJS.Timeout;
        if (isInCall) {
            timer = setInterval(() => setCallDuration(d => d + 1), 1000);
        }
        return () => clearInterval(timer);
    }, [isInCall]);

    const formatCallDuration = (s: number) => {
        const m = Math.floor(s / 60);
        const sec = s % 60;
        return `${m.toString().padStart(2, '0')}:${sec.toString().padStart(2, '0')}`;
    };

    const handleStartCall = () => {
        setIsInCall(true);
        setCallDuration(0);
        setCallNote('');
    };

    const handleEndCall = () => {
        setIsInCall(false);
        showToast(`📞 Appel terminé (${formatCallDuration(callDuration)})`);
    };

    const handleSaveField = async (appointmentId: string, field: 'serviceId' | 'hostUserId', value: string) => {
        setIsSavingField(true);
        try {
            await CalendarStore.updateAppointment(appointmentId, { [field]: value });
            // Refresh appointments
            const data = await CalendarStore.getAllAppointments();
            setAppointments(data);
            showToast(`✅ ${field === 'serviceId' ? 'Service' : 'Intervenant'} mis à jour`);
            setEditingService(false);
            setEditingStaff(false);
        } catch {
            showToast('❌ Erreur de mise à jour');
        }
        setIsSavingField(false);
    };

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
                                                            setActivePanel('none');
                                                            setEditingService(false);
                                                            setEditingStaff(false);
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

            {/* Modal Detail — Fiche RDV Enrichie */}
            {selectedAppointment && (() => {
                const apt = selectedAppointment as any;
                const prospect = apt.prospect;
                const lead = apt.lead;
                const agency = apt.agency;
                const contactName = apt.leadName || 'Inconnu';
                const contactEmail = apt.leadEmail || prospect?.email || lead?.email || '';
                const contactPhone = prospect?.phone || lead?.phone || '';
                const contactId = apt.leadId || apt.prospectId || '';
                const serviceId = apt.serviceId || prospect?.interestServiceId || lead?.serviceId || '';
                const serviceMeta = serviceId ? ServiceConfigStore.getServiceMetadata(serviceId) : null;
                const isVisio = apt.type === 'VISIO_JURISTE';
                const startDate = new Date(apt.start);
                const endDate = new Date(apt.end);
                const now = new Date();
                const diffMs = startDate.getTime() - now.getTime();
                const diffHours = Math.floor(diffMs / (1000 * 60 * 60));
                const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));
                const isUpcoming = diffMs > 0;
                const isPast = diffMs < 0;
                const isToday2 = diffDays === 0 && isUpcoming;

                return (
                    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-slate-900/40 backdrop-blur-sm" onClick={() => setSelectedAppointment(null)}>
                        <div className="bg-white rounded-3xl shadow-2xl max-w-lg w-full animate-in zoom-in-95 overflow-hidden" onClick={(e) => e.stopPropagation()}>

                            {/* Header gradient */}
                            <div className={`px-6 py-5 ${isVisio ? 'bg-gradient-to-r from-indigo-600 to-purple-600' : 'bg-gradient-to-r from-emerald-600 to-teal-600'}`}>
                                <div className="flex justify-between items-start">
                                    <div className="flex items-center gap-4">
                                        <div className="w-14 h-14 bg-white/20 backdrop-blur-sm rounded-2xl flex items-center justify-center text-white font-black text-xl">
                                            {contactName.charAt(0)}
                                        </div>
                                        <div>
                                            <h3 className="text-lg font-black text-white">{contactName}</h3>
                                            <div className="flex items-center gap-2 mt-1">
                                                <span className="text-white/70 text-xs font-medium">#{contactId.substring(0, 8)}</span>
                                                <span className={`text-[10px] font-black uppercase px-2 py-0.5 rounded-full ${apt.status === 'SCHEDULED' ? 'bg-white/20 text-white' :
                                                    apt.status === 'COMPLETED' ? 'bg-emerald-400/30 text-emerald-100' :
                                                        apt.status === 'CANCELLED' ? 'bg-rose-400/30 text-rose-100' :
                                                            'bg-amber-400/30 text-amber-100'
                                                    }`}>
                                                    {apt.status === 'SCHEDULED' ? '📅 Programmé' :
                                                        apt.status === 'COMPLETED' ? '✅ Terminé' :
                                                            apt.status === 'CANCELLED' ? '❌ Annulé' : '⚠️ No-Show'}
                                                </span>
                                            </div>
                                        </div>
                                    </div>
                                    <button onClick={() => setSelectedAppointment(null)} className="p-2 hover:bg-white/10 rounded-full text-white/60 hover:text-white transition-all">
                                        <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" /></svg>
                                    </button>
                                </div>
                            </div>

                            <div className="p-6 space-y-5 max-h-[65vh] overflow-y-auto">

                                {/* Countdown / Info Box */}
                                {isUpcoming && apt.status === 'SCHEDULED' && (
                                    <div className={`p-3 rounded-xl border text-center ${isToday2 ? 'bg-amber-50 border-amber-200' : 'bg-indigo-50 border-indigo-100'
                                        }`}>
                                        <span className={`text-xs font-black uppercase tracking-wider ${isToday2 ? 'text-amber-600' : 'text-indigo-500'}`}>
                                            {isToday2 ? `⏰ Dans ${diffHours}h` :
                                                diffDays === 1 ? '📅 Demain' :
                                                    `📅 Dans ${diffDays} jours`}
                                        </span>
                                    </div>
                                )}
                                {isPast && apt.status === 'SCHEDULED' && (
                                    <div className="p-3 rounded-xl border bg-rose-50 border-rose-200 text-center">
                                        <span className="text-xs font-black uppercase tracking-wider text-rose-500">⚠️ RDV passé — Mettre à jour le statut</span>
                                    </div>
                                )}

                                {/* Date & Heure */}
                                <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100">
                                    <div className={`w-12 h-12 rounded-xl flex items-center justify-center ${isVisio ? 'bg-indigo-100 text-indigo-600' : 'bg-emerald-100 text-emerald-600'}`}>
                                        <CalendarIcon size={22} />
                                    </div>
                                    <div className="flex-1">
                                        <p className="text-sm font-black text-slate-900 capitalize">{format(startDate, 'EEEE d MMMM yyyy', { locale: fr })}</p>
                                        <p className="text-xs text-slate-500 font-bold">{format(startDate, 'HH:mm')} — {format(endDate, 'HH:mm')} ({Math.round((endDate.getTime() - startDate.getTime()) / (1000 * 60))} min)</p>
                                    </div>
                                    <div className={`px-3 py-1.5 rounded-xl text-xs font-black ${isVisio ? 'bg-indigo-100 text-indigo-700' : 'bg-emerald-100 text-emerald-700'}`}>
                                        {isVisio ? '🎥 Visio' : '🏢 Agence'}
                                    </div>
                                </div>

                                {/* Contact & Coordonnées */}
                                <div className="space-y-2">
                                    <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 px-1">Coordonnées</h4>
                                    <div className="grid grid-cols-2 gap-2">
                                        {contactPhone && (
                                            <a href={`tel:${contactPhone}`} className="flex items-center gap-2 p-3 bg-slate-50 rounded-xl border border-slate-100 hover:bg-indigo-50 hover:border-indigo-200 transition-all group">
                                                <Phone size={14} className="text-slate-400 group-hover:text-indigo-500" />
                                                <span className="text-xs font-bold text-slate-700 truncate">{contactPhone}</span>
                                            </a>
                                        )}
                                        {contactEmail && (
                                            <a href={`mailto:${contactEmail}`} className="flex items-center gap-2 p-3 bg-slate-50 rounded-xl border border-slate-100 hover:bg-indigo-50 hover:border-indigo-200 transition-all group">
                                                <Send size={14} className="text-slate-400 group-hover:text-indigo-500" />
                                                <span className="text-xs font-bold text-slate-700 truncate">{contactEmail}</span>
                                            </a>
                                        )}
                                    </div>
                                </div>

                                {/* Lieu / Juriste — ÉDITABLE */}
                                <div className="space-y-2">
                                    <div className="flex items-center justify-between px-1">
                                        <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400">Lieu & Intervenant</h4>
                                    </div>
                                    <div className="space-y-2">
                                        {isVisio && apt.meetingLink && (
                                            <a href={apt.meetingLink} target="_blank" rel="noopener noreferrer" className="flex items-center gap-3 p-3 bg-indigo-50 rounded-xl border border-indigo-100 hover:bg-indigo-100 transition-all group">
                                                <Monitor size={16} className="text-indigo-500" />
                                                <span className="text-xs font-black text-indigo-700 flex-1">Rejoindre la visio (Meet)</span>
                                                <ExternalLink size={12} className="text-indigo-400" />
                                            </a>
                                        )}
                                        {!isVisio && (
                                            <div className="flex items-center gap-3 p-3 bg-emerald-50 rounded-xl border border-emerald-100">
                                                <MapPin size={16} className="text-emerald-500" />
                                                <div className="flex-1">
                                                    <p className="text-xs font-black text-emerald-800">{agency?.name || apt.agencyId || 'Agence non spécifiée'}</p>
                                                    {agency?.contactEmail && <p className="text-[10px] text-emerald-600">{agency.contactEmail}</p>}
                                                </div>
                                            </div>
                                        )}
                                        {/* Intervenant — éditable */}
                                        {editingStaff ? (
                                            <div className="flex items-center gap-2 p-2 bg-amber-50 rounded-xl border border-amber-200">
                                                <User size={14} className="text-amber-500" />
                                                <select
                                                    className="flex-1 text-xs font-bold bg-transparent border-none outline-none"
                                                    value={editStaffId}
                                                    onChange={(e) => setEditStaffId(e.target.value)}
                                                >
                                                    <option value="">— Non assigné —</option>
                                                    {allStaff.map((s: any) => (
                                                        <option key={s.id} value={s.id}>{s.name} ({s.role})</option>
                                                    ))}
                                                </select>
                                                <button disabled={isSavingField} onClick={() => handleSaveField(apt.id, 'hostUserId', editStaffId)} className="px-2 py-1 bg-amber-500 text-white rounded-lg text-[10px] font-black hover:bg-amber-600 disabled:opacity-50">
                                                    {isSavingField ? '...' : '✓'}
                                                </button>
                                                <button onClick={() => setEditingStaff(false)} className="text-slate-400 hover:text-slate-600 text-xs">✕</button>
                                            </div>
                                        ) : (
                                            <div className="flex items-center gap-3 p-3 bg-slate-50 rounded-xl border border-slate-100 group">
                                                <User size={16} className="text-indigo-500" />
                                                <span className="text-xs font-bold text-slate-700 flex-1">
                                                    {apt.hostUser ? `Juriste : ${(apt.hostUser as any).name}` : '— Non assigné —'}
                                                </span>
                                                <button onClick={() => { setEditingStaff(true); setEditStaffId(apt.hostUserId || ''); }} className="p-1 opacity-0 group-hover:opacity-100 hover:bg-slate-200 rounded-lg transition-all">
                                                    <Edit3 size={12} className="text-slate-400" />
                                                </button>
                                            </div>
                                        )}
                                    </div>
                                </div>

                                {/* Service — ÉDITABLE */}
                                <div className="space-y-2">
                                    <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 px-1">Service</h4>
                                    {editingService ? (
                                        <div className="flex items-center gap-2 p-2 bg-purple-50 rounded-xl border border-purple-200">
                                            <Zap size={14} className="text-purple-500" />
                                            <select
                                                className="flex-1 text-xs font-bold bg-transparent border-none outline-none"
                                                value={editServiceId}
                                                onChange={(e) => setEditServiceId(e.target.value)}
                                            >
                                                <option value="">— Choisir un service —</option>
                                                {SERVICES_CATALOG.map(s => (
                                                    <option key={s.id} value={s.id}>{s.title}</option>
                                                ))}
                                            </select>
                                            <button disabled={isSavingField} onClick={() => handleSaveField(apt.id, 'serviceId', editServiceId)} className="px-2 py-1 bg-purple-500 text-white rounded-lg text-[10px] font-black hover:bg-purple-600 disabled:opacity-50">
                                                {isSavingField ? '...' : '✓'}
                                            </button>
                                            <button onClick={() => setEditingService(false)} className="text-slate-400 hover:text-slate-600 text-xs">✕</button>
                                        </div>
                                    ) : (
                                        <div className="flex items-center gap-3 p-3 bg-purple-50 rounded-xl border border-purple-100 group">
                                            <Zap size={16} className="text-purple-500" />
                                            <div className="flex-1">
                                                <p className="text-xs font-black text-purple-800">{serviceMeta?.name || serviceId || 'Non défini'}</p>
                                                {serviceMeta?.category && <p className="text-[10px] text-purple-600">{serviceMeta.category}</p>}
                                            </div>
                                            <button onClick={() => { setEditingService(true); setEditServiceId(serviceId); }} className="p-1 opacity-0 group-hover:opacity-100 hover:bg-purple-200 rounded-lg transition-all">
                                                <Edit3 size={12} className="text-purple-400" />
                                            </button>
                                        </div>
                                    )}
                                </div>

                                {/* Dossier Status */}
                                {apt.dossierStatus && apt.dossierStatus !== 'EMPTY' && (
                                    <div className={`p-4 rounded-2xl border flex items-center justify-between ${apt.dossierStatus === 'COMPLETE' ? 'bg-emerald-50 border-emerald-100 text-emerald-700' :
                                        apt.dossierStatus === 'PARTIAL' ? 'bg-orange-50 border-orange-100 text-orange-700' :
                                            'bg-rose-50 border-rose-100 text-rose-700'
                                        }`}>
                                        <div className="flex items-center gap-2">
                                            <CheckCircle2 size={16} />
                                            <span className="text-xs font-black uppercase tracking-wider">
                                                Dossier {apt.dossierStatus === 'COMPLETE' ? 'Complet ✓' :
                                                    apt.dossierStatus === 'PARTIAL' ? 'Incomplet' : 'Vide'}
                                            </span>
                                        </div>
                                        {apt.missingDocsCount > 0 && (
                                            <span className="text-[10px] font-bold">{apt.missingDocsCount} pièces manquantes</span>
                                        )}
                                    </div>
                                )}

                                {/* ═══ ACTIONS RAPIDES ═══ */}
                                <div className="space-y-2">
                                    <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 px-1">Actions rapides</h4>
                                    <div className="grid grid-cols-4 gap-2">
                                        {/* Appeler */}
                                        {contactPhone && (
                                            <button
                                                onClick={() => setActivePanel(activePanel === 'call' ? 'none' : 'call')}
                                                className={`flex flex-col items-center gap-1.5 p-3 rounded-xl border transition-all cursor-pointer ${activePanel === 'call' ? 'bg-blue-100 border-blue-300 ring-2 ring-blue-200' : 'bg-blue-50 border-blue-100 hover:bg-blue-100'}`}
                                            >
                                                <PhoneCall size={16} className="text-blue-500" />
                                                <span className="text-[10px] font-black text-blue-700">Appeler</span>
                                            </button>
                                        )}
                                        {/* WhatsApp */}
                                        {contactPhone && (
                                            <button
                                                onClick={() => setActivePanel(activePanel === 'whatsapp' ? 'none' : 'whatsapp')}
                                                className={`flex flex-col items-center gap-1.5 p-3 rounded-xl border transition-all cursor-pointer ${activePanel === 'whatsapp' ? 'bg-emerald-100 border-emerald-300 ring-2 ring-emerald-200' : 'bg-emerald-50 border-emerald-100 hover:bg-emerald-100'}`}
                                            >
                                                <svg viewBox="0 0 24 24" className="w-4 h-4 text-emerald-500 fill-current"><path d="M17.472 14.382c-.297-.149-1.758-.867-2.03-.967-.273-.099-.471-.148-.67.15-.197.297-.767.966-.94 1.164-.173.199-.347.223-.644.075-.297-.15-1.255-.463-2.39-1.475-.883-.788-1.48-1.761-1.653-2.059-.173-.297-.018-.458.13-.606.134-.133.298-.347.446-.52.149-.174.198-.298.298-.497.099-.198.05-.371-.025-.52-.075-.149-.669-1.612-.916-2.207-.242-.579-.487-.5-.669-.51-.173-.008-.371-.01-.57-.01-.198 0-.52.074-.792.372-.272.297-1.04 1.016-1.04 2.479 0 1.462 1.065 2.875 1.213 3.074.149.198 2.096 3.2 5.077 4.487.709.306 1.262.489 1.694.625.712.227 1.36.195 1.871.118.571-.085 1.758-.719 2.006-1.413.248-.694.248-1.289.173-1.413-.074-.124-.272-.198-.57-.347z" /><path d="M12 0C5.373 0 0 5.373 0 12c0 2.625.846 5.059 2.284 7.034L.789 23.492a.5.5 0 00.612.612l4.458-1.495A11.937 11.937 0 0012 24c6.627 0 12-5.373 12-12S18.627 0 12 0zm0 22c-2.335 0-4.512-.745-6.29-2.013l-.44-.334-2.633.883.883-2.633-.334-.44A9.957 9.957 0 012 12C2 6.486 6.486 2 12 2s10 4.486 10 10-4.486 10-10 10z" /></svg>
                                                <span className="text-[10px] font-black text-emerald-700">WhatsApp</span>
                                            </button>
                                        )}
                                        {/* Email */}
                                        {contactEmail && (
                                            <button onClick={() => copyToClipboard(contactEmail, 'Email')} className="flex flex-col items-center gap-1.5 p-3 bg-violet-50 rounded-xl border border-violet-100 hover:bg-violet-100 transition-all cursor-pointer">
                                                <Send size={16} className="text-violet-500" />
                                                <span className="text-[10px] font-black text-violet-700">Email</span>
                                            </button>
                                        )}
                                        {/* Simulateur */}
                                        <button
                                            onClick={() => {
                                                const simUrl = `/?service=${serviceId || 'naturalisation'}${apt.prospectId ? `&prospect=${apt.prospectId}` : ''}&simulator=true`;
                                                window.open(simUrl, '_blank');
                                                showToast('🚀 Simulateur ouvert');
                                            }}
                                            className="flex flex-col items-center gap-1.5 p-3 bg-gradient-to-b from-indigo-50 to-purple-50 rounded-xl border border-indigo-100 hover:from-indigo-100 hover:to-purple-100 transition-all cursor-pointer"
                                        >
                                            <Zap size={16} className="text-indigo-500" />
                                            <span className="text-[10px] font-black text-indigo-700">Simulateur</span>
                                        </button>
                                    </div>

                                    {/* ════ PANEL: MINI COCKPIT D'APPEL ════ */}
                                    {activePanel === 'call' && contactPhone && (
                                        <div className="p-4 bg-gradient-to-b from-blue-50 to-slate-50 rounded-2xl border border-blue-200 space-y-3 animate-in slide-in-from-top-2">
                                            <div className="flex items-center justify-between">
                                                <div className="flex items-center gap-2">
                                                    <div className={`w-3 h-3 rounded-full ${isInCall ? 'bg-red-500 animate-pulse' : 'bg-blue-400'}`} />
                                                    <span className="text-xs font-black text-slate-700">{isInCall ? `En cours — ${formatCallDuration(callDuration)}` : contactPhone}</span>
                                                </div>
                                                <span className="text-[10px] font-bold text-slate-400">RDV: {format(startDate, 'd MMM HH:mm', { locale: fr })}</span>
                                            </div>
                                            {/* Call Controls */}
                                            <div className="flex items-center justify-center gap-4">
                                                {!isInCall ? (
                                                    <button onClick={handleStartCall} className="w-14 h-14 bg-green-500 hover:bg-green-600 text-white rounded-full flex items-center justify-center shadow-lg hover:shadow-xl transition-all active:scale-95">
                                                        <PhoneCall size={22} />
                                                    </button>
                                                ) : (
                                                    <>
                                                        <button onClick={() => setIsMuted(!isMuted)} className={`w-10 h-10 rounded-full flex items-center justify-center transition-all ${isMuted ? 'bg-amber-100 text-amber-600' : 'bg-slate-100 text-slate-500 hover:bg-slate-200'}`}>
                                                            {isMuted ? <MicOff size={16} /> : <Mic size={16} />}
                                                        </button>
                                                        <button onClick={handleEndCall} className="w-14 h-14 bg-red-500 hover:bg-red-600 text-white rounded-full flex items-center justify-center shadow-lg hover:shadow-xl transition-all active:scale-95">
                                                            <PhoneOff size={22} />
                                                        </button>
                                                        <button onClick={() => copyToClipboard(contactPhone, 'Numéro')} className="w-10 h-10 bg-slate-100 text-slate-500 hover:bg-slate-200 rounded-full flex items-center justify-center transition-all">
                                                            <Copy size={14} />
                                                        </button>
                                                    </>
                                                )}
                                            </div>
                                            {/* Call Note */}
                                            <textarea
                                                className="w-full p-2.5 rounded-xl border border-blue-100 text-xs focus:ring-2 focus:ring-blue-400 outline-none resize-none bg-white"
                                                placeholder="Notes d'appel..."
                                                rows={2}
                                                value={callNote}
                                                onChange={(e) => setCallNote(e.target.value)}
                                            />
                                            {callNote.trim() && (
                                                <button onClick={() => { showToast('💾 Note sauvegardée'); setActivePanel('none'); }} className="w-full py-2 bg-blue-500 text-white rounded-xl text-xs font-black hover:bg-blue-600 transition-all">
                                                    💾 Sauvegarder la note
                                                </button>
                                            )}
                                        </div>
                                    )}

                                    {/* ════ PANEL: HUB WHATSAPP ════ */}
                                    {activePanel === 'whatsapp' && contactPhone && (
                                        <div className="rounded-2xl overflow-hidden border border-emerald-200 animate-in slide-in-from-top-2">
                                            <WhatsAppWidget
                                                contactId={contactId}
                                                contactType={apt.leadId ? 'LEAD' : 'PROSPECT'}
                                                contactName={contactName}
                                                contactPhone={contactPhone}
                                            />
                                        </div>
                                    )}

                                    {/* Ouvrir le dossier */}
                                    {(apt.leadId || apt.prospectId) && (
                                        <button
                                            onClick={() => {
                                                setSelectedAppointment(null);
                                                if (apt.leadId) {
                                                    window.location.href = `/admin/dossiers?leadId=${apt.leadId}`;
                                                } else if (apt.prospectId) {
                                                    window.location.href = `/admin/sales`;
                                                }
                                            }}
                                            className="w-full flex items-center justify-center gap-2 p-3 bg-slate-100 rounded-xl text-slate-700 hover:bg-slate-200 transition-all"
                                        >
                                            <ExternalLink size={14} />
                                            <span className="text-xs font-bold">{apt.leadId ? 'Ouvrir le dossier client' : 'Voir le Pipeline Ventes'}</span>
                                        </button>
                                    )}
                                </div>
                            </div>

                            {/* Footer Actions */}
                            <div className="px-6 py-4 border-t border-slate-100 bg-slate-50/50">
                                {isCancelling ? (
                                    <div className="space-y-3">
                                        <label className="text-xs font-black uppercase text-rose-500 block">Motif d'annulation</label>
                                        <textarea
                                            className="w-full p-3 rounded-xl border border-rose-200 text-sm focus:ring-2 focus:ring-rose-500 outline-none"
                                            placeholder="Expliquez pourquoi..."
                                            rows={2}
                                            value={cancellationReason}
                                            onChange={(e) => setCancellationReason(e.target.value)}
                                        />
                                        <div className="flex gap-2">
                                            <button onClick={() => setIsCancelling(false)} className="px-4 py-2 text-rose-400 font-bold hover:bg-rose-100 rounded-lg text-xs">Retour</button>
                                            <button onClick={handleCancelAppointment} className="flex-1 px-4 py-2 bg-rose-500 text-white font-bold rounded-lg text-xs shadow-md hover:bg-rose-600 transition-colors">Confirmer l'annulation</button>
                                        </div>
                                    </div>
                                ) : (
                                    <div className="flex gap-3">
                                        <button onClick={() => setIsCancelling(true)} className="flex-1 py-3 text-rose-500 font-bold hover:bg-rose-50 rounded-xl transition-colors text-sm">
                                            Annuler le RDV
                                        </button>
                                        <button onClick={() => setSelectedAppointment(null)} className="flex-1 bg-slate-900 text-white py-3 rounded-xl font-black shadow-lg hover:bg-slate-800 transition-all text-sm">
                                            Fermer
                                        </button>
                                    </div>
                                )}
                            </div>
                        </div>
                    </div>
                );
            })()}

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

            {/* Toast notification */}
            {toastMessage && (
                <div className="fixed bottom-6 left-1/2 -translate-x-1/2 z-[100] animate-in fade-in slide-in-from-bottom-4">
                    <div className="bg-slate-900 text-white px-6 py-3 rounded-2xl shadow-2xl font-bold text-sm flex items-center gap-2">
                        {toastMessage}
                    </div>
                </div>
            )}
        </div>
    );
}
