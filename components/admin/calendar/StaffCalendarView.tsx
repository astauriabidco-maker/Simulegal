'use client';

import React, { useState, useEffect } from 'react';
import { Calendar as CalendarIcon, Clock, User, MapPin, Video, Monitor, ChevronLeft, ChevronRight, Filter, CheckCircle2 } from 'lucide-react';
import { CalendarStore, Appointment } from '../../../services/CalendarStore';
import { AgencyStore } from '../../../services/AgencyStore';
import { format, startOfWeek, endOfWeek, addWeeks, subWeeks, eachDayOfInterval, isSameDay, isToday } from 'date-fns';
import { fr } from 'date-fns/locale';

interface StaffCalendarViewProps {
    currentUserRole: 'HQ_ADMIN' | 'AGENCY_MANAGER' | 'CASE_WORKER';
    currentUserAgencyId?: string; // If null/HQ -> View all or filter
}

export default function StaffCalendarView({ currentUserRole, currentUserAgencyId }: StaffCalendarViewProps) {
    const [viewDate, setViewDate] = useState(new Date());
    const [appointments, setAppointments] = useState<Appointment[]>([]);
    const [selectedAppointment, setSelectedAppointment] = useState<Appointment | null>(null);
    const [loading, setLoading] = useState(false);

    // Filters
    const [filterType, setFilterType] = useState<'ALL' | 'VISIO' | 'AGENCY'>('ALL');
    const [filterAgencyId, setFilterAgencyId] = useState<string>('ALL');
    const [agencies, setAgencies] = useState<any[]>([]);

    useEffect(() => {
        loadData();
    }, [viewDate, filterAgencyId, filterType]);

    useEffect(() => {
        loadAgencies();
    }, []);

    const loadAgencies = async () => {
        if (currentUserRole === 'HQ_ADMIN') {
            const all = await AgencyStore.getAllAgencies();
            setAgencies(all);
        }
    };

    const loadData = async () => {
        setLoading(true);
        try {
            // Simulate fetching range (store mock fetches all)
            const all = await CalendarStore.getAllAppointments();

            // Apply Filters
            let filtered = all;

            // 1. Role Scope
            if (currentUserRole === 'AGENCY_MANAGER' || currentUserRole === 'CASE_WORKER') {
                if (currentUserAgencyId) {
                    filtered = filtered.filter(a => a.agencyId === currentUserAgencyId);
                }
            }

            // 2. UI Filters (HQ only essentially)
            if (filterType === 'VISIO') {
                filtered = filtered.filter(a => a.type === 'VISIO_JURISTE');
            } else if (filterType === 'AGENCY') {
                filtered = filtered.filter(a => a.type === 'PHYSICAL_AGENCY');
            }

            if (filterAgencyId !== 'ALL') {
                filtered = filtered.filter(a => a.agencyId === filterAgencyId);
            }

            setAppointments(filtered);
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

    return (
        <div className="h-full flex flex-col bg-white rounded-3xl shadow-sm border border-slate-100 overflow-hidden">
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
                                    return (
                                        <div key={`${day}-${hour}`} className="h-28 border-b border-slate-50 relative group">
                                            {/* Grid line helper */}
                                            <div className="absolute inset-x-0 top-1/2 border-t border-slate-50 border-dashed opacity-0 group-hover:opacity-50" />

                                            {slotApts.map(apt => {
                                                const startMin = new Date(apt.start).getMinutes();
                                                const duration = (new Date(apt.end).getTime() - new Date(apt.start).getTime()) / (1000 * 60);
                                                const height = (duration / 60) * 112; // 28px * 4 units? No, h-28 is 112px for 60min.
                                                const top = (startMin / 60) * 112;

                                                const isVisio = apt.type === 'VISIO_JURISTE';

                                                return (
                                                    <button
                                                        key={apt.id}
                                                        onClick={() => setSelectedAppointment(apt)}
                                                        className={`
                                                            absolute left-1 right-1 rounded-lg px-2 py-1 text-left text-xs transition-all hover:scale-[1.02] hover:z-10 shadow-sm border-l-4
                                                            ${isVisio
                                                                ? 'bg-indigo-100 border-indigo-500 text-indigo-800'
                                                                : 'bg-emerald-100 border-emerald-500 text-emerald-800'
                                                            }
                                                        `}
                                                        style={{ top: `${top}px`, height: `${height}px` }}
                                                    >
                                                        <div className="font-bold truncate">{apt.leadName}</div>
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
                <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-slate-900/40 backdrop-blur-sm">
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

                            <div className="space-y-3">
                                <div className="flex items-center gap-3 text-slate-600">
                                    <Clock size={18} className="text-indigo-500" />
                                    <span className="font-bold">{format(new Date(selectedAppointment.start), 'EEEE d MMMM', { locale: fr })}</span>
                                    <span>{format(new Date(selectedAppointment.start), 'HH:mm')} - {format(new Date(selectedAppointment.end), 'HH:mm')}</span>
                                </div>
                                {selectedAppointment.type === 'VISIO_JURISTE' && selectedAppointment.meetingLink && (
                                    <a href={selectedAppointment.meetingLink} target="_blank" rel="noopener noreferrer" className="flex items-center gap-3 text-indigo-600 hover:underline">
                                        <Monitor size={18} />
                                        <span className="font-bold">Rejoindre la visio (Meet)</span>
                                    </a>
                                )}
                                {selectedAppointment.type === 'PHYSICAL_AGENCY' && (
                                    <div className="flex items-center gap-3 text-slate-600">
                                        <MapPin size={18} className="text-emerald-500" />
                                        <span className="font-bold">Agence Physique</span>
                                    </div>
                                )}
                            </div>
                        </div>

                        <div className="flex gap-3">
                            <button className="flex-1 py-3 text-slate-400 font-bold hover:bg-slate-50 rounded-xl transition-colors">Annuler</button>
                            <button onClick={() => setSelectedAppointment(null)} className="flex-1 bg-slate-900 text-white py-3 rounded-xl font-black shadow-lg hover:bg-slate-800 transition-all">
                                OK
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
