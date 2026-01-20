import React, { useState, useEffect, useMemo } from 'react';
import {
    TrendingUp,
    AlertTriangle,
    Clock,
    Euro,
    ChevronRight,
    Info,
    CheckCircle,
    Video,
    MapPin,
    MoreVertical,
    User
} from 'lucide-react';
import { format, startOfDay, endOfDay, startOfWeek, endOfWeek, eachHourOfInterval, isSameDay, addMinutes, isAfter, isBefore, addDays } from 'date-fns';
import { fr } from 'date-fns/locale';
import { CalendarStore, Appointment } from '../../../services/CalendarStore';
import { UserStore, StaffUser } from '../../../services/UserStore';
import { AgencyStore, AgencyExt } from '../../../services/AgencyStore';
import { CalendarAnalytics, AgendaStats } from '../../../services/CalendarAnalytics';
import { AgendaFilters, AgendaFilterState } from './AgendaFilters';

export const AgendaDashboard: React.FC = () => {
    const [loading, setLoading] = useState(true);
    const [appointments, setAppointments] = useState<Appointment[]>([]);
    const [staff, setStaff] = useState<StaffUser[]>([]);
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [filters, setFilters] = useState<AgendaFilterState>({
        period: 'WEEK',
        viewMode: 'JURIST',
        services: [],
        statuses: ['SCHEDULED', 'COMPLETED', 'NO_SHOW'],
        types: ['VISIO_JURISTE', 'PHYSICAL_AGENCY'],
        search: '',
        specialFilters: []
    });

    // Load Data
    useEffect(() => {
        const loadAll = async () => {
            setLoading(true);
            try {
                const [allApts, allStaff, allAgencies] = await Promise.all([
                    CalendarStore.getAllAppointments(),
                    UserStore.getAllUsers(),
                    AgencyStore.getAllAgencies()
                ]);
                setAppointments(allApts);
                setStaff(allStaff);
                setAgencies(allAgencies);
            } finally {
                setLoading(false);
            }
        };
        loadAll();
    }, []);

    // Filtered Appointments
    const filteredApts = useMemo(() => {
        // First compute conflicts if necessary
        const conflictIds = new Set<string>();
        if (filters.specialFilters?.includes('CONFLICTS')) {
            const sorted = [...appointments].sort((a, b) => new Date(a.start).getTime() - new Date(b.start).getTime());
            for (let i = 0; i < sorted.length - 1; i++) {
                const current = sorted[i];
                const next = sorted[i + 1];
                if (current.hostUserId === next.hostUserId && isAfter(new Date(current.end), new Date(next.start))) {
                    conflictIds.add(current.id);
                    conflictIds.add(next.id);
                }
            }
        }

        return appointments.filter(apt => {
            const matchesService = filters.services.length === 0 || (apt.lead?.serviceId && filters.services.includes(apt.lead.serviceId));
            const matchesStatus = filters.statuses.length === 0 || filters.statuses.includes(apt.status); // Fixed: Check length === 0 too
            const matchesType = filters.types.includes(apt.type);
            const matchesSearch = !filters.search ||
                apt.leadName.toLowerCase().includes(filters.search.toLowerCase()) ||
                (apt.agencyId && agencies.find((a: AgencyExt) => a.id === apt.agencyId)?.name.toLowerCase().includes(filters.search.toLowerCase()));

            // Operational Filters
            let matchesSpecial = true;
            if (filters.specialFilters && filters.specialFilters.length > 0) {
                if (filters.specialFilters.includes('MISSING_DOCS')) {
                    matchesSpecial = matchesSpecial && (apt.dossierStatus === 'INCOMPLETE' || apt.dossierStatus === 'PARTIAL');
                }
                if (filters.specialFilters.includes('UNCONFIRMED')) {
                    const start = new Date(apt.start);
                    const tomorrow = endOfDay(addDays(new Date(), 1));
                    const isTomorrow = isAfter(start, new Date()) && isBefore(start, tomorrow);
                    matchesSpecial = matchesSpecial && isTomorrow && apt.status === 'SCHEDULED';
                }
                if (filters.specialFilters.includes('CONFLICTS')) {
                    matchesSpecial = matchesSpecial && conflictIds.has(apt.id);
                }
            }

            return matchesService && matchesStatus && matchesType && matchesSearch && matchesSpecial;
        });
    }, [appointments, filters, agencies]);

    // Analytics
    const stats = useMemo(() => CalendarAnalytics.computeStats(filteredApts), [filteredApts]);

    // Resources for the Y-Axis
    const resources = useMemo(() => {
        if (filters.viewMode === 'JURIST') {
            return staff.map(s => ({ id: s.id, name: s.name, type: 'STAFF' }));
        }
        return agencies.map((a: AgencyExt) => ({ id: a.id, name: a.name, type: 'AGENCY' }));
    }, [filters.viewMode, staff, agencies]);

    // Time slots for X-Axis (8h-20h)
    const timeSlots = Array.from({ length: 13 }, (_, i) => i + 8);

    // Conflicts & Alerts
    const alerts = useMemo(() => {
        const list: { id: string, type: 'CONFLICT' | 'PENDING' | 'CONFIRMATION', msg: string, severity: 'HIGH' | 'MEDIUM' }[] = [];

        // 1. Conflicts (Overlap)
        const sorted = [...filteredApts].sort((a, b) => new Date(a.start).getTime() - new Date(b.start).getTime());
        for (let i = 0; i < sorted.length - 1; i++) {
            const current = sorted[i];
            const next = sorted[i + 1];
            if (current.hostUserId === next.hostUserId && isAfter(new Date(current.end), new Date(next.start))) {
                list.push({
                    id: `conf-${current.id}`,
                    type: 'CONFLICT',
                    msg: `Double booking pour ${current.hostUser?.name || 'Staff'}: ${current.leadName} & ${next.leadName}`,
                    severity: 'HIGH'
                });
            }
        }

        // 2. Unconfirmed for tomorrow
        const tomorrow = endOfDay(addMinutes(new Date(), 24 * 60));
        const unconfirmedCount = filteredApts.filter((a: Appointment) => {
            const start = new Date(a.start);
            return isAfter(start, new Date()) && isBefore(start, tomorrow) && a.status === 'SCHEDULED';
        }).length;

        if (unconfirmedCount > 0) {
            list.push({
                id: 'unconfirmed',
                type: 'CONFIRMATION',
                msg: `${unconfirmedCount} RDV pour demain attendent une confirmation`,
                severity: 'MEDIUM'
            });
        }

        return list;
    }, [filteredApts]);

    // Drag & Drop Handlers
    const handleDragStart = (e: React.DragEvent, apt: Appointment) => {
        e.dataTransfer.setData('appointmentId', apt.id);
        e.dataTransfer.effectAllowed = 'move';
    };

    const handleDrop = async (e: React.DragEvent, resourceId: string, resourceType: 'STAFF' | 'AGENCY') => {
        e.preventDefault();
        const aptId = e.dataTransfer.getData('appointmentId');
        const apt = appointments.find((a: Appointment) => a.id === aptId);

        if (!apt) return;

        // In a real app, we'd calculate the new time based on where it was dropped.
        // For this demo/requirement, we'll just reassign the resource.
        try {
            // Update local state for immediate feedback
            const updatedApts = appointments.map((a: Appointment) => {
                if (a.id === aptId) {
                    return {
                        ...a,
                        hostUserId: resourceType === 'STAFF' ? resourceId : a.hostUserId,
                        agencyId: resourceType === 'AGENCY' ? resourceId : a.agencyId,
                        type: resourceType === 'AGENCY' ? 'PHYSICAL_AGENCY' : 'VISIO_JURISTE' as any
                    };
                }
                return a;
            });
            setAppointments(updatedApts);

            // API Call
            await CalendarStore.updateAppointment(aptId, {
                start: apt.start,
                end: apt.end,
                hostUserId: resourceType === 'STAFF' ? resourceId : undefined,
                agencyId: resourceType === 'AGENCY' ? resourceId : undefined,
                type: resourceType === 'AGENCY' ? 'PHYSICAL_AGENCY' : 'VISIO_JURISTE' as any
            });
            // Note: Our current updateAppointment only takes start/end, 
            // we'd need a more robust API for resource reassignment.
        } catch (err) {
            console.error('Failed to move appointment:', err);
        }
    };

    // KPI Click Handlers
    const handleKpiClick = (type: 'NO_SHOW' | 'OCCUPANCY' | 'VOLUME') => {
        if (type === 'NO_SHOW') {
            setFilters((prev: AgendaFilterState) => ({ ...prev, statuses: ['NO_SHOW'] }));
        } else if (type === 'VOLUME') {
            setFilters((prev: AgendaFilterState) => ({ ...prev, period: 'TODAY' }));
        }
    };

    if (loading) return (
        <div className="flex flex-col items-center justify-center h-full p-20 animate-pulse">
            <div className="w-12 h-12 bg-indigo-100 rounded-full mb-4 animate-bounce" />
            <p className="text-slate-400 font-black text-sm uppercase tracking-widest">Initialisation du Dashboard...</p>
        </div>
    );

    return (
        <div className="flex flex-col h-full bg-slate-50/30">
            {/* Header */}
            <div className="p-8 pb-0">
                <div className="flex justify-between items-end mb-8">
                    <div>
                        <h1 className="text-3xl font-black text-slate-900 tracking-tight">Cockpit Temporel</h1>
                        <p className="text-slate-500 font-bold">Pilotage et Analytics de l'activité groupe</p>
                    </div>
                    <div className="flex items-center gap-3">
                        <span className="text-xs font-black uppercase text-slate-400 bg-white px-4 py-2 rounded-xl border border-slate-100 italic">
                            Dernière refresh: {format(new Date(), 'HH:mm:ss')}
                        </span>
                    </div>
                </div>

                {/* KPI Cards */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
                    {/* Occupancy Rate */}
                    <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 relative overflow-hidden group">
                        <div className="flex justify-between items-start mb-4">
                            <div className="p-2 bg-indigo-50 rounded-xl text-indigo-600">
                                <Clock size={24} />
                            </div>
                            <div className={`text-[10px] font-black px-2 py-1 rounded-lg ${stats.occupancyRate > 80 ? 'bg-emerald-100 text-emerald-600' : 'bg-rose-100 text-rose-600'}`}>
                                {stats.occupancyRate > 80 ? 'Optimal' : 'Sous-utilisé'}
                            </div>
                        </div>
                        <h3 className="text-slate-400 text-xs font-black uppercase tracking-widest mb-1">Taux d'Occupation</h3>
                        <div className="flex items-end gap-2">
                            <span className="text-3xl font-black text-slate-900">{Math.round(stats.occupancyRate)}%</span>
                            <div className="flex-1 h-2 mb-2 bg-slate-100 rounded-full overflow-hidden">
                                <div
                                    className={`h-full transition-all duration-1000 ${stats.occupancyRate > 80 ? 'bg-emerald-500' : stats.occupancyRate > 50 ? 'bg-indigo-500' : 'bg-rose-500'}`}
                                    style={{ width: `${stats.occupancyRate}%` }}
                                />
                            </div>
                        </div>
                        <div className="absolute top-0 right-0 p-8 opacity-5 group-hover:opacity-10 transition-opacity pointer-events-none">
                            <TrendingUp size={80} />
                        </div>
                    </div>

                    {/* No-Show Rate */}
                    <div
                        onClick={() => handleKpiClick('NO_SHOW')}
                        className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 group cursor-pointer hover:border-rose-200 transition-colors"
                    >
                        <div className="flex justify-between items-start mb-4">
                            <div className="p-2 bg-rose-50 rounded-xl text-rose-600">
                                <AlertTriangle size={24} />
                            </div>
                            <button className="text-[10px] font-black underline text-slate-400 hover:text-slate-600">Liste critique</button>
                        </div>
                        <h3 className="text-slate-400 text-xs font-black uppercase tracking-widest mb-1">No-Show Rate</h3>
                        <div className="flex items-center gap-3">
                            <span className="text-3xl font-black text-slate-900">{stats.noShowRate.toFixed(1)}%</span>
                            <span className="text-[10px] font-bold text-rose-500 flex items-center gap-1">
                                <TrendingUp size={12} className="rotate-0" />
                                Attention
                            </span>
                        </div>
                        <p className="text-[10px] text-slate-400 font-medium mt-2">Impact estimé: -{Math.round(stats.noShowRate * 12)}h / semaine</p>
                    </div>

                    {/* Weekly Volume */}
                    <div
                        onClick={() => handleKpiClick('VOLUME')}
                        className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 cursor-pointer hover:border-emerald-200 transition-colors"
                    >
                        <div className="flex justify-between items-start mb-4">
                            <div className="p-2 bg-emerald-50 rounded-xl text-emerald-600">
                                <TrendingUp size={24} />
                            </div>
                        </div>
                        <h3 className="text-slate-400 text-xs font-black uppercase tracking-widest mb-1">Volume Hebdo</h3>
                        <div className="flex items-end gap-2">
                            <span className="text-3xl font-black text-slate-900">{stats.totalAppointments}</span>
                            <div className="flex-1 flex gap-1 h-8 items-end pb-1">
                                {stats.dailyVolume.map((v, i) => (
                                    <div key={i} className="flex-1 bg-emerald-100 rounded-sm hover:bg-emerald-500 transition-colors" style={{ height: `${Math.max(10, v * 10)}%` }} title={`J-${6 - i}: ${v} RDV`} />
                                ))}
                            </div>
                        </div>
                        <p className="text-[10px] text-slate-400 font-medium mt-1">+{stats.upcomingVolume} RDV dans les 24h</p>
                    </div>

                    {/* Revenue Estimate */}
                    <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 bg-gradient-to-br from-white to-indigo-50/30">
                        <div className="flex justify-between items-start mb-4">
                            <div className="p-2 bg-slate-900 rounded-xl text-white shadow-lg">
                                <Euro size={24} />
                            </div>
                        </div>
                        <h3 className="text-slate-400 text-xs font-black uppercase tracking-widest mb-1">Revenus Estimés</h3>
                        <span className="text-3xl font-black text-slate-900">{Math.round(stats.revenueEstimate).toLocaleString('fr-FR')} €</span>
                        <div className="mt-3 flex items-center gap-1">
                            <div className="flex -space-x-2">
                                {[1, 2, 3].map(i => <div key={i} className="w-5 h-5 rounded-full border-2 border-white bg-slate-200" />)}
                            </div>
                            <span className="text-[10px] text-slate-400 font-bold ml-2">Basé sur {filteredApts.length} RDV</span>
                        </div>
                    </div>
                </div>

                {/* Filters Bar */}
                <AgendaFilters filters={filters} onFilterChange={setFilters} />
            </div>

            {/* Main Content: Scheduler + alerts */}
            <div className="flex-1 flex overflow-hidden p-8 pt-0 gap-8">

                {/* Resource Scheduler Area */}
                <div className="flex-1 bg-white rounded-[40px] shadow-xl border border-slate-100 flex flex-col overflow-hidden">

                    {/* Scheduler Header (Hours) */}
                    <div className="flex border-b border-slate-100 bg-slate-50/50">
                        <div className="w-48 flex-shrink-0 p-4 border-r border-slate-100 font-black text-[10px] uppercase tracking-widest text-slate-400 flex items-center justify-center bg-white rounded-tl-[40px]">
                            Ressources ({resources.length})
                        </div>
                        <div className="flex-1 flex overflow-x-auto select-none no-scrollbar">
                            {timeSlots.map(hour => (
                                <div key={hour} className="flex-1 min-w-[100px] p-4 text-center border-r border-slate-50 last:border-0 font-black text-xs text-slate-600 h-16 flex flex-col justify-center">
                                    <span>{hour}:00</span>
                                    <div className="flex gap-0.5 justify-center mt-1">
                                        <div className="w-1 h-3 bg-slate-100 rounded-full" />
                                        <div className="w-1 h-5 bg-slate-200 rounded-full" />
                                        <div className="w-1 h-3 bg-slate-100 rounded-full" />
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Scheduler Body */}
                    <div className="flex-1 overflow-y-auto custom-scrollbar">
                        {resources.map(res => {
                            const resApts = filteredApts.filter(a => a.hostUserId === res.id || a.agencyId === res.id);

                            return (
                                <div key={res.id} className="flex border-b border-slate-50 hover:bg-slate-50/30 transition-colors group min-h-[100px]">
                                    {/* Resource Label */}
                                    <div className="w-48 flex-shrink-0 p-6 border-r border-slate-100 flex flex-col items-center justify-center bg-white sticky left-0 z-10 shadow-[5px_0_10px_rgba(0,0,0,0.02)]">
                                        <div className="w-12 h-12 bg-slate-100 rounded-2xl flex items-center justify-center text-slate-400 mb-2 group-hover:scale-110 transition-transform shadow-inner">
                                            {res.type === 'STAFF' ? <User size={20} /> : <MapPin size={20} />}
                                        </div>
                                        <span className="text-xs font-black text-slate-900 text-center leading-tight">{res.name}</span>
                                        <span className="text-[9px] font-bold text-slate-400 mt-1 uppercase tracking-tighter">
                                            {resApts.length} RDV {filters.period === 'TODAY' ? "Auj." : "Période"}
                                        </span>
                                    </div>

                                    {/* Slots Area */}
                                    <div
                                        className="flex-1 flex relative h-full min-w-[1300px]"
                                        onDragOver={(e) => e.preventDefault()}
                                        onDrop={(e) => handleDrop(e, res.id, res.type as any)}
                                    >
                                        {/* BG Grid Lines */}
                                        {timeSlots.map(h => (
                                            <div key={h} className="flex-1 border-r border-slate-50/50 h-full pointer-events-none" />
                                        ))}

                                        {/* Render Appointments */}
                                        {resApts.map(apt => {
                                            const start = new Date(apt.start);
                                            const end = new Date(apt.end);

                                            // Calculate horizontal position
                                            const startHour = start.getHours() + start.getMinutes() / 60;
                                            const endHour = end.getHours() + end.getMinutes() / 60;

                                            if (startHour < 8 || startHour > 20) return null;

                                            const left = ((startHour - 8) / (21 - 8)) * 100;
                                            const width = ((endHour - startHour) / (21 - 8)) * 100;

                                            const isVisio = apt.type === 'VISIO_JURISTE';
                                            const isPast = isBefore(new Date(apt.end), new Date());

                                            return (
                                                <button
                                                    key={apt.id}
                                                    draggable
                                                    onDragStart={(e: React.DragEvent) => handleDragStart(e, apt)}
                                                    onClick={() => console.log('Open Lead:', apt.leadId)}
                                                    className={`absolute top-4 bottom-4 rounded-2xl shadow-lg border-l-4 p-3 text-left transition-all hover:scale-[1.02] hover:z-20 overflow-hidden group/block
                                                        ${isPast ? 'bg-slate-100 border-slate-400 opacity-60' :
                                                            isVisio ? 'bg-indigo-600 border-white text-white shadow-indigo-200' :
                                                                'bg-emerald-500 border-white text-white shadow-emerald-200'}
                                                    `}
                                                    style={{ left: `${left}%`, width: `${width}%` }}
                                                >
                                                    <div className="flex justify-between items-start gap-1">
                                                        <span className="text-[10px] font-black uppercase tracking-tighter truncate">{apt.leadName}</span>
                                                        <MoreVertical size={12} className="opacity-40" />
                                                    </div>
                                                    <div className="flex items-center gap-1 mt-1 opacity-70">
                                                        {isVisio ? <Video size={10} /> : <MapPin size={10} />}
                                                        <span className="text-[9px] font-bold">{format(start, 'HH:mm')}</span>
                                                    </div>

                                                    {/* Hover Details */}
                                                    <div className="absolute inset-0 bg-black/5 opacity-0 group-hover/block:opacity-100 transition-opacity flex items-center justify-center">
                                                        <ChevronRight size={16} />
                                                    </div>
                                                </button>
                                            );
                                        })}
                                    </div>
                                </div>
                            );
                        })}
                    </div>
                </div>

                {/* Alerts Sidebar */}
                <div className="w-80 flex-shrink-0 space-y-6">

                    {/* Alerts Panel */}
                    <div className="bg-white rounded-3xl shadow-xl border border-slate-100 p-6">
                        <h2 className="text-sm font-black text-slate-900 uppercase tracking-widest flex items-center gap-2 mb-6">
                            <AlertTriangle size={16} className="text-rose-500" />
                            Alertes & Actions
                        </h2>

                        <div className="space-y-4">
                            {alerts.length === 0 ? (
                                <div className="text-center py-10">
                                    <div className="w-12 h-12 bg-emerald-50 text-emerald-500 rounded-full flex items-center justify-center mx-auto mb-4">
                                        <CheckCircle size={24} />
                                    </div>
                                    <p className="text-[10px] font-black text-slate-400 uppercase">Aucun conflit détecté</p>
                                </div>
                            ) : (
                                alerts.map(alert => (
                                    <div key={alert.id} className={`p-4 rounded-2xl border ${alert.severity === 'HIGH' ? 'bg-rose-50 border-rose-100' : 'bg-orange-50 border-orange-100'} animate-in slide-in-from-right`}>
                                        <div className="flex gap-3">
                                            <div className={`mt-0.5 ${alert.severity === 'HIGH' ? 'text-rose-500' : 'text-orange-500'}`}>
                                                <AlertTriangle size={16} />
                                            </div>
                                            <div>
                                                <p className={`text-xs font-black leading-tight ${alert.severity === 'HIGH' ? 'text-rose-900' : 'text-orange-900'}`}>
                                                    {alert.msg}
                                                </p>
                                                <button className="text-[10px] font-bold mt-2 text-rose-500 underline flex items-center gap-1">
                                                    Résoudre maintenant <ChevronRight size={10} />
                                                </button>
                                            </div>
                                        </div>
                                    </div>
                                ))
                            )}
                        </div>
                    </div>

                    {/* Quick Stats Summary */}
                    <div className="bg-slate-900 rounded-3xl shadow-xl p-6 text-white relative overflow-hidden">
                        <div className="relative z-10">
                            <h2 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-6 flex items-center gap-2">
                                <TrendingUp size={14} />
                                Répartition
                            </h2>

                            <div className="space-y-6">
                                <div>
                                    <div className="flex justify-between text-[10px] font-black uppercase mb-2">
                                        <span>Visio</span>
                                        <span className="text-indigo-400">{Math.round((stats.distribution.visio / stats.totalAppointments) * 100 || 0)}%</span>
                                    </div>
                                    <div className="h-1.5 bg-white/10 rounded-full overflow-hidden">
                                        <div className="h-full bg-indigo-500" style={{ width: `${(stats.distribution.visio / stats.totalAppointments) * 100}%` }} />
                                    </div>
                                </div>

                                <div>
                                    <div className="flex justify-between text-[10px] font-black uppercase mb-2">
                                        <span>Présentiel</span>
                                        <span className="text-emerald-400">{Math.round((stats.distribution.physical / stats.totalAppointments) * 100 || 0)}%</span>
                                    </div>
                                    <div className="h-1.5 bg-white/10 rounded-full overflow-hidden">
                                        <div className="h-full bg-emerald-500" style={{ width: `${(stats.distribution.physical / stats.totalAppointments) * 100}%` }} />
                                    </div>
                                </div>
                            </div>

                            <button className="w-full mt-10 py-3 bg-white/10 hover:bg-white/20 rounded-2xl text-[10px] font-black uppercase tracking-widest transition-all flex items-center justify-center gap-2">
                                <Info size={14} />
                                Rapport Détaillé
                            </button>
                        </div>

                        {/* Decor */}
                        <div className="absolute -bottom-10 -right-10 opacity-10">
                            <Clock size={150} />
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
};

export default AgendaDashboard;
