'use client';

import React, { useState, useEffect } from 'react';
import { AppointmentsStore, AppointmentSlot } from '@/services/AppointmentsStore';
import { ChevronLeft, ChevronRight, Loader2, Calendar, Clock } from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface BookingCalendarProps {
    onSelectSlot: (slot: AppointmentSlot | null) => void;
    selectedSlot: AppointmentSlot | null;
}

export default function BookingCalendar({ onSelectSlot, selectedSlot }: BookingCalendarProps) {
    const [loading, setLoading] = useState(true);
    const [slots, setSlots] = useState<AppointmentSlot[]>([]);

    // Start of current week (Monday)
    const [weekStart, setWeekStart] = useState<Date>(() => {
        const d = new Date();
        const day = d.getDay();
        const diff = d.getDate() - day + (day === 0 ? -6 : 1); // adjust when day is sunday
        d.setDate(diff);
        d.setHours(0, 0, 0, 0);
        return d;
    });

    const [selectedDate, setSelectedDate] = useState<Date>(new Date());

    useEffect(() => {
        fetchSlots();
    }, [weekStart]);

    const fetchSlots = async () => {
        setLoading(true);
        const start = new Date(weekStart);
        const end = new Date(weekStart);
        end.setDate(end.getDate() + 7); // Fetch 7 days

        const data = await AppointmentsStore.getAvailableSlots(start, end);
        setSlots(data);
        setLoading(false);
    };

    const nextWeek = () => {
        const d = new Date(weekStart);
        d.setDate(d.getDate() + 7);
        setWeekStart(d);
        // Also update selected date to monday of new week if needed? No, keep it or reset.
        // Let's reset selected date to the first day of new week
        setSelectedDate(d);
    };

    const prevWeek = () => {
        const d = new Date(weekStart);
        d.setDate(d.getDate() - 7);
        const today = new Date();
        today.setHours(0, 0, 0, 0);

        // Prevent going too far back? 
        // If whole week is in past, allow it? Usually no.
        // But for simplicity of admin/test, allow all.
        setWeekStart(d);
        setSelectedDate(d);
    };

    // Generate days for the view
    const days = weekStart ? Array.from({ length: 7 }).map((_, i) => {
        const d = new Date(weekStart);
        d.setDate(d.getDate() + i);
        return d;
    }) : [];

    // Group slots by day
    const slotsByDay = days.reduce((acc, day) => {
        const dayKey = day.toLocaleDateString();
        acc[dayKey] = slots.filter(s => {
            const slotDate = new Date(s.start);
            return slotDate.toDateString() === day.toDateString();
        });
        return acc;
    }, {} as Record<string, AppointmentSlot[]>);

    const isToday = (date: Date) => {
        const today = new Date();
        return date.getDate() === today.getDate() &&
            date.getMonth() === today.getMonth() &&
            date.getFullYear() === today.getFullYear();
    };

    const isSameDay = (d1: Date, d2: Date) => {
        return d1.toDateString() === d2.toDateString();
    };

    // Formatter
    const formatDate = (date: Date) => {
        return new Intl.DateTimeFormat('fr-FR', { weekday: 'short', day: 'numeric', month: 'short' }).format(date);
    };

    return (
        <div className="bg-white rounded-3xl border border-slate-200 shadow-sm p-6 overflow-hidden">
            <div className="flex items-center justify-between mb-6">
                <h3 className="text-xl font-bold text-slate-800 flex items-center gap-2">
                    <Calendar className="w-5 h-5 text-indigo-600" />
                    Disponibilités
                </h3>
                <div className="flex items-center gap-2">
                    <button onClick={prevWeek} className="p-2 hover:bg-slate-100 rounded-full transition-colors text-slate-600">
                        <ChevronLeft className="w-5 h-5" />
                    </button>
                    <span className="text-sm font-bold text-slate-600 px-2 min-w-[100px] text-center">
                        {new Intl.DateTimeFormat('fr-FR', { month: 'long', year: 'numeric' }).format(weekStart)}
                    </span>
                    <button onClick={nextWeek} className="p-2 hover:bg-slate-100 rounded-full transition-colors text-slate-600">
                        <ChevronRight className="w-5 h-5" />
                    </button>
                </div>
            </div>

            {loading ? (
                <div className="h-40 flex items-center justify-center text-slate-400 gap-2">
                    <Loader2 className="w-6 h-6 animate-spin" />
                    Chargement des créneaux...
                </div>
            ) : (
                <div className="space-y-6">
                    {/* Days Row */}
                    <div className="grid grid-cols-7 gap-2">
                        {days.map((day, i) => {
                            const selected = isSameDay(day, selectedDate);
                            return (
                                <button
                                    key={i}
                                    onClick={() => setSelectedDate(day)}
                                    className={cn(
                                        "flex flex-col items-center justify-center p-3 rounded-2xl transition-all border-2",
                                        selected
                                            ? "border-indigo-600 bg-indigo-50 text-indigo-700 shadow-sm"
                                            : "border-transparent hover:bg-slate-50 text-slate-500 hover:text-slate-800"
                                    )}
                                >
                                    <span className="text-xs font-semibold uppercase opacity-70">
                                        {new Intl.DateTimeFormat('fr-FR', { weekday: 'short' }).format(day)}
                                    </span>
                                    <span className={cn("text-lg font-black", selected ? "text-indigo-900" : "text-slate-700")}>
                                        {day.getDate()}
                                    </span>
                                </button>
                            );
                        })}
                    </div>

                    {/* Slots Grid for Selected Day */}
                    <div className="min-h-[150px]">
                        <h4 className="text-sm font-bold text-slate-400 uppercase tracking-wider mb-4 border-b border-slate-100 pb-2">
                            Créneaux du {formatDate(selectedDate)}
                        </h4>

                        {slotsByDay[selectedDate.toLocaleDateString()]?.length > 0 ? (
                            <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
                                {slotsByDay[selectedDate.toLocaleDateString()].map(slot => (
                                    <button
                                        key={slot.id}
                                        onClick={() => onSelectSlot(slot)}
                                        className={cn(
                                            "flex items-center justify-center gap-2 py-3 px-4 rounded-xl border-2 transition-all font-bold",
                                            selectedSlot?.id === slot.id
                                                ? "border-emerald-500 bg-emerald-50 text-emerald-700 ring-2 ring-emerald-100 ring-offset-2"
                                                : "border-slate-100 bg-white hover:border-indigo-200 hover:bg-slate-50 text-slate-700"
                                        )}
                                    >
                                        <Clock className="w-4 h-4 opacity-50" />
                                        {new Date(slot.start).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                    </button>
                                ))}
                            </div>
                        ) : (
                            <div className="text-center py-10 bg-slate-50 rounded-2xl border border-slate-100 border-dashed text-slate-400">
                                Aucun créneau disponible pour ce jour.
                            </div>
                        )}
                    </div>
                </div>
            )}
        </div>
    );
}
