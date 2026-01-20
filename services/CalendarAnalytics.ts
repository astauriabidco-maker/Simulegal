import { Appointment } from './CalendarStore';
import { differenceInHours, isWithinInterval, addHours, startOfDay, endOfDay, isAfter, isBefore, addDays, isSameDay } from 'date-fns';

export interface AgendaStats {
    totalAppointments: number;
    occupancyRate: number; // (Heures bookées / Heures ouvrées) * 100
    noShowRate: number;    // % de RDV marqués comme 'NO_SHOW'
    distribution: {
        visio: number;
        physical: number;
    };
    upcomingVolume: number; // RDV dans les prochaines 24h
    revenueEstimate: number; // (Heures bookées * Tarif moyen)
    dailyVolume: number[]; // Volumes des 7 derniers jours
}

export const CalendarAnalytics = {
    computeStats: (appointments: Appointment[], workingHoursPerDay: number = 8): AgendaStats => {
        const total = appointments.length;
        if (total === 0) {
            return {
                totalAppointments: 0,
                occupancyRate: 0,
                noShowRate: 0,
                distribution: { visio: 0, physical: 0 },
                upcomingVolume: 0,
                revenueEstimate: 0,
                dailyVolume: [0, 0, 0, 0, 0, 0, 0]
            };
        }

        const noShows = appointments.filter(a => a.status === 'NO_SHOW').length;
        const visio = appointments.filter(a => a.type === 'VISIO_JURISTE').length;
        const physical = appointments.filter(a => a.type === 'PHYSICAL_AGENCY').length;

        // Occupancy Rate Calculation
        // For simplicity, we assume we calculate for a single day or a specific range.
        // If we have unique days in the appointments, we multiply workingHoursPerDay by that count.
        const uniqueDays = new Set(appointments.map(a => startOfDay(new Date(a.start)).toISOString())).size || 1;
        const totalAvailableHours = uniqueDays * workingHoursPerDay;

        let totalBookedHours = 0;
        appointments.forEach(a => {
            if (a.status !== 'CANCELLED') {
                const start = new Date(a.start);
                const end = new Date(a.end);
                totalBookedHours += Math.abs(differenceInHours(end, start)) || 0.5; // Default to 30min if duration is 0
            }
        });

        const occupancyRate = totalAvailableHours > 0 ? (totalBookedHours / totalAvailableHours) * 100 : 0;

        // Upcoming Volume (Next 24h)
        const now = new Date();
        const next24h = addDays(now, 1);
        const upcoming = appointments.filter(a => {
            const start = new Date(a.start);
            return isAfter(start, now) && isBefore(start, next24h);
        }).length;

        // Revenue Estimate (Arbitrary 150€ per hour)
        const HOURLY_RATE = 150;
        const revenueEstimate = totalBookedHours * HOURLY_RATE;

        // Daily Volume (Last 7 days)
        const dailyVolume: number[] = [];
        for (let i = 6; i >= 0; i--) {
            const date = addDays(new Date(), -i);
            const count = appointments.filter(a => isSameDay(new Date(a.start), date)).length;
            dailyVolume.push(count);
        }

        return {
            totalAppointments: total,
            occupancyRate: Math.min(100, occupancyRate),
            noShowRate: (noShows / total) * 100,
            distribution: { visio, physical },
            upcomingVolume: upcoming,
            revenueEstimate,
            dailyVolume
        };
    }
};
