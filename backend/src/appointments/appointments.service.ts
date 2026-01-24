import { Injectable, BadRequestException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Appointment, AppointmentType, Prisma } from '@prisma/client';
import { v4 as uuidv4 } from 'uuid';

@Injectable()
export class AppointmentsService {
    constructor(private prisma: PrismaService) { }

    async findAll(params: {
        skip?: number;
        take?: number;
        cursor?: Prisma.AppointmentWhereUniqueInput;
        where?: Prisma.AppointmentWhereInput;
        orderBy?: Prisma.AppointmentOrderByWithRelationInput;
    }): Promise<any[]> {
        const appointments = await this.prisma.appointment.findMany({
            ...params,
            include: { lead: true }
        });

        return appointments.map((appointment: any) => {
            if (!appointment.lead) return { ...appointment, dossierStatus: 'EMPTY', missingDocsCount: 0 };

            const docs = appointment.lead.documents ? JSON.parse(appointment.lead.documents) : [];
            const required = appointment.lead.requiredDocs ? JSON.parse(appointment.lead.requiredDocs) : [];

            if (required.length === 0) return { ...appointment, dossierStatus: 'COMPLETE', missingDocsCount: 0 };

            const completedDocs = docs.filter((d: any) => d.status === 'VALID' || d.status === 'ANALYZING').length;
            const missingDocsCount = Math.max(0, required.length - completedDocs);

            let dossierStatus: 'COMPLETE' | 'PARTIAL' | 'INCOMPLETE' | 'EMPTY' = 'EMPTY';
            if (completedDocs === required.length) dossierStatus = 'COMPLETE';
            else if (completedDocs > 0) dossierStatus = 'PARTIAL';
            else dossierStatus = 'INCOMPLETE';

            return {
                ...appointment,
                dossierStatus,
                missingDocsCount
            };
        });
    }

    async findLeadById(id: string) {
        return this.prisma.lead.findUnique({
            where: { id }
        });
    }

    async create(data: Prisma.AppointmentCreateInput): Promise<Appointment> {
        // Check for overlaps
        const start = new Date(data.start);
        const end = new Date(data.end);

        const overlap = await this.prisma.appointment.findFirst({
            where: {
                OR: [
                    {
                        // Same Host (Jurist)
                        hostUserId: data.hostUser?.connect?.id,
                        start: { lt: end },
                        end: { gt: start }
                    },
                    {
                        // Same Agency Room (if PHYSICAL)
                        agencyId: data.agency?.connect?.id,
                        status: { not: 'CANCELLED' },
                        start: { lt: end },
                        end: { gt: start }
                    }
                ]
            }
        });

        if (overlap) {
            throw new BadRequestException('Slot already taken by another appointment');
        }

        // Check for host absence if host is specified
        if (data.hostUser?.connect?.id) {
            const absence = await this.prisma.absence.findFirst({
                where: {
                    userId: data.hostUser.connect.id,
                    start: { lt: end },
                    end: { gt: start }
                }
            });

            if (absence) {
                throw new BadRequestException('Staff is absent during this slot');
            }
        }

        const appointment = await this.prisma.appointment.create({
            data,
        });

        // Update Lead status and add note
        try {
            const start = new Date(data.start);
            await this.prisma.lead.update({
                where: { id: (data as any).leadId },
                data: { status: 'BOOKED' }
            });

            await this.prisma.leadNote.create({
                data: {
                    leadId: (data as any).leadId,
                    author: 'SYSTEM',
                    content: `📅 Rendez-vous ${data.type === 'VISIO_JURISTE' ? 'Visio' : 'en Agence'} confirmé pour le ${start.toLocaleDateString()} à ${start.toLocaleTimeString()}`
                }
            });
        } catch (error: any) {
            console.warn(`[Appointments] Could not update lead:`, error.message);
        }

        return appointment;
    }

    async getAvailableSlots(dateStr: string, agencyId?: string, serviceId?: string): Promise<string[]> {
        const date = new Date(dateStr);
        const day = date.getDay();

        // 0 = Dimanche, 6 = Samedi
        if (day === 0 || day === 6) return [];

        // 1. Get Host Candidates (experts for this service)
        const hostWhere: Prisma.UserWhereInput = {};
        if (agencyId && agencyId !== 'HQ') {
            hostWhere.homeAgencyId = agencyId;
        } else {
            hostWhere.homeAgencyId = null; // HQ
        }

        // If serviceId provided, we filter candidates
        if (serviceId) {
            hostWhere.expertises = {
                contains: `"${serviceId}"` // Rough check for JSON array string
            };
        }

        const candidates = await this.prisma.user.findMany({
            where: hostWhere,
            select: { id: true }
        });

        if (candidates.length === 0) return [];

        const candidateIds = candidates.map(c => c.id);

        const slots: string[] = [];
        const startHour = 9;
        const endHour = 18;

        // Generate slots
        for (let hour = startHour; hour < endHour; hour++) {
            // :00
            const d1 = new Date(date);
            d1.setHours(hour, 0, 0, 0);
            slots.push(d1.toISOString());

            // :30
            const d2 = new Date(date);
            d2.setHours(hour, 30, 0, 0);
            slots.push(d2.toISOString());
        }

        // 2. Filter booked slots where NO candidate is available
        const startOfDay = new Date(date); startOfDay.setHours(0, 0, 0, 0);
        const endOfDay = new Date(date); endOfDay.setHours(23, 59, 59, 999);

        // Get all appointments and absences for these candidates on this day
        const [existingAppointments, existingAbsences] = await Promise.all([
            this.prisma.appointment.findMany({
                where: {
                    hostUserId: { in: candidateIds },
                    start: { gte: startOfDay, lte: endOfDay },
                    status: { not: 'CANCELLED' }
                }
            }),
            this.prisma.absence.findMany({
                where: {
                    userId: { in: candidateIds },
                    start: { gte: startOfDay, lte: endOfDay }
                }
            })
        ]);

        return slots.filter(slotIso => {
            const sStart = new Date(slotIso);
            const sEnd = new Date(sStart);
            sEnd.setMinutes(sStart.getMinutes() + 30);

            // A slot is available if at least ONE candidate is neither busy nor absent
            const availableCandidates = candidateIds.filter((cid: string) => {
                const hasAppointmentConflict = existingAppointments.some((apt: Appointment) => {
                    if (apt.hostUserId !== cid) return false;
                    const aStart = new Date(apt.start);
                    const aEnd = new Date(apt.end);
                    return sStart < aEnd && sEnd > aStart;
                });

                if (hasAppointmentConflict) return false;

                const hasAbsenceConflict = existingAbsences.some((abs: any) => {
                    if (abs.userId !== cid) return false;
                    const bStart = new Date(abs.start);
                    const bEnd = new Date(abs.end);
                    return sStart < bEnd && sEnd > bStart;
                });

                return !hasAbsenceConflict;
            });

            return availableCandidates.length > 0;
        });
    }

    async update(id: string, data: { start?: Date; end?: Date; hostUserId?: string; agencyId?: string; status?: string; type?: string }): Promise<Appointment> {
        // 1. Check if appointment exists
        const appointment = await this.prisma.appointment.findUnique({ where: { id } });
        if (!appointment) throw new BadRequestException('Appointment not found');

        // 2. If rescheduling or re-assigning
        if (data.start || data.end || (data as any).hostUserId || (data as any).agencyId) {
            const newStart = data.start || new Date(appointment.start);
            const newEnd = data.end || new Date(appointment.end);
            const newHostId = (data as any).hostUserId || appointment.hostUserId;
            const newAgencyId = (data as any).agencyId || appointment.agencyId;

            // Check availability for NEW resource/slot (excluding current appointment)
            if (newHostId) {
                const conflict = await this.prisma.appointment.findFirst({
                    where: {
                        id: { not: id },
                        hostUserId: newHostId,
                        status: { not: 'CANCELLED' },
                        start: { lt: newEnd },
                        end: { gt: newStart }
                    }
                });

                if (conflict) throw new BadRequestException('Slot already taken by another appointment for this host');

                const absence = await this.prisma.absence.findFirst({
                    where: {
                        userId: newHostId,
                        start: { lt: newEnd },
                        end: { gt: newStart }
                    }
                });

                if (absence) throw new BadRequestException('Staff is absent during this slot');
            }

            if (newAgencyId) {
                const agencyConflict = await this.prisma.appointment.findFirst({
                    where: {
                        id: { not: id },
                        agencyId: newAgencyId,
                        status: { not: 'CANCELLED' },
                        start: { lt: newEnd },
                        end: { gt: newStart }
                    }
                });
                if (agencyConflict) throw new BadRequestException('Slot already taken by another appointment in this agency');
            }
        }

        return this.prisma.appointment.update({
            where: { id },
            data: data as any
        });
    }

    async getAgendaStats(start: Date, end: Date): Promise<any> {
        const appointments = await this.prisma.appointment.findMany({
            where: {
                start: { gte: start },
                end: { lte: end }
            }
        });

        const total = appointments.length;
        if (total === 0) {
            return {
                totalAppointments: 0,
                occupancyRate: 0,
                noShowRate: 0,
                distribution: { visio: 0, physical: 0 },
                upcomingVolume: 0,
                revenueEstimate: 0
            };
        }

        const noShows = appointments.filter((a: Appointment) => a.status === 'NO_SHOW').length;
        const visio = appointments.filter((a: Appointment) => a.type === 'VISIO_JURISTE').length;
        const physical = appointments.filter((a: Appointment) => a.type === 'PHYSICAL_AGENCY').length;

        // Occupancy Calculation
        // Assuming 8 hours capacity per day per resource
        // Find how many unique resources are involved
        const resources = new Set([
            ...appointments.map((a: Appointment) => a.hostUserId).filter(Boolean),
            ...appointments.map((a: Appointment) => a.agencyId).filter(Boolean)
        ]);

        const daysCount = Math.ceil((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24)) || 1;
        const totalCapacityHours = resources.size * daysCount * 8;

        let totalBookedHours = 0;
        appointments.forEach((a: Appointment) => {
            if (a.status !== 'CANCELLED') {
                const dur = (new Date(a.end).getTime() - new Date(a.start).getTime()) / (1000 * 60 * 60);
                totalBookedHours += dur > 0 ? dur : 0.5;
            }
        });

        const occupancyRate = totalCapacityHours > 0 ? (totalBookedHours / totalCapacityHours) * 100 : 0;

        // Upcoming Volume (Next 24h)
        const now = new Date();
        const next24h = new Date(now.getTime() + 24 * 60 * 60 * 1000);
        const upcoming = appointments.filter((a: Appointment) => {
            const rowStart = new Date(a.start);
            return rowStart > now && rowStart < next24h;
        }).length;

        // Revenue Estimate (150€/h)
        const revenueEstimate = totalBookedHours * 150;

        return {
            totalAppointments: total,
            occupancyRate: Math.min(100, occupancyRate),
            noShowRate: (noShows / total) * 100,
            distribution: { visio, physical },
            upcomingVolume: upcoming,
            revenueEstimate
        };
    }

    async cancel(id: string, reason: string): Promise<Appointment> {
        return this.prisma.appointment.update({
            where: { id },
            data: {
                status: 'CANCELLED',
                cancellationReason: reason
            }
        });
    }

    async findAvailableHost(slotIso: string, agencyId?: string, serviceId?: string): Promise<string | null> {
        const date = new Date(slotIso);
        const slotEnd = new Date(date);
        slotEnd.setMinutes(date.getMinutes() + 30);

        const hostWhere: Prisma.UserWhereInput = {};
        if (agencyId && agencyId !== 'HQ') {
            hostWhere.homeAgencyId = agencyId;
        } else {
            hostWhere.homeAgencyId = null;
        }

        if (serviceId) {
            hostWhere.expertises = {
                contains: `"${serviceId}"`
            };
        }

        const candidates = await this.prisma.user.findMany({
            where: hostWhere,
            select: { id: true }
        });

        for (const candidate of candidates) {
            const appointmentConflict = await this.prisma.appointment.findFirst({
                where: {
                    hostUserId: candidate.id,
                    status: { not: 'CANCELLED' },
                    start: { lt: slotEnd },
                    end: { gt: date }
                }
            });

            if (appointmentConflict) continue;

            const absenceConflict = await this.prisma.absence.findFirst({
                where: {
                    userId: candidate.id,
                    start: { lt: slotEnd },
                    end: { gt: date }
                }
            });

            if (!absenceConflict) return candidate.id;
        }

        return null;
    }
}
