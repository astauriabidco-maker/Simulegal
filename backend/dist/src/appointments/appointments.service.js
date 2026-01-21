"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AppointmentsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let AppointmentsService = class AppointmentsService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async findAll(params) {
        const appointments = await this.prisma.appointment.findMany({
            ...params,
            include: { lead: true }
        });
        return appointments.map((appointment) => {
            if (!appointment.lead)
                return { ...appointment, dossierStatus: 'EMPTY', missingDocsCount: 0 };
            const docs = appointment.lead.documents ? JSON.parse(appointment.lead.documents) : [];
            const required = appointment.lead.requiredDocs ? JSON.parse(appointment.lead.requiredDocs) : [];
            if (required.length === 0)
                return { ...appointment, dossierStatus: 'COMPLETE', missingDocsCount: 0 };
            const completedDocs = docs.filter((d) => d.status === 'VALID' || d.status === 'ANALYZING').length;
            const missingDocsCount = Math.max(0, required.length - completedDocs);
            let dossierStatus = 'EMPTY';
            if (completedDocs === required.length)
                dossierStatus = 'COMPLETE';
            else if (completedDocs > 0)
                dossierStatus = 'PARTIAL';
            else
                dossierStatus = 'INCOMPLETE';
            return {
                ...appointment,
                dossierStatus,
                missingDocsCount
            };
        });
    }
    async findLeadById(id) {
        return this.prisma.lead.findUnique({
            where: { id }
        });
    }
    async create(data) {
        const start = new Date(data.start);
        const end = new Date(data.end);
        const overlap = await this.prisma.appointment.findFirst({
            where: {
                OR: [
                    {
                        hostUserId: data.hostUser?.connect?.id,
                        start: { lt: end },
                        end: { gt: start }
                    },
                    {
                        agencyId: data.agency?.connect?.id,
                        status: { not: 'CANCELLED' },
                        start: { lt: end },
                        end: { gt: start }
                    }
                ]
            }
        });
        if (overlap) {
            throw new common_1.BadRequestException('Slot already taken by another appointment');
        }
        if (data.hostUser?.connect?.id) {
            const absence = await this.prisma.absence.findFirst({
                where: {
                    userId: data.hostUser.connect.id,
                    start: { lt: end },
                    end: { gt: start }
                }
            });
            if (absence) {
                throw new common_1.BadRequestException('Staff is absent during this slot');
            }
        }
        const appointment = await this.prisma.appointment.create({
            data,
        });
        try {
            const start = new Date(data.start);
            await this.prisma.lead.update({
                where: { id: data.leadId },
                data: { status: 'BOOKED' }
            });
            await this.prisma.leadNote.create({
                data: {
                    leadId: data.leadId,
                    author: 'SYSTEM',
                    content: `📅 Rendez-vous ${data.type === 'VISIO_JURISTE' ? 'Visio' : 'en Agence'} confirmé pour le ${start.toLocaleDateString()} à ${start.toLocaleTimeString()}`
                }
            });
        }
        catch (error) {
            console.warn(`[Appointments] Could not update lead:`, error.message);
        }
        return appointment;
    }
    async getAvailableSlots(dateStr, agencyId, serviceId) {
        const date = new Date(dateStr);
        const day = date.getDay();
        if (day === 0 || day === 6)
            return [];
        const hostWhere = {};
        if (agencyId && agencyId !== 'HQ') {
            hostWhere.homeAgencyId = agencyId;
        }
        else {
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
        if (candidates.length === 0)
            return [];
        const candidateIds = candidates.map(c => c.id);
        const slots = [];
        const startHour = 9;
        const endHour = 18;
        for (let hour = startHour; hour < endHour; hour++) {
            const d1 = new Date(date);
            d1.setHours(hour, 0, 0, 0);
            slots.push(d1.toISOString());
            const d2 = new Date(date);
            d2.setHours(hour, 30, 0, 0);
            slots.push(d2.toISOString());
        }
        const startOfDay = new Date(date);
        startOfDay.setHours(0, 0, 0, 0);
        const endOfDay = new Date(date);
        endOfDay.setHours(23, 59, 59, 999);
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
            const availableCandidates = candidateIds.filter((cid) => {
                const hasAppointmentConflict = existingAppointments.some((apt) => {
                    if (apt.hostUserId !== cid)
                        return false;
                    const aStart = new Date(apt.start);
                    const aEnd = new Date(apt.end);
                    return sStart < aEnd && sEnd > aStart;
                });
                if (hasAppointmentConflict)
                    return false;
                const hasAbsenceConflict = existingAbsences.some((abs) => {
                    if (abs.userId !== cid)
                        return false;
                    const bStart = new Date(abs.start);
                    const bEnd = new Date(abs.end);
                    return sStart < bEnd && sEnd > bStart;
                });
                return !hasAbsenceConflict;
            });
            return availableCandidates.length > 0;
        });
    }
    async update(id, data) {
        const appointment = await this.prisma.appointment.findUnique({ where: { id } });
        if (!appointment)
            throw new common_1.BadRequestException('Appointment not found');
        if (data.start || data.end || data.hostUserId || data.agencyId) {
            const newStart = data.start || new Date(appointment.start);
            const newEnd = data.end || new Date(appointment.end);
            const newHostId = data.hostUserId || appointment.hostUserId;
            const newAgencyId = data.agencyId || appointment.agencyId;
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
                if (conflict)
                    throw new common_1.BadRequestException('Slot already taken by another appointment for this host');
                const absence = await this.prisma.absence.findFirst({
                    where: {
                        userId: newHostId,
                        start: { lt: newEnd },
                        end: { gt: newStart }
                    }
                });
                if (absence)
                    throw new common_1.BadRequestException('Staff is absent during this slot');
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
                if (agencyConflict)
                    throw new common_1.BadRequestException('Slot already taken by another appointment in this agency');
            }
        }
        return this.prisma.appointment.update({
            where: { id },
            data: data
        });
    }
    async getAgendaStats(start, end) {
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
        const noShows = appointments.filter((a) => a.status === 'NO_SHOW').length;
        const visio = appointments.filter((a) => a.type === 'VISIO_JURISTE').length;
        const physical = appointments.filter((a) => a.type === 'PHYSICAL_AGENCY').length;
        const resources = new Set([
            ...appointments.map((a) => a.hostUserId).filter(Boolean),
            ...appointments.map((a) => a.agencyId).filter(Boolean)
        ]);
        const daysCount = Math.ceil((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24)) || 1;
        const totalCapacityHours = resources.size * daysCount * 8;
        let totalBookedHours = 0;
        appointments.forEach((a) => {
            if (a.status !== 'CANCELLED') {
                const dur = (new Date(a.end).getTime() - new Date(a.start).getTime()) / (1000 * 60 * 60);
                totalBookedHours += dur > 0 ? dur : 0.5;
            }
        });
        const occupancyRate = totalCapacityHours > 0 ? (totalBookedHours / totalCapacityHours) * 100 : 0;
        const now = new Date();
        const next24h = new Date(now.getTime() + 24 * 60 * 60 * 1000);
        const upcoming = appointments.filter((a) => {
            const rowStart = new Date(a.start);
            return rowStart > now && rowStart < next24h;
        }).length;
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
    async cancel(id, reason) {
        return this.prisma.appointment.update({
            where: { id },
            data: {
                status: 'CANCELLED',
                cancellationReason: reason
            }
        });
    }
    async findAvailableHost(slotIso, agencyId, serviceId) {
        const date = new Date(slotIso);
        const slotEnd = new Date(date);
        slotEnd.setMinutes(date.getMinutes() + 30);
        const hostWhere = {};
        if (agencyId && agencyId !== 'HQ') {
            hostWhere.homeAgencyId = agencyId;
        }
        else {
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
            if (appointmentConflict)
                continue;
            const absenceConflict = await this.prisma.absence.findFirst({
                where: {
                    userId: candidate.id,
                    start: { lt: slotEnd },
                    end: { gt: date }
                }
            });
            if (!absenceConflict)
                return candidate.id;
        }
        return null;
    }
};
exports.AppointmentsService = AppointmentsService;
exports.AppointmentsService = AppointmentsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], AppointmentsService);
//# sourceMappingURL=appointments.service.js.map