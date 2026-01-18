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
    }): Promise<Appointment[]> {
        const { skip, take, cursor, where, orderBy } = params;
        return this.prisma.appointment.findMany({
            skip,
            take,
            cursor,
            where,
            orderBy,
            include: {
                agency: true,
                hostUser: true
            }
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
                        // Same Agency Room (if PHYSICAL) - Assuming 1 room per agency for simplicity for now
                        // In real world, we would have resources/rooms
                        agencyId: data.agency?.connect?.id,
                        status: { not: 'CANCELLED' },
                        start: { lt: end },
                        end: { gt: start }
                    }
                ]
            }
        });

        if (overlap) {
            throw new BadRequestException('Slot already taken');
        }

        return this.prisma.appointment.create({
            data,
        });
    }

    async getAvailableSlots(dateStr: string, agencyId?: string): Promise<string[]> {
        const date = new Date(dateStr);
        const day = date.getDay();

        // 0 = Dimanche, 6 = Samedi
        if (day === 0 || day === 6) return [];

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

        // Filter booked slots
        // If agencyId provided => Filter by Agency Appointments
        // If no agencyId => Filter by VISIO (Host appointments)
        // For simplicity efficiently: Get ALL appointments for that day that match Context
        const startOfDay = new Date(date); startOfDay.setHours(0, 0, 0, 0);
        const endOfDay = new Date(date); endOfDay.setHours(23, 59, 59, 999);

        const where: Prisma.AppointmentWhereInput = {
            start: { gte: startOfDay, lte: endOfDay },
            status: { not: 'CANCELLED' }
        };

        if (agencyId) {
            where.agencyId = agencyId;
        } else {
            where.type = 'VISIO_JURISTE'; // Only check Visio slots
        }

        const appointments = await this.prisma.appointment.findMany({ where });

        return slots.filter(slotIso => {
            const sStart = new Date(slotIso);
            const sEnd = new Date(sStart);
            sEnd.setMinutes(sStart.getMinutes() + 30);

            const conflict = appointments.some(apt => {
                const aStart = new Date(apt.start);
                const aEnd = new Date(apt.end);
                return sStart < aEnd && sEnd > aStart; // Overlap
            });

            return !conflict;
        });
    }
}
