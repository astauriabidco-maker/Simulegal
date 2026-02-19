import { Injectable, BadRequestException, NotFoundException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class AppointmentsSlotsService {
    constructor(private prisma: PrismaService) { }

    // Créer des créneaux de disponibilité
    async createSlots(juristId: string, slots: { start: string; end: string }[]) {
        const createdSlots = [];
        for (const slot of slots) {
            const newSlot = await this.prisma.appointmentSlot.create({
                data: {
                    juristId,
                    start: new Date(slot.start),
                    end: new Date(slot.end),
                    status: 'AVAILABLE'
                }
            });
            createdSlots.push(newSlot);
        }
        return createdSlots;
    }

    // Trouver les créneaux disponibles
    async findAvailableSlots(startDate: string, endDate: string) {
        return this.prisma.appointmentSlot.findMany({
            where: {
                status: 'AVAILABLE',
                start: {
                    gte: new Date(startDate),
                    lte: new Date(endDate)
                }
            },
            include: {
                jurist: {
                    select: { name: true, id: true }
                }
            },
            orderBy: {
                start: 'asc'
            }
        });
    }

    // Verrouiller un créneau temporairement (avant paiement)
    async lockSlot(slotId: string, leadId?: string) {
        const slot = await this.prisma.appointmentSlot.findUnique({ where: { id: slotId } });

        if (!slot) throw new NotFoundException('Créneau non trouvé');
        if (slot.status !== 'AVAILABLE') throw new BadRequestException('Créneau non disponible');

        return this.prisma.appointmentSlot.update({
            where: { id: slotId },
            data: {
                status: 'LOCKED',
                lockedAt: new Date(),
                leadId // Optionnel si le lead est déjà créé
            }
        });
    }

    // Confirmer un créneau (après paiement)
    async bookSlot(slotId: string, leadId: string) {
        // On permet de réserver un slot LOCKED ou AVAILABLE
        const slot = await this.prisma.appointmentSlot.findUnique({ where: { id: slotId } });

        if (!slot) throw new NotFoundException('Créneau non trouvé');

        // Si déjà BOOKED par un autre, erreur
        if (slot.status === 'BOOKED' && slot.leadId !== leadId) {
            throw new BadRequestException('Créneau déjà réservé');
        }

        return this.prisma.appointmentSlot.update({
            where: { id: slotId },
            data: {
                status: 'BOOKED',
                leadId
            }
        });
    }

    // Libérer les slots expirés (LOCKED depuis > 15min)
    async releaseExpiredLocks() {
        const fifteenMinutesAgo = new Date(Date.now() - 15 * 60 * 1000);
        return this.prisma.appointmentSlot.updateMany({
            where: {
                status: 'LOCKED',
                lockedAt: { lt: fifteenMinutesAgo }
            },
            data: {
                status: 'AVAILABLE',
                lockedAt: null,
                leadId: null
            }
        });
    }
}
