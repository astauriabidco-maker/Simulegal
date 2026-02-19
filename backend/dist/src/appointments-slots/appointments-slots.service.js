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
exports.AppointmentsSlotsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let AppointmentsSlotsService = class AppointmentsSlotsService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async createSlots(juristId, slots) {
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
    async findAvailableSlots(startDate, endDate) {
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
    async lockSlot(slotId, leadId) {
        const slot = await this.prisma.appointmentSlot.findUnique({ where: { id: slotId } });
        if (!slot)
            throw new common_1.NotFoundException('Créneau non trouvé');
        if (slot.status !== 'AVAILABLE')
            throw new common_1.BadRequestException('Créneau non disponible');
        return this.prisma.appointmentSlot.update({
            where: { id: slotId },
            data: {
                status: 'LOCKED',
                lockedAt: new Date(),
                leadId
            }
        });
    }
    async bookSlot(slotId, leadId) {
        const slot = await this.prisma.appointmentSlot.findUnique({ where: { id: slotId } });
        if (!slot)
            throw new common_1.NotFoundException('Créneau non trouvé');
        if (slot.status === 'BOOKED' && slot.leadId !== leadId) {
            throw new common_1.BadRequestException('Créneau déjà réservé');
        }
        return this.prisma.appointmentSlot.update({
            where: { id: slotId },
            data: {
                status: 'BOOKED',
                leadId
            }
        });
    }
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
};
exports.AppointmentsSlotsService = AppointmentsSlotsService;
exports.AppointmentsSlotsService = AppointmentsSlotsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], AppointmentsSlotsService);
//# sourceMappingURL=appointments-slots.service.js.map