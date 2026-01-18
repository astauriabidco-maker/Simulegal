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
            throw new common_1.BadRequestException('Slot already taken');
        }
        return this.prisma.appointment.create({
            data,
        });
    }
    async getAvailableSlots(dateStr, agencyId) {
        const date = new Date(dateStr);
        const day = date.getDay();
        if (day === 0 || day === 6)
            return [];
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
        const where = {
            start: { gte: startOfDay, lte: endOfDay },
            status: { not: 'CANCELLED' }
        };
        if (agencyId) {
            where.agencyId = agencyId;
        }
        else {
            where.type = 'VISIO_JURISTE';
        }
        const appointments = await this.prisma.appointment.findMany({ where });
        return slots.filter(slotIso => {
            const sStart = new Date(slotIso);
            const sEnd = new Date(sStart);
            sEnd.setMinutes(sStart.getMinutes() + 30);
            const conflict = appointments.some(apt => {
                const aStart = new Date(apt.start);
                const aEnd = new Date(apt.end);
                return sStart < aEnd && sEnd > aStart;
            });
            return !conflict;
        });
    }
};
exports.AppointmentsService = AppointmentsService;
exports.AppointmentsService = AppointmentsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], AppointmentsService);
//# sourceMappingURL=appointments.service.js.map