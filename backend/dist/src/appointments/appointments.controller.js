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
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AppointmentsController = void 0;
const common_1 = require("@nestjs/common");
const appointments_service_1 = require("./appointments.service");
const meetings_service_1 = require("./meetings.service");
const jwt_auth_guard_1 = require("../auth/jwt-auth.guard");
const roles_guard_1 = require("../auth/roles.guard");
const roles_decorator_1 = require("../auth/roles.decorator");
const notifications_service_1 = require("../notifications/notifications.service");
let AppointmentsController = class AppointmentsController {
    appointmentsService;
    meetingsService;
    notificationsService;
    constructor(appointmentsService, meetingsService, notificationsService) {
        this.appointmentsService = appointmentsService;
        this.meetingsService = meetingsService;
        this.notificationsService = notificationsService;
    }
    async getSlots(date, agencyId, serviceId) {
        if (!date)
            return [];
        return this.appointmentsService.getAvailableSlots(date, agencyId, serviceId);
    }
    async findAll(agencyId, start, end) {
        const where = {};
        if (agencyId)
            where.agencyId = agencyId;
        if (start && end) {
            where.start = { gte: new Date(start) };
            where.end = { lte: new Date(end) };
        }
        return this.appointmentsService.findAll({
            where,
            orderBy: { start: 'asc' }
        });
    }
    async book(data) {
        const start = new Date(data.slotIso);
        const end = new Date(start);
        end.setMinutes(start.getMinutes() + 30);
        let hostUserId = data.hostUserId;
        if (!hostUserId) {
            const foundHost = await this.appointmentsService.findAvailableHost(data.slotIso, data.agencyId, data.serviceId);
            hostUserId = foundHost ?? undefined;
            if (!hostUserId && data.serviceId) {
                throw new common_1.BadRequestException('No expert available for this service at the selected time');
            }
        }
        const appointment = await this.appointmentsService.create({
            start,
            end,
            type: data.type,
            lead: { connect: { id: data.lead.id } },
            leadName: data.lead.name,
            leadEmail: data.lead.email,
            serviceId: data.serviceId,
            status: 'SCHEDULED',
            agency: data.agencyId ? { connect: { id: data.agencyId } } : undefined,
            hostUser: hostUserId ? { connect: { id: hostUserId } } : undefined,
            meetingLink: data.type === 'VISIO_JURISTE'
                ? await this.meetingsService.generateMeetingLink()
                : undefined
        });
        const lead = await this.appointmentsService.findLeadById(data.lead.id);
        if (lead) {
            await this.notificationsService.sendAppointmentConfirmation(lead, appointment);
        }
        return appointment;
    }
    async update(id, body) {
        const data = { ...body };
        if (body.start)
            data.start = new Date(body.start);
        if (body.end)
            data.end = new Date(body.end);
        return this.appointmentsService.update(id, data);
    }
    async getStats(start, end) {
        const startDate = start ? new Date(start) : new Date();
        const endDate = end ? new Date(end) : new Date();
        return this.appointmentsService.getAgendaStats(startDate, endDate);
    }
    async cancel(id, body) {
        return this.appointmentsService.cancel(id, body.reason);
    }
};
exports.AppointmentsController = AppointmentsController;
__decorate([
    (0, common_1.Get)('slots'),
    __param(0, (0, common_1.Query)('date')),
    __param(1, (0, common_1.Query)('agencyId')),
    __param(2, (0, common_1.Query)('serviceId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String, String]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "getSlots", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    (0, common_1.Get)(),
    __param(0, (0, common_1.Query)('agencyId')),
    __param(1, (0, common_1.Query)('start')),
    __param(2, (0, common_1.Query)('end')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String, String]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "findAll", null);
__decorate([
    (0, common_1.Post)('book'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "book", null);
__decorate([
    (0, common_1.Patch)(':id'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "update", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    (0, common_1.Get)('stats'),
    __param(0, (0, common_1.Query)('start')),
    __param(1, (0, common_1.Query)('end')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "getStats", null);
__decorate([
    (0, common_1.Patch)(':id/cancel'),
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], AppointmentsController.prototype, "cancel", null);
exports.AppointmentsController = AppointmentsController = __decorate([
    (0, common_1.Controller)('appointments'),
    __metadata("design:paramtypes", [appointments_service_1.AppointmentsService,
        meetings_service_1.MeetingsService,
        notifications_service_1.NotificationsService])
], AppointmentsController);
//# sourceMappingURL=appointments.controller.js.map