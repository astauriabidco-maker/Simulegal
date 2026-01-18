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
const jwt_auth_guard_1 = require("../auth/jwt-auth.guard");
const roles_guard_1 = require("../auth/roles.guard");
const roles_decorator_1 = require("../auth/roles.decorator");
let AppointmentsController = class AppointmentsController {
    appointmentsService;
    constructor(appointmentsService) {
        this.appointmentsService = appointmentsService;
    }
    async getSlots(date, agencyId) {
        if (!date)
            return [];
        return this.appointmentsService.getAvailableSlots(date, agencyId);
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
        return this.appointmentsService.create({
            start,
            end,
            type: data.type,
            leadId: data.lead.id,
            leadName: data.lead.name,
            leadEmail: data.lead.email,
            status: 'SCHEDULED',
            agency: data.agencyId ? { connect: { id: data.agencyId } } : undefined,
            meetingLink: data.type === 'VISIO_JURISTE'
                ? `https://meet.google.com/sim-ule-gal-${data.lead.id.substring(0, 4)}`
                : undefined
        });
    }
};
exports.AppointmentsController = AppointmentsController;
__decorate([
    (0, common_1.Get)('slots'),
    __param(0, (0, common_1.Query)('date')),
    __param(1, (0, common_1.Query)('agencyId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
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
exports.AppointmentsController = AppointmentsController = __decorate([
    (0, common_1.Controller)('appointments'),
    __metadata("design:paramtypes", [appointments_service_1.AppointmentsService])
], AppointmentsController);
//# sourceMappingURL=appointments.controller.js.map