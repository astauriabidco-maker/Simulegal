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
exports.AbsencesController = void 0;
const common_1 = require("@nestjs/common");
const absences_service_1 = require("./absences.service");
const jwt_auth_guard_1 = require("../auth/jwt-auth.guard");
const roles_guard_1 = require("../auth/roles.guard");
const roles_decorator_1 = require("../auth/roles.decorator");
let AbsencesController = class AbsencesController {
    absencesService;
    constructor(absencesService) {
        this.absencesService = absencesService;
    }
    async findAll(userId, start, end) {
        const where = {};
        if (userId)
            where.userId = userId;
        if (start && end) {
            where.start = { gte: new Date(start) };
            where.end = { lte: new Date(end) };
        }
        return this.absencesService.findAll({ where, orderBy: { start: 'asc' } });
    }
    async create(data) {
        return this.absencesService.create({
            user: { connect: { id: data.userId } },
            start: new Date(data.start),
            end: new Date(data.end),
            reason: data.reason
        });
    }
    async remove(id) {
        return this.absencesService.remove(id);
    }
};
exports.AbsencesController = AbsencesController;
__decorate([
    (0, common_1.Get)(),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    __param(0, (0, common_1.Query)('userId')),
    __param(1, (0, common_1.Query)('start')),
    __param(2, (0, common_1.Query)('end')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String, String]),
    __metadata("design:returntype", Promise)
], AbsencesController.prototype, "findAll", null);
__decorate([
    (0, common_1.Post)(),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], AbsencesController.prototype, "create", null);
__decorate([
    (0, common_1.Delete)(':id'),
    (0, roles_decorator_1.Roles)('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Promise)
], AbsencesController.prototype, "remove", null);
exports.AbsencesController = AbsencesController = __decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, common_1.Controller)('absences'),
    __metadata("design:paramtypes", [absences_service_1.AbsencesService])
], AbsencesController);
//# sourceMappingURL=absences.controller.js.map