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
exports.FranchiseLeadsController = void 0;
const common_1 = require("@nestjs/common");
const franchise_leads_service_1 = require("./franchise-leads.service");
const jwt_auth_guard_1 = require("../auth/jwt-auth.guard");
const roles_guard_1 = require("../auth/roles.guard");
const roles_decorator_1 = require("../auth/roles.decorator");
let FranchiseLeadsController = class FranchiseLeadsController {
    franchiseLeadsService;
    constructor(franchiseLeadsService) {
        this.franchiseLeadsService = franchiseLeadsService;
    }
    create(body) {
        return this.franchiseLeadsService.create(body);
    }
    findAll() {
        return this.franchiseLeadsService.findAll();
    }
    findOne(id) {
        return this.franchiseLeadsService.findOne(id);
    }
    update(id, body) {
        return this.franchiseLeadsService.update(id, body);
    }
    signContract(id) {
        return this.franchiseLeadsService.signContract(id);
    }
    async getContract(id, res) {
        const buffer = await this.franchiseLeadsService.generateContract(id);
        res.set({
            'Content-Type': 'application/pdf',
            'Content-Disposition': 'attachment; filename=contract.pdf',
            'Content-Length': buffer.length,
        });
        res.end(buffer);
    }
    addNote(id, body) {
        return this.franchiseLeadsService.addNote(id, body.content, body.author, body.type);
    }
    getAnalytics() {
        return this.franchiseLeadsService.getAnalytics();
    }
    async exportCSV(res) {
        const csv = await this.franchiseLeadsService.exportToCSV();
        res.set({
            'Content-Type': 'text/csv; charset=utf-8',
            'Content-Disposition': 'attachment; filename=franchise-leads.csv'
        });
        res.send('\uFEFF' + csv);
    }
};
exports.FranchiseLeadsController = FranchiseLeadsController;
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "create", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Get)(),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "findAll", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "findOne", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Patch)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "update", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Post)(':id/sign'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "signContract", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Get)(':id/contract'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Res)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], FranchiseLeadsController.prototype, "getContract", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Post)(':id/notes'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "addNote", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Get)('analytics/dashboard'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], FranchiseLeadsController.prototype, "getAnalytics", null);
__decorate([
    (0, common_1.UseGuards)(jwt_auth_guard_1.JwtAuthGuard, roles_guard_1.RolesGuard),
    (0, roles_decorator_1.Roles)('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN'),
    (0, common_1.Get)('export/csv'),
    __param(0, (0, common_1.Res)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], FranchiseLeadsController.prototype, "exportCSV", null);
exports.FranchiseLeadsController = FranchiseLeadsController = __decorate([
    (0, common_1.Controller)('franchise-leads'),
    __metadata("design:paramtypes", [franchise_leads_service_1.FranchiseLeadsService])
], FranchiseLeadsController);
//# sourceMappingURL=franchise-leads.controller.js.map