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
exports.AgenciesController = void 0;
const common_1 = require("@nestjs/common");
const agencies_service_1 = require("./agencies.service");
const passport_1 = require("@nestjs/passport");
let AgenciesController = class AgenciesController {
    agenciesService;
    constructor(agenciesService) {
        this.agenciesService = agenciesService;
    }
    findAll() {
        return this.agenciesService.findAll();
    }
    async exportCSV(res) {
        const csv = await this.agenciesService.exportToCSV();
        res.set({
            'Content-Type': 'text/csv; charset=utf-8',
            'Content-Disposition': 'attachment; filename=agencies.csv'
        });
        res.send('\uFEFF' + csv);
    }
    checkAvailability(zipCode) {
        return this.agenciesService.checkTerritoryAvailability(zipCode);
    }
    findOne(id) {
        return this.agenciesService.findOne(id);
    }
    create(data) {
        return this.agenciesService.create(data);
    }
    update(id, data) {
        return this.agenciesService.update(id, data);
    }
    delete(id) {
        return this.agenciesService.delete(id);
    }
};
exports.AgenciesController = AgenciesController;
__decorate([
    (0, common_1.Get)(),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "findAll", null);
__decorate([
    (0, common_1.Get)('export/csv'),
    __param(0, (0, common_1.Res)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], AgenciesController.prototype, "exportCSV", null);
__decorate([
    (0, common_1.Get)('check-availability/:zipCode'),
    __param(0, (0, common_1.Param)('zipCode')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "checkAvailability", null);
__decorate([
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "findOne", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "create", null);
__decorate([
    (0, common_1.Patch)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "update", null);
__decorate([
    (0, common_1.Delete)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], AgenciesController.prototype, "delete", null);
exports.AgenciesController = AgenciesController = __decorate([
    (0, common_1.Controller)('agencies'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [agencies_service_1.AgenciesService])
], AgenciesController);
//# sourceMappingURL=agencies.controller.js.map