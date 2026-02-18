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
exports.VeilleController = void 0;
const common_1 = require("@nestjs/common");
const veille_service_1 = require("./veille.service");
let VeilleController = class VeilleController {
    veilleService;
    constructor(veilleService) {
        this.veilleService = veilleService;
    }
    findAll() {
        return this.veilleService.findAll();
    }
    findPending() {
        return this.veilleService.findPending();
    }
    getStats() {
        return this.veilleService.getStats();
    }
    create(body) {
        return this.veilleService.create(body);
    }
    update(id, body) {
        return this.veilleService.update(id, body);
    }
    markAsApplied(id) {
        return this.veilleService.markAsApplied(id);
    }
    remove(id) {
        return this.veilleService.remove(id);
    }
};
exports.VeilleController = VeilleController;
__decorate([
    (0, common_1.Get)(),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "findAll", null);
__decorate([
    (0, common_1.Get)('pending'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "findPending", null);
__decorate([
    (0, common_1.Get)('stats'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "getStats", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "create", null);
__decorate([
    (0, common_1.Put)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "update", null);
__decorate([
    (0, common_1.Put)(':id/apply'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "markAsApplied", null);
__decorate([
    (0, common_1.Delete)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], VeilleController.prototype, "remove", null);
exports.VeilleController = VeilleController = __decorate([
    (0, common_1.Controller)('veille'),
    __metadata("design:paramtypes", [veille_service_1.VeilleService])
], VeilleController);
//# sourceMappingURL=veille.controller.js.map