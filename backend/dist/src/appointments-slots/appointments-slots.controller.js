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
exports.AppointmentsSlotsController = void 0;
const common_1 = require("@nestjs/common");
const appointments_slots_service_1 = require("./appointments-slots.service");
let AppointmentsSlotsController = class AppointmentsSlotsController {
    slotsService;
    constructor(slotsService) {
        this.slotsService = slotsService;
    }
    async getAvailableSlots(start, end) {
        return this.slotsService.findAvailableSlots(start, end);
    }
    async createSlots(body) {
        return this.slotsService.createSlots(body.juristId, body.slots);
    }
    async lockSlot(id, body) {
        return this.slotsService.lockSlot(id, body.leadId);
    }
};
exports.AppointmentsSlotsController = AppointmentsSlotsController;
__decorate([
    (0, common_1.Get)('available'),
    __param(0, (0, common_1.Query)('start')),
    __param(1, (0, common_1.Query)('end')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", Promise)
], AppointmentsSlotsController.prototype, "getAvailableSlots", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], AppointmentsSlotsController.prototype, "createSlots", null);
__decorate([
    (0, common_1.Post)(':id/lock'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], AppointmentsSlotsController.prototype, "lockSlot", null);
exports.AppointmentsSlotsController = AppointmentsSlotsController = __decorate([
    (0, common_1.Controller)('appointments-slots'),
    __metadata("design:paramtypes", [appointments_slots_service_1.AppointmentsSlotsService])
], AppointmentsSlotsController);
//# sourceMappingURL=appointments-slots.controller.js.map