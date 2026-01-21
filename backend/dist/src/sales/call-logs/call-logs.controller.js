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
exports.CallLogsController = void 0;
const common_1 = require("@nestjs/common");
const call_logs_service_1 = require("./call-logs.service");
const passport_1 = require("@nestjs/passport");
let CallLogsController = class CallLogsController {
    callLogsService;
    constructor(callLogsService) {
        this.callLogsService = callLogsService;
    }
    async create(req, data) {
        return this.callLogsService.create({
            prospectId: data.prospectId,
            userId: req.user.id,
            twilioCallSid: data.twilioCallSid,
        });
    }
    async update(id, data) {
        const updateData = { ...data };
        if (data.status === 'COMPLETED' || data.status === 'FAILED' || data.status === 'NO_ANSWER') {
            updateData.endedAt = new Date();
        }
        return this.callLogsService.update(id, updateData);
    }
    async findByProspect(prospectId) {
        return this.callLogsService.findByProspect(prospectId);
    }
};
exports.CallLogsController = CallLogsController;
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Object]),
    __metadata("design:returntype", Promise)
], CallLogsController.prototype, "create", null);
__decorate([
    (0, common_1.Patch)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], CallLogsController.prototype, "update", null);
__decorate([
    (0, common_1.Get)('prospect/:prospectId'),
    __param(0, (0, common_1.Param)('prospectId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Promise)
], CallLogsController.prototype, "findByProspect", null);
exports.CallLogsController = CallLogsController = __decorate([
    (0, common_1.Controller)('sales/calls'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [call_logs_service_1.CallLogsService])
], CallLogsController);
//# sourceMappingURL=call-logs.controller.js.map