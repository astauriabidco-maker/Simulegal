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
exports.EligibilityController = void 0;
const common_1 = require("@nestjs/common");
const eligibility_service_1 = require("./eligibility.service");
let EligibilityController = class EligibilityController {
    service;
    constructor(service) {
        this.service = service;
    }
    getThresholds() {
        return this.service.getThresholds();
    }
    getRules(category) {
        return this.service.getRules(category);
    }
    evaluate(category, userProfile) {
        return this.service.evaluateEligibility(userProfile, category);
    }
    updateRule(category, ruleId, body) {
        return this.service.updateRule(category, ruleId, body.conditions, body.changedBy, body.changeDetails);
    }
    updateThresholds(body) {
        return this.service.updateThresholds(body.thresholds, body.changedBy, body.changeDetails);
    }
    getAuditLog(limit) {
        return this.service.getAuditLog(limit ? parseInt(limit) : 50);
    }
    getRuleHistory(category, ruleId) {
        return this.service.getRuleHistory(category, ruleId);
    }
};
exports.EligibilityController = EligibilityController;
__decorate([
    (0, common_1.Get)('thresholds'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "getThresholds", null);
__decorate([
    (0, common_1.Get)('rules/:category'),
    __param(0, (0, common_1.Param)('category')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "getRules", null);
__decorate([
    (0, common_1.Post)('evaluate/:category'),
    __param(0, (0, common_1.Param)('category')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "evaluate", null);
__decorate([
    (0, common_1.Put)('rules/:category/:ruleId'),
    __param(0, (0, common_1.Param)('category')),
    __param(1, (0, common_1.Param)('ruleId')),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String, Object]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "updateRule", null);
__decorate([
    (0, common_1.Put)('thresholds'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "updateThresholds", null);
__decorate([
    (0, common_1.Get)('audit-log'),
    __param(0, (0, common_1.Query)('limit')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "getAuditLog", null);
__decorate([
    (0, common_1.Get)('audit-log/:category/:ruleId'),
    __param(0, (0, common_1.Param)('category')),
    __param(1, (0, common_1.Param)('ruleId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", void 0)
], EligibilityController.prototype, "getRuleHistory", null);
exports.EligibilityController = EligibilityController = __decorate([
    (0, common_1.Controller)('eligibility'),
    __metadata("design:paramtypes", [eligibility_service_1.EligibilityService])
], EligibilityController);
//# sourceMappingURL=eligibility.controller.js.map