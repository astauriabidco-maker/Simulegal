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
exports.SalesController = void 0;
const common_1 = require("@nestjs/common");
const platform_express_1 = require("@nestjs/platform-express");
const sales_service_1 = require("./sales.service");
const passport_1 = require("@nestjs/passport");
const sales_analytics_service_1 = require("./sales-analytics.service");
const assignment_service_1 = require("./assignment.service");
let SalesController = class SalesController {
    salesService;
    analyticsService;
    assignmentService;
    constructor(salesService, analyticsService, assignmentService) {
        this.salesService = salesService;
        this.analyticsService = analyticsService;
        this.assignmentService = assignmentService;
    }
    async getAnalytics(period) {
        return this.analyticsService.getDashboardStats(period);
    }
    async findAll(page = 1, limit = 50, status, agencyId, source, dateFrom, dateTo) {
        return this.salesService.findAll({
            page: Number(page),
            limit: Number(limit),
            status,
            agencyId,
            source,
            dateFrom,
            dateTo
        });
    }
    async exportProspects(agencyId, source, dateFrom, dateTo) {
        const csv = await this.salesService.exportToCSV({ agencyId, source, dateFrom, dateTo });
        return {
            filename: `prospects_export_${new Date().toISOString().split('T')[0]}.csv`,
            content: csv,
            contentType: 'text/csv'
        };
    }
    findOne(id) {
        return this.salesService.findOne(id);
    }
    create(data) {
        return this.salesService.create(data);
    }
    update(id, data) {
        return this.salesService.update(id, data);
    }
    addNote(id, req, data) {
        return this.salesService.addNote(id, req.user.id, data.text);
    }
    async reassignProspect(id, data) {
        const prospect = await this.salesService.findOne(id);
        if (!prospect) {
            return { success: false, error: 'Prospect not found' };
        }
        let newSalesId = data.salesUserId;
        if (!newSalesId) {
            newSalesId = await this.assignmentService.getNextSalesAgent(prospect.agencyId);
        }
        if (!newSalesId) {
            return { success: false, error: 'No sales agents available' };
        }
        const updated = await this.salesService.update(id, { assignedToSalesId: newSalesId });
        return { success: true, assignedTo: newSalesId, prospect: updated };
    }
    async importProspects(file) {
        return this.salesService.importFromCSV(file.buffer);
    }
};
exports.SalesController = SalesController;
__decorate([
    (0, common_1.Get)('analytics'),
    __param(0, (0, common_1.Query)('period')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Promise)
], SalesController.prototype, "getAnalytics", null);
__decorate([
    (0, common_1.Get)('prospects'),
    __param(0, (0, common_1.Query)('page')),
    __param(1, (0, common_1.Query)('limit')),
    __param(2, (0, common_1.Query)('status')),
    __param(3, (0, common_1.Query)('agencyId')),
    __param(4, (0, common_1.Query)('source')),
    __param(5, (0, common_1.Query)('dateFrom')),
    __param(6, (0, common_1.Query)('dateTo')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Number, Number, String, String, String, String, String]),
    __metadata("design:returntype", Promise)
], SalesController.prototype, "findAll", null);
__decorate([
    (0, common_1.Get)('export'),
    __param(0, (0, common_1.Query)('agencyId')),
    __param(1, (0, common_1.Query)('source')),
    __param(2, (0, common_1.Query)('dateFrom')),
    __param(3, (0, common_1.Query)('dateTo')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String, String, String]),
    __metadata("design:returntype", Promise)
], SalesController.prototype, "exportProspects", null);
__decorate([
    (0, common_1.Get)('prospects/:id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", void 0)
], SalesController.prototype, "findOne", null);
__decorate([
    (0, common_1.Post)('prospects'),
    __param(0, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], SalesController.prototype, "create", null);
__decorate([
    (0, common_1.Patch)('prospects/:id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", void 0)
], SalesController.prototype, "update", null);
__decorate([
    (0, common_1.Post)('prospects/:id/notes'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Request)()),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object, Object]),
    __metadata("design:returntype", void 0)
], SalesController.prototype, "addNote", null);
__decorate([
    (0, common_1.Post)('prospects/:id/reassign'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Promise)
], SalesController.prototype, "reassignProspect", null);
__decorate([
    (0, common_1.Post)('import'),
    (0, common_1.UseInterceptors)((0, platform_express_1.FileInterceptor)('file')),
    __param(0, (0, common_1.UploadedFile)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], SalesController.prototype, "importProspects", null);
exports.SalesController = SalesController = __decorate([
    (0, common_1.Controller)('sales'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [sales_service_1.SalesService,
        sales_analytics_service_1.SalesAnalyticsService,
        assignment_service_1.AssignmentService])
], SalesController);
//# sourceMappingURL=sales.controller.js.map