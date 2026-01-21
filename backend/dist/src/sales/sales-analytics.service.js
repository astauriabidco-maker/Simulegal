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
Object.defineProperty(exports, "__esModule", { value: true });
exports.SalesAnalyticsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let SalesAnalyticsService = class SalesAnalyticsService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async getDashboardStats(period = 'MONTH') {
        const now = new Date();
        let startDate = new Date();
        switch (period) {
            case 'TODAY':
                startDate.setHours(0, 0, 0, 0);
                break;
            case 'WEEK':
                startDate.setDate(now.getDate() - 7);
                break;
            case 'MONTH':
                startDate.setMonth(now.getMonth() - 1);
                break;
        }
        const [totalLeads, convertedLeads, newLeads] = await Promise.all([
            this.prisma.prospect.count(),
            this.prisma.prospect.count({ where: { status: 'CONVERTED' } }),
            this.prisma.prospect.count({ where: { createdAt: { gte: startDate } } })
        ]);
        const prospectsByStatus = await this.prisma.prospect.groupBy({
            by: ['status'],
            _count: { status: true },
        });
        const prospectsBySource = await this.prisma.prospect.groupBy({
            by: ['source'],
            _count: { source: true },
            where: { createdAt: { gte: startDate } }
        });
        const conversionRate = totalLeads > 0 ? (convertedLeads / totalLeads) * 100 : 0;
        return {
            period,
            kpis: {
                totalLeads,
                newLeads,
                convertedLeads,
                conversionRate: Math.round(conversionRate * 10) / 10,
                pipelineValue: this.estimatePipelineValue(prospectsByStatus)
            },
            funnel: prospectsByStatus.map(p => ({ status: p.status, count: p._count.status })),
            sources: prospectsBySource.map(s => ({ source: s.source, count: s._count.source }))
        };
    }
    estimatePipelineValue(groupedStatus) {
        let total = 0;
        groupedStatus.forEach(group => {
            const count = group._count.status;
            switch (group.status) {
                case 'TO_CALL':
                    total += count * 1500 * 0.05;
                    break;
                case 'IN_DISCUSSION':
                    total += count * 1500 * 0.15;
                    break;
                case 'MEETING_BOOKED':
                    total += count * 1500 * 0.30;
                    break;
                case 'LINK_SENT':
                    total += count * 1500 * 0.60;
                    break;
                case 'CONVERTED':
                    total += count * 1500 * 1.0;
                    break;
            }
        });
        return Math.round(total);
    }
};
exports.SalesAnalyticsService = SalesAnalyticsService;
exports.SalesAnalyticsService = SalesAnalyticsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], SalesAnalyticsService);
//# sourceMappingURL=sales-analytics.service.js.map