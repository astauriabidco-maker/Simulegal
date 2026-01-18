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
exports.DashboardService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let DashboardService = class DashboardService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async getGlobalStats() {
        const [leads, franchiseLeads, devices, agencies] = await Promise.all([
            this.prisma.lead.findMany({
                select: { amountPaid: true, status: true, updatedAt: true, serviceId: true, serviceName: true }
            }),
            this.prisma.franchiseLead.findMany({
                select: { status: true }
            }),
            this.prisma.device.findMany({
                select: { status: true }
            }),
            this.prisma.agency.findMany({
                select: {
                    id: true,
                    name: true,
                    type: true,
                    region: true,
                    city: true,
                    _count: { select: { leads: true } }
                }
            })
        ]);
        const totalRevenue = leads.reduce((acc, l) => acc + (l.amountPaid || 0), 0);
        const signed = leads.filter((l) => ['SIGNED', 'PAID', 'DONE'].includes(l.status)).length;
        const pending = leads.filter((l) => !['SIGNED', 'PAID', 'CANCELLED', 'ARCHIVED', 'DONE'].includes(l.status)).length;
        const today = new Date().toISOString().split('T')[0];
        const signedToday = leads.filter((l) => (['SIGNED', 'PAID'].includes(String(l.status))) &&
            l.updatedAt?.toISOString().startsWith(today)).length;
        const fleetHealth = devices.length ? Math.round((devices.filter((d) => d.status === 'ACTIVE').length / devices.length) * 100) : 0;
        const franchisePipeline = franchiseLeads.length;
        const serviceStats = leads.reduce((acc, l) => {
            acc[l.serviceId] = (acc[l.serviceId] || 0) + 1;
            return acc;
        }, {});
        return {
            overview: {
                totalLeads: leads.length,
                totalRevenue,
                conversionRate: leads.length ? Math.round((signed / leads.length) * 100) : 0,
                pendingDossiers: pending,
                completedToday: signedToday,
                fleetHealth,
                franchisePipeline
            },
            network: agencies,
            serviceDistribution: Object.entries(serviceStats).map(([id, count]) => ({ id, count })),
            franchiseStats: {
                new: franchiseLeads.filter((l) => l.status === 'NEW').length,
                meeting: franchiseLeads.filter((l) => l.status === 'MEETING').length,
                signed: franchiseLeads.filter((l) => l.status === 'SIGNED').length
            }
        };
    }
};
exports.DashboardService = DashboardService;
exports.DashboardService = DashboardService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], DashboardService);
//# sourceMappingURL=dashboard.service.js.map