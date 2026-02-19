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
var VeilleService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.VeilleService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let VeilleService = VeilleService_1 = class VeilleService {
    prisma;
    logger = new common_1.Logger(VeilleService_1.name);
    constructor(prisma) {
        this.prisma = prisma;
    }
    async findAll() {
        return this.prisma.legalUpdate.findMany({
            orderBy: { createdAt: 'desc' },
        });
    }
    async findPending() {
        return this.prisma.legalUpdate.findMany({
            where: { applied: false },
            orderBy: { createdAt: 'desc' },
        });
    }
    async create(data) {
        const note = await this.prisma.legalUpdate.create({
            data: {
                title: data.title,
                summary: data.summary,
                category: data.category,
                severity: data.severity || 'medium',
                sourceUrl: data.sourceUrl || null,
                authorName: data.authorName || null,
                linkedRuleIds: JSON.stringify(data.linkedRuleIds || []),
            },
        });
        this.logger.log(`✅ Note créée: "${note.title}" (${note.id})`);
        return note;
    }
    async update(id, data) {
        const { linkedRuleIds, ...rest } = data;
        const updateData = { ...rest };
        if (linkedRuleIds !== undefined) {
            updateData.linkedRuleIds = JSON.stringify(linkedRuleIds);
        }
        const note = await this.prisma.legalUpdate.update({
            where: { id },
            data: updateData,
        });
        this.logger.log(`✅ Note mise à jour: "${note.title}" (${note.id})`);
        return note;
    }
    async markAsApplied(id) {
        return this.prisma.legalUpdate.update({
            where: { id },
            data: { applied: true, appliedAt: new Date() },
        });
    }
    async remove(id) {
        await this.prisma.legalUpdate.delete({ where: { id } });
        this.logger.log(`🗑 Note supprimée: ${id}`);
        return { deleted: true };
    }
    async getStats() {
        const [total, applied, pending] = await Promise.all([
            this.prisma.legalUpdate.count(),
            this.prisma.legalUpdate.count({ where: { applied: true } }),
            this.prisma.legalUpdate.count({ where: { applied: false } }),
        ]);
        const latest = await this.prisma.legalUpdate.findFirst({
            orderBy: { createdAt: 'desc' },
            select: { createdAt: true },
        });
        return {
            totalCount: total,
            appliedCount: applied,
            pendingCount: pending,
            conformityPercent: total > 0 ? Math.round((applied / total) * 100) : 100,
            lastUpdate: latest?.createdAt || null,
        };
    }
};
exports.VeilleService = VeilleService;
exports.VeilleService = VeilleService = VeilleService_1 = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], VeilleService);
//# sourceMappingURL=veille.service.js.map