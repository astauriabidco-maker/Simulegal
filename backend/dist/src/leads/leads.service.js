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
exports.LeadsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const notifications_service_1 = require("../notifications/notifications.service");
let LeadsService = class LeadsService {
    prisma;
    notifications;
    constructor(prisma, notifications) {
        this.prisma = prisma;
        this.notifications = notifications;
    }
    async findAll() {
        return this.prisma.lead.findMany({
            include: { notes: true, originAgency: true },
            orderBy: { createdAt: 'desc' }
        });
    }
    async findByAgency(agencyId) {
        return this.prisma.lead.findMany({
            where: { originAgencyId: agencyId },
            include: { notes: true },
            orderBy: { createdAt: 'desc' }
        });
    }
    async findOne(id) {
        return this.prisma.lead.findUnique({
            where: { id },
            include: { notes: true, originAgency: true }
        });
    }
    async updateStatus(id, status) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (lead) {
            console.log(`[LeadsService] Updating lead ${id} status from ${lead.status} to ${status}`);
            await this.notifications.onStageChange(lead, lead.status, status);
        }
        return this.prisma.lead.update({
            where: { id },
            data: { status }
        });
    }
    async assignUser(id, userId) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        const user = await this.prisma.user.findUnique({ where: { id: userId } });
        if (lead && user) {
            await this.notifications.onJuristAssigned(lead, user.name);
        }
        return this.prisma.lead.update({
            where: { id },
            data: { assignedUserId: userId }
        });
    }
    async updateDocuments(id, documents) {
        const lead = await this.prisma.lead.findUnique({ where: { id } });
        if (lead) {
            const oldDocs = JSON.parse(lead.documents || '[]');
            for (const newDoc of documents) {
                const oldDoc = oldDocs.find((d) => d.id === newDoc.id);
                if (newDoc.status === 'REJECTED' && oldDoc?.status !== 'REJECTED') {
                    await this.notifications.onDocumentRejected(lead, newDoc.docType);
                }
            }
        }
        return this.prisma.lead.update({
            where: { id },
            data: { documents: JSON.stringify(documents) }
        });
    }
    async addNote(leadId, data) {
        return this.prisma.leadNote.create({
            data: {
                content: data.content,
                author: data.author,
                leadId
            }
        });
    }
    async create(data) {
        const { currentStage, contract, documents, requiredDocuments, ...rest } = data;
        const leadId = data.id || `SL-${Math.floor(Math.random() * 90000 + 10000)}`;
        return this.prisma.lead.create({
            data: {
                ...rest,
                id: leadId,
                status: currentStage || rest.status || 'NEW',
                contract: contract ? JSON.stringify(contract) : null,
                documents: documents ? JSON.stringify(documents) : '[]',
                requiredDocs: requiredDocuments ? JSON.stringify(requiredDocuments) : null,
            }
        });
    }
};
exports.LeadsService = LeadsService;
exports.LeadsService = LeadsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        notifications_service_1.NotificationsService])
], LeadsService);
//# sourceMappingURL=leads.service.js.map