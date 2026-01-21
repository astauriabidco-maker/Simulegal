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
exports.SalesService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const assignment_service_1 = require("./assignment.service");
let SalesService = class SalesService {
    prisma;
    assignmentService;
    constructor(prisma, assignmentService) {
        this.prisma = prisma;
        this.assignmentService = assignmentService;
    }
    async findAll(params) {
        const { page, limit, status, agencyId, source, dateFrom, dateTo } = params;
        const skip = (page - 1) * limit;
        const where = {};
        if (status) {
            where.status = status;
        }
        if (agencyId) {
            where.agencyId = agencyId;
        }
        if (source) {
            where.source = source;
        }
        if (dateFrom || dateTo) {
            where.createdAt = {};
            if (dateFrom) {
                where.createdAt.gte = new Date(dateFrom);
            }
            if (dateTo) {
                where.createdAt.lte = new Date(dateTo);
            }
        }
        const [data, total] = await Promise.all([
            this.prisma.prospect.findMany({
                where,
                skip,
                take: limit,
                include: { notes: true },
                orderBy: { createdAt: 'desc' },
            }),
            this.prisma.prospect.count({ where })
        ]);
        return {
            data,
            meta: {
                total,
                page,
                limit,
                totalPages: Math.ceil(total / limit)
            }
        };
    }
    async exportToCSV(filters) {
        const where = {};
        if (filters?.agencyId)
            where.agencyId = filters.agencyId;
        if (filters?.source)
            where.source = filters.source;
        if (filters?.dateFrom || filters?.dateTo) {
            where.createdAt = {};
            if (filters.dateFrom)
                where.createdAt.gte = new Date(filters.dateFrom);
            if (filters.dateTo)
                where.createdAt.lte = new Date(filters.dateTo);
        }
        const prospects = await this.prisma.prospect.findMany({
            where,
            orderBy: { createdAt: 'desc' },
        });
        const headers = ['ID', 'Prénom', 'Nom', 'Téléphone', 'Email', 'Source', 'Agence', 'Statut', 'Score', 'Date création'];
        const rows = prospects.map(p => [
            p.id,
            p.firstName,
            p.lastName,
            p.phone,
            p.email || '',
            p.source,
            p.agencyId,
            p.status,
            p.score.toString(),
            new Date(p.createdAt).toLocaleDateString('fr-FR')
        ].map(v => `"${(v || '').replace(/"/g, '""')}"`).join(';'));
        return [headers.join(';'), ...rows].join('\n');
    }
    async findOne(id) {
        return this.prisma.prospect.findUnique({
            where: { id },
            include: { notes: true },
        });
    }
    async create(data) {
        const score = this.calculateScore(data);
        const assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId || null);
        const prospect = await this.prisma.prospect.create({
            data: {
                ...data,
                score,
                status: 'TO_CALL',
                assignedToSalesId,
            },
        });
        await this.triggerAutomation(prospect, 'TO_CALL');
        return prospect;
    }
    async update(id, data) {
        const oldProspect = await this.prisma.prospect.findUnique({ where: { id } });
        if (!oldProspect)
            return null;
        const updatedProspect = await this.prisma.prospect.update({
            where: { id },
            data,
        });
        if (data.status && data.status !== oldProspect.status) {
            await this.triggerAutomation(updatedProspect, data.status);
        }
        return updatedProspect;
    }
    calculateScore(prospect) {
        let score = 0;
        if (prospect.source === 'GOOGLE_ADS')
            score += 30;
        if (prospect.source === 'META_ADS')
            score += 20;
        if (prospect.source === 'TIKTOK_ADS')
            score += 15;
        if (prospect.email && prospect.email.length > 5)
            score += 10;
        if (prospect.phone && prospect.phone.length > 8)
            score += 10;
        if (prospect.interestServiceId)
            score += 10;
        return Math.min(score, 100);
    }
    async triggerAutomation(prospect, status) {
        console.log(`[BACKEND AUTOMATION] Trigger for ${prospect.firstName} -> ${status}`);
        switch (status) {
            case 'TO_CALL':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, merci de votre intérêt pour Simulegal. Un expert va vous rappeler ds les 2h."`);
                break;
            case 'MEETING_BOOKED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Votre RDV Simulegal est confirmé."`);
                break;
            case 'LINK_SENT':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Voici votre lien sécurisé: https://simulegal.fr/pay/${prospect.id}"`);
                break;
        }
    }
    async addNote(prospectId, authorId, text) {
        return this.prisma.prospectNote.create({
            data: {
                prospectId,
                authorId,
                text,
            },
        });
    }
    async importFromCSV(buffer) {
        const content = buffer.toString('utf-8');
        const lines = content.split(/\r?\n/);
        let count = 0;
        const headers = lines[0]?.toLowerCase().split(/[;,]/).map(h => h.trim().replace(/"/g, ''));
        if (!headers || !headers.includes('firstname') || !headers.includes('lastname')) {
            throw new Error('Invalid CSV headers. Expected firstName, lastName, phone, email');
        }
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line)
                continue;
            const values = line.split(/[;,]/).map(v => v.trim().replace(/"/g, ''));
            const data = {};
            headers.forEach((header, index) => {
                const val = values[index];
                if (header === 'firstname')
                    data.firstName = val;
                if (header === 'lastname')
                    data.lastName = val;
                if (header === 'email')
                    data.email = val;
                if (header === 'phone')
                    data.phone = val;
                if (header === 'source')
                    data.source = val;
            });
            if (data.firstName && data.lastName) {
                data.source = data.source || 'CSV_IMPORT';
                data.status = 'TO_CALL';
                data.score = 10;
                data.agencyId = data.agencyId || 'HQ-001';
                data.assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId);
                await this.prisma.prospect.create({ data });
                count++;
            }
        }
        return count;
    }
};
exports.SalesService = SalesService;
exports.SalesService = SalesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        assignment_service_1.AssignmentService])
], SalesService);
//# sourceMappingURL=sales.service.js.map