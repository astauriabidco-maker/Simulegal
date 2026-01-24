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
exports.AgenciesService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const devices_service_1 = require("../devices/devices.service");
let AgenciesService = class AgenciesService {
    prisma;
    devicesService;
    constructor(prisma, devicesService) {
        this.prisma = prisma;
        this.devicesService = devicesService;
    }
    async findAll() {
        const agencies = await this.prisma.agency.findMany({
            include: {
                _count: {
                    select: { leads: true }
                }
            }
        });
        return agencies.map(a => this.mapAgency(a));
    }
    async findOne(id) {
        const agency = await this.prisma.agency.findUnique({
            where: { id },
            include: { leads: true, users: true }
        });
        if (!agency)
            return null;
        return this.mapAgency(agency);
    }
    mapAgency(agency) {
        return {
            ...agency,
            zipCodes: this.parseZipCodes(agency.zipCodes)
        };
    }
    parseZipCodes(zipCodes) {
        if (!zipCodes)
            return [];
        try {
            const parsed = JSON.parse(zipCodes);
            return Array.isArray(parsed) ? parsed : [];
        }
        catch {
            return zipCodes.split(',').map(s => s.trim()).filter(Boolean);
        }
    }
    async create(data) {
        const createData = { ...data };
        if (createData.zipCodes && Array.isArray(createData.zipCodes)) {
            createData.zipCodes = JSON.stringify(createData.zipCodes);
        }
        else if (typeof createData.zipCodes === 'string') {
            createData.zipCodes = JSON.stringify(createData.zipCodes.split(',').map((s) => s.trim()).filter(Boolean));
        }
        const agency = await this.prisma.agency.create({
            data: createData
        });
        if (data.type === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }
        return this.mapAgency(agency);
    }
    async update(id, data) {
        const updateData = { ...data };
        if (updateData.zipCodes && Array.isArray(updateData.zipCodes)) {
            updateData.zipCodes = JSON.stringify(updateData.zipCodes);
        }
        else if (typeof updateData.zipCodes === 'string') {
            updateData.zipCodes = JSON.stringify(updateData.zipCodes.split(',').map((s) => s.trim()).filter(Boolean));
        }
        const agency = await this.prisma.agency.update({
            where: { id },
            data: updateData
        });
        return this.mapAgency(agency);
    }
    async checkTerritoryAvailability(zipCode, excludeAgencyId) {
        const agencies = await this.prisma.agency.findMany({
            where: {
                status: 'ACTIVE',
                id: { not: excludeAgencyId }
            },
            select: { id: true, name: true, zipCodes: true }
        });
        for (const agency of agencies) {
            try {
                const codes = JSON.parse(agency.zipCodes || '[]');
                if (codes.includes(zipCode)) {
                    return { available: false, agencyId: agency.id, agencyName: agency.name };
                }
            }
            catch (e) {
                if (agency.zipCodes.split(',').includes(zipCode)) {
                    return { available: false, agencyId: agency.id, agencyName: agency.name };
                }
            }
        }
        return { available: true };
    }
    async delete(id) {
        const agency = await this.prisma.agency.findUnique({
            where: { id },
            include: { _count: { select: { leads: true } } }
        });
        if (!agency) {
            throw new Error('Agency not found');
        }
        return this.prisma.agency.update({
            where: { id },
            data: { status: 'INACTIVE' }
        });
    }
    async exportToCSV() {
        const agencies = await this.prisma.agency.findMany({
            include: {
                _count: { select: { leads: true, users: true } }
            },
            orderBy: { name: 'asc' }
        });
        const headers = ['ID', 'Nom', 'Type', 'Statut', 'Email', 'Région', 'Codes Postaux', 'Commission %', 'Nb Leads', 'Nb Users'];
        const rows = agencies.map(a => [
            a.id,
            a.name,
            a.type,
            a.status,
            a.contactEmail || '',
            a.region || '',
            a.zipCodes || '',
            a.commissionRate?.toString() || '0',
            a._count.leads.toString(),
            a._count.users.toString()
        ].map(v => `"${(v || '').toString().replace(/"/g, '""')}"`).join(';'));
        return [headers.join(';'), ...rows].join('\n');
    }
};
exports.AgenciesService = AgenciesService;
exports.AgenciesService = AgenciesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        devices_service_1.DevicesService])
], AgenciesService);
//# sourceMappingURL=agencies.service.js.map