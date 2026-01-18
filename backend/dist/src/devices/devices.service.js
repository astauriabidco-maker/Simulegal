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
exports.DevicesService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let DevicesService = class DevicesService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    generatePairingCode() {
        const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
        const numbers = '0123456789';
        let code = '';
        for (let i = 0; i < 4; i++)
            code += letters.charAt(Math.floor(Math.random() * letters.length));
        code += '-';
        for (let i = 0; i < 4; i++)
            code += numbers.charAt(Math.floor(Math.random() * numbers.length));
        return code;
    }
    async register() {
        const device = await this.prisma.device.create({
            data: {
                pairingCode: this.generatePairingCode(),
                status: 'UNPAIRED',
                name: 'Nouveau Terminal',
                appVersion: '1.0.0',
                lastHeartbeat: new Date()
            }
        });
        console.log(`[DEVICES] 📱 New device registered: ${device.id} (code: ${device.pairingCode})`);
        return device;
    }
    async createProvisioned(agencyId, agencyName) {
        const device = await this.prisma.device.create({
            data: {
                pairingCode: this.generatePairingCode(),
                status: 'UNPAIRED',
                name: `Borne ${agencyName}`,
                assignedAgencyId: agencyId,
                appVersion: '1.0.0',
                lastHeartbeat: new Date()
            }
        });
        console.log(`[DEVICES] 🤖 Auto-provisioned device for agency ${agencyId}: ${device.id}`);
        return device;
    }
    async activate(code) {
        const device = await this.prisma.device.findUnique({
            where: { pairingCode: code.toUpperCase() }
        });
        if (!device) {
            return null;
        }
        const updated = await this.prisma.device.update({
            where: { id: device.id },
            data: {
                status: 'ACTIVE',
                lastHeartbeat: new Date()
            },
            include: { assignedAgency: true }
        });
        console.log(`[DEVICES] 🚀 Device activated via code: ${updated.id}`);
        return updated;
    }
    async findAll() {
        return this.prisma.device.findMany({
            include: { assignedAgency: true },
            orderBy: { createdAt: 'desc' }
        });
    }
    async findById(id) {
        return this.prisma.device.findUnique({
            where: { id },
            include: { assignedAgency: true }
        });
    }
    async findByCode(code) {
        return this.prisma.device.findUnique({
            where: { pairingCode: code.toUpperCase() },
            include: { assignedAgency: true }
        });
    }
    async pair(pairingCode, agencyId, name) {
        const device = await this.prisma.device.findUnique({
            where: { pairingCode: pairingCode.toUpperCase() }
        });
        if (!device) {
            return null;
        }
        const updated = await this.prisma.device.update({
            where: { id: device.id },
            data: {
                status: 'ACTIVE',
                assignedAgencyId: agencyId,
                name: name || device.name,
                lastHeartbeat: new Date()
            },
            include: { assignedAgency: true }
        });
        console.log(`[DEVICES] ✅ Device paired: ${device.id} → ${agencyId} `);
        return updated;
    }
    async heartbeat(id) {
        try {
            const device = await this.prisma.device.update({
                where: { id },
                data: {
                    lastHeartbeat: new Date(),
                    status: 'ACTIVE'
                }
            });
            return device;
        }
        catch {
            return null;
        }
    }
    async reset(id) {
        return this.prisma.device.update({
            where: { id },
            data: {
                status: 'UNPAIRED',
                pairingCode: this.generatePairingCode(),
                assignedAgencyId: null
            }
        });
    }
    async remove(id) {
        return this.prisma.device.delete({
            where: { id }
        });
    }
    async findByAgency(agencyId) {
        return this.prisma.device.findMany({
            where: { assignedAgencyId: agencyId },
            include: { assignedAgency: true }
        });
    }
};
exports.DevicesService = DevicesService;
exports.DevicesService = DevicesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], DevicesService);
//# sourceMappingURL=devices.service.js.map