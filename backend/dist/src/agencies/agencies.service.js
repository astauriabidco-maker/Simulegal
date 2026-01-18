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
        return this.prisma.agency.findMany({
            include: {
                _count: {
                    select: { leads: true }
                }
            }
        });
    }
    async findOne(id) {
        return this.prisma.agency.findUnique({
            where: { id },
            include: { leads: true, users: true }
        });
    }
    async create(data) {
        const agency = await this.prisma.agency.create({
            data
        });
        if (data.type === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }
        return agency;
    }
    async update(id, data) {
        return this.prisma.agency.update({
            where: { id },
            data
        });
    }
};
exports.AgenciesService = AgenciesService;
exports.AgenciesService = AgenciesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        devices_service_1.DevicesService])
], AgenciesService);
//# sourceMappingURL=agencies.service.js.map