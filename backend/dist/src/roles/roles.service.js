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
exports.RolesService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let RolesService = class RolesService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async onModuleInit() {
        await this.seedDefaultRoles();
    }
    async seedDefaultRoles() {
        const defaultRoles = [
            {
                id: 'SUPERADMIN',
                label: 'Super Administrateur',
                description: 'Accès total et illimité à toutes les fonctions du système.',
                permissions: 'crm.view_all,crm.view_agency,crm.create,crm.edit_technical,crm.validate_doc,crm.delete,finance.view_global,finance.view_agency,finance.payout,network.manage,fleet.manage,users.manage,roles.manage,settings.manage',
                isSystem: true,
            },
            {
                id: 'HQ',
                label: 'Staff Siège (HQ)',
                description: 'Gestion opérationnelle des dossiers et du réseau.',
                permissions: 'crm.view_all,crm.view_agency,crm.create,crm.edit_technical,crm.validate_doc,finance.view_global,finance.view_agency,network.manage,fleet.manage,users.manage',
                isSystem: true,
            },
            {
                id: 'AGENCY',
                label: 'Directeur d\'Agence',
                description: 'Gestion des leads de l\'agence et suivi des commissions.',
                permissions: 'crm.view_agency,crm.create,finance.view_agency',
                isSystem: true,
            },
        ];
        for (const role of defaultRoles) {
            await this.prisma.role.upsert({
                where: { id: role.id },
                update: {
                    label: role.label,
                    description: role.description,
                    permissions: role.permissions,
                },
                create: role,
            });
        }
    }
    async findAll() {
        return this.prisma.role.findMany({
            orderBy: { createdAt: 'desc' },
        });
    }
    async findOne(id) {
        return this.prisma.role.findUnique({ where: { id } });
    }
    async create(data) {
        return this.prisma.role.create({
            data: {
                ...data,
                isSystem: false,
            },
        });
    }
    async update(id, data) {
        if (id === 'SUPERADMIN') {
        }
        return this.prisma.role.update({
            where: { id },
            data,
        });
    }
    async remove(id) {
        const role = await this.prisma.role.findUnique({ where: { id } });
        if (role?.isSystem) {
            throw new Error('Impossible de supprimer un rôle système');
        }
        return this.prisma.role.delete({ where: { id } });
    }
};
exports.RolesService = RolesService;
exports.RolesService = RolesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], RolesService);
//# sourceMappingURL=roles.service.js.map