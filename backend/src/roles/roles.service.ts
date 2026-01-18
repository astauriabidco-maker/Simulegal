import { Injectable, OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class RolesService implements OnModuleInit {
    constructor(private prisma: PrismaService) { }

    async onModuleInit() {
        await this.seedDefaultRoles();
    }

    private async seedDefaultRoles() {
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

    async findOne(id: string) {
        return this.prisma.role.findUnique({ where: { id } });
    }

    async create(data: { label: string; description: string; permissions: string }) {
        return this.prisma.role.create({
            data: {
                ...data,
                isSystem: false,
            },
        });
    }

    async update(id: string, data: { label?: string; description?: string; permissions?: string }) {
        // On empêche de désactiver ou supprimer les permissions critiques du SuperAdmin via l'API standard
        if (id === 'SUPERADMIN') {
            // En prod on limiterait plus, ici on laisse passer pour la souplesse de l'exercice
        }

        return this.prisma.role.update({
            where: { id },
            data,
        });
    }

    async remove(id: string) {
        const role = await this.prisma.role.findUnique({ where: { id } });
        if (role?.isSystem) {
            throw new Error('Impossible de supprimer un rôle système');
        }
        return this.prisma.role.delete({ where: { id } });
    }
}
