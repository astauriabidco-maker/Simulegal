import { Injectable, OnModuleInit, BadRequestException, ForbiddenException } from '@nestjs/common';
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
                id: 'SUPER_ADMIN',
                label: 'Super Administrateur',
                description: 'Accès total et illimité à toutes les fonctions du système.',
                permissions: 'crm.view_all,crm.view_agency,crm.view_own,crm.create,crm.edit_technical,crm.validate_doc,crm.delete,crm.assign,sales.view,sales.manage,sales.tracking,calendar.view,calendar.manage,finance.view_global,finance.view_agency,finance.payout,network.manage,fleet.manage,franchise.manage,blog.view,blog.manage,users.manage,roles.manage,settings.manage,automations.manage,audit.view,inbox.view,inbox.send,profile.view_own,profile.edit_own,profile.view_team',
                isSystem: true,
            },
            {
                id: 'HQ_ADMIN',
                label: 'Administrateur Siège',
                description: 'Gestion opérationnelle des dossiers, du réseau et des utilisateurs.',
                permissions: 'crm.view_all,crm.view_agency,crm.view_own,crm.create,crm.edit_technical,crm.validate_doc,crm.assign,sales.view,sales.manage,sales.tracking,calendar.view,calendar.manage,finance.view_global,finance.view_agency,network.manage,fleet.manage,franchise.manage,blog.view,blog.manage,users.manage,audit.view,inbox.view,inbox.send,profile.view_own,profile.edit_own,profile.view_team',
                isSystem: true,
            },
            {
                id: 'CASE_WORKER',
                label: 'Juriste / Opérateur',
                description: 'Traitement juridique des dossiers, validation de documents, communication client.',
                permissions: 'crm.view_all,crm.view_agency,crm.view_own,crm.create,crm.edit_technical,crm.validate_doc,crm.assign,calendar.view,calendar.manage,blog.view,inbox.view,inbox.send,profile.view_own,profile.edit_own,profile.view_team',
                isSystem: true,
            },
            {
                id: 'AGENCY_MANAGER',
                label: 'Manager Agence',
                description: 'Gestion des leads de l\'agence, suivi des commissions, communication client et dispatch aux commerciaux.',
                permissions: 'crm.view_agency,crm.view_own,crm.create,crm.assign,sales.view,sales.manage,sales.tracking,calendar.view,calendar.manage,finance.view_agency,inbox.view,inbox.send,profile.view_own,profile.edit_own,profile.view_team',
                isSystem: true,
            },
            {
                id: 'SALES',
                label: 'Commercial',
                description: 'Prospection commerciale, création de leads et communication client.',
                permissions: 'crm.view_own,crm.create,sales.view,sales.tracking,calendar.view,inbox.view,inbox.send,profile.view_own,profile.edit_own',
                isSystem: true,
            },
            {
                id: 'KIOSK_AGENT',
                label: 'Agent de Kiosque',
                description: 'Création de dossiers depuis une tablette en agence. Accès très limité.',
                permissions: 'crm.create,profile.view_own',
                isSystem: true,
            },
            {
                id: 'API_PARTNER',
                label: 'Partenaire API',
                description: 'Accès API pour intégrations externes (pas d\'interface back-office).',
                permissions: 'crm.create,crm.view_own',
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

        // Nettoyage des anciens rôles legacy s'ils existent
        const legacyIds = ['SUPERADMIN', 'HQ', 'AGENCY'];
        for (const legacyId of legacyIds) {
            const users = await this.prisma.user.findMany({ where: { roleId: legacyId } });
            if (users.length === 0) {
                await this.prisma.role.deleteMany({ where: { id: legacyId } });
            }
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
        if (id === 'SUPER_ADMIN') {
            throw new ForbiddenException('Le rôle Super Admin ne peut pas être modifié');
        }

        return this.prisma.role.update({
            where: { id },
            data,
        });
    }

    async remove(id: string) {
        const role = await this.prisma.role.findUnique({ where: { id } });
        if (!role) {
            throw new BadRequestException('Rôle introuvable');
        }
        if (role.isSystem) {
            throw new ForbiddenException('Impossible de supprimer un rôle système');
        }

        // Vérifier qu'aucun utilisateur n'utilise ce rôle
        const usersWithRole = await this.prisma.user.count({ where: { roleId: id } });
        if (usersWithRole > 0) {
            throw new BadRequestException(`Impossible de supprimer : ${usersWithRole} utilisateur(s) utilisent ce rôle`);
        }

        return this.prisma.role.delete({ where: { id } });
    }
}
