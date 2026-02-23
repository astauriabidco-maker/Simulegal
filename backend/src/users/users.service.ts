import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import * as bcrypt from 'bcrypt';

@Injectable()
export class UsersService {
    constructor(private prisma: PrismaService) { }

    private mapUser(user: any, agencyMap?: Map<string, string>) {
        if (!user) return null;
        const mapped: any = {
            ...user,
            scopeAgencyIds: user.scopeAgencyIds ? (typeof user.scopeAgencyIds === 'string' ? JSON.parse(user.scopeAgencyIds) : user.scopeAgencyIds) : [],
            expertises: user.expertises ? (typeof user.expertises === 'string' ? JSON.parse(user.expertises) : user.expertises) : []
        };
        // Résoudre le nom de l'agence de rattachement
        if (user.homeAgencyId && agencyMap) {
            mapped.homeAgencyName = agencyMap.get(user.homeAgencyId) || null;
        }
        return mapped;
    }

    async findAll() {
        const [users, agencies] = await Promise.all([
            this.prisma.user.findMany({
                include: { agency: true, roleRef: true }
            }),
            this.prisma.agency.findMany({ select: { id: true, name: true } })
        ]);
        const agencyMap = new Map(agencies.map(a => [a.id, a.name]));
        return users.map(u => this.mapUser(u, agencyMap));
    }

    async findSystemUsers() {
        const systemRoles = ['SUPER_ADMIN', 'HQ_ADMIN', 'API_PARTNER'];
        const users = await this.prisma.user.findMany({
            where: {
                OR: [
                    { isSystemUser: true },
                    { role: { in: systemRoles as any } }
                ]
            },
            include: { agency: true, roleRef: true },
            orderBy: { createdAt: 'desc' }
        });
        return users.map(u => this.mapUser(u));
    }

    async findOneByEmail(email: string) {
        const user = await this.prisma.user.findUnique({
            where: { email },
            include: { agency: true, roleRef: true }
        });
        return this.mapUser(user);
    }

    async findOneById(id: string) {
        const user = await this.prisma.user.findUnique({
            where: { id },
            include: { agency: true, roleRef: true }
        });
        return this.mapUser(user);
    }

    async create(data: any) {
        const hashedPassword = await bcrypt.hash(data.password || 'demo123', 10);

        // Sérialisation des périmètres
        const scopeAgencyIds = data.scopeAgencyIds ? JSON.stringify(data.scopeAgencyIds) : "[]";
        const expertises = data.expertises ? JSON.stringify(data.expertises) : "[]";

        const user = await this.prisma.user.create({
            data: {
                ...data,
                scopeAgencyIds,
                expertises,
                password: hashedPassword
            }
        });

        return this.mapUser(user);
    }

    async update(id: string, data: any) {
        const updateData = { ...data };

        if (updateData.password) {
            updateData.password = await bcrypt.hash(updateData.password, 10);
        }

        if (updateData.scopeAgencyIds && Array.isArray(updateData.scopeAgencyIds)) {
            updateData.scopeAgencyIds = JSON.stringify(updateData.scopeAgencyIds);
        }

        if (updateData.expertises && Array.isArray(updateData.expertises)) {
            updateData.expertises = JSON.stringify(updateData.expertises);
        }

        const user = await this.prisma.user.update({
            where: { id },
            data: updateData
        });

        return this.mapUser(user);
    }

    async delete(id: string) {
        // On pourrait faire un soft delete ici si nécessaire
        return this.prisma.user.delete({
            where: { id }
        });
    }
}
