import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import * as bcrypt from 'bcrypt';

@Injectable()
export class UsersService {
    constructor(private prisma: PrismaService) { }

    async findAll() {
        const users = await this.prisma.user.findMany({
            include: { agency: true, roleRef: true }
        });
        return users.map(u => ({
            ...u,
            scopeAgencyIds: u.scopeAgencyIds ? JSON.parse(u.scopeAgencyIds) : []
        }));
    }

    async findOneByEmail(email: string) {
        const user = await this.prisma.user.findUnique({
            where: { email },
            include: { agency: true, roleRef: true }
        });
        if (!user) return null;
        return {
            ...user,
            scopeAgencyIds: user.scopeAgencyIds ? JSON.parse(user.scopeAgencyIds) : []
        };
    }

    async findOneById(id: string) {
        const user = await this.prisma.user.findUnique({
            where: { id },
            include: { agency: true, roleRef: true }
        });
        if (!user) return null;
        return {
            ...user,
            scopeAgencyIds: user.scopeAgencyIds ? JSON.parse(user.scopeAgencyIds) : []
        };
    }

    async create(data: any) {
        const hashedPassword = await bcrypt.hash(data.password || 'demo123', 10);

        // Sérialisation des périmètres
        const scopeAgencyIds = data.scopeAgencyIds ? JSON.stringify(data.scopeAgencyIds) : "[]";

        return this.prisma.user.create({
            data: {
                ...data,
                scopeAgencyIds,
                password: hashedPassword
            }
        });
    }

    async update(id: string, data: any) {
        const updateData = { ...data };

        if (updateData.password) {
            updateData.password = await bcrypt.hash(updateData.password, 10);
        }

        if (updateData.scopeAgencyIds && Array.isArray(updateData.scopeAgencyIds)) {
            updateData.scopeAgencyIds = JSON.stringify(updateData.scopeAgencyIds);
        }

        return this.prisma.user.update({
            where: { id },
            data: updateData
        });
    }

    async delete(id: string) {
        // On pourrait faire un soft delete ici si nécessaire
        return this.prisma.user.delete({
            where: { id }
        });
    }
}
