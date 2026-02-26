import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import * as bcrypt from 'bcrypt';
import * as crypto from 'crypto';

@Injectable()
export class UsersService {
    constructor(private prisma: PrismaService) { }

    /**
     * Génère un mot de passe temporaire aléatoire sécurisé (12 caractères)
     */
    private generateTempPassword(): string {
        const chars = 'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789!@#$';
        let password = '';
        const randomBytes = crypto.randomBytes(12);
        for (let i = 0; i < 12; i++) {
            password += chars[randomBytes[i] % chars.length];
        }
        return password;
    }

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
        // Ne jamais retourner le hash du password
        delete mapped.password;
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

    /**
     * Version pour l'auth — retourne le password hash pour la validation bcrypt
     */
    async findOneByEmailWithPassword(email: string) {
        const user = await this.prisma.user.findUnique({
            where: { email },
            include: { agency: true, roleRef: true }
        });
        if (!user) return null;
        // Retourne le user AVEC le password (pour bcrypt.compare)
        return {
            ...user,
            scopeAgencyIds: user.scopeAgencyIds ? (typeof user.scopeAgencyIds === 'string' ? JSON.parse(user.scopeAgencyIds) : user.scopeAgencyIds) : [],
            expertises: user.expertises ? (typeof user.expertises === 'string' ? JSON.parse(user.expertises) : user.expertises) : []
        };
    }

    async findOneById(id: string) {
        const user = await this.prisma.user.findUnique({
            where: { id },
            include: { agency: true, roleRef: true }
        });
        return this.mapUser(user);
    }

    /**
     * Crée un utilisateur avec un mot de passe temporaire aléatoire.
     * Retourne le user + le tempPassword en clair (une seule fois).
     */
    async create(data: any) {
        const tempPassword = data.password || this.generateTempPassword();
        const hashedPassword = await bcrypt.hash(tempPassword, 10);

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

        const mapped = this.mapUser(user);
        console.log(`[USERS] 👤 Nouvel utilisateur créé: ${user.name} (${user.email}) — rôle: ${user.role}`);

        // Retourner le mot de passe temporaire une seule fois
        return { ...mapped, tempPassword };
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

    /**
     * Active/Désactive un compte utilisateur
     */
    async toggleActive(id: string) {
        const user = await this.prisma.user.findUnique({ where: { id } });
        if (!user) throw new Error('Utilisateur non trouvé');

        const updated = await this.prisma.user.update({
            where: { id },
            data: { isActive: !user.isActive }
        });

        console.log(`[USERS] ${updated.isActive ? '✅' : '🚫'} Compte ${updated.name} ${updated.isActive ? 'activé' : 'désactivé'}`);
        return this.mapUser(updated);
    }

    /**
     * Réinitialise le mot de passe et retourne le nouveau temporaire
     */
    async resetPassword(id: string) {
        const tempPassword = this.generateTempPassword();
        const hashedPassword = await bcrypt.hash(tempPassword, 10);

        const user = await this.prisma.user.update({
            where: { id },
            data: { password: hashedPassword }
        });

        console.log(`[USERS] 🔑 Mot de passe réinitialisé pour ${user.name}`);
        return { ...this.mapUser(user), tempPassword };
    }

    async delete(id: string) {
        // Soft delete — désactiver plutôt que supprimer définitivement
        const user = await this.prisma.user.update({
            where: { id },
            data: { isActive: false }
        });
        console.log(`[USERS] 🗑️ Utilisateur ${user.name} désactivé (soft delete)`);
        return this.mapUser(user);
    }
}
