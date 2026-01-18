import { Injectable, UnauthorizedException } from '@nestjs/common';
import { UsersService } from '../users/users.service';
import { JwtService } from '@nestjs/jwt';
import { PrismaService } from '../prisma/prisma.service';
import * as bcrypt from 'bcrypt';

@Injectable()
export class AuthService {
    constructor(
        private usersService: UsersService,
        private jwtService: JwtService,
        private prisma: PrismaService
    ) { }

    async validateUser(email: string, pass: string): Promise<any> {
        const user = await this.usersService.findOneByEmail(email);
        if (user && await bcrypt.compare(pass, user.password)) {
            const { password, ...result } = user;
            return result;
        }
        return null;
    }

    async login(user: any) {
        const permissions = user.roleRef?.permissions || user.permissions || '';

        const payload = {
            email: user.email,
            sub: user.id,
            role: user.role,
            roleId: user.roleId,
            permissions: permissions,
            agencyId: user.homeAgencyId || user.agencyId
        };
        return {
            access_token: this.jwtService.sign(payload),
            user: {
                id: user.id,
                email: user.email,
                name: user.name,
                role: user.role,
                roleId: user.roleId,
                agencyId: user.agencyId,
                agencyName: user.agency?.name,
                permissions: permissions.split(',')
            }
        };
    }

    /**
     * Demo login for demo candidates (*@demo.fr emails)
     * Generates a real JWT without password validation
     */
    async demoLogin(email: string) {
        // Only allow demo emails
        if (!email.endsWith('@demo.fr')) {
            throw new UnauthorizedException('Cette fonctionnalité est réservée aux comptes démo');
        }

        // Find lead by email
        const lead = await this.prisma.lead.findFirst({
            where: { email: email.toLowerCase() }
        });

        if (!lead) {
            throw new UnauthorizedException('Aucun dossier démo trouvé pour cet email');
        }

        // Generate JWT with CLIENT role
        const payload = {
            email: lead.email,
            sub: lead.id,
            role: 'CLIENT',
            leadId: lead.id
        };

        return {
            access_token: this.jwtService.sign(payload),
            lead: {
                id: lead.id,
                name: lead.name,
                email: lead.email,
                serviceId: lead.serviceId,
                serviceName: lead.serviceName,
                status: lead.status
            }
        };
    }

    async devLogin() {
        const payload = {
            email: 'admin@simulegal.fr',
            sub: 'dev-admin-id',
            role: 'SUPER_ADMIN',
            permissions: '*'
        };
        return {
            access_token: this.jwtService.sign(payload),
            user: {
                email: 'admin@simulegal.fr',
                role: 'SUPER_ADMIN'
            }
        };
    }
}
