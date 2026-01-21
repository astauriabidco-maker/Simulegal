import { Injectable, ConflictException, UnauthorizedException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { RegisterDto } from './dto/register.dto';
import { LoginDto } from './dto/login.dto';
import * as bcrypt from 'bcrypt';
import { JwtService } from '@nestjs/jwt';

@Injectable()
export class AuthService {
    constructor(
        private prisma: PrismaService,
        private jwtService: JwtService,
    ) { }

    async register(dto: RegisterDto) {
        // 1. Check if user already exists
        const existingUser = await this.prisma.user.findFirst({
            where: { email: dto.adminEmail },
        });

        if (existingUser) {
            throw new ConflictException('Un utilisateur avec cet email existe déjà.');
        }

        // 2. Hash password
        const passwordHash = await bcrypt.hash(dto.adminPassword, 10);

        // 3. Transaction: Create Association + Admin User
        const result = await this.prisma.$transaction(async (tx) => {
            const association = await tx.association.create({
                data: {
                    name: dto.associationName,
                    settings: {}, // Default settings
                    subscription_plan: 'FREE',
                },
            });

            const user = await tx.user.create({
                data: {
                    associationId: association.id,
                    email: dto.adminEmail,
                    password_hash: passwordHash,
                    firstName: dto.adminFirstName,
                    lastName: dto.adminLastName,
                    phone: dto.phone,
                    role: 'ADMIN',
                    status: 'ACTIVE',
                },
            });

            return { association, user };
        });

        // 4. Generate JWT
        const payload = {
            sub: result.user.id,
            email: result.user.email,
            associationId: result.association.id,
            role: result.user.role
        };

        return {
            access_token: await this.jwtService.signAsync(payload),
            user: {
                id: result.user.id,
                email: result.user.email,
                role: result.user.role,
            },
            association: {
                id: result.association.id,
                name: result.association.name,
            }
        };
    }

    async login(dto: LoginDto) {
        // 1. Find user
        const user = await this.prisma.user.findFirst({
            where: { email: dto.email },
            include: { association: true },
        });

        if (!user) {
            throw new UnauthorizedException('Identifiants invalides.');
        }

        // 2. Verify password
        const isPasswordValid = await bcrypt.compare(dto.password as string, user.password_hash);
        if (!isPasswordValid) {
            throw new UnauthorizedException('Identifiants invalides.');
        }

        // 3. Generate JWT
        const payload = {
            sub: user.id,
            email: user.email,
            associationId: user.associationId,
            role: user.role
        };

        return {
            access_token: await this.jwtService.signAsync(payload),
            user: {
                id: user.id,
                email: user.email,
                role: user.role,
            },
            association: {
                id: user.association.id,
                name: user.association.name,
            }
        };
    }
}
