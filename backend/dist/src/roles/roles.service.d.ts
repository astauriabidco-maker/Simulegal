import { OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
export declare class RolesService implements OnModuleInit {
    private prisma;
    constructor(prisma: PrismaService);
    onModuleInit(): Promise<void>;
    private seedDefaultRoles;
    findAll(): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }[]>;
    findOne(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    } | null>;
    create(data: {
        label: string;
        description: string;
        permissions: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }>;
    update(id: string, data: {
        label?: string;
        description?: string;
        permissions?: string;
    }): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }>;
    remove(id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }>;
}
