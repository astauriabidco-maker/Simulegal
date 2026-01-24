import { OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
export declare class SettingsService implements OnModuleInit {
    private prisma;
    constructor(prisma: PrismaService);
    onModuleInit(): Promise<void>;
    getSettings(): Promise<{
        company: any;
        payment: any;
        notifications: any;
        integrations: any;
        storage: any;
        updatedAt: Date;
    }>;
    updateSection(section: string, data: any): Promise<{
        id: string;
        updatedAt: Date;
        company: string;
        payment: string;
        notifications: string;
        integrations: string;
        storage: string;
    }>;
}
