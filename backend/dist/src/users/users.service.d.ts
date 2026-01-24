import { PrismaService } from '../prisma/prisma.service';
export declare class UsersService {
    private prisma;
    constructor(prisma: PrismaService);
    private mapUser;
    findAll(): Promise<any[]>;
    findSystemUsers(): Promise<any[]>;
    findOneByEmail(email: string): Promise<any>;
    findOneById(id: string): Promise<any>;
    create(data: any): Promise<any>;
    update(id: string, data: any): Promise<any>;
    delete(id: string): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        expertises: string;
        agencyId: string | null;
        email: string;
        role: import(".prisma/client").$Enums.UserRole;
        password: string;
        roleId: string | null;
        homeAgencyId: string | null;
        scopeAgencyIds: string;
        permissions: string;
        lastLogin: Date | null;
        isSystemUser: boolean;
    }>;
}
