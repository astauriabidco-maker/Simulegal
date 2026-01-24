import { UsersService } from './users.service';
export declare class UsersController {
    private readonly usersService;
    constructor(usersService: UsersService);
    findSystemUsers(req: any): Promise<any[]>;
    findAll(req: any): Promise<any[]>;
    findOne(id: string): Promise<any>;
    create(req: any, data: any): Promise<any>;
    update(req: any, id: string, data: any): Promise<any>;
    remove(req: any, id: string): Promise<{
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
