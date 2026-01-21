import { RolesService } from './roles.service';
export declare class RolesController {
    private readonly rolesService;
    constructor(rolesService: RolesService);
    findAll(req: any): Promise<{
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
    create(req: any, data: {
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
    update(req: any, id: string, data: any): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }>;
    remove(req: any, id: string): Promise<{
        id: string;
        createdAt: Date;
        updatedAt: Date;
        permissions: string;
        label: string;
        description: string;
        isSystem: boolean;
    }>;
}
