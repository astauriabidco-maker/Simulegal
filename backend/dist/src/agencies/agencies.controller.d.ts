import { AgenciesService } from './agencies.service';
export declare class AgenciesController {
    private readonly agenciesService;
    constructor(agenciesService: AgenciesService);
    findAll(): Promise<({
        _count: {
            leads: number;
        };
    } & {
        id: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        zipCodes: string;
        commissionRate: number;
        contactEmail: string;
        kioskUrl: string;
    })[]>;
    findOne(id: string): Promise<({
        users: {
            agencyId: string | null;
            role: import(".prisma/client").$Enums.UserRole;
            id: string;
            email: string;
            password: string;
            name: string;
            roleId: string | null;
            homeAgencyId: string | null;
            scopeAgencyIds: string;
            permissions: string;
            createdAt: Date;
            updatedAt: Date;
            lastLogin: Date | null;
        }[];
        leads: {
            id: string;
            email: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            status: import(".prisma/client").$Enums.LeadStatus;
            phone: string;
            serviceId: string;
            serviceName: string;
            amountPaid: number;
            originAgencyId: string | null;
            contract: string | null;
            documents: string;
            requiredDocs: string | null;
        }[];
    } & {
        id: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        zipCodes: string;
        commissionRate: number;
        contactEmail: string;
        kioskUrl: string;
    }) | null>;
    create(data: any): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        zipCodes: string;
        commissionRate: number;
        contactEmail: string;
        kioskUrl: string;
    }>;
    update(id: string, data: any): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        zipCodes: string;
        commissionRate: number;
        contactEmail: string;
        kioskUrl: string;
    }>;
}
