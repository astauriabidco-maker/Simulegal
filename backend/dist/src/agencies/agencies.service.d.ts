import { PrismaService } from '../prisma/prisma.service';
import { DevicesService } from '../devices/devices.service';
export declare class AgenciesService {
    private prisma;
    private devicesService;
    constructor(prisma: PrismaService, devicesService: DevicesService);
    findAll(): Promise<({
        _count: {
            leads: number;
        };
    } & {
        id: string;
        name: string;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        city: string;
        zipCodes: string;
        commissionRate: number;
        serviceCommissionOverrides: string | null;
        contactEmail: string;
        kioskUrl: string;
        createdAt: Date;
        updatedAt: Date;
    })[]>;
    findOne(id: string): Promise<({
        users: {
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
        }[];
        leads: {
            id: string;
            name: string;
            status: import(".prisma/client").$Enums.LeadStatus;
            createdAt: Date;
            updatedAt: Date;
            email: string;
            phone: string;
            serviceId: string;
            serviceName: string;
            amountPaid: number;
            contract: string | null;
            documents: string;
            requiredDocs: string | null;
            originAgencyId: string | null;
            assignedUserId: string | null;
        }[];
    } & {
        id: string;
        name: string;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        city: string;
        zipCodes: string;
        commissionRate: number;
        serviceCommissionOverrides: string | null;
        contactEmail: string;
        kioskUrl: string;
        createdAt: Date;
        updatedAt: Date;
    }) | null>;
    create(data: any): Promise<{
        id: string;
        name: string;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        city: string;
        zipCodes: string;
        commissionRate: number;
        serviceCommissionOverrides: string | null;
        contactEmail: string;
        kioskUrl: string;
        createdAt: Date;
        updatedAt: Date;
    }>;
    update(id: string, data: any): Promise<{
        id: string;
        name: string;
        type: import(".prisma/client").$Enums.AgencyType;
        status: import(".prisma/client").$Enums.AgencyStatus;
        region: string;
        city: string;
        zipCodes: string;
        commissionRate: number;
        serviceCommissionOverrides: string | null;
        contactEmail: string;
        kioskUrl: string;
        createdAt: Date;
        updatedAt: Date;
    }>;
    checkTerritoryAvailability(zipCode: string, excludeAgencyId?: string): Promise<{
        available: boolean;
        agencyId: string;
        agencyName: string;
    } | {
        available: boolean;
        agencyId?: undefined;
        agencyName?: undefined;
    }>;
}
