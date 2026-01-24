import { PrismaService } from '../prisma/prisma.service';
import { DevicesService } from '../devices/devices.service';
export declare class AgenciesService {
    private prisma;
    private devicesService;
    constructor(prisma: PrismaService, devicesService: DevicesService);
    findAll(): Promise<any[]>;
    findOne(id: string): Promise<any>;
    private mapAgency;
    private parseZipCodes;
    create(data: any): Promise<any>;
    update(id: string, data: any): Promise<any>;
    checkTerritoryAvailability(zipCode: string, excludeAgencyId?: string): Promise<{
        available: boolean;
        agencyId: string;
        agencyName: string;
    } | {
        available: boolean;
        agencyId?: undefined;
        agencyName?: undefined;
    }>;
    delete(id: string): Promise<{
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
    exportToCSV(): Promise<string>;
}
