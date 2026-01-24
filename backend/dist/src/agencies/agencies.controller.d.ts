import type { Response } from 'express';
import { AgenciesService } from './agencies.service';
export declare class AgenciesController {
    private readonly agenciesService;
    constructor(agenciesService: AgenciesService);
    findAll(): Promise<any[]>;
    exportCSV(res: Response): Promise<void>;
    checkAvailability(zipCode: string): Promise<{
        available: boolean;
        agencyId: string;
        agencyName: string;
    } | {
        available: boolean;
        agencyId?: undefined;
        agencyName?: undefined;
    }>;
    findOne(id: string): Promise<any>;
    create(data: any): Promise<any>;
    update(id: string, data: any): Promise<any>;
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
}
