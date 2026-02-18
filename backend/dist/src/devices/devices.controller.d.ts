import { DevicesService } from './devices.service';
export declare class DevicesController {
    private readonly devicesService;
    constructor(devicesService: DevicesService);
    findAll(agencyId?: string): Promise<({
        assignedAgency: {
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
            iban: string | null;
            bic: string | null;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        } | null;
    } & {
        id: string;
        name: string;
        status: string;
        createdAt: Date;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    })[]>;
    findOne(id: string): Promise<({
        assignedAgency: {
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
            iban: string | null;
            bic: string | null;
            kioskUrl: string;
            createdAt: Date;
            updatedAt: Date;
        } | null;
    } & {
        id: string;
        name: string;
        status: string;
        createdAt: Date;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }) | null>;
    activate(body: {
        code: string;
    }): Promise<{
        success: boolean;
        error: string;
        device?: undefined;
    } | {
        success: boolean;
        device: {
            assignedAgency: {
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
                iban: string | null;
                bic: string | null;
                kioskUrl: string;
                createdAt: Date;
                updatedAt: Date;
            } | null;
        } & {
            id: string;
            name: string;
            status: string;
            createdAt: Date;
            pairingCode: string;
            appVersion: string;
            lastHeartbeat: Date;
            assignedAgencyId: string | null;
        };
        error?: undefined;
    }>;
    register(): Promise<{
        id: string;
        name: string;
        status: string;
        createdAt: Date;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    pair(body: {
        pairingCode: string;
        agencyId: string;
        name?: string;
    }): Promise<{
        success: boolean;
        error: string;
        device?: undefined;
    } | {
        success: boolean;
        device: {
            assignedAgency: {
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
                iban: string | null;
                bic: string | null;
                kioskUrl: string;
                createdAt: Date;
                updatedAt: Date;
            } | null;
        } & {
            id: string;
            name: string;
            status: string;
            createdAt: Date;
            pairingCode: string;
            appVersion: string;
            lastHeartbeat: Date;
            assignedAgencyId: string | null;
        };
        error?: undefined;
    }>;
    heartbeat(id: string): Promise<{
        success: boolean;
        error: string;
    } | {
        success: boolean;
        error?: undefined;
    }>;
    reset(id: string): Promise<{
        id: string;
        name: string;
        status: string;
        createdAt: Date;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    remove(id: string): Promise<{
        id: string;
        name: string;
        status: string;
        createdAt: Date;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
}
