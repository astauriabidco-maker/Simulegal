import { PrismaService } from '../prisma/prisma.service';
export declare class DevicesService {
    private prisma;
    constructor(prisma: PrismaService);
    private generatePairingCode;
    register(): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    createProvisioned(agencyId: string, agencyName: string): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    activate(code: string): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }) | null>;
    findAll(): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    })[]>;
    findById(id: string): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }) | null>;
    findByCode(code: string): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }) | null>;
    pair(pairingCode: string, agencyId: string, name?: string): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }) | null>;
    heartbeat(id: string): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    } | null>;
    reset(id: string): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    remove(id: string): Promise<{
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    }>;
    findByAgency(agencyId: string): Promise<({
        assignedAgency: {
            id: string;
            name: string;
            createdAt: Date;
            updatedAt: Date;
            type: import(".prisma/client").$Enums.AgencyType;
            status: import(".prisma/client").$Enums.AgencyStatus;
            region: string;
            city: string;
            zipCodes: string;
            commissionRate: number;
            contactEmail: string;
            kioskUrl: string;
        } | null;
    } & {
        id: string;
        name: string;
        createdAt: Date;
        status: string;
        pairingCode: string;
        appVersion: string;
        lastHeartbeat: Date;
        assignedAgencyId: string | null;
    })[]>;
}
