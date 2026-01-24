import { PrismaService } from '../prisma/prisma.service';
import { AgenciesService } from '../agencies/agencies.service';
import { UsersService } from '../users/users.service';
import { DevicesService } from '../devices/devices.service';
import { NotificationsService } from '../notifications/notifications.service';
export declare class FranchiseLeadsService {
    private prisma;
    private agenciesService;
    private usersService;
    private devicesService;
    private notificationsService;
    constructor(prisma: PrismaService, agenciesService: AgenciesService, usersService: UsersService, devicesService: DevicesService, notificationsService: NotificationsService);
    findAll(): Promise<any[]>;
    findOne(id: string): Promise<any>;
    private mapLead;
    create(data: any): Promise<any>;
    update(id: string, data: any): Promise<any>;
    signContract(id: string): Promise<{
        lead: {
            id: string;
            name: string;
            status: import(".prisma/client").$Enums.FranchiseLeadStatus;
            region: string;
            createdAt: Date;
            updatedAt: Date;
            email: string;
            phone: string;
            documents: string;
            targetCity: string;
            companyName: string | null;
            siret: string | null;
            legalForm: string | null;
            contractDetails: string;
            contractHistory: string;
            convertedAgencyId: string | null;
            rejectionReason: string | null;
        };
        agency: any;
        user: any;
    }>;
    updateDocuments(id: string, documents: any[]): Promise<any>;
    logContractHistory(id: string, version: any): Promise<any>;
    generateContract(id: string): Promise<Buffer>;
    addNote(id: string, content: string, author: string, type?: 'NOTE' | 'CALL' | 'EMAIL'): Promise<{
        id: string;
        type: string;
        createdAt: Date;
        content: string;
        author: string;
        leadId: string;
    }>;
    getAnalytics(): Promise<{
        total: number;
        statusCounts: Record<string, number>;
        regionCounts: Record<string, number>;
        conversionRate: number;
        monthlyTrend: {
            month: string;
            count: number;
            signed: number;
        }[];
    }>;
    exportToCSV(filters?: {
        region?: string;
        status?: string;
    }): Promise<string>;
}
