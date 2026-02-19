import { PrismaService } from '../prisma/prisma.service';
import { Appointment, Prisma } from '@prisma/client';
export declare class AppointmentsService {
    private prisma;
    constructor(prisma: PrismaService);
    findAll(params: {
        skip?: number;
        take?: number;
        cursor?: Prisma.AppointmentWhereUniqueInput;
        where?: Prisma.AppointmentWhereInput;
        orderBy?: Prisma.AppointmentOrderByWithRelationInput;
    }): Promise<any[]>;
    findLeadById(id: string): Promise<{
        id: string;
        name: string;
        status: import(".prisma/client").$Enums.LeadStatus;
        createdAt: Date;
        updatedAt: Date;
        email: string;
        data: string;
        phone: string;
        serviceId: string;
        serviceName: string;
        amountPaid: number;
        paymentMethod: string | null;
        paymentDate: Date | null;
        paymentRef: string | null;
        invoiceNumber: string | null;
        contract: string | null;
        documents: string;
        requiredDocs: string | null;
        originAgencyId: string | null;
        assignedUserId: string | null;
    } | null>;
    create(data: Prisma.AppointmentCreateInput): Promise<Appointment>;
    getAvailableSlots(dateStr: string, agencyId?: string, serviceId?: string): Promise<string[]>;
    update(id: string, data: {
        start?: Date;
        end?: Date;
        hostUserId?: string;
        agencyId?: string;
        status?: string;
        type?: string;
    }): Promise<Appointment>;
    getAgendaStats(start: Date, end: Date): Promise<any>;
    cancel(id: string, reason: string): Promise<Appointment>;
    findAvailableHost(slotIso: string, agencyId?: string, serviceId?: string): Promise<string | null>;
}
