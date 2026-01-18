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
    }): Promise<Appointment[]>;
    create(data: Prisma.AppointmentCreateInput): Promise<Appointment>;
    getAvailableSlots(dateStr: string, agencyId?: string): Promise<string[]>;
}
