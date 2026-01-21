import { PrismaService } from '../prisma/prisma.service';
import { Absence, Prisma } from '@prisma/client';
export declare class AbsencesService {
    private prisma;
    constructor(prisma: PrismaService);
    findAll(params: {
        where?: Prisma.AbsenceWhereInput;
        orderBy?: Prisma.AbsenceOrderByWithRelationInput;
    }): Promise<Absence[]>;
    findOne(id: string): Promise<Absence | null>;
    create(data: Prisma.AbsenceCreateInput): Promise<Absence>;
    remove(id: string): Promise<Absence>;
}
