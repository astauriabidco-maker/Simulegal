import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Absence, Prisma } from '@prisma/client';

@Injectable()
export class AbsencesService {
    constructor(private prisma: PrismaService) { }

    async findAll(params: {
        where?: Prisma.AbsenceWhereInput;
        orderBy?: Prisma.AbsenceOrderByWithRelationInput;
    }): Promise<Absence[]> {
        return this.prisma.absence.findMany({
            ...params,
            include: { user: { select: { name: true } } }
        });
    }

    async findOne(id: string): Promise<Absence | null> {
        return this.prisma.absence.findUnique({
            where: { id }
        });
    }

    async create(data: Prisma.AbsenceCreateInput): Promise<Absence> {
        return this.prisma.absence.create({
            data,
        });
    }

    async remove(id: string): Promise<Absence> {
        return this.prisma.absence.delete({
            where: { id }
        });
    }
}
