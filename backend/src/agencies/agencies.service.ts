import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { DevicesService } from '../devices/devices.service';

@Injectable()
export class AgenciesService {
    constructor(
        private prisma: PrismaService,
        private devicesService: DevicesService
    ) { }

    async findAll() {
        return this.prisma.agency.findMany({
            include: {
                _count: {
                    select: { leads: true }
                }
            }
        });
    }

    async findOne(id: string) {
        return this.prisma.agency.findUnique({
            where: { id },
            include: { leads: true, users: true }
        });
    }

    async create(data: any) {
        const agency = await this.prisma.agency.create({
            data
        });

        // Auto-provision device for Corners
        if (data.type === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }

        return agency;
    }

    async update(id: string, data: any) {
        return this.prisma.agency.update({
            where: { id },
            data
        });
    }
}
