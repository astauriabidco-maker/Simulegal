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

    /**
     * Vérifie si un code postal est déjà couvert par une agence exclusive.
     * @param zipCode Le code postal à vérifier
     * @param excludeAgencyId Optionnel, ID d'agence à exclure (pour les mises à jour)
     */
    async checkTerritoryAvailability(zipCode: string, excludeAgencyId?: string) {
        const agencies = await this.prisma.agency.findMany({
            where: {
                status: 'ACTIVE',
                id: { not: excludeAgencyId }
            },
            select: { id: true, name: true, zipCodes: true }
        });

        for (const agency of agencies) {
            try {
                const codes = JSON.parse(agency.zipCodes || '[]');
                if (codes.includes(zipCode)) {
                    return { available: false, agencyId: agency.id, agencyName: agency.name };
                }
            } catch (e) {
                // Silently skip malformed JSON
                if (agency.zipCodes.split(',').includes(zipCode)) {
                    return { available: false, agencyId: agency.id, agencyName: agency.name };
                }
            }
        }

        return { available: true };
    }
}
