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
        const agencies = await this.prisma.agency.findMany({
            include: {
                _count: {
                    select: { leads: true }
                }
            }
        });
        return agencies.map(a => this.mapAgency(a));
    }

    async findOne(id: string) {
        const agency = await this.prisma.agency.findUnique({
            where: { id },
            include: { leads: true, users: true }
        });
        if (!agency) return null;
        return this.mapAgency(agency);
    }

    private mapAgency(agency: any) {
        return {
            ...agency,
            zipCodes: this.parseZipCodes(agency.zipCodes)
        };
    }

    private parseZipCodes(zipCodes: string | null): string[] {
        if (!zipCodes) return [];
        try {
            const parsed = JSON.parse(zipCodes);
            return Array.isArray(parsed) ? parsed : [];
        } catch {
            return zipCodes.split(',').map(s => s.trim()).filter(Boolean);
        }
    }

    async create(data: any) {
        const createData = { ...data };
        if (createData.zipCodes && Array.isArray(createData.zipCodes)) {
            createData.zipCodes = JSON.stringify(createData.zipCodes);
        } else if (typeof createData.zipCodes === 'string') {
            createData.zipCodes = JSON.stringify(createData.zipCodes.split(',').map((s: string) => s.trim()).filter(Boolean));
        }

        const agency = await this.prisma.agency.create({
            data: createData
        });

        // Auto-provision device for Corners
        if (data.type === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }

        return this.mapAgency(agency);
    }

    async update(id: string, data: any) {
        const updateData = { ...data };
        if (updateData.zipCodes && Array.isArray(updateData.zipCodes)) {
            updateData.zipCodes = JSON.stringify(updateData.zipCodes);
        } else if (typeof updateData.zipCodes === 'string') {
            updateData.zipCodes = JSON.stringify(updateData.zipCodes.split(',').map((s: string) => s.trim()).filter(Boolean));
        }

        const agency = await this.prisma.agency.update({
            where: { id },
            data: updateData
        });

        return this.mapAgency(agency);
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

    /**
     * Soft delete an agency (set status to INACTIVE)
     */
    async delete(id: string) {
        // Check if agency has active leads
        const agency = await this.prisma.agency.findUnique({
            where: { id },
            include: { _count: { select: { leads: true } } }
        });

        if (!agency) {
            throw new Error('Agency not found');
        }

        // Soft delete - set status to INACTIVE
        return this.prisma.agency.update({
            where: { id },
            data: { status: 'INACTIVE' }
        });
    }

    /**
     * Export agencies to CSV
     */
    async exportToCSV(): Promise<string> {
        const agencies = await this.prisma.agency.findMany({
            include: {
                _count: { select: { leads: true, users: true } }
            },
            orderBy: { name: 'asc' }
        });

        const headers = ['ID', 'Nom', 'Type', 'Statut', 'Email', 'Région', 'Codes Postaux', 'Commission %', 'Nb Leads', 'Nb Users'];
        const rows = agencies.map(a => [
            a.id,
            a.name,
            a.type,
            a.status,
            a.contactEmail || '',
            a.region || '',
            a.zipCodes || '',
            a.commissionRate?.toString() || '0',
            a._count.leads.toString(),
            a._count.users.toString()
        ].map(v => `"${(v || '').toString().replace(/"/g, '""')}"`).join(';'));

        return [headers.join(';'), ...rows].join('\n');
    }
}

