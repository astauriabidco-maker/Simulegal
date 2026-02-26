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

    async exportToCSV(): Promise<string> {
        const agencies = await this.prisma.agency.findMany({
            include: { _count: { select: { leads: true, users: true } } },
            orderBy: { name: 'asc' }
        });
        const headers = ['ID', 'Nom', 'Type', 'Statut', 'Email', 'Région', 'Codes Postaux', 'Commission %', 'Nb Leads', 'Nb Users'];
        const rows = agencies.map(a => [
            a.id, a.name, a.type, a.status, a.contactEmail || '', a.region || '',
            a.zipCodes || '', a.commissionRate?.toString() || '0',
            a._count.leads.toString(), a._count.users.toString()
        ].map(v => `"${(v || '').toString().replace(/"/g, '""')}"`).join(';'));
        return [headers.join(';'), ...rows].join('\n');
    }

    // ── NETWORK ANALYTICS ──
    async getNetworkAnalytics() {
        const agencies = await this.prisma.agency.findMany({
            include: { _count: { select: { leads: true, users: true } }, leads: { select: { amountPaid: true, status: true } } }
        });
        const active = agencies.filter(a => a.status === 'ACTIVE');
        const typeCounts: Record<string, number> = {};
        const regionCounts: Record<string, number> = {};
        let totalLeads = 0, totalRevenue = 0, totalSigned = 0;

        for (const a of active) {
            typeCounts[a.type] = (typeCounts[a.type] || 0) + 1;
            regionCounts[a.region] = (regionCounts[a.region] || 0) + 1;
            totalLeads += a._count.leads;
            totalRevenue += a.leads.reduce((sum, l) => sum + l.amountPaid, 0);
            totalSigned += a.leads.filter(l => l.status === 'SIGNED' || l.status === 'COMPLETED').length;
        }

        const avgLeadsPerAgency = active.length > 0 ? Math.round(totalLeads / active.length) : 0;
        const conversionRate = totalLeads > 0 ? Math.round((totalSigned / totalLeads) * 100) : 0;

        // Top performers
        const ranked = active.map(a => ({
            id: a.id, name: a.name, type: a.type, region: a.region,
            leads: a._count.leads,
            revenue: a.leads.reduce((s, l) => s + l.amountPaid, 0),
            signed: a.leads.filter(l => l.status === 'SIGNED' || l.status === 'COMPLETED').length,
            healthScore: this.computeHealthScore(a),
        })).sort((a, b) => b.revenue - a.revenue);

        return {
            totalAgencies: agencies.length,
            activeAgencies: active.length,
            typeCounts, regionCounts,
            totalLeads, totalRevenue: totalRevenue / 100, totalSigned,
            avgLeadsPerAgency, conversionRate,
            topPerformers: ranked.slice(0, 5),
            lowPerformers: ranked.filter(a => a.healthScore < 40).slice(0, 5),
        };
    }

    // ── INDIVIDUAL AGENCY PERFORMANCE ──
    async getAgencyPerformance(id: string) {
        const agency = await this.prisma.agency.findUnique({
            where: { id },
            include: { leads: true, users: true, _count: { select: { leads: true, users: true, appointments: true } } }
        });
        if (!agency) return null;

        const leads = agency.leads || [];
        const totalRevenue = leads.reduce((s, l) => s + l.amountPaid, 0);
        const signed = leads.filter(l => l.status === 'SIGNED' || l.status === 'COMPLETED').length;
        const conversion = leads.length > 0 ? Math.round((signed / leads.length) * 100) : 0;

        // Monthly trend (last 6 months)
        const monthlyTrend: { month: string; leads: number; revenue: number }[] = [];
        const now = new Date();
        for (let i = 5; i >= 0; i--) {
            const start = new Date(now.getFullYear(), now.getMonth() - i, 1);
            const end = new Date(now.getFullYear(), now.getMonth() - i + 1, 0);
            const label = start.toLocaleDateString('fr-FR', { month: 'short', year: '2-digit' });
            const monthLeads = leads.filter(l => { const d = new Date(l.createdAt); return d >= start && d <= end; });
            monthlyTrend.push({ month: label, leads: monthLeads.length, revenue: monthLeads.reduce((s, l) => s + l.amountPaid, 0) / 100 });
        }

        // Status breakdown
        const statusBreakdown: Record<string, number> = {};
        leads.forEach(l => { statusBreakdown[l.status] = (statusBreakdown[l.status] || 0) + 1; });

        return {
            ...this.mapAgency(agency),
            stats: {
                totalLeads: leads.length, totalRevenue: totalRevenue / 100,
                signed, conversion, usersCount: agency._count.users,
                appointmentsCount: agency._count.appointments,
                healthScore: this.computeHealthScore(agency),
                monthlyTrend, statusBreakdown,
            }
        };
    }

    // ── HEALTH SCORE ──
    computeHealthScore(agency: any): number {
        let score = 0;
        const leads = agency.leads || [];
        const count = agency._count?.leads || leads.length;

        // Active status (10pts)
        if (agency.status === 'ACTIVE') score += 10;
        // Has email (5pts)
        if (agency.contactEmail) score += 5;
        // Has leads (max 25pts)
        if (count >= 20) score += 25;
        else if (count >= 10) score += 20;
        else if (count >= 5) score += 15;
        else if (count >= 1) score += 10;
        // Revenue (max 25pts)
        const revenue = leads.reduce((s: number, l: any) => s + (l.amountPaid || 0), 0);
        if (revenue >= 500000) score += 25;
        else if (revenue >= 100000) score += 20;
        else if (revenue >= 50000) score += 15;
        else if (revenue > 0) score += 10;
        // Conversion (max 20pts)
        const signed = leads.filter((l: any) => l.status === 'SIGNED' || l.status === 'COMPLETED').length;
        const convRate = count > 0 ? signed / count : 0;
        score += Math.round(convRate * 20);
        // Recent activity (15pts)
        const recentLeads = leads.filter((l: any) => (Date.now() - new Date(l.createdAt).getTime()) < 30 * 24 * 60 * 60 * 1000).length;
        if (recentLeads >= 3) score += 15;
        else if (recentLeads >= 1) score += 10;

        return Math.min(100, score);
    }

    // ── MAP DATA ──
    async getMapData() {
        const agencies = await this.prisma.agency.findMany({
            include: { _count: { select: { leads: true } } }
        });
        const cityCoords: Record<string, [number, number]> = {
            'Paris': [48.856, 2.352], 'Lyon': [45.764, 4.836], 'Marseille': [43.296, 5.370],
            'Toulouse': [43.605, 1.444], 'Nice': [43.710, 7.262], 'Nantes': [47.218, -1.554],
            'Strasbourg': [48.573, 7.752], 'Montpellier': [43.611, 3.877], 'Bordeaux': [44.838, -0.579],
            'Lille': [50.629, 3.057], 'Rennes': [48.117, -1.678], 'Grenoble': [45.189, 5.725],
        };
        const regionCoords: Record<string, [number, number]> = {
            'IDF': [48.85, 2.35], 'AURA': [45.75, 4.85], 'PACA': [43.50, 5.90],
            'OCC': [43.60, 2.50], 'NAQ': [45.00, 0.00], 'HDF': [49.90, 2.80],
            'GES': [48.60, 6.50], 'BRE': [48.20, -2.90], 'NOR': [49.10, 0.20],
            'PDL': [47.40, -1.00], 'BFC': [47.00, 5.00], 'CVL': [47.50, 1.50],
        };
        return agencies.map(a => {
            const cityKey = Object.keys(cityCoords).find(c => a.city.toLowerCase().includes(c.toLowerCase()));
            const coords = cityKey ? cityCoords[cityKey] : regionCoords[a.region] || [46.5, 2.5];
            return {
                id: a.id, name: a.name, city: a.city, region: a.region,
                type: a.type, status: a.status,
                lat: coords[0], lng: coords[1],
                leadsCount: a._count.leads, commissionRate: a.commissionRate,
                zipCodes: this.parseZipCodes(a.zipCodes),
            };
        });
    }
}
