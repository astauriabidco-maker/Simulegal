import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class DashboardService {
    constructor(private prisma: PrismaService) { }

    async getGlobalStats() {
        const [
            leads,
            franchiseLeads,
            devices,
            agencies
        ] = await Promise.all([
            this.prisma.lead.findMany({
                select: { amountPaid: true, status: true, updatedAt: true, serviceId: true, serviceName: true }
            }),
            this.prisma.franchiseLead.findMany({
                select: { status: true }
            }),
            this.prisma.device.findMany({
                select: { status: true }
            }),
            this.prisma.agency.findMany({
                select: {
                    id: true,
                    name: true,
                    type: true,
                    region: true,
                    city: true,
                    _count: { select: { leads: true } }
                }
            })
        ]);

        // Revenue & Leads
        const totalRevenue = leads.reduce((acc: number, l: any) => acc + (l.amountPaid || 0), 0);
        const signed = leads.filter((l: any) => ['SIGNED', 'PAID', 'DONE'].includes(l.status)).length;
        const pending = leads.filter((l: any) => !['SIGNED', 'PAID', 'CANCELLED', 'ARCHIVED', 'DONE'].includes(l.status)).length;

        const today = new Date().toISOString().split('T')[0];
        const signedToday = leads.filter((l: any) =>
            (['SIGNED', 'PAID'].includes(String(l.status))) &&
            l.updatedAt?.toISOString().startsWith(today)
        ).length;

        // Fleet
        const fleetHealth = devices.length ? Math.round((devices.filter((d: any) => d.status === 'ACTIVE').length / devices.length) * 100) : 0;

        // Franchise
        const franchisePipeline = franchiseLeads.length;

        // Service Distribution
        const serviceStats = leads.reduce((acc: Record<string, number>, l: any) => {
            acc[l.serviceId] = (acc[l.serviceId] || 0) + 1;
            return acc;
        }, {} as Record<string, number>);

        return {
            overview: {
                totalLeads: leads.length,
                totalRevenue,
                conversionRate: leads.length ? Math.round((signed / leads.length) * 100) : 0,
                pendingDossiers: pending,
                completedToday: signedToday,
                fleetHealth,
                franchisePipeline
            },
            network: agencies,
            serviceDistribution: Object.entries(serviceStats).map(([id, count]) => ({ id, count })),
            franchiseStats: {
                new: franchiseLeads.filter((l: any) => l.status === 'NEW').length,
                meeting: franchiseLeads.filter((l: any) => l.status === 'MEETING').length,
                signed: franchiseLeads.filter((l: any) => l.status === 'SIGNED').length
            }
        };
    }
}
