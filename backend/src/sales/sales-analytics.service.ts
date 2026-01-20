import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class SalesAnalyticsService {
    constructor(private prisma: PrismaService) { }

    async getDashboardStats(period: 'TODAY' | 'WEEK' | 'MONTH' = 'MONTH') {
        const now = new Date();
        let startDate = new Date();

        switch (period) {
            case 'TODAY':
                startDate.setHours(0, 0, 0, 0);
                break;
            case 'WEEK':
                startDate.setDate(now.getDate() - 7);
                break;
            case 'MONTH':
                startDate.setMonth(now.getMonth() - 1);
                break;
        }

        // 1. Basic Counts
        const [totalLeads, convertedLeads, newLeads] = await Promise.all([
            this.prisma.prospect.count(),
            this.prisma.prospect.count({ where: { status: 'CONVERTED' } }),
            this.prisma.prospect.count({ where: { createdAt: { gte: startDate } } })
        ]);

        // 2. Funnel (Group by Status)
        const prospectsByStatus = await this.prisma.prospect.groupBy({
            by: ['status'],
            _count: { status: true },
        });

        // 3. Acquisition (Group by Source)
        const prospectsBySource = await this.prisma.prospect.groupBy({
            by: ['source'],
            _count: { source: true },
            where: { createdAt: { gte: startDate } } // Only recent sources
        });

        // 4. Conversion Rate
        const conversionRate = totalLeads > 0 ? (convertedLeads / totalLeads) * 100 : 0;

        return {
            period,
            kpis: {
                totalLeads,
                newLeads,
                convertedLeads,
                conversionRate: Math.round(conversionRate * 10) / 10,
                pipelineValue: this.estimatePipelineValue(prospectsByStatus)
            },
            funnel: prospectsByStatus.map(p => ({ status: p.status, count: p._count.status })),
            sources: prospectsBySource.map(s => ({ source: s.source, count: s._count.source }))
        };
    }

    private estimatePipelineValue(groupedStatus: any[]): number {
        // Simple estimation: 1 Client = 1500€ (avg basket)
        // Probabilities: TO_CALL (5%), MEETING (30%), CONVERTED (100%)
        let total = 0;
        groupedStatus.forEach(group => {
            const count = group._count.status;
            switch (group.status) {
                case 'TO_CALL': total += count * 1500 * 0.05; break;
                case 'IN_DISCUSSION': total += count * 1500 * 0.15; break;
                case 'MEETING_BOOKED': total += count * 1500 * 0.30; break;
                case 'LINK_SENT': total += count * 1500 * 0.60; break;
                case 'CONVERTED': total += count * 1500 * 1.0; break;
            }
        });
        return Math.round(total);
    }
}
