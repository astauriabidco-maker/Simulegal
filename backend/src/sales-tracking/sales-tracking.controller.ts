import { Controller, Get, Post, Patch, Body, Param, Query, UseGuards, Request } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { SalesTrackingService } from './sales-tracking.service';

/**
 * SalesTrackingController — API REST Suivi Commercial
 *
 * Préfixe : /sales-tracking
 * Toutes les routes sont protégées par JWT
 */
@Controller('sales-tracking')
@UseGuards(AuthGuard('jwt'))
export class SalesTrackingController {
    constructor(private readonly trackingService: SalesTrackingService) { }

    // ═══════════════════════════════════════════════
    // OBJECTIFS
    // ═══════════════════════════════════════════════

    @Post('objectives')
    async setObjective(@Body() data: {
        userId: string;
        agencyId?: string;
        period: string;
        periodType?: string;
        metric: string;
        targetValue: number;
    }) {
        return this.trackingService.setObjective(data);
    }

    @Post('objectives/bulk')
    async setBulkObjectives(@Body() data: {
        objectives: {
            userId: string;
            agencyId?: string;
            period: string;
            periodType?: string;
            metric: string;
            targetValue: number;
        }[];
    }) {
        return this.trackingService.setBulkObjectives(data.objectives);
    }

    @Get('objectives/:userId')
    async getObjectives(
        @Param('userId') userId: string,
        @Query('period') period?: string,
    ) {
        return this.trackingService.getObjectives(userId, period);
    }

    @Get('objectives/agency/:agencyId')
    async getAgencyObjectives(
        @Param('agencyId') agencyId: string,
        @Query('period') period?: string,
    ) {
        return this.trackingService.getAgencyObjectives(agencyId, period);
    }

    @Post('objectives/recalculate')
    async recalculateAllObjectives() {
        return this.trackingService.recalculateAllCurrentObjectives();
    }

    @Post('objectives/:id/recalculate')
    async recalculateObjective(@Param('id') id: string) {
        return this.trackingService.recalculateObjective(id);
    }

    // ═══════════════════════════════════════════════
    // ACTIVITÉS
    // ═══════════════════════════════════════════════

    @Post('activities')
    async logActivity(
        @Request() req: any,
        @Body() data: {
            activityType: string;
            prospectId?: string;
            prospectName?: string;
            outcome?: string;
            duration?: number;
            notes?: string;
            metadata?: any;
        }
    ) {
        return this.trackingService.logActivity({
            userId: req.user.userId,
            agencyId: req.user.agencyId,
            ...data,
        });
    }

    @Get('activities/:userId')
    async getActivities(
        @Param('userId') userId: string,
        @Query('dateFrom') dateFrom?: string,
        @Query('dateTo') dateTo?: string,
        @Query('activityType') activityType?: string,
        @Query('limit') limit?: string,
    ) {
        return this.trackingService.getActivities(userId, {
            dateFrom,
            dateTo,
            activityType,
            limit: limit ? parseInt(limit) : undefined,
        });
    }

    @Get('activities/:userId/daily')
    async getDailySummary(
        @Param('userId') userId: string,
        @Query('date') date?: string,
    ) {
        return this.trackingService.getDailySummary(userId, date);
    }

    // ═══════════════════════════════════════════════
    // COMMISSIONS
    // ═══════════════════════════════════════════════

    @Post('commissions')
    async createCommission(@Body() data: {
        userId: string;
        agencyId?: string;
        prospectId?: string;
        prospectName?: string;
        serviceId?: string;
        serviceName?: string;
        baseAmount: number;
        rate: number;
    }) {
        return this.trackingService.createCommission(data);
    }

    @Get('commissions/:userId')
    async getCommissions(
        @Param('userId') userId: string,
        @Query('period') period?: string,
        @Query('status') status?: string,
    ) {
        return this.trackingService.getCommissions(userId, { period, status });
    }

    @Patch('commissions/:id/status')
    async updateCommissionStatus(
        @Param('id') id: string,
        @Body() data: { status: 'APPROVED' | 'PAID' | 'CANCELLED' },
    ) {
        return this.trackingService.updateCommissionStatus(id, data.status);
    }

    @Get('commissions-summary')
    async getCommissionSummary(
        @Query('period') period?: string,
        @Query('agencyId') agencyId?: string,
    ) {
        return this.trackingService.getCommissionSummary({ period, agencyId });
    }

    // ═══════════════════════════════════════════════
    // LEADERBOARD
    // ═══════════════════════════════════════════════

    @Get('leaderboard')
    async getLeaderboard(
        @Query('period') period?: string,
        @Query('agencyId') agencyId?: string,
        @Query('metric') metric?: string,
    ) {
        return this.trackingService.getLeaderboard({ period, agencyId, metric });
    }

    // ═══════════════════════════════════════════════
    // PERFORMANCE INDIVIDUELLE
    // ═══════════════════════════════════════════════

    @Get('performance/me')
    async getMyPerformance(
        @Request() req: any,
        @Query('period') period?: string,
    ) {
        const userId = req.user.userId;
        return this.trackingService.getUserPerformance(userId, period);
    }

    @Get('performance/:userId')
    async getUserPerformance(
        @Param('userId') userId: string,
        @Query('period') period?: string,
    ) {
        return this.trackingService.getUserPerformance(userId, period);
    }
}
