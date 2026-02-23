import { Controller, Get, Post, Body, Patch, Param, UseGuards, Request, Query, UseInterceptors, UploadedFile } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { SalesService } from './sales.service';
import { AuthGuard } from '@nestjs/passport';

import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';
import { ProspectPipelineService } from './prospect-pipeline.service';

@Controller('sales')
@UseGuards(AuthGuard('jwt'))
export class SalesController {
    constructor(
        private readonly salesService: SalesService,
        private readonly analyticsService: SalesAnalyticsService,
        private readonly assignmentService: AssignmentService,
        private readonly prospectPipeline: ProspectPipelineService,
    ) { }

    @Get('analytics')
    async getAnalytics(@Query('period') period?: 'TODAY' | 'WEEK' | 'MONTH') {
        return this.analyticsService.getDashboardStats(period);
    }

    @Get('prospects')
    async findAll(
        @Query('page') page: number = 1,
        @Query('limit') limit: number = 50,
        @Query('status') status?: string,
        @Query('agencyId') agencyId?: string,
        @Query('source') source?: string,
        @Query('dateFrom') dateFrom?: string,
        @Query('dateTo') dateTo?: string
    ) {
        return this.salesService.findAll({
            page: Number(page),
            limit: Number(limit),
            status,
            agencyId,
            source,
            dateFrom,
            dateTo
        });
    }

    @Get('export')
    async exportProspects(
        @Query('agencyId') agencyId?: string,
        @Query('source') source?: string,
        @Query('dateFrom') dateFrom?: string,
        @Query('dateTo') dateTo?: string
    ) {
        const csv = await this.salesService.exportToCSV({ agencyId, source, dateFrom, dateTo });
        return {
            filename: `prospects_export_${new Date().toISOString().split('T')[0]}.csv`,
            content: csv,
            contentType: 'text/csv'
        };
    }

    @Get('prospects/:id')
    findOne(@Param('id') id: string) {
        return this.salesService.findOne(id);
    }

    @Post('prospects')
    create(@Body() data: any) {
        return this.salesService.create(data);
    }

    @Patch('prospects/:id')
    update(@Param('id') id: string, @Body() data: any) {
        return this.salesService.update(id, data);
    }

    @Post('prospects/:id/notes')
    addNote(
        @Param('id') id: string,
        @Request() req: any,
        @Body() data: { text: string }
    ) {
        return this.salesService.addNote(id, req.user.id, data.text);
    }

    @Post('prospects/:id/reassign')
    async reassignProspect(
        @Param('id') id: string,
        @Body() data: { salesUserId?: string }
    ) {
        const prospect = await this.salesService.findOne(id);
        if (!prospect) {
            return { success: false, error: 'Prospect not found' };
        }

        let newSalesId: string | null | undefined = data.salesUserId;
        if (!newSalesId) {
            newSalesId = await this.assignmentService.getNextSalesAgent(prospect.agencyId);
        }

        if (!newSalesId) {
            return { success: false, error: 'No sales agents available' };
        }

        const updated = await this.salesService.update(id, { assignedToSalesId: newSalesId });
        return { success: true, assignedTo: newSalesId, prospect: updated };
    }

    @Post('import')
    @UseInterceptors(FileInterceptor('file'))
    async importProspects(@UploadedFile() file: any) {
        return this.salesService.importFromCSV(file.buffer);
    }

    // ═══════════════════════════════════════════════
    // PIPELINE AUTOMATION ENDPOINTS
    // ═══════════════════════════════════════════════

    @Get('pipeline/stats')
    async getPipelineStats() {
        return this.prospectPipeline.getAutomationStats();
    }

    @Get('prospects/:id/transitions')
    async getTransitions(@Param('id') id: string) {
        const logs = await this.prospectPipeline.getTransitionLogs(id);
        return { transitions: logs, count: logs.length };
    }

    @Post('pipeline/force-check')
    async forceCheck() {
        const result = await this.prospectPipeline.forceCheck();
        return { success: true, ...result };
    }
}
