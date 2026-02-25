import { Controller, Get, Post, Delete, Body, Patch, Param, UseGuards, Request, Query, UseInterceptors, UploadedFile, Res } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { SalesService } from './sales.service';
import { AuthGuard } from '@nestjs/passport';


import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';
import { ProspectPipelineService } from './prospect-pipeline.service';
import { QuotePdfService } from './quote-pdf.service';

@Controller('sales')
@UseGuards(AuthGuard('jwt'))
export class SalesController {
    constructor(
        private readonly salesService: SalesService,
        private readonly analyticsService: SalesAnalyticsService,
        private readonly assignmentService: AssignmentService,
        private readonly prospectPipeline: ProspectPipelineService,
        private readonly quotePdfService: QuotePdfService,
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
        @Query('dateTo') dateTo?: string,
        @Query('search') search?: string,
        @Query('tags') tags?: string
    ) {
        return this.salesService.findAll({
            page: Number(page),
            limit: Number(limit),
            status,
            agencyId,
            source,
            dateFrom,
            dateTo,
            search,
            tags,
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

    // ═══════════════════════════════════════════════
    // SUPPRESSION
    // ═══════════════════════════════════════════════

    @Delete('prospects/:id')
    async deleteProspect(@Param('id') id: string) {
        const result = await this.salesService.delete(id);
        if (!result) {
            return { success: false, error: 'Prospect not found' };
        }
        return { success: true, ...result };
    }

    // ═══════════════════════════════════════════════
    // ACTIONS EN MASSE (BULK)
    // ═══════════════════════════════════════════════

    @Post('bulk/update')
    async bulkUpdate(@Body() data: { ids: string[]; updates: { status?: string; assignedToSalesId?: string; tags?: string[] } }) {
        return this.salesService.bulkUpdate(data.ids, data.updates);
    }

    @Post('bulk/delete')
    async bulkDelete(@Body() data: { ids: string[] }) {
        return this.salesService.bulkDelete(data.ids);
    }

    // ═══════════════════════════════════════════════
    // APPOINTMENTS
    // ═══════════════════════════════════════════════

    @Post('prospects/:id/book-appointment')
    async bookAppointment(
        @Param('id') id: string,
        @Body() data: {
            date: string;
            agencyId: string;
            agencyName: string;
            serviceId?: string;
            confirmed?: boolean;
            confirmationSentVia?: string;
        }
    ) {
        const result = await this.salesService.bookAppointment(id, data);
        if (!result) {
            return { success: false, error: 'Prospect not found' };
        }
        return { success: true, ...result };
    }

    @Post('prospects/:id/cancel-appointment')
    async cancelAppointment(
        @Param('id') id: string,
        @Body() data: { reason?: string }
    ) {
        const result = await this.salesService.cancelAppointment(id, data.reason);
        if (!result) {
            return { success: false, error: 'Prospect not found' };
        }
        return { success: true, prospect: result };
    }

    @Post('prospects/:id/reschedule-appointment')
    async rescheduleAppointment(
        @Param('id') id: string,
        @Body() data: {
            date: string;
            agencyId: string;
            agencyName: string;
            serviceId?: string;
        }
    ) {
        try {
            const result = await this.salesService.rescheduleAppointment(id, data);
            if (!result) {
                return { success: false, error: 'Prospect not found or slot unavailable' };
            }
            return { success: true, ...result };
        } catch (err: any) {
            return { success: false, error: err.message };
        }
    }

    @Get('appointments')
    async getAppointments(
        @Query('agencyId') agencyId?: string,
        @Query('status') status?: string,
        @Query('dateFrom') dateFrom?: string,
        @Query('dateTo') dateTo?: string
    ) {
        return this.salesService.getAppointments({ agencyId, status, dateFrom, dateTo });
    }

    // ═══════════════════════════════════════════════
    // RELANCES PROGRAMMÉES
    // ═══════════════════════════════════════════════

    @Post('prospects/:id/schedule-followup')
    async scheduleFollowUp(
        @Param('id') id: string,
        @Request() req: any,
        @Body() data: { scheduledAt: string; reason?: string }
    ) {
        const userId = req.user?.userId || req.user?.id || 'system';
        const result = await this.salesService.scheduleFollowUp(id, userId, data.scheduledAt, data.reason);
        if (!result) {
            return { success: false, error: 'Prospect not found' };
        }
        return { success: true, prospect: result };
    }

    @Get('followups/due')
    async getDueFollowUps(@Query('agencyId') agencyId?: string) {
        return this.salesService.getDueFollowUps(agencyId);
    }

    // ═══════════════════════════════════════════════
    // HISTORIQUE / TIMELINE
    // ═══════════════════════════════════════════════

    @Get('prospects/:id/timeline')
    async getTimeline(@Param('id') id: string) {
        return this.salesService.getCommunicationTimeline(id);
    }

    // ═══════════════════════════════════════════════
    // NOTES & INTERACTIONS
    // ═══════════════════════════════════════════════

    @Post('prospects/:id/notes')
    addNote(
        @Param('id') id: string,
        @Request() req: any,
        @Body() data: { text: string }
    ) {
        return this.salesService.addNote(id, req.user?.userId || req.user?.id || 'system', data.text);
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

    @Get('pipeline/velocity')
    async getPipelineVelocity() {
        return this.salesService.getPipelineVelocity();
    }

    @Post('prospects/:id/reactivate')
    async reactivateProspect(@Param('id') id: string) {
        const result = await this.salesService.reactivateProspect(id);
        if (!result) {
            return { success: false, error: 'Prospect not found or not LOST' };
        }
        return { success: true, prospect: result };
    }

    @Post('prospects/:id/convert')
    async convertToLead(
        @Param('id') id: string,
        @Body() data: { serviceId?: string }
    ) {
        try {
            const result = await this.salesService.convertToLead(id, data.serviceId);
            if (!result) {
                return { success: false, error: 'Prospect not found' };
            }
            return { success: true, leadId: result.leadId, prospect: result.prospect };
        } catch (err: any) {
            return { success: false, error: err.message };
        }
    }

    // ═══════════════════════════════════════════════
    // GÉNÉRATION DE DEVIS PDF
    // ═══════════════════════════════════════════════

    @Post('prospects/:id/quote')
    async generateQuote(
        @Param('id') id: string,
        @Body() data: { serviceId?: string; serviceName?: string; priceEuros?: number; notes?: string },
        @Res() res: any
    ) {
        try {
            const { buffer, filename } = await this.quotePdfService.generateQuotePdf(id, data);
            res.set({
                'Content-Type': 'application/pdf',
                'Content-Disposition': `attachment; filename="${filename}"`,
                'Content-Length': buffer.length,
            });
            res.end(buffer);
        } catch (err: any) {
            res.status(400).json({ success: false, error: err.message });
        }
    }
}
