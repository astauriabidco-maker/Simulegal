import { Controller, Post, Get, Delete, Param, Body, UseGuards, Request } from '@nestjs/common';
import { AiReportingService } from './ai-reporting.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';

@Controller('ai-reporting')
@UseGuards(JwtAuthGuard, RolesGuard)
export class AiReportingController {
    constructor(private readonly aiReportingService: AiReportingService) { }

    @Post('query')
    @Roles('SUPER_ADMIN', 'SUPERADMIN', 'HQ_ADMIN', 'HQ', 'AGENCY_MANAGER', 'AGENCY')
    async query(@Body('prompt') prompt: string, @Request() req: any) {
        return this.aiReportingService.generateWidget(
            prompt,
            req.user.role,
            req.user.agencyId
        );
    }

    @Get('widgets')
    async getWidgets(@Request() req: any) {
        return this.aiReportingService.getUserWidgets(req.user.userId || req.user.id);
    }

    @Post('widgets')
    async saveWidget(@Body() widgetData: any, @Request() req: any) {
        return this.aiReportingService.saveWidget(req.user.userId || req.user.id, req.user.agencyId, widgetData);
    }

    @Delete('widgets/:id')
    async deleteWidget(@Param('id') id: string, @Request() req: any) {
        return this.aiReportingService.deleteWidget(id, req.user.userId || req.user.id);
    }
}
