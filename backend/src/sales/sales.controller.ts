import { Controller, Get, Post, Body, Patch, Param, UseGuards, Request, Query, UseInterceptors, UploadedFile } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { SalesService } from './sales.service';
import { AuthGuard } from '@nestjs/passport';

import { SalesAnalyticsService } from './sales-analytics.service';

@Controller('sales')
@UseGuards(AuthGuard('jwt'))
export class SalesController {
    constructor(
        private readonly salesService: SalesService,
        private readonly analyticsService: SalesAnalyticsService
    ) { }

    @Get('analytics')
    async getAnalytics(@Query('period') period?: 'TODAY' | 'WEEK' | 'MONTH') {
        return this.analyticsService.getDashboardStats(period);
    }

    @Get('prospects')
    async findAll(
        @Query('page') page: number = 1,
        @Query('limit') limit: number = 50,
        @Query('status') status?: string
    ) {
        return this.salesService.findAll({
            page: Number(page),
            limit: Number(limit),
            status
        });
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

    @Post('import')
    @UseInterceptors(FileInterceptor('file'))
    async importProspects(@UploadedFile() file: any) {
        return this.salesService.importFromCSV(file.buffer);
    }
}
