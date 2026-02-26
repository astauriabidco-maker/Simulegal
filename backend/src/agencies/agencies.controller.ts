import { Controller, Get, Post, Body, Patch, Param, Delete, Res, UseGuards } from '@nestjs/common';
import type { Response } from 'express';
import { AgenciesService } from './agencies.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('agencies')
@UseGuards(AuthGuard('jwt'))
export class AgenciesController {
    constructor(private readonly agenciesService: AgenciesService) { }

    // Non-param routes first
    @Get('export/csv')
    async exportCSV(@Res() res: Response) {
        const csv = await this.agenciesService.exportToCSV();
        res.set({ 'Content-Type': 'text/csv; charset=utf-8', 'Content-Disposition': 'attachment; filename=agencies.csv' });
        res.send('\uFEFF' + csv);
    }

    @Get('analytics/network')
    getNetworkAnalytics() {
        return this.agenciesService.getNetworkAnalytics();
    }

    @Get('analytics/map')
    getMapData() {
        return this.agenciesService.getMapData();
    }

    @Get('check-availability/:zipCode')
    checkAvailability(@Param('zipCode') zipCode: string) {
        return this.agenciesService.checkTerritoryAvailability(zipCode);
    }

    @Get()
    findAll() {
        return this.agenciesService.findAll();
    }

    @Get(':id')
    findOne(@Param('id') id: string) {
        return this.agenciesService.findOne(id);
    }

    @Get(':id/performance')
    getPerformance(@Param('id') id: string) {
        return this.agenciesService.getAgencyPerformance(id);
    }

    @Post()
    create(@Body() data: any) {
        return this.agenciesService.create(data);
    }

    @Patch(':id')
    update(@Param('id') id: string, @Body() data: any) {
        return this.agenciesService.update(id, data);
    }

    @Delete(':id')
    delete(@Param('id') id: string) {
        return this.agenciesService.delete(id);
    }
}
