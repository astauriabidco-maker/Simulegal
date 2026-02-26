import { Controller, Get, Post, Patch, Delete, Body, Param, Query } from '@nestjs/common';
import { DevicesService } from './devices.service';

@Controller('devices')
export class DevicesController {
    constructor(private readonly devicesService: DevicesService) { }

    // Non-param routes first
    @Get('stats/fleet')
    getFleetStats() {
        return this.devicesService.getFleetStats();
    }

    @Post('activate')
    async activate(@Body() body: { code: string }) {
        const device = await this.devicesService.activate(body.code);
        if (!device) return { success: false, error: 'Code invalide' };
        return { success: true, device };
    }

    @Post('register')
    async register() {
        return this.devicesService.register();
    }

    @Post('pair')
    async pair(@Body() body: { pairingCode: string; agencyId: string; name?: string }) {
        const device = await this.devicesService.pair(body.pairingCode, body.agencyId, body.name);
        if (!device) return { success: false, error: 'Code d\'appairage invalide' };
        return { success: true, device };
    }

    @Get()
    async findAll(@Query('agencyId') agencyId?: string) {
        if (agencyId) return this.devicesService.findByAgency(agencyId);
        return this.devicesService.findAll();
    }

    @Get(':id')
    async findOne(@Param('id') id: string) {
        return this.devicesService.findById(id);
    }

    @Patch(':id/heartbeat')
    async heartbeat(@Param('id') id: string) {
        const device = await this.devicesService.heartbeat(id);
        if (!device) return { success: false, error: 'Device not found' };
        return { success: true };
    }

    @Patch(':id/reset')
    async reset(@Param('id') id: string) {
        return this.devicesService.reset(id);
    }

    @Delete(':id')
    async remove(@Param('id') id: string) {
        return this.devicesService.remove(id);
    }
}
