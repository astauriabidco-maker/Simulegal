import { Controller, Get, Post, Patch, Delete, Body, Param, Query } from '@nestjs/common';
import { DevicesService } from './devices.service';

@Controller('devices')
export class DevicesController {
    constructor(private readonly devicesService: DevicesService) { }

    /**
     * GET /devices - List all devices
     */
    @Get()
    async findAll(@Query('agencyId') agencyId?: string) {
        if (agencyId) {
            return this.devicesService.findByAgency(agencyId);
        }
        return this.devicesService.findAll();
    }

    /**
     * GET /devices/:id - Get a single device
     */
    @Get(':id')
    async findOne(@Param('id') id: string) {
        return this.devicesService.findById(id);
    }

    /**
     * POST /devices/activate - Activate a device with code (called by tablet)
     */
    @Post('activate')
    async activate(@Body() body: { pairingCode: string }) {
        const device = await this.devicesService.activate(body.pairingCode);
        if (!device) {
            return { success: false, error: 'Code invalide' };
        }
        return { success: true, device };
    }

    /**
     * POST /devices/register - Register a new device (called by tablet)
     */
    @Post('register')
    async register() {
        return this.devicesService.register();
    }

    /**
     * POST /devices/pair - Pair a device to an agency (called by admin)
     */
    @Post('pair')
    async pair(@Body() body: { pairingCode: string; agencyId: string; name?: string }) {
        const device = await this.devicesService.pair(body.pairingCode, body.agencyId, body.name);
        if (!device) {
            return { success: false, error: 'Code d\'appairage invalide' };
        }
        return { success: true, device };
    }

    /**
     * PATCH /devices/:id/heartbeat - Update device heartbeat
     */
    @Patch(':id/heartbeat')
    async heartbeat(@Param('id') id: string) {
        const device = await this.devicesService.heartbeat(id);
        if (!device) {
            return { success: false, error: 'Device not found' };
        }
        return { success: true };
    }

    /**
     * PATCH /devices/:id/reset - Reset device for re-pairing
     */
    @Patch(':id/reset')
    async reset(@Param('id') id: string) {
        return this.devicesService.reset(id);
    }

    /**
     * DELETE /devices/:id - Remove a device
     */
    @Delete(':id')
    async remove(@Param('id') id: string) {
        return this.devicesService.remove(id);
    }
}
