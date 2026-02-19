import { Controller, Get, Post, Body, Query, Param, UseGuards } from '@nestjs/common';
import { AppointmentsSlotsService } from './appointments-slots.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard'; // Assuming you have auth

@Controller('appointments-slots')
export class AppointmentsSlotsController {
    constructor(private readonly slotsService: AppointmentsSlotsService) { }

    @Get('available')
    async getAvailableSlots(
        @Query('start') start: string,
        @Query('end') end: string
    ) {
        return this.slotsService.findAvailableSlots(start, end);
    }

    // Admin/Jurist only
    // @UseGuards(JwtAuthGuard)
    @Post()
    async createSlots(
        @Body() body: { juristId: string; slots: { start: string; end: string }[] }
    ) {
        return this.slotsService.createSlots(body.juristId, body.slots);
    }

    @Post(':id/lock')
    async lockSlot(@Param('id') id: string, @Body() body: { leadId?: string }) {
        return this.slotsService.lockSlot(id, body.leadId);
    }
}
