import { Controller, Get, Post, Body, Query, UseGuards, Param } from '@nestjs/common';
import { AppointmentsService } from './appointments.service';
import { Appointment as AppointmentModel, Prisma } from '@prisma/client';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';

@Controller('appointments')
export class AppointmentsController {
    constructor(private readonly appointmentsService: AppointmentsService) { }

    @Get('slots')
    async getSlots(
        @Query('date') date: string,
        @Query('agencyId') agencyId?: string
    ): Promise<string[]> {
        // Public endpoint (or arguably protected)
        if (!date) return [];
        return this.appointmentsService.getAvailableSlots(date, agencyId);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    @Get()
    async findAll(
        @Query('agencyId') agencyId?: string,
        @Query('start') start?: string,
        @Query('end') end?: string
    ): Promise<AppointmentModel[]> {
        const where: Prisma.AppointmentWhereInput = {};

        if (agencyId) where.agencyId = agencyId;
        if (start && end) {
            where.start = { gte: new Date(start) };
            where.end = { lte: new Date(end) };
        }

        return this.appointmentsService.findAll({
            where,
            orderBy: { start: 'asc' }
        });
    }

    // Public Booking Endpoint (no Auth required for Leads)
    @Post('book')
    async book(@Body() data: {
        slotIso: string;
        lead: { id: string; name: string; email: string };
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY';
        agencyId?: string;
    }): Promise<AppointmentModel> {

        const start = new Date(data.slotIso);
        const end = new Date(start);
        end.setMinutes(start.getMinutes() + 30);

        // Simple round-robin or first available host assignment for Visio could happen here
        // For now, we leave hostUserId null or assign a default HQ user if implemented

        return this.appointmentsService.create({
            start,
            end,
            type: data.type,
            leadId: data.lead.id,
            leadName: data.lead.name,
            leadEmail: data.lead.email,
            status: 'SCHEDULED',
            agency: data.agencyId ? { connect: { id: data.agencyId } } : undefined,
            meetingLink: data.type === 'VISIO_JURISTE'
                ? `https://meet.google.com/sim-ule-gal-${data.lead.id.substring(0, 4)}`
                : undefined
        });
    }
}
