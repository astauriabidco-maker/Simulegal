import { Controller, Get, Post, Body, Query, UseGuards, Param, BadRequestException, Patch } from '@nestjs/common';
import { AppointmentsService } from './appointments.service';
import { Appointment as AppointmentModel, Prisma } from '@prisma/client';
import { MeetingsService } from './meetings.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';
import { NotificationsService } from '../notifications/notifications.service';

@Controller('appointments')
export class AppointmentsController {
    constructor(
        private readonly appointmentsService: AppointmentsService,
        private readonly meetingsService: MeetingsService,
        private readonly notificationsService: NotificationsService
    ) { }

    @Get('slots')
    async getSlots(
        @Query('date') date: string,
        @Query('agencyId') agencyId?: string,
        @Query('serviceId') serviceId?: string
    ): Promise<string[]> {
        if (!date) return [];
        return this.appointmentsService.getAvailableSlots(date, agencyId, serviceId);
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
        serviceId?: string;
        hostUserId?: string;
    }): Promise<AppointmentModel> {

        const start = new Date(data.slotIso);
        const end = new Date(start);
        end.setMinutes(start.getMinutes() + 30);

        // Find an available host with the requested expertise, if not manually provided
        let hostUserId = data.hostUserId;
        if (!hostUserId) {
            const foundHost = await this.appointmentsService.findAvailableHost(
                data.slotIso,
                data.agencyId,
                data.serviceId
            );
            hostUserId = foundHost ?? undefined;

            if (!hostUserId && data.serviceId) {
                throw new BadRequestException('No expert available for this service at the selected time');
            }
        }

        const appointment = await this.appointmentsService.create({
            start,
            end,
            type: data.type,
            lead: { connect: { id: data.lead.id } },
            leadName: data.lead.name,
            leadEmail: data.lead.email,
            serviceId: data.serviceId,
            status: 'SCHEDULED',
            agency: data.agencyId ? { connect: { id: data.agencyId } } : undefined,
            hostUser: hostUserId ? { connect: { id: hostUserId } } : undefined,
            meetingLink: data.type === 'VISIO_JURISTE'
                ? await this.meetingsService.generateMeetingLink()
                : undefined
        });

        // Trigger notification
        const lead = await this.appointmentsService.findLeadById(data.lead.id);
        if (lead) {
            await this.notificationsService.sendAppointmentConfirmation(lead, appointment);
        }

        return appointment;
    }

    @Patch(':id')
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    async update(
        @Param('id') id: string,
        @Body() body: { start?: string; end?: string; hostUserId?: string; agencyId?: string; status?: string; type?: string }
    ) {
        const data: any = { ...body };
        if (body.start) data.start = new Date(body.start);
        if (body.end) data.end = new Date(body.end);
        return this.appointmentsService.update(id, data);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    @Get('stats')
    async getStats(
        @Query('start') start: string,
        @Query('end') end: string
    ) {
        const startDate = start ? new Date(start) : new Date();
        const endDate = end ? new Date(end) : new Date();
        return this.appointmentsService.getAgendaStats(startDate, endDate);
    }

    @Patch(':id/cancel')
    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    async cancel(
        @Param('id') id: string,
        @Body() body: { reason: string }
    ) {
        return this.appointmentsService.cancel(id, body.reason);
    }
}
