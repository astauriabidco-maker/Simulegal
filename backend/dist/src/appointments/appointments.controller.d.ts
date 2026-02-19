import { AppointmentsService } from './appointments.service';
import { Appointment as AppointmentModel } from '@prisma/client';
import { MeetingsService } from './meetings.service';
import { NotificationsService } from '../notifications/notifications.service';
export declare class AppointmentsController {
    private readonly appointmentsService;
    private readonly meetingsService;
    private readonly notificationsService;
    constructor(appointmentsService: AppointmentsService, meetingsService: MeetingsService, notificationsService: NotificationsService);
    getSlots(date: string, agencyId?: string, serviceId?: string): Promise<string[]>;
    findAll(agencyId?: string, start?: string, end?: string): Promise<AppointmentModel[]>;
    book(data: {
        slotIso: string;
        lead: {
            id: string;
            name: string;
            email: string;
        };
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY';
        agencyId?: string;
        serviceId?: string;
        hostUserId?: string;
    }): Promise<AppointmentModel>;
    update(id: string, body: {
        start?: string;
        end?: string;
        hostUserId?: string;
        agencyId?: string;
        status?: string;
        type?: string;
    }): Promise<{
        id: string;
        type: import(".prisma/client").$Enums.AppointmentType;
        status: import(".prisma/client").$Enums.AppointmentStatus;
        createdAt: Date;
        updatedAt: Date;
        agencyId: string | null;
        start: Date;
        end: Date;
        leadId: string;
        serviceId: string | null;
        leadName: string;
        leadEmail: string | null;
        hostUserId: string | null;
        meetingLink: string | null;
        cancellationReason: string | null;
    }>;
    getStats(start: string, end: string): Promise<any>;
    cancel(id: string, body: {
        reason: string;
    }): Promise<{
        id: string;
        type: import(".prisma/client").$Enums.AppointmentType;
        status: import(".prisma/client").$Enums.AppointmentStatus;
        createdAt: Date;
        updatedAt: Date;
        agencyId: string | null;
        start: Date;
        end: Date;
        leadId: string;
        serviceId: string | null;
        leadName: string;
        leadEmail: string | null;
        hostUserId: string | null;
        meetingLink: string | null;
        cancellationReason: string | null;
    }>;
}
