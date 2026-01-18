import { AppointmentsService } from './appointments.service';
import { Appointment as AppointmentModel } from '@prisma/client';
export declare class AppointmentsController {
    private readonly appointmentsService;
    constructor(appointmentsService: AppointmentsService);
    getSlots(date: string, agencyId?: string): Promise<string[]>;
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
    }): Promise<AppointmentModel>;
}
