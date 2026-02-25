import { Module } from '@nestjs/common';
import { AppointmentsController } from './appointments.controller';
import { AppointmentsService } from './appointments.service';
import { PrismaService } from '../prisma/prisma.service';
import { MeetingsService } from './meetings.service';
import { NotificationsModule } from '../notifications/notifications.module';

@Module({
    imports: [NotificationsModule],
    controllers: [AppointmentsController],
    providers: [AppointmentsService, MeetingsService, PrismaService],
    exports: [AppointmentsService, MeetingsService]
})
export class AppointmentsModule { }
