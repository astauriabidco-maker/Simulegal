import { Module } from '@nestjs/common';
import { FranchiseLeadsService } from './franchise-leads.service';
import { FranchiseLeadsController } from './franchise-leads.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { AgenciesModule } from '../agencies/agencies.module';
import { UsersModule } from '../users/users.module';
import { DevicesModule } from '../devices/devices.module';
import { NotificationsModule } from '../notifications/notifications.module';

@Module({
    imports: [PrismaModule, AgenciesModule, UsersModule, DevicesModule, NotificationsModule],
    providers: [FranchiseLeadsService],
    controllers: [FranchiseLeadsController]
})
export class FranchiseLeadsModule { }
