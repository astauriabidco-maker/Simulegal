import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { UsersModule } from './users/users.module';
import { PrismaModule } from './prisma/prisma.module';
import { AgenciesModule } from './agencies/agencies.module';
import { LeadsModule } from './leads/leads.module';
import { FinanceModule } from './finance/finance.module';
import { DevicesModule } from './devices/devices.module';
import { NotificationsModule } from './notifications/notifications.module';
import { RolesModule } from './roles/roles.module';

import { FranchiseLeadsModule } from './franchise-leads/franchise-leads.module';

@Module({
  imports: [AuthModule, UsersModule, PrismaModule, AgenciesModule, LeadsModule, FinanceModule, DevicesModule, NotificationsModule, RolesModule, FranchiseLeadsModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule { }
