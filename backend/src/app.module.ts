import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { ScheduleModule } from '@nestjs/schedule';
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
import { AbsencesModule } from './absences/absences.module';

import { FranchiseLeadsModule } from './franchise-leads/franchise-leads.module';
import { AppointmentsModule } from './appointments/appointments.module';
import { DocumentsModule } from './documents/documents.module';
import { EligibilityModule } from './eligibility/eligibility.module';
import { CatalogModule } from './catalog/catalog.module';
import { SettingsModule } from './settings/settings.module';
import { DashboardModule } from './dashboard/dashboard.module';
import { SalesModule } from './sales/sales.module';
import { InvoicesModule } from './invoices/invoices.module';
import { PaymentsModule } from './payments/payments.module';
import { WhatsappModule } from './whatsapp/whatsapp.module';
import { VeilleModule } from './veille/veille.module';
import { AppointmentsSlotsModule } from './appointments-slots/appointments-slots.module';
import { EmailModule } from './email/email.module';
import { PipelineAutomationModule } from './pipeline-automation/pipeline-automation.module';
import { BlogModule } from './blog/blog.module';

@Module({
  imports: [
    ConfigModule.forRoot({ isGlobal: true }),
    AuthModule,
    UsersModule,
    PrismaModule,
    AgenciesModule,
    LeadsModule,
    FinanceModule,
    DevicesModule,
    NotificationsModule,
    RolesModule,
    FranchiseLeadsModule,
    AppointmentsModule,
    DocumentsModule,
    EligibilityModule,
    CatalogModule,
    SettingsModule,
    DashboardModule,
    SalesModule,
    InvoicesModule,
    PaymentsModule,
    AbsencesModule,
    WhatsappModule,
    VeilleModule,
    AppointmentsSlotsModule,
    EmailModule,
    ScheduleModule.forRoot(),
    PipelineAutomationModule,
    BlogModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule { }
