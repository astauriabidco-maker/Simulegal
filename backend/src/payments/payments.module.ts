import { forwardRef, Module } from '@nestjs/common';
import { PaymentsService } from './payments.service';
import { PaymentsController } from './payments.controller';
import { SettingsModule } from '../settings/settings.module';
import { LeadsModule } from '../leads/leads.module';
import { EmailModule } from '../email/email.module';
import { PipelineAutomationModule } from '../pipeline-automation/pipeline-automation.module';
import { SalesModule } from '../sales/sales.module';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [
        SettingsModule,
        forwardRef(() => LeadsModule),
        EmailModule,
        PipelineAutomationModule,
        forwardRef(() => SalesModule),
        PrismaModule,
    ],
    providers: [PaymentsService],
    controllers: [PaymentsController],
    exports: [PaymentsService]
})
export class PaymentsModule { }
