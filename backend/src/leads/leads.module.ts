import { Module, forwardRef } from '@nestjs/common';
import { LeadsService } from './leads.service';
import { LeadsController } from './leads.controller';
import { PublicLeadsController } from './leads.public.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { InvoicesModule } from '../invoices/invoices.module';
import { EmailModule } from '../email/email.module';
import { PaymentsModule } from '../payments/payments.module';
import { PipelineAutomationModule } from '../pipeline-automation/pipeline-automation.module';

@Module({
  imports: [PrismaModule, InvoicesModule, EmailModule, forwardRef(() => PaymentsModule), PipelineAutomationModule],
  providers: [LeadsService],
  controllers: [LeadsController, PublicLeadsController],
  exports: [LeadsService]
})
export class LeadsModule { }
