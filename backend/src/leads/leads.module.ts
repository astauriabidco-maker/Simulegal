import { Module } from '@nestjs/common';
import { LeadsService } from './leads.service';
import { LeadsController } from './leads.controller';
import { PublicLeadsController } from './leads.public.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { InvoicesModule } from '../invoices/invoices.module';
import { EmailModule } from '../email/email.module';

@Module({
  imports: [PrismaModule, InvoicesModule, EmailModule],
  providers: [LeadsService],
  controllers: [LeadsController, PublicLeadsController],
  exports: [LeadsService]
})
export class LeadsModule { }
