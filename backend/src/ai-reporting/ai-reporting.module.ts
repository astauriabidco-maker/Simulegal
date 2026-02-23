import { Module } from '@nestjs/common';
import { AiReportingService } from './ai-reporting.service';
import { AiReportingController } from './ai-reporting.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { ConfigModule } from '@nestjs/config';

@Module({
  imports: [PrismaModule, ConfigModule],
  providers: [AiReportingService],
  controllers: [AiReportingController]
})
export class AiReportingModule { }
