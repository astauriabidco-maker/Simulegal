import { Module } from '@nestjs/common';
import { SalesService } from './sales.service';
import { SalesController } from './sales.controller';
import { PrismaModule } from '../prisma/prisma.module';

import { VoiceModule } from './voice/voice.module';
import { CallLogsModule } from './call-logs/call-logs.module';

import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';

@Module({
    imports: [PrismaModule, VoiceModule, CallLogsModule],
    providers: [SalesService, SalesAnalyticsService, AssignmentService],
    controllers: [SalesController],
    exports: [SalesService, SalesAnalyticsService, AssignmentService],
})
export class SalesModule { }
