import { Module, forwardRef } from '@nestjs/common';
import { SalesService } from './sales.service';
import { SalesController } from './sales.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { AppointmentsModule } from '../appointments/appointments.module';

import { VoiceModule } from './voice/voice.module';
import { CallLogsModule } from './call-logs/call-logs.module';

import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';
import { ProspectPipelineService } from './prospect-pipeline.service';

@Module({
    imports: [PrismaModule, VoiceModule, forwardRef(() => CallLogsModule), forwardRef(() => AppointmentsModule)],
    providers: [SalesService, SalesAnalyticsService, AssignmentService, ProspectPipelineService],
    controllers: [SalesController],
    exports: [SalesService, SalesAnalyticsService, AssignmentService, ProspectPipelineService],
})
export class SalesModule { }
