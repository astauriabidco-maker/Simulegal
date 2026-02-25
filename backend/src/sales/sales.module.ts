import { Module, forwardRef } from '@nestjs/common';
import { SalesService } from './sales.service';
import { SalesController } from './sales.controller';
import { LeadCaptureController } from './lead-capture.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { AppointmentsModule } from '../appointments/appointments.module';

import { VoiceModule } from './voice/voice.module';
import { CallLogsModule } from './call-logs/call-logs.module';

import { SalesAnalyticsService } from './sales-analytics.service';
import { AssignmentService } from './assignment.service';
import { ProspectPipelineService } from './prospect-pipeline.service';
import { QuotePdfService } from './quote-pdf.service';
import { SalesTrackingModule } from '../sales-tracking/sales-tracking.module';

@Module({
    imports: [PrismaModule, VoiceModule, forwardRef(() => CallLogsModule), forwardRef(() => AppointmentsModule), SalesTrackingModule],
    providers: [SalesService, SalesAnalyticsService, AssignmentService, ProspectPipelineService, QuotePdfService],
    controllers: [SalesController, LeadCaptureController],
    exports: [SalesService, SalesAnalyticsService, AssignmentService, ProspectPipelineService, QuotePdfService],
})
export class SalesModule { }
