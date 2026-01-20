import { Module } from '@nestjs/common';
import { SalesService } from './sales.service';
import { SalesController } from './sales.controller';
import { PrismaModule } from '../prisma/prisma.module';

import { VoiceModule } from './voice/voice.module';

import { SalesAnalyticsService } from './sales-analytics.service';

@Module({
    imports: [PrismaModule, VoiceModule],
    providers: [SalesService, SalesAnalyticsService],
    controllers: [SalesController],
    exports: [SalesService, SalesAnalyticsService],
})
export class SalesModule { }
