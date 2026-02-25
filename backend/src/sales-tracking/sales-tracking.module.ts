import { Module } from '@nestjs/common';
import { SalesTrackingService } from './sales-tracking.service';
import { SalesTrackingController } from './sales-tracking.controller';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule],
    providers: [SalesTrackingService],
    controllers: [SalesTrackingController],
    exports: [SalesTrackingService],
})
export class SalesTrackingModule { }
