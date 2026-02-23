import { Module } from '@nestjs/common';
import { PipelineAutomationService } from './pipeline-automation.service';
import { PipelineAutomationController } from './pipeline-automation.controller';
import { PrismaModule } from '../prisma/prisma.module';
import { NotificationsModule } from '../notifications/notifications.module';

@Module({
    imports: [PrismaModule, NotificationsModule],
    providers: [PipelineAutomationService],
    controllers: [PipelineAutomationController],
    exports: [PipelineAutomationService],
})
export class PipelineAutomationModule { }
