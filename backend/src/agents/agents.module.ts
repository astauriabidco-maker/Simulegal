import { Module } from '@nestjs/common';
import { PrismaModule } from '../prisma/prisma.module';
import { SupervisionAgentService } from './supervision-agent.service';
import { NotificationsModule } from '../notifications/notifications.module';

@Module({
    imports: [PrismaModule, NotificationsModule],
    providers: [SupervisionAgentService],
    exports: [SupervisionAgentService],
})
export class AgentsModule { }
