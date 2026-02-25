import { Module } from '@nestjs/common';
import { PrismaModule } from '../prisma/prisma.module';
import { SupervisionAgentService } from './supervision-agent.service';
import { OllamaTextService } from './ollama-text.service';
import { NotificationsModule } from '../notifications/notifications.module';

@Module({
    imports: [PrismaModule, NotificationsModule],
    providers: [SupervisionAgentService, OllamaTextService],
    exports: [SupervisionAgentService, OllamaTextService],
})
export class AgentsModule { }
