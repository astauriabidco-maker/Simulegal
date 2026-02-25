import { Module, forwardRef } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { WhatsappController } from './whatsapp.controller';
import { WhatsappService } from './whatsapp.service';
import { WhatsappGateway } from './whatsapp.gateway';
import { TwilioWebhookGuard } from './twilio-webhook.guard';
import { PrismaModule } from '../prisma/prisma.module';
import { LeadsModule } from '../leads/leads.module';

@Module({
    imports: [PrismaModule, ConfigModule, forwardRef(() => LeadsModule)],
    controllers: [WhatsappController],
    providers: [WhatsappService, WhatsappGateway, TwilioWebhookGuard],
    exports: [WhatsappService, WhatsappGateway],
})
export class WhatsappModule { }
