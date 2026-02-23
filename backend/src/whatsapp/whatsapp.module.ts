import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { WhatsappController } from './whatsapp.controller';
import { WhatsappService } from './whatsapp.service';
import { WhatsappGateway } from './whatsapp.gateway';
import { TwilioWebhookGuard } from './twilio-webhook.guard';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule, ConfigModule],
    controllers: [WhatsappController],
    providers: [WhatsappService, WhatsappGateway, TwilioWebhookGuard],
    exports: [WhatsappService, WhatsappGateway],
})
export class WhatsappModule { }
