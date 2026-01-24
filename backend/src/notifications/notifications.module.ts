import { Module, Global } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { NotificationsService } from './notifications.service';
import { NotificationsController } from './notifications.controller';
import { EmailTemplatesService } from './email-templates.service';
import { SettingsModule } from '../settings/settings.module';
import { PrismaModule } from '../prisma/prisma.module';

@Global()
@Module({
    imports: [ConfigModule, SettingsModule, PrismaModule],
    controllers: [NotificationsController],
    providers: [NotificationsService, EmailTemplatesService],
    exports: [NotificationsService, EmailTemplatesService],
})
export class NotificationsModule { }


