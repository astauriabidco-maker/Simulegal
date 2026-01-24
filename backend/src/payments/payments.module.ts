import { Module } from '@nestjs/common';
import { PaymentsService } from './payments.service';
import { PaymentsController } from './payments.controller';
import { SettingsModule } from '../settings/settings.module';
import { LeadsModule } from '../leads/leads.module';

@Module({
    imports: [SettingsModule, LeadsModule],
    providers: [PaymentsService],
    controllers: [PaymentsController],
    exports: [PaymentsService]
})
export class PaymentsModule { }
