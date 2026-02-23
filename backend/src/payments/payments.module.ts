import { forwardRef, Module } from '@nestjs/common';
import { PaymentsService } from './payments.service';
import { PaymentsController } from './payments.controller';
import { SettingsModule } from '../settings/settings.module';
import { LeadsModule } from '../leads/leads.module';
import { EmailModule } from '../email/email.module';
import { PipelineAutomationModule } from '../pipeline-automation/pipeline-automation.module';

@Module({
    imports: [SettingsModule, forwardRef(() => LeadsModule), EmailModule, PipelineAutomationModule],
    providers: [PaymentsService],
    controllers: [PaymentsController],
    exports: [PaymentsService]
})
export class PaymentsModule { }
