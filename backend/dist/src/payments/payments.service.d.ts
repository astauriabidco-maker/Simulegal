import { OnModuleInit } from '@nestjs/common';
import { SettingsService } from '../settings/settings.service';
import { LeadsService } from '../leads/leads.service';
export declare class PaymentsService implements OnModuleInit {
    private settingsService;
    private leadsService;
    private readonly logger;
    private stripe;
    constructor(settingsService: SettingsService, leadsService: LeadsService);
    onModuleInit(): Promise<void>;
    createCheckoutSession(leadId: string, successUrl: string, cancelUrl: string): Promise<{
        url: string | null;
    }>;
    handleWebhook(signature: string, payload: Buffer): Promise<void>;
}
