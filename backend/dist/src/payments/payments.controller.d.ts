import type { RawBodyRequest } from '@nestjs/common';
import type { Request } from 'express';
import { PaymentsService } from './payments.service';
export declare class PaymentsController {
    private readonly paymentsService;
    constructor(paymentsService: PaymentsService);
    createSession(data: {
        leadId: string;
        successUrl: string;
        cancelUrl: string;
    }): Promise<{
        url: string | null;
    }>;
    stripeWebhook(signature: string, req: RawBodyRequest<Request>): Promise<void>;
}
