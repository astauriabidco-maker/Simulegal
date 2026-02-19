import { LeadsService } from './leads.service';
import { EmailService } from '../email/email.service';
export declare class PublicLeadsController {
    private readonly leadsService;
    private readonly emailService;
    constructor(leadsService: LeadsService, emailService: EmailService);
    create(leadData: any): Promise<any>;
    createCheckoutSession(body: {
        leadId: string;
        amount: number;
        label: string;
        successUrl: string;
        cancelUrl: string;
    }): Promise<{
        url: string;
        sessionId: string;
    }>;
}
