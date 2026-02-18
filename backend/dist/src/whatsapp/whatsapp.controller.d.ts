import { WhatsappService } from './whatsapp.service';
export declare class WhatsappController {
    private readonly whatsappService;
    private readonly logger;
    constructor(whatsappService: WhatsappService);
    healthCheck(): {
        status: string;
    };
    handleWebhook(body: any): Promise<string>;
    getConversations(): Promise<any[]>;
    getMessages(type: 'LEAD' | 'PROSPECT', id: string): Promise<{
        id: string;
        type: string;
        createdAt: Date;
        sender: string;
        direction: string;
        content: string;
        senderName: string | null;
        leadId: string | null;
        prospectId: string | null;
    }[]>;
    sendMessage(body: {
        type: 'LEAD' | 'PROSPECT';
        id: string;
        content: string;
    }): Promise<{
        success: boolean;
        messageId?: undefined;
        error?: undefined;
    } | {
        success: boolean;
        messageId: string;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
}
