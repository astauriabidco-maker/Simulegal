import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
export declare class WhatsappService {
    private prisma;
    private notificationsService;
    private readonly logger;
    constructor(prisma: PrismaService, notificationsService: NotificationsService);
    handleIncoming(data: {
        from: string;
        body: string;
        messageSid: string;
    }): Promise<{
        success: boolean;
        communicationId: string;
        matchedType: string;
        matchedId: string | undefined;
    }>;
    getConversations(): Promise<any[]>;
    getMessages(type: 'LEAD' | 'PROSPECT', id: string): Promise<{
        id: string;
        type: string;
        createdAt: Date;
        leadId: string | null;
        sender: string;
        direction: string;
        content: string;
        senderName: string | null;
        prospectId: string | null;
    }[]>;
    sendMessage(type: 'LEAD' | 'PROSPECT', id: string, content: string): Promise<{
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
