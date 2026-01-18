import { NotificationsService } from './notifications.service';
export declare class NotificationsController {
    private readonly notificationsService;
    constructor(notificationsService: NotificationsService);
    sendWhatsApp(data: {
        phone: string;
        template: string;
        params: any;
    }): Promise<{
        success: boolean;
        messageId: string;
    }>;
    triggerStageChange(data: {
        lead: any;
        oldStage: string;
        newStage: string;
    }): Promise<void>;
}
