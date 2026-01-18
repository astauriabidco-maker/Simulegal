export declare class NotificationsService {
    sendWhatsApp(phone: string, template: string, params: any): Promise<{
        success: boolean;
        messageId: string;
    }>;
    onStageChange(lead: any, oldStage: string, newStage: string): Promise<void>;
}
