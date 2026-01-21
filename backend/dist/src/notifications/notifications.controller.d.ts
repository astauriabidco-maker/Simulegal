import { NotificationsService } from './notifications.service';
import { ConfigService } from '@nestjs/config';
export declare class NotificationsController {
    private readonly notificationsService;
    private readonly configService;
    constructor(notificationsService: NotificationsService, configService: ConfigService);
    sendWhatsApp(data: {
        phone: string;
        template: string;
        params: any;
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
    sendSms(data: {
        phone: string;
        message: string;
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
    sendProspectLink(data: {
        prospectId: string;
        prospectPhone: string;
        prospectFirstName: string;
        channel: 'SMS' | 'WHATSAPP';
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
    triggerStageChange(data: {
        lead: any;
        oldStage: string;
        newStage: string;
    }): Promise<void>;
}
