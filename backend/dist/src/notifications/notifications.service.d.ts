import { ConfigService } from '@nestjs/config';
export declare class NotificationsService {
    private configService;
    private twilioClient;
    private mailTransporter;
    constructor(configService: ConfigService);
    sendWhatsApp(phone: string, template: string, params: any): Promise<{
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
    sendEmail(to: string, subject: string, body: string): Promise<{
        success: boolean;
        messageId?: undefined;
        error?: undefined;
    } | {
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendSMS(phone: string, message: string): Promise<{
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
    sendAppointmentConfirmation(lead: any, appointment: any): Promise<void>;
    onStageChange(lead: any, oldStage: string, newStage: string): Promise<void>;
    onDocumentRejected(lead: any, docLabel: string): Promise<void>;
    onJuristAssigned(lead: any, juristName: string): Promise<void>;
    onFranchiseOnboarding(lead: any, tempPassword: string): Promise<void>;
}
