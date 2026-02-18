import { ConfigService } from '@nestjs/config';
import { EmailTemplatesService } from './email-templates.service';
import { SettingsService } from '../settings/settings.service';
import { PrismaService } from '../prisma/prisma.service';
export declare class NotificationsService {
    private configService;
    private emailTemplates;
    private settingsService;
    private prisma;
    private twilioClient;
    private cachedSmtpConfig;
    private lastConfigCheck;
    private readonly CONFIG_TTL;
    constructor(configService: ConfigService, emailTemplates: EmailTemplatesService, settingsService: SettingsService, prisma: PrismaService);
    private getSmtpConfig;
    private createTransporter;
    refreshSmtpConfig(): Promise<void>;
    sendWhatsApp(phone: string, template: string, params: any, metadata?: {
        leadId?: string;
        prospectId?: string;
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
    sendEmail(to: string, subject: string, body: string, html?: string): Promise<{
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendWelcomeEmail(user: {
        name: string;
        email: string;
    }, tempPassword: string): Promise<{
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendDiagnosticInvitation(prospect: {
        name: string;
        email: string;
    }, magicLink: string): Promise<{
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendAppointmentConfirmationEmail(lead: {
        name: string;
        email: string;
    }, appointment: {
        start: Date;
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY';
        meetingLink?: string;
        agencyAddress?: string;
    }): Promise<{
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendPaymentConfirmation(lead: {
        name: string;
        email: string;
    }, amount: number, refundCode: string): Promise<{
        success: boolean;
        messageId: any;
        error?: undefined;
    } | {
        success: boolean;
        error: any;
        messageId?: undefined;
    }>;
    sendAppointmentReminder(lead: {
        name: string;
        email: string;
    }, appointment: {
        start: Date;
        type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY';
        meetingLink?: string;
    }): Promise<{
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
