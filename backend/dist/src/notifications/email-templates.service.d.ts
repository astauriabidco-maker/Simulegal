export declare class EmailTemplatesService {
    private readonly brandColor;
    private readonly bgColor;
    private readonly textColor;
    getBaseTemplate(content: string, previewText?: string): string;
    renderWelcome(name: string, email: string, tempPassword: string): {
        subject: string;
        html: string;
    };
    renderDiagnosticInvitation(name: string, magicLink: string): {
        subject: string;
        html: string;
    };
    renderAppointmentConfirmation(name: string, date: Date, type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY', meetingLink?: string, agencyAddress?: string): {
        subject: string;
        html: string;
    };
    renderPaymentConfirmation(name: string, amount: number, refundCode: string): {
        subject: string;
        html: string;
    };
    renderAppointmentReminder(name: string, date: Date, type: 'VISIO_JURISTE' | 'PHYSICAL_AGENCY', meetingLink?: string): {
        subject: string;
        html: string;
    };
}
