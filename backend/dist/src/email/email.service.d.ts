export declare class EmailService {
    private readonly logger;
    sendOrderConfirmation(to: string, clientName: string, serviceName: string, amount: number, transactionRef: string): Promise<boolean>;
    sendMandateCopy(to: string, clientName: string): Promise<boolean>;
}
