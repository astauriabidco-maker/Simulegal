import { ConfigService } from '@nestjs/config';
export declare class VoiceService {
    private configService;
    private twilioClient;
    constructor(configService: ConfigService);
    generateToken(identity: string): {
        token: string;
        identity: string;
    };
    handleIncomingCall(from: string, to: string): any;
    handleOutgoingCall(to: string): any;
}
