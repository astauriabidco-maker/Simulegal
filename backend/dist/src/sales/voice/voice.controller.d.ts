import { VoiceService } from './voice.service';
import type { Request, Response } from 'express';
export declare class VoiceController {
    private readonly voiceService;
    constructor(voiceService: VoiceService);
    getToken(req: Request): {
        token: string;
        identity: string;
    };
    handleVoiceWebhook(body: any, res: Response): void;
}
