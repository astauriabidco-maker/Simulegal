import { ConfigService } from '@nestjs/config';
import { OCRProvider, OCRResult } from './ocr.interface';
export declare class OpenAIProvider implements OCRProvider {
    private configService;
    private readonly logger;
    private readonly openai;
    constructor(configService: ConfigService);
    analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult>;
}
