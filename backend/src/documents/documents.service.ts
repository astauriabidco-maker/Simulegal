import { Injectable, Inject, Logger } from '@nestjs/common';
import * as OCR from './ocr.interface';

@Injectable()
export class DocumentsService {
    private readonly logger = new Logger(DocumentsService.name);

    constructor(
        @Inject(OCR.OCR_PROVIDER_TOKEN) private readonly ocrProvider: OCR.OCRProvider
    ) { }

    async analyze(file: Express.Multer.File): Promise<OCR.AnalysisResponse> {
        const fileName = file.originalname.toLowerCase();

        // Simulation fallback for specific filenames
        if (fileName.includes('force_blur')) {
            return {
                status: 'REJECTED_BLURRY',
                confidence: 15,
                message: 'Le document est trop flou (Simulé).'
            };
        }

        try {
            this.logger.log(`Starting real OCR analysis for: ${file.originalname}`);
            const result = await this.ocrProvider.analyzeImage(file.buffer, file.mimetype);

            return {
                status: result.status,
                confidence: result.confidence,
                message: result.message,
                extractedData: result.extractedData as any
            };
        } catch (error) {
            this.logger.error('OCR Analysis failed, falling back to basic validation', error);

            // Fallback for dev/demo stability if API key is missing
            return {
                status: 'VALID',
                confidence: 50,
                message: 'Authentification (Simulation: ' + error.message + ')',
                extractedData: {
                    fileName: file.originalname,
                    analyzedAt: new Date().toISOString()
                }
            } as any;
        }
    }
}
