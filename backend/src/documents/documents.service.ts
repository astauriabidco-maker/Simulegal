import { Injectable } from '@nestjs/common';

export interface AnalysisResponse {
    status: 'VALID' | 'REJECTED_BLURRY' | 'REJECTED_INCOMPLETE' | 'REJECTED_WRONG_TYPE' | 'REJECTED_EXPIRED';
    confidence: number;
    message: string;
    extractedData?: Record<string, string>;
}

@Injectable()
export class DocumentsService {
    async analyze(file: Express.Multer.File): Promise<AnalysisResponse> {
        // Simulate delay
        await new Promise(resolve => setTimeout(resolve, 1500));

        const fileName = file.originalname.toLowerCase();

        if (fileName.includes('flou') || fileName.includes('blur')) {
            return {
                status: 'REJECTED_BLURRY',
                confidence: 15,
                message: 'Le document est trop flou. Veuillez reprendre la photo avec un meilleur éclairage.'
            };
        }

        if (fileName.includes('incomplet')) {
            return {
                status: 'REJECTED_INCOMPLETE',
                confidence: 30,
                message: 'Le document n\'est pas entièrement visible.'
            };
        }

        return {
            status: 'VALID',
            confidence: 95,
            message: 'Document validé avec succès !',
            extractedData: {
                fileName: file.originalname,
                analyzedAt: new Date().toISOString()
            }
        };
    }
}
