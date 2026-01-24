export const OCR_PROVIDER_TOKEN = 'OCR_PROVIDER_TOKEN';

export interface ExtractedData {
    firstName?: string;
    lastName?: string;
    expiryDate?: string;
    documentType?: string;
    documentNumber?: string;
    nationality?: string;
}

export interface OCRResult {
    status: 'VALID' | 'REJECTED_BLURRY' | 'REJECTED_INCOMPLETE' | 'REJECTED_WRONG_TYPE' | 'REJECTED_EXPIRED';
    confidence: number;
    message: string;
    extractedData?: ExtractedData;
}

export interface AnalysisResponse {
    status: 'VALID' | 'REJECTED_BLURRY' | 'REJECTED_INCOMPLETE' | 'REJECTED_WRONG_TYPE' | 'REJECTED_EXPIRED';
    confidence: number;
    message: string;
    extractedData?: ExtractedData;
}

export interface OCRProvider {
    analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult>;
}
