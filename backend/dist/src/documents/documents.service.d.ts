export interface AnalysisResponse {
    status: 'VALID' | 'REJECTED_BLURRY' | 'REJECTED_INCOMPLETE' | 'REJECTED_WRONG_TYPE' | 'REJECTED_EXPIRED';
    confidence: number;
    message: string;
    extractedData?: Record<string, string>;
}
export declare class DocumentsService {
    analyze(file: Express.Multer.File): Promise<AnalysisResponse>;
}
