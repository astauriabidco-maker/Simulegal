import * as OCR from './ocr.interface';
export declare class DocumentsService {
    private readonly ocrProvider;
    private readonly logger;
    constructor(ocrProvider: OCR.OCRProvider);
    analyze(file: Express.Multer.File): Promise<OCR.AnalysisResponse>;
}
