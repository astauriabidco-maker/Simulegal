import { DocumentsService } from './documents.service';
export declare class DocumentsController {
    private readonly documentsService;
    constructor(documentsService: DocumentsService);
    analyzeFile(file: Express.Multer.File): Promise<import("./ocr.interface").AnalysisResponse>;
}
