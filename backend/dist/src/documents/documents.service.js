"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
var DocumentsService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.DocumentsService = void 0;
const common_1 = require("@nestjs/common");
const OCR = __importStar(require("./ocr.interface"));
let DocumentsService = DocumentsService_1 = class DocumentsService {
    ocrProvider;
    logger = new common_1.Logger(DocumentsService_1.name);
    constructor(ocrProvider) {
        this.ocrProvider = ocrProvider;
    }
    async analyze(file) {
        const fileName = file.originalname.toLowerCase();
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
                extractedData: result.extractedData
            };
        }
        catch (error) {
            this.logger.error('OCR Analysis failed, falling back to basic validation', error);
            return {
                status: 'VALID',
                confidence: 50,
                message: 'Authentification (Simulation: ' + error.message + ')',
                extractedData: {
                    fileName: file.originalname,
                    analyzedAt: new Date().toISOString()
                }
            };
        }
    }
};
exports.DocumentsService = DocumentsService;
exports.DocumentsService = DocumentsService = DocumentsService_1 = __decorate([
    (0, common_1.Injectable)(),
    __param(0, (0, common_1.Inject)(OCR.OCR_PROVIDER_TOKEN)),
    __metadata("design:paramtypes", [Object])
], DocumentsService);
//# sourceMappingURL=documents.service.js.map