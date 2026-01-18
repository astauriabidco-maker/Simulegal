"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DocumentsService = void 0;
const common_1 = require("@nestjs/common");
let DocumentsService = class DocumentsService {
    async analyze(file) {
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
};
exports.DocumentsService = DocumentsService;
exports.DocumentsService = DocumentsService = __decorate([
    (0, common_1.Injectable)()
], DocumentsService);
//# sourceMappingURL=documents.service.js.map