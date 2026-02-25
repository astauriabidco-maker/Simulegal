import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { DocumentsController } from './documents.controller';
import { DocumentsService } from './documents.service';
import { TesseractLocalProvider } from './tesseract-local.provider';
import { OllamaVisionProvider } from './ollama-vision.provider';
import { HybridOCRProvider } from './hybrid-ocr.provider';
import { OCR_PROVIDER_TOKEN } from './ocr.interface';

/**
 * 🔒 Documents Module — OCR 100% local
 *
 * Pipeline :
 *   Tier 1 : Tesseract.js + Sharp (fast check, toujours dispo)
 *   Tier 2 : Ollama LLaVA (analyse fine, si installé)
 *
 * Aucune donnée ne quitte le serveur.
 *
 * Pour activer Ollama (optionnel) :
 *   1. brew install ollama
 *   2. ollama pull llava:7b
 *   3. ollama serve
 *   4. Les variables d'env optionnelles :
 *      - OLLAMA_URL=http://localhost:11434
 *      - OLLAMA_VISION_MODEL=llava:7b
 */
@Module({
    imports: [ConfigModule],
    controllers: [DocumentsController],
    providers: [
        DocumentsService,
        TesseractLocalProvider,
        OllamaVisionProvider,
        HybridOCRProvider,
        {
            provide: OCR_PROVIDER_TOKEN,
            useExisting: HybridOCRProvider,
        },
    ],
    exports: [DocumentsService],
})
export class DocumentsModule { }
