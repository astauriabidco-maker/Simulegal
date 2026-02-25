import { Injectable, Logger } from '@nestjs/common';
import { OCRProvider, OCRResult } from './ocr.interface';
import { TesseractLocalProvider } from './tesseract-local.provider';
import { OllamaVisionProvider } from './ollama-vision.provider';

/**
 * 🔒 Provider OCR Hybride — 100% local, zéro donnée externalisée
 *
 * Pipeline intelligent :
 *   Tier 1 : Tesseract.js + Sharp (fast check — lisibilité, OCR basique)
 *   Tier 2 : Ollama LLaVA (analyse fine — si dispo et si T1 est incertain)
 *
 * Résultat final :
 *   - Si Tesseract donne un résultat clair (VALID ≥80% ou REJECTED) → on l'utilise
 *   - Si Tesseract est incertain (50-80%) et Ollama est dispo → analyse fine Ollama
 *   - Si Ollama n'est pas dispo → fallback Tesseract seul
 */
@Injectable()
export class HybridOCRProvider implements OCRProvider {
    private readonly logger = new Logger(HybridOCRProvider.name);

    constructor(
        private readonly tesseract: TesseractLocalProvider,
        private readonly ollama: OllamaVisionProvider,
    ) {
        this.logger.log('🔒 [Hybrid OCR] Provider initialisé — 100% local (Tesseract + Ollama)');
    }

    async analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult> {
        const startTime = Date.now();

        // ════════════════════════════════════════════════
        // TIER 1 : Tesseract.js + Sharp (toujours exécuté)
        // ════════════════════════════════════════════════
        this.logger.log(`🔍 [Tier 1] Analyse Tesseract + Sharp...`);
        let tesseractResult: OCRResult;

        try {
            tesseractResult = await this.tesseract.analyzeImage(buffer, mimetype);
        } catch (error: any) {
            this.logger.error(`[Tier 1] Erreur Tesseract: ${error.message}`);
            return {
                status: 'VALID', // Fallback permissif — le juriste vérifiera
                confidence: 30,
                message: 'Analyse automatique partielle. Vérification manuelle recommandée.',
            };
        }

        const tier1Ms = Date.now() - startTime;
        this.logger.log(`[Tier 1] Résultat: ${tesseractResult.status} (${tesseractResult.confidence}%) — ${tier1Ms}ms`);

        // ── Cas clairs : pas besoin de Tier 2 ──

        // REJECTED par Tesseract → retour immédiat (flou, trop petit, etc.)
        if (tesseractResult.status !== 'VALID') {
            this.logger.log(`[Hybrid] ❌ Rejeté par Tier 1 — pas besoin de Tier 2`);
            return {
                ...tesseractResult,
                message: `[Local] ${tesseractResult.message}`,
            };
        }

        // Haute confiance Tesseract → pas besoin de Tier 2
        if (tesseractResult.confidence >= 80) {
            this.logger.log(`[Hybrid] ✅ Confiance haute (${tesseractResult.confidence}%) — Tier 1 suffisant`);
            return {
                ...tesseractResult,
                message: `[Local] ${tesseractResult.message}`,
            };
        }

        // ════════════════════════════════════════════════
        // TIER 2 : Ollama LLaVA (analyse fine si disponible)
        // ════════════════════════════════════════════════
        const ollamaAvailable = await this.ollama.getAvailability();

        if (!ollamaAvailable) {
            this.logger.log(`[Hybrid] ⏳ Ollama non disponible — résultat Tesseract conservé`);
            return {
                ...tesseractResult,
                message: `[Local/Tesseract] ${tesseractResult.message}`,
            };
        }

        this.logger.log(`🔍 [Tier 2] Analyse Ollama LLaVA (confiance T1: ${tesseractResult.confidence}%)...`);

        try {
            const ollamaResult = await this.ollama.analyzeImage(buffer, mimetype);
            const tier2Ms = Date.now() - startTime - tier1Ms;
            this.logger.log(`[Tier 2] Résultat: ${ollamaResult.status} (${ollamaResult.confidence}%) — ${tier2Ms}ms`);

            // Fusionner les résultats : Ollama a la priorité sur Tesseract
            const mergedData = {
                ...tesseractResult.extractedData,
                ...ollamaResult.extractedData, // Ollama écrase si présent
            };

            // Choisir le meilleur résultat
            const finalResult: OCRResult = {
                status: ollamaResult.status,
                confidence: Math.max(tesseractResult.confidence, ollamaResult.confidence),
                message: `[Local/Vision] ${ollamaResult.message}`,
                extractedData: mergedData,
            };

            const totalMs = Date.now() - startTime;
            this.logger.log(`[Hybrid] 📊 Final: ${finalResult.status} (${finalResult.confidence}%) — Total: ${totalMs}ms`);

            return finalResult;
        } catch (ollamaError: any) {
            this.logger.warn(`[Tier 2] Erreur Ollama (fallback T1): ${ollamaError.message}`);

            // Fallback sur résultat Tesseract
            return {
                ...tesseractResult,
                message: `[Local/Tesseract] ${tesseractResult.message}`,
            };
        }
    }
}
