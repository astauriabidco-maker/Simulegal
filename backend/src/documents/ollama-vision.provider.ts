import { Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { OCRProvider, OCRResult } from './ocr.interface';

/**
 * 🔒 Provider OCR local via Ollama LLaVA — Vision AI 100% privée
 *
 * Nécessite Ollama installé localement avec le modèle LLaVA.
 * Installation :
 *   1. brew install ollama   (ou https://ollama.com/download)
 *   2. ollama pull llava:7b
 *   3. ollama serve
 *
 * Toutes les données restent sur votre machine.
 */
@Injectable()
export class OllamaVisionProvider implements OCRProvider {
    private readonly logger = new Logger(OllamaVisionProvider.name);
    private readonly ollamaUrl: string;
    private readonly model: string;
    private isAvailable: boolean | null = null;

    constructor(private configService: ConfigService) {
        this.ollamaUrl = this.configService.get<string>('OLLAMA_URL') || 'http://localhost:11434';
        this.model = this.configService.get<string>('OLLAMA_VISION_MODEL') || 'llava:7b';
    }

    /**
     * Vérifie si Ollama est disponible et le modèle chargé
     */
    async checkAvailability(): Promise<boolean> {
        try {
            const response = await fetch(`${this.ollamaUrl}/api/tags`, {
                signal: AbortSignal.timeout(3000) // Timeout 3s
            });

            if (!response.ok) {
                this.isAvailable = false;
                return false;
            }

            const data: any = await response.json();
            const models = data.models || [];
            const hasVisionModel = models.some((m: any) =>
                m.name.includes('llava') || m.name.includes('llama') || m.name.includes(this.model.split(':')[0])
            );

            this.isAvailable = !!hasVisionModel;

            if (!hasVisionModel) {
                this.logger.warn(`[Ollama] Modèle Vision non trouvé. Modèles disponibles: ${models.map((m: any) => m.name).join(', ')}`);
                this.logger.warn(`[Ollama] Installez-le: ollama pull ${this.model}`);
            } else {
                this.logger.log(`[Ollama] 🟢 Vision locale disponible (${this.model})`);
            }

            return this.isAvailable;
        } catch (error: any) {
            this.isAvailable = false;
            this.logger.warn(`[Ollama] ⚠️ Non disponible (${error.message}). L'OCR utilisera Tesseract seul.`);
            return false;
        }
    }

    /**
     * Retourne si Ollama est disponible (avec cache)
     */
    async getAvailability(): Promise<boolean> {
        if (this.isAvailable === null) {
            return this.checkAvailability();
        }
        return this.isAvailable;
    }

    async analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult> {
        if (!await this.getAvailability()) {
            throw new Error('Ollama Vision non disponible');
        }

        const base64Image = buffer.toString('base64');

        const prompt = `Tu es un expert en analyse de documents administratifs français.
Analyse cette image de document et réponds UNIQUEMENT en JSON valide avec cette structure exacte :
{
  "status": "VALID" ou "REJECTED_BLURRY" ou "REJECTED_INCOMPLETE" ou "REJECTED_WRONG_TYPE" ou "REJECTED_EXPIRED",
  "confidence": nombre entre 0 et 100,
  "message": "explication concise en français",
  "extractedData": {
    "firstName": "prénom ou null",
    "lastName": "NOM ou null",
    "expiryDate": "YYYY-MM-DD ou null",
    "documentType": "type du document ou null",
    "documentNumber": "numéro ou null",
    "nationality": "nationalité ou null"
  }
}

Règles :
- Si le document est flou ou illisible → REJECTED_BLURRY
- Si le document est tronqué ou incomplet → REJECTED_INCOMPLETE
- Si la date d'expiration est passée → REJECTED_EXPIRED
- Si le document est lisible avec les informations principales → VALID
- La confiance doit refléter la qualité de l'extraction`;

        try {
            this.logger.log(`[Ollama] 🔍 Analyse en cours avec ${this.model}...`);

            const response = await fetch(`${this.ollamaUrl}/api/generate`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    model: this.model,
                    prompt,
                    images: [base64Image],
                    stream: false,
                    options: {
                        temperature: 0.1, // Réponse déterministe
                        num_predict: 500,
                    }
                }),
                signal: AbortSignal.timeout(30000) // Timeout 30s
            });

            if (!response.ok) {
                throw new Error(`Ollama HTTP ${response.status}: ${response.statusText}`);
            }

            const result: any = await response.json();
            const content = result.response || '';

            // Extraire le JSON de la réponse
            const jsonMatch = content.match(/\{[\s\S]*\}/);
            if (!jsonMatch) {
                throw new Error('Pas de JSON dans la réponse Ollama');
            }

            const parsed = JSON.parse(jsonMatch[0]) as OCRResult;
            this.logger.log(`[Ollama] ✅ Résultat: ${parsed.status} (${parsed.confidence}%) — ${parsed.message}`);

            return parsed;
        } catch (error: any) {
            this.logger.error(`[Ollama] Erreur: ${error.message}`);

            // Marquer comme indisponible si c'est une erreur de connexion
            if (error.message?.includes('fetch') || error.message?.includes('ECONNREFUSED')) {
                this.isAvailable = false;
            }

            throw error;
        }
    }

    /**
     * Force un re-check de disponibilité
     */
    async refresh(): Promise<void> {
        this.isAvailable = null;
        await this.checkAvailability();
    }
}
