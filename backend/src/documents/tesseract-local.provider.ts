import { Injectable, Logger } from '@nestjs/common';
import { OCRProvider, OCRResult, ExtractedData } from './ocr.interface';
import Tesseract from 'tesseract.js';
import sharp from 'sharp';

/**
 * 🔒 Provider OCR 100% local — Tesseract.js + Sharp
 *
 * Aucune donnée ne quitte le serveur.
 *
 * Pipeline :
 *   1. Sharp  → Analyse qualité image (flou, résolution, format)
 *   2. Tesseract → Extraction texte (OCR)
 *   3. Règles métier → Parsing dates, noms, numéros
 */
@Injectable()
export class TesseractLocalProvider implements OCRProvider {
    private readonly logger = new Logger(TesseractLocalProvider.name);
    private worker: Tesseract.Worker | null = null;

    async analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult> {
        // ════════════════════════════════════════════
        // 1. ANALYSE QUALITÉ IMAGE (Sharp)
        // ════════════════════════════════════════════
        const quality = await this.analyzeImageQuality(buffer);

        if (!quality.isReadable) {
            return {
                status: 'REJECTED_BLURRY',
                confidence: quality.sharpnessScore,
                message: quality.reason || 'Image trop floue ou de mauvaise qualité.',
            };
        }

        // ════════════════════════════════════════════
        // 2. OCR — EXTRACTION TEXTE (Tesseract.js)
        // ════════════════════════════════════════════
        const ocrText = await this.extractText(buffer);

        if (!ocrText || ocrText.trim().length < 10) {
            return {
                status: 'REJECTED_INCOMPLETE',
                confidence: 10,
                message: 'Aucun texte lisible détecté. Merci de capturer l\'intégralité du document.',
            };
        }

        // ════════════════════════════════════════════
        // 3. RÈGLES MÉTIER — Parsing intelligent
        // ════════════════════════════════════════════
        const extracted = this.parseDocumentData(ocrText);
        const confidence = this.computeConfidence(ocrText, extracted, quality);

        // Vérifier la date d'expiration
        if (extracted.expiryDate) {
            const expiry = new Date(extracted.expiryDate);
            if (!isNaN(expiry.getTime()) && expiry < new Date()) {
                return {
                    status: 'REJECTED_EXPIRED',
                    confidence,
                    message: `Document expiré le ${expiry.toLocaleDateString('fr-FR')}.`,
                    extractedData: extracted,
                };
            }
        }

        // Détection du type de document
        const docType = this.detectDocumentType(ocrText);
        if (docType) {
            extracted.documentType = docType;
        }

        return {
            status: 'VALID',
            confidence,
            message: `Document analysé localement (confiance: ${confidence}%).`,
            extractedData: extracted,
        };
    }

    // ═══════════════════════════════════════════════════
    //  SHARP — Qualité d'image
    // ═══════════════════════════════════════════════════
    private async analyzeImageQuality(buffer: Buffer): Promise<{
        isReadable: boolean;
        sharpnessScore: number;
        width: number;
        height: number;
        reason?: string;
    }> {
        try {
            const metadata = await sharp(buffer).metadata();
            const width = metadata.width || 0;
            const height = metadata.height || 0;

            // Résolution trop faible
            if (width < 300 || height < 200) {
                return {
                    isReadable: false,
                    sharpnessScore: 5,
                    width, height,
                    reason: `Résolution trop faible (${width}x${height}). Minimum requis: 300x200.`
                };
            }

            // Analyse de netteté via la variance du Laplacien (approximation)
            // On convertit en niveaux de gris et on mesure la variance des pixels
            const { data, info } = await sharp(buffer)
                .greyscale()
                .resize(400, 400, { fit: 'inside' })
                .raw()
                .toBuffer({ resolveWithObject: true });

            let sum = 0;
            let sumSq = 0;
            for (let i = 0; i < data.length; i++) {
                sum += data[i];
                sumSq += data[i] * data[i];
            }
            const mean = sum / data.length;
            const variance = (sumSq / data.length) - (mean * mean);

            // Variance faible = image fade/floue (seuil empirique)
            const sharpnessScore = Math.min(100, Math.round(variance / 20));

            if (sharpnessScore < 15) {
                return {
                    isReadable: false,
                    sharpnessScore,
                    width, height,
                    reason: `Image trop floue ou trop sombre (netteté: ${sharpnessScore}%).`
                };
            }

            return { isReadable: true, sharpnessScore, width, height };
        } catch (error: any) {
            this.logger.warn(`[Sharp] Erreur analyse qualité: ${error.message}`);
            // En cas d'erreur, on laisse passer — Tesseract jugera
            return { isReadable: true, sharpnessScore: 50, width: 0, height: 0 };
        }
    }

    // ═══════════════════════════════════════════════════
    //  TESSERACT — Extraction du texte
    // ═══════════════════════════════════════════════════
    private async extractText(buffer: Buffer): Promise<string> {
        try {
            // Pré-traitement de l'image pour améliorer l'OCR
            const processedBuffer = await sharp(buffer)
                .greyscale()             // Niveaux de gris
                .normalise()             // Normaliser le contraste
                .sharpen({ sigma: 1.5 }) // Accentuer la netteté
                .toBuffer();

            const result = await Tesseract.recognize(processedBuffer, 'fra+eng', {
                logger: (m) => {
                    if (m.status === 'recognizing text') {
                        // Log silencieux — trop verbeux
                    }
                },
            });

            this.logger.log(`[Tesseract] OCR terminé: ${result.data.text.length} caractères extraits (confiance moy: ${Math.round(result.data.confidence)}%)`);
            return result.data.text;
        } catch (error: any) {
            this.logger.error(`[Tesseract] Erreur OCR: ${error.message}`);
            return '';
        }
    }

    // ═══════════════════════════════════════════════════
    //  PARSING — Extraction structurée
    // ═══════════════════════════════════════════════════
    private parseDocumentData(text: string): ExtractedData {
        const data: ExtractedData = {};

        // ── Dates (format JJ/MM/AAAA, JJ-MM-AAAA, JJ.MM.AAAA, AAAA-MM-JJ) ──
        const datePatterns = [
            /(?:expir|valid|échéance|fin de validité|date d'expiration)[^0-9]*(\d{2}[\/\-\.]\d{2}[\/\-\.]\d{4})/i,
            /(\d{2}[\/\-\.]\d{2}[\/\-\.]\d{4})\s*(?:\n|$)/g,
            /(\d{4}-\d{2}-\d{2})/g,
        ];

        // Chercher une date d'expiration explicite
        const expiryMatch = text.match(datePatterns[0]);
        if (expiryMatch) {
            data.expiryDate = this.parseDate(expiryMatch[1]);
        } else {
            // Sinon, prendre la dernière date trouvée (souvent l'expiration)
            const allDates = [...text.matchAll(/(\d{2}[\/\-\.]\d{2}[\/\-\.]\d{4})/g)];
            if (allDates.length > 0) {
                const lastDate = allDates[allDates.length - 1][1];
                const parsed = this.parseDate(lastDate);
                // Ne garder que si c'est dans le futur ou passé récent (< 10 ans)
                if (parsed) {
                    const dt = new Date(parsed);
                    const tenYearsAgo = new Date();
                    tenYearsAgo.setFullYear(tenYearsAgo.getFullYear() - 10);
                    if (dt > tenYearsAgo) {
                        data.expiryDate = parsed;
                    }
                }
            }
        }

        // ── Nom / Prénom ──
        const nomMatch = text.match(/(?:nom|surname|name)[:\s]*([A-ZÀ-Ü]{2,}(?:\s+[A-ZÀ-Ü]{2,})*)/i);
        if (nomMatch) {
            data.lastName = nomMatch[1].trim();
        }

        const prenomMatch = text.match(/(?:prénom|prénoms|given name|first name)[:\s]*([A-ZÀ-Üa-zà-ü]{2,}(?:\s+[A-ZÀ-Üa-zà-ü]{2,})*)/i);
        if (prenomMatch) {
            data.firstName = prenomMatch[1].trim();
        }

        // ── Numéro de document ──
        const numMatch = text.match(/(?:n°|no\.|numéro|number|document)[:\s]*([A-Z0-9]{6,})/i);
        if (numMatch) {
            data.documentNumber = numMatch[1].trim();
        }

        // ── Nationalité ──
        const natMatch = text.match(/(?:nationalit|nationality)[éy]?[:\s]*([A-Za-zÀ-ÿ]+)/i);
        if (natMatch) {
            data.nationality = natMatch[1].trim();
        }

        return data;
    }

    private parseDate(dateStr: string): string | undefined {
        // JJ/MM/AAAA → YYYY-MM-DD
        const match = dateStr.match(/(\d{2})[\/\-\.](\d{2})[\/\-\.](\d{4})/);
        if (match) {
            const [_, day, month, year] = match;
            return `${year}-${month}-${day}`;
        }
        // YYYY-MM-DD déjà
        if (/^\d{4}-\d{2}-\d{2}$/.test(dateStr)) return dateStr;
        return undefined;
    }

    // ═══════════════════════════════════════════════════
    //  DÉTECTION TYPE DE DOCUMENT
    // ═══════════════════════════════════════════════════
    private detectDocumentType(text: string): string | null {
        const t = text.toLowerCase();

        if (t.includes('passeport') || t.includes('passport')) return 'Passeport';
        if (t.includes('carte nationale') || t.includes("carte d'identité") || t.includes('identity card')) return "Carte d'identité";
        if (t.includes('titre de séjour') || t.includes('carte de séjour') || t.includes('residence permit')) return 'Titre de séjour';
        if (t.includes('récépissé') || t.includes('recepisse')) return 'Récépissé';
        if (t.includes('acte de naissance') || t.includes('birth certificate')) return 'Acte de naissance';
        if (t.includes('acte de mariage') || t.includes('marriage certificate')) return 'Acte de mariage';
        if (t.includes('certificat de nationalité')) return 'Certificat de nationalité';
        if (t.includes('justificatif de domicile') || t.includes('attestation de domicile')) return 'Justificatif de domicile';
        if (t.includes('quittance de loyer')) return 'Quittance de loyer';
        if (t.includes('avis d\'imposition') || t.includes('avis d\'imposition')) return "Avis d'imposition";
        if (t.includes('cerfa')) return 'Formulaire CERFA';
        if (t.includes('facture') || t.includes('invoice')) return 'Facture';
        if (t.includes('attestation')) return 'Attestation';

        return null;
    }

    // ═══════════════════════════════════════════════════
    //  SCORE DE CONFIANCE
    // ═══════════════════════════════════════════════════
    private computeConfidence(text: string, data: ExtractedData, quality: { sharpnessScore: number }): number {
        let score = 30; // Base

        // Qualité image
        score += Math.min(20, quality.sharpnessScore / 5);

        // Quantité de texte extrait
        if (text.length > 100) score += 10;
        if (text.length > 300) score += 5;

        // Données structurées trouvées
        if (data.lastName) score += 10;
        if (data.firstName) score += 5;
        if (data.expiryDate) score += 10;
        if (data.documentNumber) score += 5;
        if (data.documentType) score += 5;

        return Math.min(95, Math.round(score));
    }
}
