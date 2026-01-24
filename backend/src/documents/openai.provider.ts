import { Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import OpenAI from 'openai';
import { OCRProvider, OCRResult } from './ocr.interface';

@Injectable()
export class OpenAIProvider implements OCRProvider {
    private readonly logger = new Logger(OpenAIProvider.name);
    private readonly openai: OpenAI;

    constructor(private configService: ConfigService) {
        const apiKey = this.configService.get<string>('OPENAI_API_KEY');
        if (apiKey) {
            this.openai = new OpenAI({ apiKey });
        } else {
            this.logger.warn('OPENAI_API_KEY is not set. Real OCR will be disabled.');
        }
    }

    async analyzeImage(buffer: Buffer, mimetype: string): Promise<OCRResult> {
        if (!this.openai) {
            throw new Error('OCR Provider not configured (Missing API Key)');
        }
        const base64Image = buffer.toString('base64');
        const dataUrl = `data:${mimetype};base64,${base64Image}`;

        try {
            const response = await this.openai.chat.completions.create({
                model: "gpt-4o-mini",
                messages: [
                    {
                        role: "system",
                        content: `Vous êtes un expert en analyse de documents d'identité français et étrangers. 
                        Analysez l'image fournie et extrayez les informations de manière structurée. 
                        Répondez UNIQUEMENT en format JSON avec les champs suivants :
                        {
                          "status": "VALID" | "REJECTED_BLURRY" | "REJECTED_INCOMPLETE" | "REJECTED_WRONG_TYPE" | "REJECTED_EXPIRED",
                          "confidence": number (0-100),
                          "message": "explication concise",
                          "extractedData": {
                            "firstName": "prénom",
                            "lastName": "NOM",
                            "expiryDate": "YYYY-MM-DD",
                            "documentType": "Titre de Séjour" | "Passeport" | "Autre",
                            "documentNumber": "numéro",
                            "nationality": "nationalité"
                          }
                        }`
                    },
                    {
                        role: "user",
                        content: [
                            { type: "text", text: "Analysez ce document d'identité." },
                            {
                                type: "image_url",
                                image_url: { url: dataUrl }
                            }
                        ],
                    },
                ],
                response_format: { type: "json_object" }
            });

            const content = response.choices[0].message.content;
            if (!content) throw new Error('OpenAI returned empty content');

            const result = JSON.parse(content) as OCRResult;
            this.logger.log(`OCR Result: ${result.status} (${result.confidence}%)`);
            return result;

        } catch (error) {
            this.logger.error('OpenAI Vision API Error', error);
            throw new Error('Échec de l\'analyse OCR du document.');
        }
    }
}
