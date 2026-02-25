import { Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';

export interface TextIntentResult {
    urngecy: 'LOW' | 'MEDIUM' | 'HIGH';
    intent: string;
    summary: string;
    actionable: boolean;
    reasoning: string;
}

@Injectable()
export class OllamaTextService {
    private readonly logger = new Logger(OllamaTextService.name);
    private readonly ollamaUrl: string;
    private readonly model: string;
    private isAvailable: boolean | null = null;

    constructor(private configService: ConfigService) {
        this.ollamaUrl = this.configService.get<string>('OLLAMA_URL') || 'http://localhost:11434';
        this.model = this.configService.get<string>('OLLAMA_TEXT_MODEL') || 'mistral:7b';
    }

    async checkAvailability(): Promise<boolean> {
        try {
            const response = await fetch(`${this.ollamaUrl}/api/tags`, {
                signal: AbortSignal.timeout(3000)
            });

            if (!response.ok) return false;

            const data: any = await response.json();
            const models = data.models || [];
            this.isAvailable = models.some((m: any) => m.name.includes(this.model.split(':')[0]));
            return !!this.isAvailable;
        } catch {
            this.isAvailable = false;
            return false;
        }
    }

    async analyzeCustomerMessage(message: string, context?: string): Promise<TextIntentResult | null> {
        if (this.isAvailable === false || !(this.isAvailable ?? await this.checkAvailability())) {
            return null; // Silent skip if no local AI
        }

        const prompt = `Tu es un expert en relation client et juridique. Analyse ce message entrant envoyé par un client sur WhatsApp.
${context ? `Contexte: ${context}` : ''}
Message du client :
"${message}"

Réponds UNIQUEMENT via un objet JSON valide, sans texte additionnel, avec cette structure :
{
  "urgency": "LOW" | "MEDIUM" | "HIGH",
  "intent": "le motif principal (ex: question de suivi, colère, demande d'information, urgence préfecture)",
  "summary": "Résumé ultra concis du message en 1 ligne",
  "actionable": boolean (vrai si cela nécessite une réponse juridique ou commerciale, faux si c'est un simple merci ou confirmation basique),
  "reasoning": "Pourquoi et quel conseil donner au juriste"
}`;

        try {
            const response = await fetch(`${this.ollamaUrl}/api/generate`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    model: this.model,
                    prompt: prompt,
                    stream: false,
                    options: { temperature: 0.1, num_predict: 250 },
                    format: "json" // Mode strict JSON
                }),
                signal: AbortSignal.timeout(15000)
            });

            if (!response.ok) throw new Error('Ollama connection failed');

            const result: any = await response.json();
            const parsed = JSON.parse(result.response);

            return {
                urngecy: parsed.urgency || parsed.urngecy || 'LOW',
                intent: parsed.intent || 'Message client',
                summary: parsed.summary || message.slice(0, 50),
                actionable: parsed.actionable ?? true,
                reasoning: parsed.reasoning || ''
            };
        } catch (error: any) {
            this.logger.warn(`[OllamaText] Failed to analyze message: ${error.message}`);
            return null; // Fail gracefully
        }
    }
}
