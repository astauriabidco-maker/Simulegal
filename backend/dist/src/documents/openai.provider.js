"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
var OpenAIProvider_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.OpenAIProvider = void 0;
const common_1 = require("@nestjs/common");
const config_1 = require("@nestjs/config");
const openai_1 = __importDefault(require("openai"));
let OpenAIProvider = OpenAIProvider_1 = class OpenAIProvider {
    configService;
    logger = new common_1.Logger(OpenAIProvider_1.name);
    openai;
    constructor(configService) {
        this.configService = configService;
        const apiKey = this.configService.get('OPENAI_API_KEY');
        if (apiKey) {
            this.openai = new openai_1.default({ apiKey });
        }
        else {
            this.logger.warn('OPENAI_API_KEY is not set. Real OCR will be disabled.');
        }
    }
    async analyzeImage(buffer, mimetype) {
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
            if (!content)
                throw new Error('OpenAI returned empty content');
            const result = JSON.parse(content);
            this.logger.log(`OCR Result: ${result.status} (${result.confidence}%)`);
            return result;
        }
        catch (error) {
            this.logger.error('OpenAI Vision API Error', error);
            throw new Error('Échec de l\'analyse OCR du document.');
        }
    }
};
exports.OpenAIProvider = OpenAIProvider;
exports.OpenAIProvider = OpenAIProvider = OpenAIProvider_1 = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [config_1.ConfigService])
], OpenAIProvider);
//# sourceMappingURL=openai.provider.js.map