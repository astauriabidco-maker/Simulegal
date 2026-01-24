"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DocumentsModule = void 0;
const common_1 = require("@nestjs/common");
const config_1 = require("@nestjs/config");
const documents_controller_1 = require("./documents.controller");
const documents_service_1 = require("./documents.service");
const openai_provider_1 = require("./openai.provider");
const ocr_interface_1 = require("./ocr.interface");
let DocumentsModule = class DocumentsModule {
};
exports.DocumentsModule = DocumentsModule;
exports.DocumentsModule = DocumentsModule = __decorate([
    (0, common_1.Module)({
        imports: [config_1.ConfigModule],
        controllers: [documents_controller_1.DocumentsController],
        providers: [
            documents_service_1.DocumentsService,
            openai_provider_1.OpenAIProvider,
            {
                provide: ocr_interface_1.OCR_PROVIDER_TOKEN,
                useExisting: openai_provider_1.OpenAIProvider
            }
        ],
        exports: [documents_service_1.DocumentsService]
    })
], DocumentsModule);
//# sourceMappingURL=documents.module.js.map