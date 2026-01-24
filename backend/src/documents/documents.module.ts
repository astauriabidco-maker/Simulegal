import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { DocumentsController } from './documents.controller';
import { DocumentsService } from './documents.service';
import { OpenAIProvider } from './openai.provider';
import { OCR_PROVIDER_TOKEN } from './ocr.interface';

@Module({
    imports: [ConfigModule],
    controllers: [DocumentsController],
    providers: [
        DocumentsService,
        OpenAIProvider,
        {
            provide: OCR_PROVIDER_TOKEN,
            useExisting: OpenAIProvider
        }
    ],
    exports: [DocumentsService]
})
export class DocumentsModule { }
