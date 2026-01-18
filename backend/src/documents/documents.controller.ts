import { Controller, Post, UseInterceptors, UploadedFile, BadRequestException } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { DocumentsService } from './documents.service';

@Controller('documents')
export class DocumentsController {
    constructor(private readonly documentsService: DocumentsService) { }

    @Post('analyze')
    @UseInterceptors(FileInterceptor('file'))
    async analyzeFile(@UploadedFile() file: Express.Multer.File) {
        if (!file) throw new BadRequestException('File is required');
        return this.documentsService.analyze(file);
    }
}
