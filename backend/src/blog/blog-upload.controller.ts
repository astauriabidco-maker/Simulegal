import {
    Controller, Post, UseGuards, UseInterceptors,
    UploadedFile, BadRequestException, Get, Param, Res,
} from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { diskStorage } from 'multer';
import { extname, join } from 'path';
import { existsSync, mkdirSync, readdirSync, statSync, unlinkSync } from 'fs';
import { Response } from 'express';

const UPLOAD_DIR = join(process.cwd(), 'uploads', 'blog');

// Ensure upload directory exists
if (!existsSync(UPLOAD_DIR)) {
    mkdirSync(UPLOAD_DIR, { recursive: true });
}

const storage = diskStorage({
    destination: (_req, _file, cb) => cb(null, UPLOAD_DIR),
    filename: (_req, file, cb) => {
        const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1e6);
        const ext = extname(file.originalname).toLowerCase();
        cb(null, `blog-${uniqueSuffix}${ext}`);
    },
});

const fileFilter = (_req: any, file: Express.Multer.File, cb: any) => {
    const allowed = ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.svg'];
    const ext = extname(file.originalname).toLowerCase();
    if (allowed.includes(ext)) {
        cb(null, true);
    } else {
        cb(new BadRequestException(`Format non supporté: ${ext}. Formats acceptés: ${allowed.join(', ')}`), false);
    }
};

@Controller('blog/upload')
export class BlogUploadController {

    @UseGuards(JwtAuthGuard)
    @Post()
    @UseInterceptors(FileInterceptor('file', {
        storage,
        fileFilter,
        limits: { fileSize: 5 * 1024 * 1024 }, // 5MB max
    }))
    async uploadImage(@UploadedFile() file: Express.Multer.File) {
        if (!file) throw new BadRequestException('Aucun fichier fourni');

        const baseUrl = process.env.PUBLIC_URL || 'http://localhost:4000';
        const url = `${baseUrl}/blog/upload/${file.filename}`;

        return {
            url,
            filename: file.filename,
            originalName: file.originalname,
            size: file.size,
            mimeType: file.mimetype,
        };
    }

    @Get(':filename')
    async serveImage(@Param('filename') filename: string, @Res() res: Response) {
        // Sanitize filename to prevent directory traversal
        const safeName = filename.replace(/[^a-zA-Z0-9._-]/g, '');
        const filePath = join(UPLOAD_DIR, safeName);

        if (!existsSync(filePath)) {
            return res.status(404).json({ message: 'Image non trouvée' });
        }

        return res.sendFile(filePath);
    }

    @UseGuards(JwtAuthGuard)
    @Get('list/all')
    async listImages() {
        if (!existsSync(UPLOAD_DIR)) return [];

        const files = readdirSync(UPLOAD_DIR)
            .filter(f => /\.(jpg|jpeg|png|gif|webp|svg)$/i.test(f))
            .map(filename => {
                const filePath = join(UPLOAD_DIR, filename);
                const stats = statSync(filePath);
                const baseUrl = process.env.PUBLIC_URL || 'http://localhost:4000';
                return {
                    filename,
                    url: `${baseUrl}/blog/upload/${filename}`,
                    size: stats.size,
                    createdAt: stats.birthtime,
                };
            })
            .sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());

        return files;
    }
}
