import { Module } from '@nestjs/common';
import { BlogController } from './blog.controller';
import { BlogUploadController } from './blog-upload.controller';
import { BlogService } from './blog.service';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule],
    controllers: [BlogController, BlogUploadController],
    providers: [BlogService],
    exports: [BlogService],
})
export class BlogModule { }
