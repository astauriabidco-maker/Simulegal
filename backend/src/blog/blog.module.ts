import { Module } from '@nestjs/common';
import { BlogController } from './blog.controller';
import { BlogUploadController } from './blog-upload.controller';
import { BlogAutoController } from './blog-auto.controller';
import { BlogService } from './blog.service';
import { BlogAutoService } from './blog-auto.service';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule],
    controllers: [BlogController, BlogUploadController, BlogAutoController],
    providers: [BlogService, BlogAutoService],
    exports: [BlogService, BlogAutoService],
})
export class BlogModule { }
