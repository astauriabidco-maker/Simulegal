import {
    Controller,
    Get,
    Post,
    Put,
    Delete,
    Param,
    Body,
    Query,
    UseGuards,
} from '@nestjs/common';
import { BlogService } from './blog.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';

@Controller()
export class BlogController {
    constructor(private readonly blogService: BlogService) { }

    // ═══════════════════════════════════════════
    // PUBLIC ENDPOINTS (no auth)
    // ═══════════════════════════════════════════

    @Get('public/blog')
    async getPublished(
        @Query('category') category?: string,
        @Query('limit') limit?: string,
        @Query('offset') offset?: string,
    ) {
        return this.blogService.findPublished(
            category,
            limit ? parseInt(limit) : 20,
            offset ? parseInt(offset) : 0,
        );
    }

    @Get('public/blog/featured')
    async getFeatured() {
        return this.blogService.findFeatured();
    }

    @Get('public/blog/:slug')
    async getBySlug(@Param('slug') slug: string) {
        return this.blogService.findBySlug(slug);
    }

    // ═══════════════════════════════════════════
    // ADMIN ENDPOINTS (JWT protected)
    // ═══════════════════════════════════════════

    @UseGuards(JwtAuthGuard)
    @Get('blog')
    async adminList(@Query('status') status?: string) {
        return this.blogService.findAll(status);
    }

    @UseGuards(JwtAuthGuard)
    @Get('blog/:id')
    async adminGet(@Param('id') id: string) {
        return this.blogService.findOne(id);
    }

    @UseGuards(JwtAuthGuard)
    @Post('blog')
    async create(@Body() data: {
        title: string;
        slug?: string;
        excerpt?: string;
        content: string;
        category?: string;
        status?: string;
        coverImage?: string;
        authorName?: string;
        authorRole?: string;
        tags?: string;
        metaTitle?: string;
        metaDescription?: string;
        readTimeMin?: number;
        featured?: boolean;
    }) {
        return this.blogService.create(data);
    }

    @UseGuards(JwtAuthGuard)
    @Put('blog/:id')
    async update(@Param('id') id: string, @Body() data: Record<string, unknown>) {
        return this.blogService.update(id, data);
    }

    @UseGuards(JwtAuthGuard)
    @Delete('blog/:id')
    async remove(@Param('id') id: string) {
        return this.blogService.remove(id);
    }
}
