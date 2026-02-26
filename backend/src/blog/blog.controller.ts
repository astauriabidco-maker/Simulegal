import {
    Controller, Get, Post, Put, Delete, Patch,
    Param, Body, Query, UseGuards, Header, Req, Ip,
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
        @Query('search') search?: string,
        @Query('tag') tag?: string,
    ) {
        return this.blogService.findPublished(
            category,
            limit ? parseInt(limit) : 20,
            offset ? parseInt(offset) : 0,
            search,
            tag,
        );
    }

    @Get('public/blog/featured')
    async getFeatured() {
        return this.blogService.findFeatured();
    }

    @Get('public/blog/rss')
    @Header('Content-Type', 'application/rss+xml; charset=utf-8')
    async getRssFeed() {
        const baseUrl = process.env.PUBLIC_URL || 'https://simulegal.fr';
        return this.blogService.generateRssFeed(baseUrl);
    }

    @Get('public/blog/sitemap.xml')
    @Header('Content-Type', 'application/xml; charset=utf-8')
    async getSitemap() {
        const baseUrl = process.env.PUBLIC_URL || 'https://simulegal.fr';
        return this.blogService.generateSitemap(baseUrl);
    }

    @Get('public/blog/:slug')
    async getBySlug(@Param('slug') slug: string, @Ip() ip: string) {
        return this.blogService.findBySlug(slug, ip);
    }

    // ═══════════════════════════════════════════
    // PUBLIC COMMENTS
    // ═══════════════════════════════════════════

    @Post('public/blog/:slug/comments')
    async addComment(
        @Param('slug') slug: string,
        @Body() data: {
            authorName: string;
            authorEmail?: string;
            content: string;
            parentId?: string;
            honeypot?: string;
        },
    ) {
        return this.blogService.addComment(slug, data);
    }

    // ═══════════════════════════════════════════
    // PUBLIC REACTIONS
    // ═══════════════════════════════════════════

    @Post('public/blog/:articleId/reactions')
    async addReaction(
        @Param('articleId') articleId: string,
        @Body() data: { type: string },
        @Ip() ip: string,
    ) {
        return this.blogService.addReaction(articleId, data.type, ip);
    }

    @Get('public/blog/:articleId/reactions')
    async getReactions(@Param('articleId') articleId: string) {
        return this.blogService.getReactionCounts(articleId);
    }

    // ═══════════════════════════════════════════
    // NEWSLETTER (public)
    // ═══════════════════════════════════════════

    @Post('public/newsletter/subscribe')
    async subscribe(@Body() data: { email: string; name?: string }) {
        return this.blogService.subscribe(data.email, data.name);
    }

    @Get('public/newsletter/unsubscribe/:token')
    async unsubscribe(@Param('token') token: string) {
        return this.blogService.unsubscribe(token);
    }

    // ═══════════════════════════════════════════
    // ADMIN ENDPOINTS
    // Read = no auth (admin layout handles UI protection)
    // Write = JWT protected
    // ═══════════════════════════════════════════

    @Get('blog')
    async adminList(@Query('status') status?: string) {
        return this.blogService.findAll(status);
    }

    @Get('blog/comments/pending')
    async getPendingComments() {
        return this.blogService.getPendingComments();
    }

    @Get('blog/analytics/dashboard')
    async getDashboardAnalytics(@Query('days') days?: string) {
        return this.blogService.getDashboardAnalytics(days ? parseInt(days) : 30);
    }

    @Get('blog/analytics/:id')
    async getArticleAnalytics(
        @Param('id') id: string,
        @Query('days') days?: string,
    ) {
        return this.blogService.getArticleAnalytics(id, days ? parseInt(days) : 30);
    }

    @Get('blog/newsletter/subscribers')
    async getSubscribers() {
        return this.blogService.getSubscribers();
    }

    @Get('blog/newsletter/stats')
    async getSubscriberStats() {
        return this.blogService.getSubscriberStats();
    }

    @Get('blog/categories')
    async getCategories() {
        return this.blogService.getCategories();
    }

    @Get('blog/:id')
    async adminGet(@Param('id') id: string) {
        return this.blogService.findOne(id);
    }

    @Get('blog/:id/revisions')
    async getRevisions(@Param('id') id: string) {
        return this.blogService.getRevisions(id);
    }

    @Get('blog/revisions/:revisionId')
    async getRevision(@Param('revisionId') revisionId: string) {
        return this.blogService.getRevision(revisionId);
    }

    // ── Write endpoints (JWT protected) ──

    @UseGuards(JwtAuthGuard)
    @Post('blog')
    async create(@Body() data: {
        title: string; slug?: string; excerpt?: string; content: string;
        category?: string; status?: string; coverImage?: string;
        authorName?: string; authorRole?: string; tags?: string;
        metaTitle?: string; metaDescription?: string;
        readTimeMin?: number; featured?: boolean; scheduledAt?: string;
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

    @UseGuards(JwtAuthGuard)
    @Post('blog/:id/revisions/:revisionId/restore')
    async restoreRevision(
        @Param('id') id: string,
        @Param('revisionId') revisionId: string,
    ) {
        return this.blogService.restoreRevision(id, revisionId);
    }

    // ── Comments moderation ──

    @UseGuards(JwtAuthGuard)
    @Patch('blog/comments/:commentId')
    async moderateComment(
        @Param('commentId') commentId: string,
        @Body() data: { status: 'APPROVED' | 'REJECTED' },
    ) {
        return this.blogService.moderateComment(commentId, data.status);
    }

    @UseGuards(JwtAuthGuard)
    @Delete('blog/comments/:commentId')
    async deleteComment(@Param('commentId') commentId: string) {
        return this.blogService.deleteComment(commentId);
    }

    // ── Categories CRUD ──

    @UseGuards(JwtAuthGuard)
    @Post('blog/categories')
    async createCategory(@Body() data: { name: string; icon?: string; color?: string }) {
        return this.blogService.createCategory(data);
    }

    @UseGuards(JwtAuthGuard)
    @Put('blog/categories/:id')
    async updateCategory(
        @Param('id') id: string,
        @Body() data: { name?: string; icon?: string; color?: string; sortOrder?: number },
    ) {
        return this.blogService.updateCategory(id, data);
    }

    @UseGuards(JwtAuthGuard)
    @Delete('blog/categories/:id')
    async deleteCategory(@Param('id') id: string) {
        return this.blogService.deleteCategory(id);
    }

    // ── Export / Import ──

    @Get('blog/export/:format')
    async exportArticles(@Param('format') format: 'json' | 'markdown') {
        return this.blogService.exportArticles(format);
    }

    @UseGuards(JwtAuthGuard)
    @Post('blog/import')
    async importArticle(@Body() data: {
        title: string; content: string; slug?: string;
        category?: string; status?: string; tags?: string;
    }) {
        return this.blogService.importArticle(data);
    }
}
