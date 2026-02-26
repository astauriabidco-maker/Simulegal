import {
    Controller, Get, Post, Put, Delete, Param, Body, Query, UseGuards,
} from '@nestjs/common';
import { BlogAutoService } from './blog-auto.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';

@Controller('blog/auto')
export class BlogAutoController {
    constructor(private readonly autoService: BlogAutoService) { }

    // ═══════════════════════════════════════════
    // Topics
    // ═══════════════════════════════════════════

    @Get('topics')
    async getTopics(@Query('status') status?: string) {
        return this.autoService.getTopics(status);
    }

    @Get('topics/:id')
    async getTopic(@Param('id') id: string) {
        return this.autoService.getTopicById(id);
    }

    @UseGuards(JwtAuthGuard)
    @Post('topics/:id/reject')
    async rejectTopic(@Param('id') id: string) {
        return this.autoService.rejectTopic(id);
    }

    @UseGuards(JwtAuthGuard)
    @Post('topics/:id/retry')
    async retryTopic(@Param('id') id: string) {
        return this.autoService.retryTopic(id);
    }

    // ═══════════════════════════════════════════
    // Configuration
    // ═══════════════════════════════════════════

    @Get('config')
    async getConfig() {
        return this.autoService.getOrCreateConfig();
    }

    @UseGuards(JwtAuthGuard)
    @Put('config')
    async updateConfig(@Body() data: {
        enabled?: boolean;
        frequency?: string;
        maxArticlesPerRun?: number;
        minRelevanceScore?: number;
        targetCategories?: string;
        aiModel?: string;
        aiPromptTemplate?: string;
    }) {
        return this.autoService.updateConfig(data);
    }

    // ═══════════════════════════════════════════
    // Stats & Manual Trigger
    // ═══════════════════════════════════════════

    @Get('stats')
    async getStats() {
        return this.autoService.getStats();
    }

    @UseGuards(JwtAuthGuard)
    @Post('trigger')
    async trigger() {
        return this.autoService.triggerDiscoveryAndGeneration();
    }

    @UseGuards(JwtAuthGuard)
    @Post('discover')
    async discover() {
        return this.autoService.discoverTopics();
    }

    @UseGuards(JwtAuthGuard)
    @Post('generate')
    async generate() {
        return this.autoService.generateDrafts();
    }
}
