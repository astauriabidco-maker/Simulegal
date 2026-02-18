import { Controller, Get, Post, Put, Delete, Param, Body } from '@nestjs/common';
import { VeilleService } from './veille.service';

@Controller('veille')
export class VeilleController {
    constructor(private readonly veilleService: VeilleService) { }

    @Get()
    findAll() {
        return this.veilleService.findAll();
    }

    @Get('pending')
    findPending() {
        return this.veilleService.findPending();
    }

    @Get('stats')
    getStats() {
        return this.veilleService.getStats();
    }

    @Post()
    create(@Body() body: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
    }) {
        return this.veilleService.create(body);
    }

    @Put(':id')
    update(@Param('id') id: string, @Body() body: any) {
        return this.veilleService.update(id, body);
    }

    @Put(':id/apply')
    markAsApplied(@Param('id') id: string) {
        return this.veilleService.markAsApplied(id);
    }

    @Delete(':id')
    remove(@Param('id') id: string) {
        return this.veilleService.remove(id);
    }
}
