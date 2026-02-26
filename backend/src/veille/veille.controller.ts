import { Controller, Get, Post, Put, Delete, Param, Body, UseGuards, ForbiddenException, Request } from '@nestjs/common';
import { VeilleService } from './veille.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('veille')
@UseGuards(AuthGuard('jwt'))
export class VeilleController {
    constructor(private readonly veilleService: VeilleService) { }

    // ─── Lecture (tout utilisateur authentifié) ───────────────

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

    // ─── Scan automatique (admin / juristes) ─────────────────

    @Post('scan')
    async triggerScan(@Request() req: any) {
        this.requireWriteAccess(req);
        const result = await this.veilleService.scanSources();
        return {
            message: result.created > 0
                ? `${result.created} nouvelle(s) note(s) détectée(s) depuis ${result.sourcesUsed.join(', ')}`
                : 'Aucune nouvelle note détectée',
            ...result,
        };
    }

    // ─── Écriture (réservée au siège / juristes) ─────────────

    @Post()
    create(@Request() req: any, @Body() body: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
    }) {
        this.requireWriteAccess(req);
        return this.veilleService.create(body);
    }

    @Put(':id')
    update(@Request() req: any, @Param('id') id: string, @Body() body: any) {
        this.requireWriteAccess(req);
        return this.veilleService.update(id, body);
    }

    @Put(':id/apply')
    markAsApplied(@Request() req: any, @Param('id') id: string) {
        this.requireWriteAccess(req);
        return this.veilleService.markAsApplied(id);
    }

    @Delete(':id')
    remove(@Request() req: any, @Param('id') id: string) {
        this.requireWriteAccess(req);
        return this.veilleService.remove(id);
    }

    // ─── Helpers ──────────────────────────────────────────────

    private requireWriteAccess(req: any) {
        const writeRoles = ['SUPER_ADMIN', 'HQ_ADMIN', 'CASE_WORKER'];
        if (!writeRoles.includes(req.user.role)) {
            throw new ForbiddenException('Seuls les juristes et admins du siège peuvent modifier les notes de veille');
        }
    }
}
