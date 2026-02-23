import { Controller, Get, Post, Body, Param, Patch, Res, UseGuards, Query } from '@nestjs/common';
import type { Response } from 'express';
import { FranchiseLeadsService } from './franchise-leads.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';
import { UserRole } from '@prisma/client';

@Controller('franchise-leads')
export class FranchiseLeadsController {
    constructor(private readonly franchiseLeadsService: FranchiseLeadsService) { }

    @Post()
    create(@Body() body: any) {
        // PUBLIC: Accessible from landing page
        return this.franchiseLeadsService.create(body);
    }

    // --- PROTECTED ROUTES (HQ ONLY) ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get()
    findAll() {
        return this.franchiseLeadsService.findAll();
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get(':id')
    findOne(@Param('id') id: string) {
        return this.franchiseLeadsService.findOne(id);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Patch(':id')
    update(@Param('id') id: string, @Body() body: any) {
        return this.franchiseLeadsService.update(id, body);
    }

    // --- VALIDATION SIRET ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Post('siret/validate')
    validateSiret(@Body() body: { siret: string }) {
        return this.franchiseLeadsService.validateSiret(body.siret);
    }

    // --- LOI DOUBIN: DIP (Document d'Information Précontractuelle) ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Post(':id/dip/send')
    sendDIP(@Param('id') id: string) {
        return this.franchiseLeadsService.sendDIP(id);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get(':id/dip')
    async getDIP(@Param('id') id: string, @Res() res: Response) {
        const buffer = await this.franchiseLeadsService.generateDIP(id);
        res.set({
            'Content-Type': 'application/pdf',
            'Content-Disposition': `attachment; filename=DIP-${id}.pdf`,
            'Content-Length': buffer.length,
        });
        res.end(buffer);
    }

    // --- COOLING PERIOD STATUS ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get(':id/cooling-status')
    async getCoolingStatus(@Param('id') id: string) {
        const lead = await this.franchiseLeadsService.findOne(id);
        return this.franchiseLeadsService.getDIPCoolingStatus(lead);
    }

    // --- CONTRAT ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Post(':id/sign')
    signContract(@Param('id') id: string) {
        return this.franchiseLeadsService.signContract(id);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get(':id/contract')
    async getContract(@Param('id') id: string, @Res() res: Response) {
        const buffer = await this.franchiseLeadsService.generateContract(id);
        res.set({
            'Content-Type': 'application/pdf',
            'Content-Disposition': `attachment; filename=contrat-franchise-${id}.pdf`,
            'Content-Length': buffer.length,
        });
        res.end(buffer);
    }

    // --- KIT D'OUVERTURE ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get(':id/opening-kit')
    async getOpeningKit(@Param('id') id: string, @Res() res: Response) {
        const buffer = await this.franchiseLeadsService.generateOpeningKit(id);
        res.set({
            'Content-Type': 'application/pdf',
            'Content-Disposition': `attachment; filename=kit-ouverture-${id}.pdf`,
            'Content-Length': buffer.length,
        });
        res.end(buffer);
    }

    // --- NOTES ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Post(':id/notes')
    addNote(@Param('id') id: string, @Body() body: { content: string, author: string, type?: 'NOTE' | 'CALL' | 'EMAIL' }) {
        return this.franchiseLeadsService.addNote(id, body.content, body.author, body.type);
    }

    // --- ANALYTICS & EXPORT ---

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get('analytics/dashboard')
    getAnalytics() {
        return this.franchiseLeadsService.getAnalytics();
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Get('export/csv')
    async exportCSV(@Res() res: Response) {
        const csv = await this.franchiseLeadsService.exportToCSV();
        res.set({
            'Content-Type': 'text/csv; charset=utf-8',
            'Content-Disposition': 'attachment; filename=franchise-leads.csv'
        });
        res.send('\uFEFF' + csv); // BOM for Excel UTF-8
    }
}
