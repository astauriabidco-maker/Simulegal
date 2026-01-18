import { Controller, Get, Post, Body, Param, Patch, Res, UseGuards } from '@nestjs/common';
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
            'Content-Disposition': 'attachment; filename=contract.pdf',
            'Content-Length': buffer.length,
        });

        res.end(buffer);
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('HQ_ADMIN', 'SUPER_ADMIN', 'SUPERADMIN')
    @Post(':id/notes')
    addNote(@Param('id') id: string, @Body() body: { content: string, author: string, type?: 'NOTE' | 'CALL' | 'EMAIL' }) {
        return this.franchiseLeadsService.addNote(id, body.content, body.author, body.type);
    }
}
