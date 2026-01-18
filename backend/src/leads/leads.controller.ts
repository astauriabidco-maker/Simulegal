import { Controller, Get, Post, Body, Patch, Param, UseGuards, Request, Query } from '@nestjs/common';
import { LeadsService } from './leads.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('leads')
@UseGuards(AuthGuard('jwt'))
export class LeadsController {
    constructor(private readonly leadsService: LeadsService) { }

    @Get()
    findAll(@Request() req: any, @Query('agencyId') agencyId?: string) {
        // Si l'utilisateur est une agence, il ne peut voir que ses propres leads
        if (req.user.role === 'AGENCY_MANAGER' || req.user.role === 'KIOSK_AGENT') {
            return this.leadsService.findByAgency(req.user.agencyId);
        }

        // Si admin, il peut filtrer par agence optionnellement
        if (agencyId) {
            return this.leadsService.findByAgency(agencyId);
        }

        return this.leadsService.findAll();
    }

    @Get(':id')
    findOne(@Param('id') id: string) {
        return this.leadsService.findOne(id);
    }

    @Post()
    create(@Body() data: any) {
        return this.leadsService.create(data);
    }

    @Patch(':id/status')
    updateStatus(@Param('id') id: string, @Body('status') status: string) {
        return this.leadsService.updateStatus(id, status);
    }

    @Post(':id/notes')
    addNote(@Param('id') id: string, @Body() data: { content: string, author: string }) {
        return this.leadsService.addNote(id, data);
    }
}
