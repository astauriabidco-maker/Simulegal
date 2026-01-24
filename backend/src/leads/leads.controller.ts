import { Controller, Get, Post, Body, Patch, Param, Delete, UseGuards, ForbiddenException, Request, Query } from '@nestjs/common';
import { LeadsService } from './leads.service';
import { InvoicesService } from '../invoices/invoices.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('leads')
@UseGuards(AuthGuard('jwt'))
export class LeadsController {
    constructor(
        private readonly leadsService: LeadsService,
        private readonly invoicesService: InvoicesService
    ) { }

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

    @Delete(':id')
    remove(@Request() req: any, @Param('id') id: string) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.leadsService.delete(id);
    }

    @Post(':id/payment')
    recordPayment(@Param('id') id: string, @Body() data: { amount: number, method: string, reference?: string }) {
        return this.leadsService.recordPayment(id, data);
    }

    @Get(':id/invoice')
    async getInvoice(@Param('id') id: string) {
        const invoice = await this.invoicesService.getInvoiceData(id);
        if (!invoice) throw new Error('Invoice not found');
        return invoice;
    }

    @Get(':id/invoice/pdf')
    async downloadPdf(@Param('id') id: string) {
        return this.invoicesService.generatePdf(id);
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

    @Patch(':id/assign')
    assignUser(@Param('id') id: string, @Body('userId') userId: string) {
        return this.leadsService.assignUser(id, userId);
    }

    @Patch(':id/documents')
    updateDocuments(@Param('id') id: string, @Body('documents') documents: any[]) {
        return this.leadsService.updateDocuments(id, documents);
    }
}
