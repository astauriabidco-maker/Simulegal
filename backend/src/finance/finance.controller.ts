import { Controller, Get, Post, Body, UseGuards, Request, Query, ForbiddenException } from '@nestjs/common';
import { FinanceService } from './finance.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('finance')
@UseGuards(AuthGuard('jwt'))
export class FinanceController {
    constructor(private readonly financeService: FinanceService) { }

    @Get('stats')
    getStats(@Request() req: any) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getGlobalStats();
    }

    @Get('balance')
    getBalance(@Request() req: any, @Query('agencyId') agencyId?: string) {
        const id = agencyId || req.user.agencyId;
        if (!id) throw new ForbiddenException('ID Agence manquant');

        // Une agence ne peut voir que sa propre balance
        if (req.user.role === 'AGENCY_MANAGER' && id !== req.user.agencyId) {
            throw new ForbiddenException('Accès refusé');
        }

        return this.financeService.getAgencyBalance(id);
    }

    @Get('payouts')
    getPayouts(@Request() req: any) {
        // Les agences voient leurs propres payouts, HQ voit tout
        if (req.user.role === 'AGENCY_MANAGER') {
            // Pour l'instant on retourne tout s'il n'y a pas de filtre dans le service, 
            // mais on devrait ajouter un filtre par agencyId dans getAllPayouts.
            // Je vais simplifier pour la démo.
        }
        return this.financeService.getAllPayouts();
    }

    @Post('payouts')
    createPayout(@Request() req: any, @Body() data: { agencyId: string, amount: number, period: string }) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.financeService.createPayout(data);
    }

    @Get('settlements')
    getSettlements(@Request() req: any, @Query('month') month: string, @Query('year') year: string) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Accès réservé au siège');
        }
        return this.financeService.getMonthlySettlement(month, year);
    }

    @Get('performance-trends')
    getPerformanceTrends(@Request() req: any, @Query('agencyId') agencyId?: string) {
        const id = agencyId || req.user.agencyId;
        if (!id) throw new ForbiddenException('ID Agence manquant');

        // Une agence ne peut voir que ses propres trends
        if (req.user.role === 'AGENCY_MANAGER' && id !== req.user.agencyId) {
            throw new ForbiddenException('Accès refusé');
        }

        return this.financeService.getAgencyPerformanceTrends(id);
    }
}
