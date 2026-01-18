import { Controller, Get, UseGuards } from '@nestjs/common';
import { DashboardService } from './dashboard.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';

@Controller('admin/stats')
@UseGuards(JwtAuthGuard, RolesGuard)
@Roles('SUPER_ADMIN', 'HQ_ADMIN')
export class DashboardController {
    constructor(private readonly dashboardService: DashboardService) { }

    @Get()
    async getGlobalStats() {
        return this.dashboardService.getGlobalStats();
    }
}
