import { Controller, Get, Post, Body, Param, Delete, Query, UseGuards } from '@nestjs/common';
import { AbsencesService } from './absences.service';
import { Prisma } from '@prisma/client';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';

@UseGuards(JwtAuthGuard, RolesGuard)
@Controller('absences')
export class AbsencesController {
    constructor(private readonly absencesService: AbsencesService) { }

    @Get()
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    async findAll(
        @Query('userId') userId?: string,
        @Query('start') start?: string,
        @Query('end') end?: string
    ) {
        const where: Prisma.AbsenceWhereInput = {};
        if (userId) where.userId = userId;
        if (start && end) {
            where.start = { gte: new Date(start) };
            where.end = { lte: new Date(end) };
        }
        return this.absencesService.findAll({ where, orderBy: { start: 'asc' } });
    }

    @Post()
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    async create(@Body() data: {
        userId: string;
        start: string;
        end: string;
        reason?: string;
    }) {
        return this.absencesService.create({
            user: { connect: { id: data.userId } },
            start: new Date(data.start),
            end: new Date(data.end),
            reason: data.reason
        });
    }

    @Delete(':id')
    @Roles('SUPER_ADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'CASE_WORKER')
    async remove(@Param('id') id: string) {
        return this.absencesService.remove(id);
    }
}
