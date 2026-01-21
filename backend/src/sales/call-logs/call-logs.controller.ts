import { Controller, Get, Post, Patch, Param, Body, UseGuards, Request } from '@nestjs/common';
import { CallLogsService } from './call-logs.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('sales/calls')
@UseGuards(AuthGuard('jwt'))
export class CallLogsController {
    constructor(private readonly callLogsService: CallLogsService) { }

    @Post()
    async create(
        @Request() req: any,
        @Body() data: { prospectId: string; twilioCallSid?: string }
    ) {
        return this.callLogsService.create({
            prospectId: data.prospectId,
            userId: req.user.id,
            twilioCallSid: data.twilioCallSid,
        });
    }

    @Patch(':id')
    async update(
        @Param('id') id: string,
        @Body() data: { status?: string; duration?: number; notes?: string }
    ) {
        const updateData: any = { ...data };
        if (data.status === 'COMPLETED' || data.status === 'FAILED' || data.status === 'NO_ANSWER') {
            updateData.endedAt = new Date();
        }
        return this.callLogsService.update(id, updateData);
    }

    @Get('prospect/:prospectId')
    async findByProspect(@Param('prospectId') prospectId: string) {
        return this.callLogsService.findByProspect(prospectId);
    }
}
