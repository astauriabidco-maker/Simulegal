import { Controller, Get, Post, Body, Patch, Param, UseGuards, Request } from '@nestjs/common';
import { SalesService } from './sales.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('sales')
@UseGuards(AuthGuard('jwt'))
export class SalesController {
    constructor(private readonly salesService: SalesService) { }

    @Get('prospects')
    findAll() {
        return this.salesService.findAll();
    }

    @Get('prospects/:id')
    findOne(@Param('id') id: string) {
        return this.salesService.findOne(id);
    }

    @Post('prospects')
    create(@Body() data: any) {
        return this.salesService.create(data);
    }

    @Patch('prospects/:id')
    update(@Param('id') id: string, @Body() data: any) {
        return this.salesService.update(id, data);
    }

    @Post('prospects/:id/notes')
    addNote(
        @Param('id') id: string,
        @Request() req: any,
        @Body() data: { text: string }
    ) {
        return this.salesService.addNote(id, req.user.id, data.text);
    }
}
