import { Controller, Get, Post, Body, Patch, Param, Delete, UseGuards, ForbiddenException, Request } from '@nestjs/common';
import { RolesService } from './roles.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('roles')
@UseGuards(AuthGuard('jwt'))
export class RolesController {
    constructor(private readonly rolesService: RolesService) { }

    @Get()
    findAll(@Request() req: any) {
        // Optionnel: restreindre aux admins HQ
        return this.rolesService.findAll();
    }

    @Get(':id')
    findOne(@Param('id') id: string) {
        return this.rolesService.findOne(id);
    }

    @Post()
    create(@Request() req: any, @Body() data: { label: string; description: string; permissions: string }) {
        if (req.user.role !== 'SUPERADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.rolesService.create(data);
    }

    @Patch(':id')
    update(@Request() req: any, @Param('id') id: string, @Body() data: any) {
        if (req.user.role !== 'SUPERADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.rolesService.update(id, data);
    }

    @Delete(':id')
    remove(@Request() req: any, @Param('id') id: string) {
        if (req.user.role !== 'SUPERADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.rolesService.remove(id);
    }
}
