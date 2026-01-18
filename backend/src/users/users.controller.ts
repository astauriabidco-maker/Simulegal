import { Controller, Get, Post, Body, Patch, Param, Delete, UseGuards, ForbiddenException, Request } from '@nestjs/common';
import { UsersService } from './users.service';
import { AuthGuard } from '@nestjs/passport';

@Controller('users')
@UseGuards(AuthGuard('jwt'))
export class UsersController {
    constructor(private readonly usersService: UsersService) { }

    @Get()
    findAll(@Request() req: any) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Accès réservé au siège');
        }
        return this.usersService.findAll();
    }

    @Get(':id')
    findOne(@Param('id') id: string) {
        return this.usersService.findOneById(id);
    }

    @Post()
    create(@Request() req: any, @Body() data: any) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.usersService.create(data);
    }

    @Patch(':id')
    update(@Request() req: any, @Param('id') id: string, @Body() data: any) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN' && req.user.userId !== id) {
            throw new ForbiddenException('Action non autorisée');
        }
        return this.usersService.update(id, data);
    }

    @Delete(':id')
    remove(@Request() req: any, @Param('id') id: string) {
        if (req.user.role !== 'SUPER_ADMIN' && req.user.role !== 'HQ_ADMIN') {
            throw new ForbiddenException('Action réservée au siège');
        }
        return this.usersService.delete(id);
    }
}
