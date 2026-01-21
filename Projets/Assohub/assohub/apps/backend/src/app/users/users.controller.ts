import {
    Controller,
    Get,
    Post,
    Body,
    Patch,
    Param,
    Delete,
    UseGuards,
} from '@nestjs/common';
import { UsersService } from './users.service';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { GetUser } from '../auth/get-user.decorator';

@Controller('users')
@UseGuards(JwtAuthGuard)
export class UsersController {
    constructor(private readonly usersService: UsersService) { }

    @Get()
    findAll(@GetUser('associationId') associationId: string) {
        return this.usersService.findAll(associationId);
    }

    @Post()
    create(
        @GetUser('associationId') associationId: string,
        @Body() createUserDto: CreateUserDto,
    ) {
        return this.usersService.create(associationId, createUserDto);
    }

    @Patch(':id')
    update(
        @GetUser('associationId') associationId: string,
        @Param('id') id: string,
        @Body() updateUserDto: UpdateUserDto,
    ) {
        return this.usersService.update(associationId, id, updateUserDto);
    }

    @Delete(':id')
    remove(
        @GetUser('associationId') associationId: string,
        @Param('id') id: string,
    ) {
        return this.usersService.remove(associationId, id);
    }
}
