import { Controller, Get, Patch, Body, UseGuards, Param } from '@nestjs/common';
import { SettingsService } from './settings.service';
import { JwtAuthGuard } from '../auth/jwt-auth.guard';
import { RolesGuard } from '../auth/roles.guard';
import { Roles } from '../auth/roles.decorator';

@Controller('settings')
export class SettingsController {
    constructor(private readonly settingsService: SettingsService) { }

    @Get()
    async getSettings() {
        return this.settingsService.getSettings();
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN')
    @Patch(':section')
    async updateSection(
        @Body() data: any,
        @Body('section') section: string // Using param or body
    ) {
        // Simple logic: section is passed as key or we can use generic update
        // We'll use the body keys to determine section for flexibility or explicit param
    }

    @UseGuards(JwtAuthGuard, RolesGuard)
    @Roles('SUPER_ADMIN')
    @Patch('update/:section')
    async update(@Body() data: any, @Param('section') section: string) {
        return this.settingsService.updateSection(section, data);
    }
}
