import { Controller, Post, UseGuards, Request, Get, Body } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { AuthService } from './auth.service';

@Controller('auth')
export class AuthController {
    constructor(private authService: AuthService) { }

    @UseGuards(AuthGuard('local'))
    @Post('login')
    async login(@Request() req: any) {
        return this.authService.login(req.user);
    }

    /**
     * Demo login endpoint - no password required for *@demo.fr emails
     */
    @Post('demo-login')
    async demoLogin(@Body() body: { email: string }) {
        return this.authService.demoLogin(body.email);
    }

    @Post('dev-login')
    async devLogin() {
        return this.authService.devLogin();
    }

    @UseGuards(AuthGuard('jwt'))
    @Get('profile')
    getProfile(@Request() req: any) {
        return req.user;
    }
}
