import { UsersService } from '../users/users.service';
import { JwtService } from '@nestjs/jwt';
import { PrismaService } from '../prisma/prisma.service';
export declare class AuthService {
    private usersService;
    private jwtService;
    private prisma;
    constructor(usersService: UsersService, jwtService: JwtService, prisma: PrismaService);
    validateUser(email: string, pass: string): Promise<any>;
    login(user: any): Promise<{
        access_token: string;
        user: {
            id: any;
            email: any;
            name: any;
            role: any;
            roleId: any;
            agencyId: any;
            agencyName: any;
            permissions: any;
        };
    }>;
    demoLogin(email: string): Promise<{
        access_token: string;
        lead: {
            id: string;
            name: string;
            email: string;
            serviceId: string;
            serviceName: string;
            status: import(".prisma/client").$Enums.LeadStatus;
        };
    }>;
    devLogin(): Promise<{
        access_token: string;
        user: {
            email: string;
            role: string;
        };
    }>;
}
