import { AuthService } from './auth.service';
export declare class AuthController {
    private authService;
    constructor(authService: AuthService);
    login(req: any): Promise<{
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
    demoLogin(body: {
        email: string;
    }): Promise<{
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
    getProfile(req: any): any;
}
