import { SettingsService } from './settings.service';
export declare class SettingsController {
    private readonly settingsService;
    constructor(settingsService: SettingsService);
    getSettings(): Promise<{
        company: any;
        payment: any;
        notifications: any;
        integrations: any;
        storage: any;
        updatedAt: Date;
    }>;
    updateSection(section: string, data: any): Promise<{
        id: string;
        updatedAt: Date;
        company: string;
        payment: string;
        notifications: string;
        integrations: string;
        storage: string;
    }>;
    update(data: any, section: string): Promise<{
        id: string;
        updatedAt: Date;
        company: string;
        payment: string;
        notifications: string;
        integrations: string;
        storage: string;
    }>;
}
