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
    updateSection(data: any, section: string): Promise<void>;
    update(data: any, section: string): Promise<{
        id: string;
        updatedAt: Date;
        notifications: string;
        company: string;
        payment: string;
        integrations: string;
        storage: string;
    }>;
}
