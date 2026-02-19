import { Injectable, OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class SettingsService implements OnModuleInit {
    constructor(private prisma: PrismaService) { }

    async onModuleInit() {
        // Ensure the settings row exists
        const settings = await this.prisma.systemSettings.findUnique({
            where: { id: 'GLOBAL' }
        });

        if (!settings) {
            await this.prisma.systemSettings.create({
                data: {
                    id: 'GLOBAL',
                    company: JSON.stringify({
                        name: "Simulegal SAS",
                        address: "123 Avenue de la Justice",
                        zipCode: "75000",
                        city: "Paris",
                        siret: "123 456 789 00012",
                        tvaNumber: "FR 12 123456789",
                        supportEmail: "support@simulegal.fr",
                        supportPhone: "+33 1 23 45 67 89"
                    }),
                    payment: JSON.stringify({
                        provider: 'STRIPE',
                        mode: 'TEST',
                        publicKey: 'pk_test_sample',
                        secretKey: 'sk_test_sample',
                        currency: 'EUR'
                    }),
                    notifications: JSON.stringify({
                        smtpHost: 'smtp.gmail.com',
                        smtpPort: 587,
                        smtpUser: 'notifications@simulegal.fr',
                        smtpPass: '******',
                        smsProvider: 'TWILIO',
                        smsSid: 'AC_sample',
                        smsToken: 'token_sample',
                        whatsappEnabled: false
                    }),
                    integrations: JSON.stringify({
                        ocrProvider: 'GOOGLE_VISION',
                        ocrApiKey: 'key_sample',
                        mapsApiKey: 'key_sample'
                    }),
                    storage: JSON.stringify({
                        provider: 'LOCAL',
                        bucketName: 'simulegal-docs',
                        region: 'eu-west-3',
                        accessKey: 'key_sample',
                        secretKey: 'secret_sample'
                    })
                }
            });
            console.log('[Settings] ⚙️ Global settings initialized in DB');
        }
    }

    async getSettings() {
        let settings = await this.prisma.systemSettings.findUnique({
            where: { id: 'GLOBAL' }
        });

        if (!settings) {
            // Fallback to initialization logic if missing at runtime
            await this.onModuleInit();
            settings = await this.prisma.systemSettings.findUnique({
                where: { id: 'GLOBAL' }
            });
        }

        if (!settings) {
            throw new Error('System settings not initialized');
        }

        return {
            company: JSON.parse(settings.company),
            payment: JSON.parse(settings.payment),
            notifications: JSON.parse(settings.notifications),
            integrations: JSON.parse(settings.integrations),
            storage: JSON.parse(settings.storage),
            updatedAt: settings.updatedAt
        };
    }

    async updateSection(section: string, data: any) {
        const current = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });

        const updateData: any = {};
        updateData[section] = JSON.stringify(data);

        return this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: updateData
        });
    }
}
