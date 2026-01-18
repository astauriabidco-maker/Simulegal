"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SettingsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let SettingsService = class SettingsService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async onModuleInit() {
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
                        zipCode: "75001",
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
    async updateSection(section, data) {
        const current = await this.prisma.systemSettings.findUnique({ where: { id: 'GLOBAL' } });
        const updateData = {};
        updateData[section] = JSON.stringify(data);
        return this.prisma.systemSettings.update({
            where: { id: 'GLOBAL' },
            data: updateData
        });
    }
};
exports.SettingsService = SettingsService;
exports.SettingsService = SettingsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], SettingsService);
//# sourceMappingURL=settings.service.js.map