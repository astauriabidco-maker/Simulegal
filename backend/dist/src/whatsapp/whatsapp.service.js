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
var WhatsappService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.WhatsappService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const notifications_service_1 = require("../notifications/notifications.service");
let WhatsappService = WhatsappService_1 = class WhatsappService {
    prisma;
    notificationsService;
    logger = new common_1.Logger(WhatsappService_1.name);
    constructor(prisma, notificationsService) {
        this.prisma = prisma;
        this.notificationsService = notificationsService;
    }
    async handleIncoming(data) {
        const cleanPhone = data.from.replace('whatsapp:', '').replace('+', '').trim();
        this.logger.log(`Received WhatsApp from ${cleanPhone}: ${data.body}`);
        const lead = await this.prisma.lead.findFirst({
            where: {
                phone: {
                    contains: cleanPhone.substring(cleanPhone.length - 9)
                }
            }
        });
        const prospect = !lead ? await this.prisma.prospect.findFirst({
            where: {
                phone: {
                    contains: cleanPhone.substring(cleanPhone.length - 9)
                }
            }
        }) : null;
        const communication = await this.prisma.communication.create({
            data: {
                direction: 'INBOUND',
                type: 'WHATSAPP',
                content: data.body,
                sender: data.from,
                senderName: lead?.name || (prospect ? `${prospect.firstName} ${prospect.lastName}`.trim() : 'Unknown'),
                leadId: lead?.id,
                prospectId: prospect?.id
            }
        });
        return {
            success: true,
            communicationId: communication.id,
            matchedType: lead ? 'LEAD' : (prospect ? 'PROSPECT' : 'NONE'),
            matchedId: lead?.id || prospect?.id
        };
    }
    async getConversations() {
        const communications = await this.prisma.communication.findMany({
            orderBy: { createdAt: 'desc' },
            include: {
                lead: true,
                prospect: true
            }
        });
        const conversations = [];
        const seen = new Set();
        for (const comm of communications) {
            const id = comm.leadId || comm.prospectId;
            const type = comm.leadId ? 'LEAD' : (comm.prospectId ? 'PROSPECT' : 'UNKNOWN');
            if (id && !seen.has(`${type}:${id}`)) {
                seen.add(`${type}:${id}`);
                conversations.push({
                    id,
                    type,
                    name: comm.lead?.name || (comm.prospect ? `${comm.prospect.firstName} ${comm.prospect.lastName}`.trim() : comm.senderName || 'Unknown'),
                    lastMessage: comm.content,
                    lastAt: comm.createdAt,
                    phone: comm.sender
                });
            }
        }
        return conversations;
    }
    async getMessages(type, id) {
        return this.prisma.communication.findMany({
            where: type === 'LEAD' ? { leadId: id } : { prospectId: id },
            orderBy: { createdAt: 'asc' }
        });
    }
    async sendMessage(type, id, content) {
        let phone = '';
        const metadata = {};
        if (type === 'LEAD') {
            const lead = await this.prisma.lead.findUnique({ where: { id } });
            if (!lead)
                throw new Error('Lead not found');
            phone = lead.phone;
            metadata.leadId = id;
        }
        else {
            const prospect = await this.prisma.prospect.findUnique({ where: { id } });
            if (!prospect)
                throw new Error('Prospect not found');
            phone = prospect.phone;
            metadata.prospectId = id;
        }
        return this.notificationsService.sendWhatsApp(phone, 'manual_reply', { message: content }, metadata);
    }
};
exports.WhatsappService = WhatsappService;
exports.WhatsappService = WhatsappService = WhatsappService_1 = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        notifications_service_1.NotificationsService])
], WhatsappService);
//# sourceMappingURL=whatsapp.service.js.map