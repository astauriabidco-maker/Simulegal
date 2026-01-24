import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';

@Injectable()
export class WhatsappService {
    private readonly logger = new Logger(WhatsappService.name);

    constructor(
        private prisma: PrismaService,
        private notificationsService: NotificationsService
    ) { }

    /**
     * Handle incoming message from Twilio Webhook
     */
    async handleIncoming(data: { from: string; body: string; messageSid: string }) {
        const cleanPhone = data.from.replace('whatsapp:', '').replace('+', '').trim();
        this.logger.log(`Received WhatsApp from ${cleanPhone}: ${data.body}`);

        // Try to find a matching Lead or Prospect
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

        // Persist the communication
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

    /**
     * Get unique conversations (grouped by Lead or Prospect)
     */
    async getConversations() {
        // We fetch communications and group them manually or via raw query
        // Simple approach: find communications, then group by lead/prospect
        const communications = await this.prisma.communication.findMany({
            orderBy: { createdAt: 'desc' },
            include: {
                lead: true,
                prospect: true
            }
        });

        const conversations: any[] = [];
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

    /**
     * Get messages for a specific Lead or Prospect
     */
    async getMessages(type: 'LEAD' | 'PROSPECT', id: string) {
        return this.prisma.communication.findMany({
            where: type === 'LEAD' ? { leadId: id } : { prospectId: id },
            orderBy: { createdAt: 'asc' }
        });
    }

    /**
     * Send manual reply
     */
    async sendMessage(type: 'LEAD' | 'PROSPECT', id: string, content: string) {
        let phone = '';
        const metadata: any = {};

        if (type === 'LEAD') {
            const lead = await this.prisma.lead.findUnique({ where: { id } });
            if (!lead) throw new Error('Lead not found');
            phone = lead.phone;
            metadata.leadId = id;
        } else {
            const prospect = await this.prisma.prospect.findUnique({ where: { id } });
            if (!prospect) throw new Error('Prospect not found');
            phone = prospect.phone;
            metadata.prospectId = id;
        }

        return this.notificationsService.sendWhatsApp(phone, 'manual_reply', { message: content }, metadata);
    }
}
