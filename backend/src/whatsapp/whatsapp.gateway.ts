import {
    WebSocketGateway,
    WebSocketServer,
    OnGatewayInit,
    OnGatewayConnection,
    OnGatewayDisconnect
} from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';
import { Logger } from '@nestjs/common';

@WebSocketGateway({
    cors: {
        origin: '*', // En prod, restreindre à ton domaine
    },
    namespace: '/ws/inbox'
})
export class WhatsappGateway implements OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect {
    @WebSocketServer()
    server: Server;

    private readonly logger = new Logger(WhatsappGateway.name);

    afterInit() {
        this.logger.log('🔌 WebSocket Gateway initialized on /ws/inbox');
    }

    handleConnection(client: Socket) {
        this.logger.log(`🟢 Client connected: ${client.id}`);
    }

    handleDisconnect(client: Socket) {
        this.logger.log(`🔴 Client disconnected: ${client.id}`);
    }

    /**
     * Émet un nouveau message entrant à tous les clients connectés
     * Appelé par le WhatsappService quand un message arrive du webhook
     */
    emitNewMessage(message: {
        id: string;
        direction: string;
        type: string;
        content: string;
        sender: string;
        senderName?: string;
        mediaUrl?: string | null;
        mediaType?: string | null;
        mediaFilename?: string | null;
        leadId?: string | null;
        prospectId?: string | null;
        createdAt: Date | string;
    }) {
        this.server.emit('new_message', message);
        this.logger.log(`📡 Emitted new_message to all clients (from: ${message.senderName || message.sender})`);
    }

    /**
     * Émet une mise à jour de la liste des conversations
     * Appelé après qu'un nouveau message ait été reçu
     */
    emitConversationsUpdate(conversations: any[]) {
        this.server.emit('conversations_update', conversations);
        this.logger.log(`📡 Emitted conversations_update (${conversations.length} conversations)`);
    }
}
