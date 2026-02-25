import { io, Socket } from 'socket.io-client';

const WS_URL = 'http://localhost:4000/ws/inbox';

class SocketService {
    private socket: Socket | null = null;
    private listeners: Map<string, Set<(...args: any[]) => void>> = new Map();
    private isConnected = false;

    /**
     * Initialise la connexion WebSocket
     * Appelé une seule fois au démarrage de l'app
     */
    connect() {
        if (this.socket?.connected) return;

        this.socket = io(WS_URL, {
            transports: ['websocket', 'polling'],
            reconnection: true,
            reconnectionDelay: 2000,
            reconnectionAttempts: 10,
        });

        this.socket.on('connect', () => {
            this.isConnected = true;
            console.log('🔌 [WS] Connected to inbox WebSocket');
        });

        this.socket.on('disconnect', (reason) => {
            this.isConnected = false;
            console.log(`🔴 [WS] Disconnected: ${reason}`);
        });

        this.socket.on('connect_error', (err) => {
            console.warn(`⚠️ [WS] Connection error: ${err.message}`);
        });

        // Re-emit all registered events on reconnect
        this.socket.on('connect', () => {
            this.listeners.forEach((callbacks, event) => {
                callbacks.forEach(cb => {
                    this.socket?.off(event, cb);
                    this.socket?.on(event, cb);
                });
            });
        });
    }

    /**
     * Écoute un événement WebSocket
     * Retourne une fonction pour se désabonner
     */
    on(event: string, callback: (...args: any[]) => void): () => void {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, new Set());
        }
        this.listeners.get(event)!.add(callback);

        this.socket?.on(event, callback);

        // Retourne la fonction de nettoyage
        return () => {
            this.listeners.get(event)?.delete(callback);
            this.socket?.off(event, callback);
        };
    }

    /**
     * Déconnexion propre
     */
    disconnect() {
        this.socket?.disconnect();
        this.socket = null;
        this.isConnected = false;
        this.listeners.clear();
    }

    /**
     * Vérifie si le socket est connecté
     */
    getStatus(): boolean {
        return this.isConnected;
    }
}

// Singleton global
export const socketService = new SocketService();
