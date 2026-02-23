import { create } from 'zustand';
import { socketService } from './socketService';

export interface Message {
    id: string;
    direction: 'INBOUND' | 'OUTBOUND';
    content: string;
    createdAt: string;
    sender: string;
    senderName?: string;
    mediaUrl?: string | null;
    mediaType?: string | null;
    mediaFilename?: string | null;
    leadId?: string | null;
    prospectId?: string | null;
}

export interface Conversation {
    id: string;
    type: 'LEAD' | 'PROSPECT';
    name: string;
    lastMessage: string;
    lastAt: string;
    phone: string;
    hasMedia?: boolean;
    unreadCount?: number;
}

interface MessageState {
    conversations: Conversation[];
    activeConversation: Conversation | null;
    messages: Message[];
    isLoading: boolean;
    wsConnected: boolean;

    setConversations: (convs: Conversation[]) => void;
    setActiveConversation: (conv: Conversation | null) => void;
    setMessages: (msgs: Message[]) => void;
    addMessage: (msg: Message) => void;
    setLoading: (loading: boolean) => void;
    initWebSocket: () => () => void;
}

export const useMessageStore = create<MessageState>((set, get) => ({
    conversations: [],
    activeConversation: null,
    messages: [],
    isLoading: false,
    wsConnected: false,

    setConversations: (conversations) => set({ conversations }),
    setActiveConversation: (activeConversation) => set({ activeConversation, messages: [] }),
    setMessages: (messages) => set({ messages }),
    addMessage: (msg) => set((state) => ({
        messages: [...state.messages, msg],
        conversations: state.conversations.map(c =>
            c.id === state.activeConversation?.id
                ? { ...c, lastMessage: msg.content, lastAt: msg.createdAt }
                : c
        )
    })),
    setLoading: (isLoading) => set({ isLoading }),

    /**
     * Initialise la connexion WebSocket et écoute les événements
     * Retourne une fonction cleanup pour se désabonner
     */
    initWebSocket: () => {
        socketService.connect();
        set({ wsConnected: true });

        // Écouter les nouveaux messages
        const unsubMessage = socketService.on('new_message', (message: Message) => {
            const state = get();
            const active = state.activeConversation;

            // Si le message concerne la conversation active, l'ajouter aux messages affichés
            if (active) {
                const isForActive =
                    (active.type === 'LEAD' && message.leadId === active.id) ||
                    (active.type === 'PROSPECT' && message.prospectId === active.id);

                if (isForActive) {
                    // Vérifier que le message n'est pas déjà présent (déduplications)
                    const exists = state.messages.some(m => m.id === message.id);
                    if (!exists) {
                        set({ messages: [...state.messages, message] });
                    }
                }
            }
        });

        // Écouter les mises à jour de conversations
        const unsubConversations = socketService.on('conversations_update', (conversations: Conversation[]) => {
            set({ conversations });
        });

        // Retourne la fonction cleanup
        return () => {
            unsubMessage();
            unsubConversations();
            set({ wsConnected: false });
        };
    }
}));
