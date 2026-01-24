import { create } from 'zustand';

export interface Message {
    id: string;
    direction: 'INBOUND' | 'OUTBOUND';
    content: string;
    createdAt: string;
    sender: string;
    senderName?: string;
}

export interface Conversation {
    id: string;
    type: 'LEAD' | 'PROSPECT';
    name: string;
    lastMessage: string;
    lastAt: string;
    phone: string;
}

interface MessageState {
    conversations: Conversation[];
    activeConversation: Conversation | null;
    messages: Message[];
    isLoading: boolean;

    setConversations: (convs: Conversation[]) => void;
    setActiveConversation: (conv: Conversation | null) => void;
    setMessages: (msgs: Message[]) => void;
    addMessage: (msg: Message) => void;
    setLoading: (loading: boolean) => void;
}

export const useMessageStore = create<MessageState>((set) => ({
    conversations: [],
    activeConversation: null,
    messages: [],
    isLoading: false,

    setConversations: (conversations) => set({ conversations }),
    setActiveConversation: (activeConversation) => set({ activeConversation, messages: [] }),
    setMessages: (messages) => set({ messages }),
    addMessage: (msg) => set((state) => ({
        messages: [...state.messages, msg],
        // Update conversation last message preview
        conversations: state.conversations.map(c =>
            (msg.direction === 'INBOUND' ? (c.id === (msg as any).leadId || c.id === (msg as any).prospectId) : c.id === state.activeConversation?.id)
                ? { ...c, lastMessage: msg.content, lastAt: msg.createdAt }
                : c
        )
    })),
    setLoading: (isLoading) => set({ isLoading }),
}));
