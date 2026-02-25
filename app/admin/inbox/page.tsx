'use client';

import React, { useEffect, useState, useRef } from 'react';
import Image from 'next/image';
import {
    Search,
    MoreVertical,
    Send,
    Phone,
    CheckCheck,
    Loader2,
    MessageSquare,
    ChevronLeft,
    FileText,
    Download,
    Image as ImageIcon,
    Paperclip
} from 'lucide-react';
import { useMessageStore, Conversation, Message } from '../../../services/MessageStore';
import { InboxAPI } from '../../../services/inbox.api';

// Helpers pour les médias
function getMediaUrl(mediaUrl: string | undefined | null): string | null {
    if (!mediaUrl) return null;
    if (mediaUrl.startsWith('http')) return mediaUrl;
    const filename = mediaUrl.split('/').pop();
    return `http://localhost:4000/whatsapp/media/${filename}`;
}

function isImageType(mediaType: string | undefined | null): boolean {
    if (!mediaType) return false;
    return mediaType.startsWith('image/');
}

function isPdfType(mediaType: string | undefined | null): boolean {
    return mediaType === 'application/pdf';
}

function getMediaLabel(mediaType: string | undefined | null): string {
    if (!mediaType) return 'Fichier';
    if (mediaType.startsWith('image/')) return 'Photo';
    if (mediaType.startsWith('video/')) return 'Vidéo';
    if (mediaType.startsWith('audio/')) return 'Audio';
    if (mediaType === 'application/pdf') return 'Document PDF';
    return 'Fichier';
}

export default function InboxPage() {
    const {
        conversations,
        setConversations,
        activeConversation,
        setActiveConversation,
        messages,
        setMessages,
        addMessage,
        isLoading,
        setLoading,
        initWebSocket
    } = useMessageStore();

    const [reply, setReply] = useState('');
    const [searchQuery, setSearchQuery] = useState('');
    const [lightboxImage, setLightboxImage] = useState<string | null>(null);
    const chatEndRef = useRef<HTMLDivElement>(null);

    // Initial load + WebSocket real-time
    useEffect(() => {
        const fetchConversations = async () => {
            setLoading(true);
            const data = await InboxAPI.getConversations();
            setConversations(data);
            setLoading(false);
        };
        fetchConversations();

        // Initialiser le WebSocket pour les mises à jour temps réel
        const cleanupWs = initWebSocket();

        return () => {
            cleanupWs();
        };
    }, []);

    // Load messages when conversation changes
    useEffect(() => {
        if (activeConversation) {
            const fetchMessages = async () => {
                const data = await InboxAPI.getMessages(activeConversation.type, activeConversation.id);
                setMessages(data);
            };
            fetchMessages();
        }
    }, [activeConversation]);

    // Scroll to bottom
    useEffect(() => {
        chatEndRef.current?.scrollIntoView({ behavior: 'smooth' });
    }, [messages]);

    const handleSendMessage = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!reply.trim() || !activeConversation) return;

        const content = reply.trim();
        setReply('');

        const result = await InboxAPI.sendMessage(activeConversation.type, activeConversation.id, content);

        if (result.success) {
            addMessage({
                id: Math.random().toString(),
                direction: 'OUTBOUND',
                content,
                createdAt: new Date().toISOString(),
                sender: 'SYSTEM',
                senderName: 'Moi'
            });
        }
    };

    const filteredConversations = conversations.filter(c =>
        c.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        c.phone.includes(searchQuery)
    );

    return (
        <div className="flex h-screen bg-slate-50 overflow-hidden">
            {/* Conversations Sidebar */}
            <div className={`w-full md:w-80 lg:w-96 border-r border-slate-200 bg-white flex flex-col ${activeConversation ? 'hidden md:flex' : 'flex'}`}>
                <div className="p-6 border-b border-slate-100">
                    <h1 className="text-xl font-black text-slate-800 mb-4 flex items-center gap-2">
                        <MessageSquare className="text-indigo-600" size={24} />
                        WhatsApp Inbox
                    </h1>
                    <div className="relative">
                        <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                        <input
                            type="text"
                            placeholder="Rechercher..."
                            className="w-full bg-slate-100 border-none rounded-xl py-2.5 pl-10 pr-4 text-sm focus:ring-2 focus:ring-indigo-500 transition-all font-medium"
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                        />
                    </div>
                </div>

                <div className="flex-1 overflow-y-auto">
                    {isLoading && conversations.length === 0 ? (
                        <div className="flex flex-col items-center justify-center p-12 text-slate-400 gap-3">
                            <Loader2 className="animate-spin" size={32} />
                            <p className="font-bold text-sm">Chargement...</p>
                        </div>
                    ) : filteredConversations.length === 0 ? (
                        <div className="p-12 text-center text-slate-400">
                            <p className="text-sm font-bold">Aucune conversation trouvée</p>
                        </div>
                    ) : (
                        filteredConversations.map((conv: any) => (
                            <button
                                key={`${conv.type}:${conv.id}`}
                                onClick={() => setActiveConversation(conv)}
                                className={`w-full p-4 flex gap-4 hover:bg-slate-50 transition-all border-l-4 ${activeConversation?.id === conv.id ? 'bg-indigo-50/50 border-indigo-600' : 'border-transparent'
                                    }`}
                            >
                                <div className={`w-12 h-12 rounded-full flex items-center justify-center font-bold text-white shadow-md ${conv.type === 'LEAD' ? 'bg-indigo-600' : 'bg-emerald-500'
                                    }`}>
                                    {conv.name.charAt(0)}
                                </div>
                                <div className="flex-1 text-left min-w-0">
                                    <div className="flex justify-between items-start mb-1">
                                        <h3 className="font-bold text-slate-800 truncate text-sm">{conv.name}</h3>
                                        <span className="text-[10px] text-slate-400 font-bold uppercase shrink-0">
                                            {new Date(conv.lastAt).toLocaleDateString() === new Date().toLocaleDateString()
                                                ? new Date(conv.lastAt).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
                                                : new Date(conv.lastAt).toLocaleDateString([], { day: 'numeric', month: 'short' })
                                            }
                                        </span>
                                    </div>
                                    <p className="text-xs text-slate-500 truncate font-medium flex items-center gap-1">
                                        {conv.hasMedia && <Paperclip size={10} className="text-slate-400 shrink-0" />}
                                        {conv.lastMessage}
                                    </p>
                                    <div className="mt-1 flex items-center gap-1">
                                        <span className={`text-[9px] font-black px-1.5 py-0.5 rounded-md uppercase tracking-wider ${conv.type === 'LEAD' ? 'bg-indigo-100 text-indigo-600' : 'bg-emerald-100 text-emerald-600'
                                            }`}>
                                            {conv.type}
                                        </span>
                                    </div>
                                </div>
                            </button>
                        ))
                    )}
                </div>
            </div>

            {/* Chat Window */}
            <div className={`flex-1 flex flex-col bg-slate-50 ${!activeConversation ? 'hidden md:flex' : 'flex'}`}>
                {activeConversation ? (
                    <>
                        {/* Header */}
                        <div className="h-20 bg-white border-b border-slate-200 px-6 flex items-center justify-between shrink-0">
                            <div className="flex items-center gap-4">
                                <button
                                    onClick={() => setActiveConversation(null)}
                                    className="md:hidden p-2 hover:bg-slate-100 rounded-lg transition-colors"
                                >
                                    <ChevronLeft size={20} />
                                </button>
                                <div className={`w-10 h-10 rounded-full flex items-center justify-center font-bold text-white ${activeConversation.type === 'LEAD' ? 'bg-indigo-600' : 'bg-emerald-500'
                                    }`}>
                                    {activeConversation.name.charAt(0)}
                                </div>
                                <div>
                                    <h2 className="font-black text-slate-800 text-sm">{activeConversation.name}</h2>
                                    <p className="text-[10px] text-slate-500 font-bold flex items-center gap-1">
                                        <Phone size={10} /> {activeConversation.phone}
                                    </p>
                                </div>
                            </div>
                            <div className="flex items-center gap-2">
                                <button className="p-2 hover:bg-slate-100 rounded-xl transition-colors text-slate-400">
                                    <MoreVertical size={20} />
                                </button>
                            </div>
                        </div>

                        {/* Messages Area */}
                        <div className="flex-1 overflow-y-auto p-6 space-y-4 bg-[#EFEAE2]">
                            {messages.map((msg: any, idx) => {
                                const mediaUrl = getMediaUrl(msg.mediaUrl);
                                const hasImage = isImageType(msg.mediaType) && mediaUrl;
                                const hasPdf = isPdfType(msg.mediaType) && mediaUrl;
                                const hasOtherMedia = msg.mediaType && !hasImage && !hasPdf && mediaUrl;

                                return (
                                    <div
                                        key={msg.id || idx}
                                        className={`flex ${msg.direction === 'OUTBOUND' ? 'justify-end' : 'justify-start'}`}
                                    >
                                        <div className={`max-w-[75%] rounded-2xl shadow-sm relative overflow-hidden ${msg.direction === 'OUTBOUND'
                                            ? 'bg-[#D9FDD3] text-slate-800 rounded-br-none'
                                            : 'bg-white text-slate-700 rounded-bl-none'
                                            }`}>

                                            {/* Image inline */}
                                            {hasImage && (
                                                <div
                                                    className="cursor-pointer group relative"
                                                    onClick={() => setLightboxImage(mediaUrl)}
                                                >
                                                    <Image
                                                        src={mediaUrl}
                                                        alt="Photo reçue"
                                                        width={500}
                                                        height={256}
                                                        className="w-full max-h-64 object-cover"
                                                        unoptimized
                                                        onError={(e) => {
                                                            (e.target as HTMLImageElement).style.display = 'none';
                                                        }}
                                                    />
                                                    <div className="absolute inset-0 bg-black/0 group-hover:bg-black/10 transition-colors flex items-center justify-center opacity-0 group-hover:opacity-100">
                                                        <ImageIcon size={32} className="text-white drop-shadow-lg" />
                                                    </div>
                                                </div>
                                            )}

                                            {/* PDF inline */}
                                            {hasPdf && (
                                                <a
                                                    href={mediaUrl}
                                                    target="_blank"
                                                    rel="noopener noreferrer"
                                                    className="flex items-center gap-3 p-4 mx-2 mt-2 bg-slate-100 rounded-xl hover:bg-slate-200 transition-colors group"
                                                >
                                                    <div className="p-3 bg-red-100 text-red-600 rounded-xl shrink-0">
                                                        <FileText size={24} />
                                                    </div>
                                                    <div className="flex-1 min-w-0">
                                                        <p className="text-sm font-bold text-slate-700 truncate">{msg.mediaFilename || 'document.pdf'}</p>
                                                        <p className="text-xs text-slate-400">PDF • Cliquer pour ouvrir</p>
                                                    </div>
                                                    <Download size={18} className="text-slate-400 group-hover:text-slate-600 transition-colors shrink-0" />
                                                </a>
                                            )}

                                            {/* Autre fichier */}
                                            {hasOtherMedia && (
                                                <a
                                                    href={mediaUrl}
                                                    target="_blank"
                                                    rel="noopener noreferrer"
                                                    className="flex items-center gap-3 p-4 mx-2 mt-2 bg-slate-100 rounded-xl hover:bg-slate-200 transition-colors"
                                                >
                                                    <div className="p-3 bg-blue-100 text-blue-600 rounded-xl shrink-0">
                                                        <FileText size={24} />
                                                    </div>
                                                    <div className="flex-1 min-w-0">
                                                        <p className="text-sm font-bold text-slate-700">{getMediaLabel(msg.mediaType)}</p>
                                                        <p className="text-xs text-slate-400">{msg.mediaFilename || 'fichier'}</p>
                                                    </div>
                                                </a>
                                            )}

                                            {/* Texte */}
                                            <div className="p-4">
                                                {(msg.content && !(msg.mediaUrl && msg.content === '📎 Pièce jointe')) && (
                                                    <p className="text-sm font-medium whitespace-pre-wrap">{msg.content}</p>
                                                )}
                                                <div className={`flex items-center justify-end gap-1 mt-2 ${msg.direction === 'OUTBOUND' ? 'text-slate-400' : 'text-slate-400'
                                                    }`}>
                                                    <span className="text-[9px] font-bold">
                                                        {new Date(msg.createdAt).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                                                    </span>
                                                    {msg.direction === 'OUTBOUND' && <CheckCheck size={12} className="text-[#53BDEB]" />}
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                );
                            })}
                            <div ref={chatEndRef} />
                        </div>

                        {/* Input Area */}
                        <div className="p-6 bg-white border-t border-slate-200 shrink-0">
                            <form
                                onSubmit={handleSendMessage}
                                className="flex items-center gap-3"
                            >
                                <input
                                    type="text"
                                    placeholder="Écrivez votre message..."
                                    value={reply}
                                    onChange={(e) => setReply(e.target.value)}
                                    className="flex-1 bg-slate-100 border-none rounded-xl py-4 px-6 text-sm focus:ring-2 focus:ring-indigo-500 transition-all font-medium"
                                />
                                <button
                                    type="submit"
                                    disabled={!reply.trim()}
                                    className="w-12 h-12 bg-indigo-600 hover:bg-indigo-700 disabled:bg-slate-300 text-white rounded-xl flex items-center justify-center transition-all shadow-lg shadow-indigo-600/20 active:scale-95"
                                >
                                    <Send size={20} />
                                </button>
                            </form>
                        </div>
                    </>
                ) : (
                    <div className="flex-1 flex flex-col items-center justify-center text-slate-400 p-12 text-center">
                        <div className="w-24 h-24 bg-slate-100 rounded-3xl flex items-center justify-center mb-6 text-slate-300">
                            <MessageSquare size={48} />
                        </div>
                        <h2 className="text-xl font-black text-slate-800 mb-2">Sélectionnez une conversation</h2>
                        <p className="max-w-xs text-sm font-medium">Choisissez un client dans la liste pour voir l'historique et répondre.</p>
                    </div>
                )}
            </div>

            {/* Lightbox pour les images agrandies */}
            {lightboxImage && (
                <div
                    className="fixed inset-0 bg-black/80 backdrop-blur-sm z-[999] flex items-center justify-center cursor-pointer"
                    onClick={() => setLightboxImage(null)}
                >
                    <Image
                        src={lightboxImage}
                        alt="Image agrandie"
                        width={1200}
                        height={800}
                        className="max-w-[90vw] max-h-[85vh] object-contain rounded-2xl shadow-2xl"
                        unoptimized
                    />
                    <button className="absolute top-6 right-6 text-white/80 hover:text-white font-black text-2xl">✕</button>
                </div>
            )}
        </div>
    );
}
