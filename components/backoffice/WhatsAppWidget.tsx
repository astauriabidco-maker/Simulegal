import React, { useState, useEffect, useRef } from 'react';
import Image from 'next/image';
import { MessageSquare, Send, CheckCheck, Loader2, FileText, Download, Image as ImageIcon } from 'lucide-react';
import { InboxAPI } from '../../services/inbox.api';
import { Message } from '../../services/MessageStore';
import { socketService } from '../../services/socketService';

interface WhatsAppWidgetProps {
    contactId: string;
    contactType: 'LEAD' | 'PROSPECT';
    contactName: string;
    contactPhone: string;
}

// Fonction pour construire l'URL d'un média WhatsApp
function getMediaUrl(mediaUrl: string | undefined | null): string | null {
    if (!mediaUrl) return null;
    if (mediaUrl.startsWith('http')) return mediaUrl;
    const filename = mediaUrl.split('/').pop();
    const baseUrl = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:6060';
    return `${baseUrl}/whatsapp/media/${filename}`;
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

export function WhatsAppWidget({ contactId, contactType, contactName, contactPhone }: WhatsAppWidgetProps) {
    const [messages, setMessages] = useState<any[]>([]);
    const [reply, setReply] = useState('');
    const [isLoading, setIsLoading] = useState(true);
    const [isSending, setIsSending] = useState(false);
    const [lightboxImage, setLightboxImage] = useState<string | null>(null);
    const chatEndRef = useRef<HTMLDivElement>(null);

    const loadMessages = async () => {
        try {
            const data = await InboxAPI.getMessages(contactType, contactId);
            setMessages(data);
        } catch (e) {
            console.error(e);
        } finally {
            setIsLoading(false);
        }
    };

    useEffect(() => {
        loadMessages();

        // Écouter les messages temps réel via WebSocket
        socketService.connect();
        const unsub = socketService.on('new_message', (msg: any) => {
            const isForMe =
                (contactType === 'LEAD' && msg.leadId === contactId) ||
                (contactType === 'PROSPECT' && msg.prospectId === contactId);
            if (isForMe) {
                setMessages(prev => {
                    if (prev.some(m => m.id === msg.id)) return prev;
                    return [...prev, msg];
                });
            }
        });

        return () => unsub();
    }, [contactId, contactType]);

    useEffect(() => {
        chatEndRef.current?.scrollIntoView({ behavior: 'smooth' });
    }, [messages]);

    const handleSendMessage = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!reply.trim() || isSending) return;

        const content = reply.trim();
        setReply('');
        setIsSending(true);

        const result = await InboxAPI.sendMessage(contactType, contactId, content);

        if (result.success) {
            setMessages(prev => [...prev, {
                id: Math.random().toString(),
                direction: 'OUTBOUND',
                content,
                createdAt: new Date().toISOString(),
                sender: 'SYSTEM',
                senderName: 'Moi'
            }]);
        }
        setIsSending(false);
    };

    if (isLoading) {
        return (
            <div className="flex flex-col items-center justify-center p-8 text-slate-400 bg-slate-50 rounded-2xl h-[400px]">
                <Loader2 className="animate-spin mb-2" size={24} />
                <p className="text-sm font-bold">Chargement des messages...</p>
            </div>
        );
    }

    return (
        <div className="flex flex-col bg-slate-50 border border-slate-200 rounded-2xl overflow-hidden h-[400px]">
            {/* Header */}
            <div className="bg-[#25D366] text-white px-4 py-3 flex items-center justify-between shadow-sm z-10">
                <div className="flex items-center gap-3">
                    <div className="bg-white/20 p-2 rounded-full backdrop-blur-sm">
                        <MessageSquare size={16} className="text-white" fill="currentColor" />
                    </div>
                    <div>
                        <h3 className="font-bold text-sm leading-tight text-white">{contactName}</h3>
                        <p className="text-[10px] text-white/80 font-medium">{contactPhone}</p>
                    </div>
                </div>
            </div>

            {/* Messages */}
            <div className="flex-1 overflow-y-auto p-4 space-y-3 bg-[#EFEAE2]">
                {messages.length === 0 ? (
                    <div className="text-center p-4 bg-white/60 backdrop-blur-md rounded-xl mx-8 text-xs font-bold text-slate-500 shadow-sm">
                        Aucun message reçu ou envoyé pour le moment.
                    </div>
                ) : (
                    messages.map((msg: any, idx: number) => {
                        const mediaUrl = getMediaUrl(msg.mediaUrl);
                        const hasImage = isImageType(msg.mediaType) && mediaUrl;
                        const hasPdf = isPdfType(msg.mediaType) && mediaUrl;
                        const hasOtherMedia = msg.mediaType && !hasImage && !hasPdf && mediaUrl;

                        return (
                            <div key={msg.id || idx} className={`flex ${msg.direction === 'OUTBOUND' ? 'justify-end' : 'justify-start'}`}>
                                <div className={`max-w-[85%] rounded-2xl shadow-sm relative overflow-hidden ${msg.direction === 'OUTBOUND'
                                    ? 'bg-[#D9FDD3] text-slate-800 rounded-tr-sm'
                                    : 'bg-white text-slate-800 rounded-tl-sm'
                                    }`}>

                                    {/* Image inline  */}
                                    {hasImage && (
                                        <div
                                            className="cursor-pointer group relative"
                                            onClick={() => setLightboxImage(mediaUrl)}
                                        >
                                            <Image
                                                src={mediaUrl}
                                                alt="Photo reçue"
                                                width={400}
                                                height={200}
                                                className="w-full max-h-48 object-cover rounded-t-2xl"
                                                unoptimized
                                                onError={(e) => {
                                                    (e.target as HTMLImageElement).style.display = 'none';
                                                }}
                                            />
                                            <div className="absolute inset-0 bg-black/0 group-hover:bg-black/10 transition-colors flex items-center justify-center opacity-0 group-hover:opacity-100">
                                                <ImageIcon size={24} className="text-white drop-shadow-lg" />
                                            </div>
                                        </div>
                                    )}

                                    {/* PDF inline */}
                                    {hasPdf && (
                                        <a
                                            href={mediaUrl}
                                            target="_blank"
                                            rel="noopener noreferrer"
                                            className="flex items-center gap-3 p-3 mx-1 mt-1 bg-slate-100 rounded-xl hover:bg-slate-200 transition-colors group"
                                        >
                                            <div className="p-2 bg-red-100 text-red-600 rounded-lg shrink-0">
                                                <FileText size={20} />
                                            </div>
                                            <div className="flex-1 min-w-0">
                                                <p className="text-xs font-bold text-slate-700 truncate">{msg.mediaFilename || 'document.pdf'}</p>
                                                <p className="text-[10px] text-slate-400">PDF • Cliquer pour ouvrir</p>
                                            </div>
                                            <Download size={14} className="text-slate-400 group-hover:text-slate-600 transition-colors shrink-0" />
                                        </a>
                                    )}

                                    {/* Autre type de média */}
                                    {hasOtherMedia && (
                                        <a
                                            href={mediaUrl}
                                            target="_blank"
                                            rel="noopener noreferrer"
                                            className="flex items-center gap-3 p-3 mx-1 mt-1 bg-slate-100 rounded-xl hover:bg-slate-200 transition-colors"
                                        >
                                            <div className="p-2 bg-blue-100 text-blue-600 rounded-lg shrink-0">
                                                <FileText size={20} />
                                            </div>
                                            <div className="flex-1 min-w-0">
                                                <p className="text-xs font-bold text-slate-700">{getMediaLabel(msg.mediaType)}</p>
                                                <p className="text-[10px] text-slate-400">{msg.mediaFilename || 'fichier'}</p>
                                            </div>
                                        </a>
                                    )}

                                    {/* Texte du message */}
                                    <div className="p-3">
                                        {/* Ne pas afficher le contenu si c'est le placeholder "Pièce jointe" pour un média déjà affiché */}
                                        {(msg.content && !(msg.mediaUrl && msg.content === '📎 Pièce jointe')) && (
                                            <p className="text-sm font-medium whitespace-pre-wrap">{msg.content}</p>
                                        )}
                                        <div className="flex items-center justify-end gap-1 mt-1 text-slate-400">
                                            <span className="text-[9px] font-bold">
                                                {new Date(msg.createdAt).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                                            </span>
                                            {msg.direction === 'OUTBOUND' && <CheckCheck size={12} className="text-[#53BDEB]" />}
                                        </div>
                                    </div>
                                </div>
                            </div>
                        );
                    })
                )}
                <div ref={chatEndRef} />
            </div>

            {/* Input Form */}
            <div className="bg-white p-3 border-t border-slate-200">
                <form onSubmit={handleSendMessage} className="flex items-center gap-2">
                    <input
                        type="text"
                        placeholder="Écrivez un message..."
                        value={reply}
                        onChange={(e) => setReply(e.target.value)}
                        className="flex-1 bg-slate-100 border-none rounded-full py-2.5 px-4 text-sm focus:ring-2 focus:ring-[#25D366] transition-all"
                        disabled={isSending}
                    />
                    <button
                        type="submit"
                        disabled={!reply.trim() || isSending}
                        className="w-10 h-10 bg-[#25D366] hover:bg-[#20BE5C] disabled:bg-slate-300 text-white rounded-full flex items-center justify-center transition-all shadow-md active:scale-95 shrink-0"
                    >
                        {isSending ? <Loader2 size={16} className="animate-spin" /> : <Send size={16} className="ml-0.5" />}
                    </button>
                </form>
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
