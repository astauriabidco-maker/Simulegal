import React, { useState } from 'react';
import { Send, Phone, User, MessageSquare, CheckCircle, Clock } from 'lucide-react';
import { FranchiseLead, FranchiseLeadStore } from '../../services/FranchiseLeadStore';

interface LeadActivityFeedProps {
    lead: FranchiseLead;
    onUpdate: () => void;
}

export default function LeadActivityFeed({ lead, onUpdate }: LeadActivityFeedProps) {
    const [newNote, setNewNote] = useState('');
    const [noteType, setNoteType] = useState<'NOTE' | 'CALL' | 'EMAIL'>('NOTE');
    const [sending, setSending] = useState(false);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!newNote.trim()) return;

        setSending(true);
        try {
            await FranchiseLeadStore.addNote(lead.id, {
                content: newNote,
                author: 'Admin', // In real app, get from AuthStore
                type: noteType
            });
            setNewNote('');
            onUpdate();
        } catch (error) {
            console.error('Failed to add note', error);
            alert('Erreur lors de l\'ajout de la note');
        }
        setSending(false);
    };

    const getIcon = (type: string) => {
        switch (type) {
            case 'CALL': return <Phone size={14} className="text-emerald-500" />;
            case 'EMAIL': return <Send size={14} className="text-blue-500" />;
            case 'SYSTEM': return <CheckCircle size={14} className="text-indigo-500" />;
            default: return <MessageSquare size={14} className="text-slate-500" />;
        }
    };

    const QUICK_ACTIONS = [
        { label: '📞 Pas de réponse', content: 'Appel sortant - Pas de réponse, message laissé', type: 'CALL' },
        { label: '📧 Relance Email', content: 'Email de relance envoyé au candidat', type: 'EMAIL' },
        { label: '🤝 RDV Planifié', content: 'RDV de présentation planifié avec le candidat', type: 'CALL' },
        { label: '👎 Pas intéressé', content: 'Le candidat a indiqué ne plus être intéressé', type: 'NOTE' },
    ];

    const handleQuickAction = async (action: { content: string, type: string }) => {
        setSending(true);
        try {
            await FranchiseLeadStore.addNote(lead.id, {
                content: action.content,
                author: 'Admin',
                type: action.type as any
            });
            onUpdate();
        } catch (error) {
            console.error('Failed to add note', error);
        }
        setSending(false);
    };

    // Smart Alerts Logic
    const daysSinceUpdate = Math.floor((new Date().getTime() - new Date(lead.updatedAt).getTime()) / (1000 * 3600 * 24));
    const showRelanceAlert = daysSinceUpdate >= 5 && lead.status !== 'SIGNED' && lead.status !== 'REJECTED';

    return (
        <div className="bg-white rounded-2xl shadow-sm border border-slate-200 flex flex-col h-full">
            <div className="p-4 border-b border-slate-100 bg-slate-50/50 rounded-t-2xl space-y-3">
                <h3 className="font-bold text-slate-800 flex items-center gap-2">
                    <MessageSquare size={18} className="text-indigo-500" />
                    Fil d'activité
                </h3>

                {/* Smart Alert */}
                {showRelanceAlert && (
                    <div className="bg-amber-50 border border-amber-200 rounded-lg p-3 flex items-start gap-3 animate-in fade-in slide-in-from-top-2 duration-500">
                        <div className="bg-amber-100 p-1.5 rounded-full text-amber-600 mt-0.5">
                            <Clock size={14} />
                        </div>
                        <div className="flex-1">
                            <p className="text-xs font-bold text-amber-800 mb-0.5">Relance conseillée</p>
                            <p className="text-xs text-amber-700 leading-relaxed">
                                Aucune activité depuis {daysSinceUpdate} jours. Le dossier semble à l'arrêt.
                            </p>
                            <button
                                onClick={() => handleQuickAction({ content: '⚠️ Relance suite à inactivité prolongée', type: 'NOTE' })}
                                className="mt-2 text-xs bg-amber-100 hover:bg-amber-200 text-amber-800 px-2 py-1 rounded transition-colors font-medium"
                            >
                                Noter une relance
                            </button>
                        </div>
                    </div>
                )}
            </div>

            {/* List */}
            <div className="flex-1 overflow-y-auto p-4 space-y-4 max-h-[400px]">
                {!lead.notes || lead.notes.length === 0 ? (
                    <div className="text-center text-slate-400 py-8 text-sm">
                        Aucune activité enregistrée.
                    </div>
                ) : (
                    lead.notes.map((note) => {
                        const isSystem = note.type === 'SYSTEM';
                        return (
                            <div key={note.id} className={`flex gap-3 text-sm ${isSystem ? 'opacity-75' : ''}`}>
                                <div className={`shrink-0 w-8 h-8 rounded-full flex items-center justify-center border
                                    ${note.type === 'CALL' ? 'bg-emerald-50 border-emerald-200' :
                                        note.type === 'SYSTEM' ? 'bg-indigo-50 border-indigo-200' : 'bg-slate-50 border-slate-200'}
                                `}>
                                    {getIcon(note.type)}
                                </div>
                                <div className="flex-1 space-y-1">
                                    <div className="flex items-center justify-between">
                                        <span className={`font-semibold ${isSystem ? 'text-indigo-600 text-xs uppercase tracking-wider' : 'text-slate-700'}`}>
                                            {note.author}
                                        </span>
                                        <span className="text-xs text-slate-400">
                                            {new Intl.DateTimeFormat('fr-FR', { day: 'numeric', month: 'long', hour: '2-digit', minute: '2-digit' }).format(new Date(note.createdAt))}
                                        </span>
                                    </div>
                                    <div className={`p-3 rounded-lg border
                                        ${isSystem ? 'bg-indigo-50/50 border-indigo-100 text-slate-600 italic' : 'bg-slate-50 border-slate-100 text-slate-600'}
                                    `}>
                                        {note.content}
                                    </div>
                                </div>
                            </div>
                        );
                    })
                )}
            </div>

            {/* Quick Actions & Input */}
            <div className="p-4 border-t border-slate-100 bg-slate-50/30 rounded-b-2xl space-y-3">
                <div className="flex flex-wrap gap-2">
                    {QUICK_ACTIONS.map((action, i) => (
                        <button
                            key={i}
                            onClick={() => handleQuickAction(action)}
                            disabled={sending}
                            className="text-xs px-2 py-1.5 bg-white border border-slate-200 rounded-md text-slate-600 hover:border-indigo-300 hover:text-indigo-600 hover:bg-indigo-50 transition-colors shadow-sm"
                        >
                            {action.label}
                        </button>
                    ))}
                </div>

                <form onSubmit={handleSubmit}>
                    <div className="flex gap-2 mb-2">
                        <button
                            type="button"
                            onClick={() => setNoteType('NOTE')}
                            className={`text-xs px-2 py-1 rounded border ${noteType === 'NOTE' ? 'bg-slate-800 text-white border-slate-800' : 'bg-white text-slate-600 border-slate-200'}`}
                        >
                            Note
                        </button>
                        <button
                            type="button"
                            onClick={() => setNoteType('CALL')}
                            className={`text-xs px-2 py-1 rounded border ${noteType === 'CALL' ? 'bg-emerald-600 text-white border-emerald-600' : 'bg-white text-slate-600 border-slate-200'}`}
                        >
                            Appel
                        </button>
                    </div>
                    <div className="flex gap-2">
                        <textarea
                            value={newNote}
                            onChange={(e) => setNewNote(e.target.value)}
                            placeholder="Écrire une note..."
                            className="flex-1 p-2 border border-slate-300 rounded-lg text-sm focus:ring-2 focus:ring-indigo-500 outline-none resize-none h-[40px]"
                        />
                        <button
                            type="submit"
                            disabled={sending || !newNote.trim()}
                            className="p-2 bg-indigo-600 text-white rounded-lg hover:bg-indigo-700 disabled:opacity-50"
                        >
                            <Send size={18} />
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}
