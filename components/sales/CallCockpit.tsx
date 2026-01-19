'use client';

import React, { useState, useEffect } from 'react';
import {
    Phone,
    PhoneOff,
    Mic,
    MicOff,
    Volume2,
    User,
    Clock,
    MessageSquare,
    X,
    Circle,
    Play,
    Pause
} from 'lucide-react';
import { Prospect } from '../../services/SalesStore';

interface CallCockpitProps {
    prospect: Prospect;
    onClose: () => void;
    onSaveNote: (note: string) => void;
}

export default function CallCockpit({ prospect, onClose, onSaveNote }: CallCockpitProps) {
    const [status, setStatus] = useState<'IDLE' | 'CALLING' | 'CONNECTED' | 'ENDED'>('IDLE');
    const [duration, setDuration] = useState(0);
    const [isMuted, setIsMuted] = useState(false);
    const [note, setNote] = useState('');
    const [isRecording, setIsRecording] = useState(false);

    useEffect(() => {
        let timer: NodeJS.Timeout;
        if (status === 'CONNECTED') {
            timer = setInterval(() => {
                setDuration(prev => prev + 1);
            }, 1000);
        }
        return () => clearInterval(timer);
    }, [status]);

    const formatDuration = (s: number) => {
        const mins = Math.floor(s / 60);
        const secs = s % 60;
        return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    };

    const handleStartCall = () => {
        setStatus('CALLING');
        setTimeout(() => setStatus('CONNECTED'), 2000);
    };

    const handleEndCall = () => {
        setStatus('ENDED');
        if (note.trim()) {
            onSaveNote(note);
        }
    };

    return (
        <div className="fixed inset-0 z-[100] flex items-center justify-center p-4 bg-slate-900/60 backdrop-blur-md animate-in fade-in duration-300">
            <div className="bg-white w-full max-w-2xl rounded-3xl shadow-2xl overflow-hidden flex flex-col md:flex-row h-[500px] animate-in zoom-in-95 duration-300">

                {/* Left Side: Call Interface */}
                <div className="flex-1 bg-slate-50 p-8 flex flex-col items-center justify-center relative border-r border-slate-100">
                    <button
                        onClick={onClose}
                        className="absolute top-4 left-4 p-2 text-slate-400 hover:text-slate-600 transition-colors"
                    >
                        <X size={20} />
                    </button>

                    <div className="mb-8 text-center">
                        <div className="w-24 h-24 bg-indigo-100 rounded-full flex items-center justify-center mx-auto mb-4 ring-8 ring-indigo-50">
                            <User size={48} className="text-indigo-600" />
                        </div>
                        <h2 className="text-2xl font-bold text-slate-900">{prospect.firstName} {prospect.lastName}</h2>
                        <p className="text-slate-500 font-medium">{prospect.phone}</p>
                    </div>

                    <div className="mb-12 text-center h-8">
                        {status === 'IDLE' && <span className="text-slate-400 font-medium italic">Prêt pour l'appel</span>}
                        {status === 'CALLING' && (
                            <span className="flex items-center gap-2 text-indigo-600 font-semibold animate-pulse">
                                <Circle size={8} fill="currentColor" /> Appel en cours...
                            </span>
                        )}
                        {status === 'CONNECTED' && (
                            <div className="flex flex-col items-center">
                                <span className="text-emerald-600 font-bold mb-1 flex items-center gap-2">
                                    <div className="w-2 h-2 rounded-full bg-emerald-500 animate-ping" />
                                    Connecté
                                </span>
                                <span className="text-2xl font-mono text-slate-700 font-light">{formatDuration(duration)}</span>
                            </div>
                        )}
                        {status === 'ENDED' && <span className="text-slate-400 font-bold">Appel terminé</span>}
                    </div>

                    <div className="flex items-center gap-6">
                        {status === 'IDLE' ? (
                            <button
                                onClick={handleStartCall}
                                className="w-16 h-16 bg-emerald-500 text-white rounded-full flex items-center justify-center shadow-lg shadow-emerald-200 hover:bg-emerald-600 transition-all hover:scale-110 active:scale-95"
                            >
                                <Phone size={28} />
                            </button>
                        ) : status === 'CONNECTED' || status === 'CALLING' ? (
                            <>
                                <button
                                    onClick={() => setIsMuted(!isMuted)}
                                    className={`w-12 h-12 rounded-full flex items-center justify-center transition-all ${isMuted ? 'bg-red-100 text-red-600' : 'bg-slate-100 text-slate-600 hover:bg-slate-200'}`}
                                >
                                    {isMuted ? <MicOff size={20} /> : <Mic size={20} />}
                                </button>
                                <button
                                    onClick={handleEndCall}
                                    className="w-16 h-16 bg-red-500 text-white rounded-full flex items-center justify-center shadow-lg shadow-red-200 hover:bg-red-600 transition-all hover:scale-110 active:scale-95"
                                >
                                    <PhoneOff size={28} />
                                </button>
                                <button
                                    onClick={() => setIsRecording(!isRecording)}
                                    className={`w-12 h-12 rounded-full flex items-center justify-center transition-all ${isRecording ? 'bg-indigo-100 text-indigo-600' : 'bg-slate-100 text-slate-600 hover:bg-slate-200'}`}
                                >
                                    <Circle size={20} fill={isRecording ? "currentColor" : "none"} className={isRecording ? "animate-pulse" : ""} />
                                </button>
                            </>
                        ) : (
                            <button
                                onClick={onClose}
                                className="px-6 py-2 bg-slate-900 text-white rounded-full font-bold hover:bg-slate-800 transition-colors"
                            >
                                Fermer le cockpit
                            </button>
                        )}
                    </div>
                </div>

                {/* Right Side: Notes & CRM */}
                <div className="flex-[0.8] p-8 flex flex-col">
                    <div className="mb-6 flex items-center gap-2 text-slate-900">
                        <MessageSquare size={20} className="text-indigo-600" />
                        <h3 className="font-bold">Compte-rendu d'appel</h3>
                    </div>

                    <textarea
                        value={note}
                        onChange={(e) => setNote(e.target.value)}
                        placeholder="Points clés abordés, objections, prochaines étapes..."
                        className="flex-1 w-full p-4 bg-slate-50 border border-slate-200 rounded-2xl resize-none focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all outline-none text-slate-700 placeholder:text-slate-400"
                    />

                    <div className="mt-6 space-y-3">
                        <div className="flex items-center gap-3 p-3 bg-indigo-50 rounded-xl text-indigo-700 text-sm">
                            <Clock size={16} />
                            <span>Rappel automatique suggéré dans 2 jours</span>
                        </div>
                        <button
                            disabled={status !== 'ENDED' && status !== 'IDLE'}
                            onClick={() => {
                                onSaveNote(note);
                                onClose();
                            }}
                            className="w-full py-3 bg-white border border-slate-200 text-slate-700 rounded-xl font-bold hover:bg-slate-50 transition-colors disabled:opacity-50"
                        >
                            Enregistrer et Quitter
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}
