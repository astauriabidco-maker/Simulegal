'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Device, Call } from '@twilio/voice-sdk';
import {
    Phone,
    PhoneOff,
    Mic,
    MicOff,
    User,
    Circle,
    X,
    MessageSquare,
    Clock
} from 'lucide-react';
import { SalesStore, Prospect } from '../../services/SalesStore';
import { AuthStore } from '../../services/authStore';

interface CallCockpitProps {
    prospect: Prospect;
    onClose: () => void;
    onSaveNote: (note: string) => void;
}

export default function CallCockpit({ prospect, onClose, onSaveNote }: CallCockpitProps) {
    const [status, setStatus] = useState<'IDLE' | 'CALLING' | 'CONNECTED' | 'ENDED' | 'ERROR'>('IDLE');
    const [duration, setDuration] = useState(0);
    const [isMuted, setIsMuted] = useState(false);
    const [note, setNote] = useState('');
    const [device, setDevice] = useState<Device | null>(null);
    const [activeConnection, setActiveConnection] = useState<Call | null>(null);
    const [errorMessage, setErrorMessage] = useState('');

    // Timer ref
    const timerRef = useRef<NodeJS.Timeout | null>(null);

    // Initialisation Twilio Device
    useEffect(() => {
        const initDevice = async () => {
            try {
                // 1. Récupérer le token depuis le backend
                // Note: On utilise fetch direct ici car SalesStore n'a pas encore cette méthode
                const tokenResponse = await fetch('http://localhost:3001/voice/token', {
                    headers: {
                        'Authorization': `Bearer ${AuthStore.getToken()}`
                    }
                });

                if (!tokenResponse.ok) throw new Error('Impossible de récupérer le token Twilio');

                const { token } = await tokenResponse.json();

                // 2. Créer le Device
                const newDevice = new Device(token, {
                    logLevel: 1,
                    codecPreferences: ['opus', 'pcmu']
                });

                // 3. Listeners
                newDevice.on('ready', () => console.log('Twilio Device Ready'));
                newDevice.on('error', (error) => {
                    console.error('Twilio Error:', error);
                    setErrorMessage(error.message);
                    setStatus('ERROR');
                });

                // Pas besoin de newDevice.register() si on ne fait que des appels sortants
                setDevice(newDevice);

            } catch (err: any) {
                console.error('Setup failed', err);
                setErrorMessage(err.message);
                setStatus('ERROR');
            }
        };

        if (!prospect.phone) {
            setErrorMessage("Le prospect n'a pas de numéro de téléphone.");
            setStatus('ERROR');
        } else {
            initDevice();
        }

        // Cleanup
        return () => {
            if (device) device.destroy();
            if (activeConnection) activeConnection.disconnect();
            if (timerRef.current) clearInterval(timerRef.current);
        };
    }, [prospect.phone]);

    // Timer logic
    useEffect(() => {
        if (status === 'CONNECTED') {
            timerRef.current = setInterval(() => {
                setDuration(prev => prev + 1);
            }, 1000);
        } else {
            if (timerRef.current) clearInterval(timerRef.current);
        }
    }, [status]);

    const formatDuration = (s: number) => {
        const mins = Math.floor(s / 60);
        const secs = s % 60;
        return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    };

    const handleStartCall = async () => {
        if (!device) return;

        setStatus('CALLING');

        try {
            const params = { To: prospect.phone };
            const call = await device.connect({ params });

            call.on('accept', () => {
                setStatus('CONNECTED');
                setActiveConnection(call);
            });

            call.on('disconnect', () => {
                setStatus('ENDED');
                setActiveConnection(null);
            });

            call.on('error', (err) => {
                console.error('Call Error:', err);
                setErrorMessage(err.message || 'Erreur lors de l\'appel');
                setStatus('ERROR');
            });

        } catch (err: any) {
            console.error('Connection failed', err);
            setErrorMessage(err.message);
            setStatus('ERROR');
        }
    };

    const handleEndCall = () => {
        if (activeConnection) {
            activeConnection.disconnect();
        }
        setStatus('ENDED'); // Force UI update just in case
    };

    const toggleMute = () => {
        if (activeConnection) {
            const newMuted = !isMuted;
            activeConnection.mute(newMuted);
            setIsMuted(newMuted);
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
                        {status === 'IDLE' && <span className="text-slate-400 font-medium italic">Prêt pour l'appel (Twilio Ready)</span>}
                        {status === 'CALLING' && (
                            <span className="flex items-center gap-2 text-indigo-600 font-semibold animate-pulse">
                                <Circle size={8} fill="currentColor" /> Appelle en cours...
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
                        {status === 'ERROR' && <span className="text-red-500 font-bold text-sm max-w-[200px] truncate">{errorMessage || 'Erreur'}</span>}
                    </div>

                    <div className="flex items-center gap-6">
                        {status === 'IDLE' || status === 'ERROR' ? (
                            <button
                                onClick={handleStartCall}
                                disabled={status === 'ERROR' && !device}
                                className="w-16 h-16 bg-emerald-500 text-white rounded-full flex items-center justify-center shadow-lg shadow-emerald-200 hover:bg-emerald-600 transition-all hover:scale-110 active:scale-95 disabled:opacity-50 disabled:grayscale"
                            >
                                <Phone size={28} />
                            </button>
                        ) : (status === 'CONNECTED' || status === 'CALLING') ? (
                            <>
                                <button
                                    onClick={toggleMute}
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
                            disabled={status !== 'ENDED' && status !== 'IDLE' && status !== 'ERROR'}
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
