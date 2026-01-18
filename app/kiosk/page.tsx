'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { DeviceStore, KioskDevice } from '../../services/DeviceStore';
import CheckoutFlow from '../../components/CheckoutFlow';
import {
    Tablet,
    Wifi,
    WifiOff,
    CheckCircle,
    Loader2,
    RefreshCw,
    Building2
} from 'lucide-react';

type KioskState = 'LOADING' | 'PAIRING' | 'ACTIVE';

const DEVICE_ID_KEY = 'kiosk_device_id';
const POLLING_INTERVAL = 5000; // 5 secondes pour vérifier le statut
const HEARTBEAT_INTERVAL = 60000; // 60 secondes pour le heartbeat

export default function KioskLauncherPage() {
    const [kioskState, setKioskState] = useState<KioskState>('LOADING');
    const [device, setDevice] = useState<KioskDevice | null>(null);
    const [showCheckout, setShowCheckout] = useState(false);

    // Enregistre un nouveau device (async)
    const registerNewDevice = useCallback(async () => {
        try {
            const newDevice = await DeviceStore.registerNewDevice();
            localStorage.setItem(DEVICE_ID_KEY, newDevice.id);
            setDevice(newDevice);
            setKioskState('PAIRING');
        } catch (error) {
            console.error('[KIOSK] ❌ Erreur enregistrement:', error);
        }
    }, []);

    // Initialisation du kiosque (async)
    const initializeKiosk = useCallback(async () => {
        const savedDeviceId = localStorage.getItem(DEVICE_ID_KEY);

        if (savedDeviceId) {
            // Device déjà enregistré, vérifie son statut
            const existingDevice = await DeviceStore.getDeviceById(savedDeviceId);

            if (existingDevice) {
                setDevice(existingDevice);

                if (existingDevice.status === 'ACTIVE') {
                    setKioskState('ACTIVE');
                    DeviceStore.sendHeartbeat(existingDevice.id);
                } else {
                    setKioskState('PAIRING');
                }
            } else {
                // Device non trouvé, on le réenregistre
                await registerNewDevice();
            }
        } else {
            // Nouveau device, on l'enregistre
            await registerNewDevice();
        }
    }, [registerNewDevice]);

    // Polling pour vérifier le statut d'appairage (async)
    useEffect(() => {
        if (kioskState !== 'PAIRING' || !device) return;

        const interval = setInterval(async () => {
            const updatedDevice = await DeviceStore.getDeviceById(device.id);
            if (updatedDevice && updatedDevice.status === 'ACTIVE') {
                setDevice(updatedDevice);
                setKioskState('ACTIVE');
                console.log('[KIOSK] ✅ Device appairé, passage en mode actif');
            }
        }, POLLING_INTERVAL);

        return () => clearInterval(interval);
    }, [kioskState, device]);

    // Heartbeat pour les devices actifs
    useEffect(() => {
        if (kioskState !== 'ACTIVE' || !device) return;

        const interval = setInterval(() => {
            DeviceStore.sendHeartbeat(device.id);
            console.log('[KIOSK] 💓 Heartbeat envoyé');
        }, HEARTBEAT_INTERVAL);

        // Premier heartbeat immédiat
        DeviceStore.sendHeartbeat(device.id);

        return () => clearInterval(interval);
    }, [kioskState, device]);

    // Initialisation au montage
    useEffect(() => {
        initializeKiosk();
    }, [initializeKiosk]);

    // ============================================
    // ÉTAT: CHARGEMENT
    // ============================================
    if (kioskState === 'LOADING') {
        return (
            <div className="fixed inset-0 bg-gradient-to-br from-slate-900 to-slate-800 flex items-center justify-center">
                <div className="text-center">
                    <Loader2 className="w-16 h-16 text-indigo-500 animate-spin mx-auto mb-4" />
                    <p className="text-slate-400">Initialisation du terminal...</p>
                </div>
            </div>
        );
    }

    // ============================================
    // ÉTAT: ATTENTE D'APPAIRAGE
    // ============================================
    // ============================================
    // ÉTAT: SCAN / ACTIVATION
    // ============================================
    if (kioskState === 'PAIRING') {
        return (
            <div className="fixed inset-0 bg-gradient-to-br from-indigo-900 via-indigo-800 to-purple-900 flex items-center justify-center p-8">
                <div className="text-center max-w-lg w-full">
                    {/* Icône */}
                    <div className="w-24 h-24 bg-white/10 backdrop-blur-xl rounded-3xl flex items-center justify-center mx-auto mb-8 border border-white/20">
                        <RefreshCw className="w-10 h-10 text-white animate-pulse" />
                    </div>

                    {/* Titre */}
                    <h1 className="text-3xl font-black text-white mb-2">
                        Activation requise
                    </h1>
                    <p className="text-indigo-200 text-sm mb-8">
                        Veuillez saisir le code d'activation fourni par le siège pour configurer ce terminal.
                    </p>

                    {/* Formulaire Code */}
                    <div className="bg-white rounded-[32px] p-2 shadow-2xl mb-8">
                        <input
                            type="text"
                            placeholder="XXXX-XXXX"
                            maxLength={9}
                            className="w-full h-20 bg-transparent text-center text-4xl font-black text-slate-900 placeholder:text-slate-200 outline-none uppercase font-mono tracking-widest"
                            onKeyDown={async (e) => {
                                if (e.key === 'Enter') {
                                    const code = (e.currentTarget.value).toUpperCase();
                                    if (code.length >= 8) {
                                        const device = await DeviceStore.activateDevice(code);
                                        if (device) {
                                            localStorage.setItem(DEVICE_ID_KEY, device.id);
                                            setDevice(device);
                                            setKioskState('ACTIVE');
                                        } else {
                                            alert('Code invalide');
                                        }
                                    }
                                }
                            }}
                        />
                    </div>

                    {/* Info device */}
                    <div className="mt-8 pt-6 border-t border-white/5">
                        {device && (
                            <p className="text-indigo-300/40 text-[10px] font-mono">
                                ID PROVISOIRE: {device.id.slice(0, 8)}...
                            </p>
                        )}
                        <p className="text-indigo-300/40 text-[10px] font-mono mt-2">
                            Appuyez sur ENTRÉE pour valider
                        </p>
                    </div>
                </div>
            </div>
        );
    }

    // ============================================
    // ÉTAT: ACTIF (Mode Kiosk)
    // ============================================
    if (kioskState === 'ACTIVE' && device) {
        // Si le checkout est ouvert, on affiche le flow
        if (showCheckout) {
            return (
                <CheckoutFlow
                    isOpen={true}
                    onClose={() => setShowCheckout(false)}
                    serviceId="simulator"
                    serviceName="Service Kiosk"
                    price={0}
                    isKioskMode={true}
                />
            );
        }

        // Écran d'accueil du kiosque
        return (
            <div className="fixed inset-0 bg-gradient-to-br from-emerald-600 via-emerald-500 to-teal-500 flex flex-col items-center justify-center p-8">
                {/* Header avec statut */}
                <div className="absolute top-4 left-4 right-4 flex items-center justify-between">
                    <div className="flex items-center gap-2 bg-white/10 backdrop-blur-sm rounded-full px-4 py-2">
                        <Wifi className="w-4 h-4 text-white" />
                        <span className="text-white text-sm font-medium">Connecté</span>
                    </div>
                    <div className="flex items-center gap-2 bg-white/10 backdrop-blur-sm rounded-full px-4 py-2">
                        <Building2 className="w-4 h-4 text-white" />
                        <span className="text-white text-sm font-medium">{device.assignedAgency?.name || device.name}</span>
                    </div>
                </div>

                {/* Contenu principal */}
                <div className="text-center">
                    <div className="w-24 h-24 bg-white rounded-3xl flex items-center justify-center mx-auto mb-8 shadow-2xl">
                        <CheckCircle className="w-12 h-12 text-emerald-500" />
                    </div>

                    <h1 className="text-4xl font-black text-white mb-4">
                        Terminal Actif
                    </h1>
                    <p className="text-emerald-100 text-lg mb-12">
                        Appuyez sur l'écran pour démarrer
                    </p>

                    {/* Bouton principal */}
                    <button
                        onClick={() => setShowCheckout(true)}
                        className="bg-white hover:bg-emerald-50 text-emerald-600 px-16 py-6 rounded-3xl font-black text-2xl shadow-2xl transform hover:scale-105 transition-all active:scale-95"
                    >
                        Commencer
                    </button>
                </div>

                {/* Footer */}
                <div className="absolute bottom-4 left-4 right-4 text-center">
                    <p className="text-emerald-200/60 text-xs">
                        {device.name} • v{device.appVersion}
                    </p>
                </div>
            </div>
        );
    }

    return null;
}
