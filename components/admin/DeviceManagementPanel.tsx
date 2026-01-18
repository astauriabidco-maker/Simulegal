'use client';

import React, { useState, useEffect } from 'react';
import { DeviceStore, KioskDevice } from '../../services/DeviceStore';
import {
    Tablet,
    Wifi,
    WifiOff,
    CheckCircle,
    Plus,
    Trash2,
    RefreshCw,
    Building2,
    Copy,
    Check,
    Clock,
    AlertTriangle
} from 'lucide-react';

export default function DeviceManagementPanel() {
    const [devices, setDevices] = useState<KioskDevice[]>([]);
    const [showPairingModal, setShowPairingModal] = useState(false);
    const [pairingCode, setPairingCode] = useState('');
    const [pairingAgencyId, setPairingAgencyId] = useState('');
    const [pairingDeviceName, setPairingDeviceName] = useState('');
    const [copiedId, setCopiedId] = useState<string | null>(null);

    // Charger les devices (async)
    const loadDevices = async () => {
        const allDevices = await DeviceStore.getAllDevices();
        setDevices(allDevices);
    };

    useEffect(() => {
        loadDevices();
        // Rafraîchir toutes les 10 secondes
        const interval = setInterval(loadDevices, 10000);
        return () => clearInterval(interval);
    }, []);


    // Appairer un device (async)
    const handlePairDevice = async () => {
        if (!pairingCode || !pairingAgencyId) return;

        const result = await DeviceStore.pairDevice(
            pairingCode,
            pairingAgencyId,
            pairingAgencyId === 'HQ-001' ? 'SimuLegal HQ' : 'Agence ' + pairingAgencyId,
            pairingDeviceName || undefined
        );

        if (result) {
            await loadDevices();
            setShowPairingModal(false);
            setPairingCode('');
            setPairingAgencyId('');
            setPairingDeviceName('');
        }
    };


    // Supprimer un device (async)
    const handleDeleteDevice = async (deviceId: string) => {
        if (confirm('Supprimer ce terminal ?')) {
            await DeviceStore.removeDevice(deviceId);
            await loadDevices();
        }
    };

    // Réinitialiser un device (async)
    const handleResetDevice = async (deviceId: string) => {
        if (confirm('Réinitialiser ce terminal ? Il devra être ré-appairé.')) {
            await DeviceStore.resetDevice(deviceId);
            await loadDevices();
        }
    };


    // Copier l'ID
    const handleCopyId = (id: string) => {
        navigator.clipboard.writeText(id);
        setCopiedId(id);
        setTimeout(() => setCopiedId(null), 2000);
    };

    // Vérifier si le heartbeat est récent (moins de 2 minutes)
    const isOnline = (lastHeartbeat: string) => {
        const diff = Date.now() - new Date(lastHeartbeat).getTime();
        return diff < 2 * 60 * 1000;
    };

    const getStatusBadge = (device: KioskDevice) => {
        if (device.status === 'UNPAIRED') {
            return (
                <span className="flex items-center gap-1 px-2 py-1 rounded-full bg-amber-100 text-amber-700 text-xs font-bold">
                    <AlertTriangle size={12} />
                    Non appairé
                </span>
            );
        }

        if (device.status === 'ACTIVE' && isOnline(device.lastHeartbeat)) {
            return (
                <span className="flex items-center gap-1 px-2 py-1 rounded-full bg-emerald-100 text-emerald-700 text-xs font-bold">
                    <Wifi size={12} />
                    En ligne
                </span>
            );
        }

        return (
            <span className="flex items-center gap-1 px-2 py-1 rounded-full bg-red-100 text-red-700 text-xs font-bold">
                <WifiOff size={12} />
                Hors ligne
            </span>
        );
    };

    return (
        <div className="p-8">
            {/* Header */}
            <div className="flex items-center justify-between mb-8">
                <div>
                    <h1 className="text-3xl font-black text-slate-900">Gestion des Terminaux</h1>
                    <p className="text-slate-500 font-medium">
                        Gérez les tablettes et bornes déployées dans le réseau
                    </p>
                </div>

                <div className="flex gap-3">
                    <button
                        onClick={loadDevices}
                        className="flex items-center gap-2 px-4 py-2 bg-slate-100 hover:bg-slate-200 rounded-xl font-bold text-slate-700 transition-colors"
                    >
                        <RefreshCw size={16} />
                        Actualiser
                    </button>
                    <button
                        onClick={() => setShowPairingModal(true)}
                        className="flex items-center gap-2 px-4 py-2 bg-indigo-600 hover:bg-indigo-700 rounded-xl font-bold text-white transition-colors"
                    >
                        <Plus size={16} />
                        Appairer un terminal
                    </button>
                </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
                <div className="bg-white rounded-2xl p-6 border border-slate-100">
                    <div className="flex items-center gap-3 mb-2">
                        <Tablet className="text-indigo-600" />
                        <span className="text-2xl font-black text-slate-900">{devices.length}</span>
                    </div>
                    <p className="text-sm text-slate-500">Terminaux totaux</p>
                </div>
                <div className="bg-white rounded-2xl p-6 border border-slate-100">
                    <div className="flex items-center gap-3 mb-2">
                        <Wifi className="text-emerald-600" />
                        <span className="text-2xl font-black text-slate-900">
                            {devices.filter(d => d.status === 'ACTIVE' && isOnline(d.lastHeartbeat)).length}
                        </span>
                    </div>
                    <p className="text-sm text-slate-500">En ligne</p>
                </div>
                <div className="bg-white rounded-2xl p-6 border border-slate-100">
                    <div className="flex items-center gap-3 mb-2">
                        <WifiOff className="text-red-600" />
                        <span className="text-2xl font-black text-slate-900">
                            {devices.filter(d => d.status === 'ACTIVE' && !isOnline(d.lastHeartbeat)).length}
                        </span>
                    </div>
                    <p className="text-sm text-slate-500">Hors ligne</p>
                </div>
                <div className="bg-white rounded-2xl p-6 border border-slate-100">
                    <div className="flex items-center gap-3 mb-2">
                        <AlertTriangle className="text-amber-600" />
                        <span className="text-2xl font-black text-slate-900">
                            {devices.filter(d => d.status === 'UNPAIRED').length}
                        </span>
                    </div>
                    <p className="text-sm text-slate-500">En attente</p>
                </div>
            </div>

            {/* Devices List */}
            <div className="bg-white rounded-2xl border border-slate-100 overflow-hidden">
                <div className="p-6 border-b border-slate-100">
                    <h2 className="font-bold text-slate-900">Liste des terminaux</h2>
                </div>

                {devices.length === 0 ? (
                    <div className="p-12 text-center">
                        <Tablet className="mx-auto mb-4 text-slate-300" size={48} />
                        <p className="text-slate-500 font-medium">Aucun terminal enregistré</p>
                        <p className="text-slate-400 text-sm">
                            Les terminaux s'enregistrent automatiquement en accédant à /kiosk
                        </p>
                    </div>
                ) : (
                    <div className="divide-y divide-slate-50">
                        {devices.map((device) => (
                            <div key={device.id} className="p-4 hover:bg-slate-50 transition-colors">
                                <div className="flex items-center gap-4">
                                    {/* Icon */}
                                    <div className={`w-12 h-12 rounded-xl flex items-center justify-center ${device.status === 'ACTIVE' ? 'bg-emerald-100' :
                                        device.status === 'UNPAIRED' ? 'bg-amber-100' : 'bg-slate-100'
                                        }`}>
                                        <Tablet className={
                                            device.status === 'ACTIVE' ? 'text-emerald-600' :
                                                device.status === 'UNPAIRED' ? 'text-amber-600' : 'text-slate-400'
                                        } size={24} />
                                    </div>

                                    {/* Info */}
                                    <div className="flex-1 min-w-0">
                                        <div className="flex items-center gap-2 mb-1">
                                            <p className="font-bold text-slate-900">{device.name}</p>
                                            {getStatusBadge(device)}
                                        </div>
                                        <div className="flex items-center gap-4 text-xs text-slate-400">
                                            <span className="flex items-center gap-1">
                                                <Building2 size={12} />
                                                {device.assignedAgency?.name || 'Non assigné'}
                                            </span>

                                            <span className="flex items-center gap-1">
                                                <Clock size={12} />
                                                {new Date(device.lastHeartbeat).toLocaleString('fr-FR')}
                                            </span>
                                            <span>v{device.appVersion}</span>
                                        </div>
                                    </div>

                                    {/* Pairing Code (si non appairé) */}
                                    {device.status === 'UNPAIRED' && (
                                        <div className="bg-amber-50 border border-amber-200 rounded-xl px-4 py-2 text-center">
                                            <p className="text-[10px] text-amber-600 font-bold uppercase">Code</p>
                                            <p className="font-mono text-lg font-black text-amber-800">
                                                {device.pairingCode}
                                            </p>
                                        </div>
                                    )}

                                    {/* ID (copiable) */}
                                    <button
                                        onClick={() => handleCopyId(device.id)}
                                        className="flex items-center gap-1 px-3 py-1.5 bg-slate-100 hover:bg-slate-200 rounded-lg text-xs font-mono text-slate-600 transition-colors"
                                    >
                                        {copiedId === device.id ? <Check size={12} /> : <Copy size={12} />}
                                        {device.id.slice(0, 8)}...
                                    </button>

                                    {/* Actions */}
                                    <div className="flex gap-2">
                                        {device.status === 'ACTIVE' && (
                                            <button
                                                onClick={() => handleResetDevice(device.id)}
                                                className="p-2 text-slate-400 hover:text-amber-600 hover:bg-amber-50 rounded-lg transition-colors"
                                                title="Réinitialiser"
                                            >
                                                <RefreshCw size={16} />
                                            </button>
                                        )}
                                        <button
                                            onClick={() => handleDeleteDevice(device.id)}
                                            className="p-2 text-slate-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition-colors"
                                            title="Supprimer"
                                        >
                                            <Trash2 size={16} />
                                        </button>
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>
                )}
            </div>

            {/* Modal d'appairage */}
            {showPairingModal && (
                <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
                    <div className="bg-white rounded-3xl w-full max-w-md p-8">
                        <h2 className="text-2xl font-black text-slate-900 mb-2">Appairer un terminal</h2>
                        <p className="text-slate-500 mb-6">Entrez le code affiché sur le terminal</p>

                        <div className="space-y-4">
                            <div>
                                <label className="block text-sm font-bold text-slate-700 mb-2">
                                    Code d'appairage
                                </label>
                                <input
                                    type="text"
                                    value={pairingCode}
                                    onChange={(e) => setPairingCode(e.target.value.toUpperCase())}
                                    placeholder="ABCD-1234"
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl text-center font-mono text-2xl tracking-widest focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-bold text-slate-700 mb-2">
                                    Agence assignée
                                </label>
                                <select
                                    value={pairingAgencyId}
                                    onChange={(e) => setPairingAgencyId(e.target.value)}
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none"
                                >
                                    <option value="">Sélectionner une agence</option>
                                    <option value="HQ-001">SimuLegal HQ (Paris)</option>
                                    <option value="OWN-001">SimuLegal Direct Lyon</option>
                                    <option value="FRAN-001">Franchise Marseille</option>
                                    <option value="RELAY-001">Point Relais Bordeaux</option>
                                </select>
                            </div>

                            <div>
                                <label className="block text-sm font-bold text-slate-700 mb-2">
                                    Nom du terminal (optionnel)
                                </label>
                                <input
                                    type="text"
                                    value={pairingDeviceName}
                                    onChange={(e) => setPairingDeviceName(e.target.value)}
                                    placeholder="Ex: Tablette Entrée"
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none"
                                />
                            </div>
                        </div>

                        <div className="flex gap-3 mt-8">
                            <button
                                onClick={() => setShowPairingModal(false)}
                                className="flex-1 py-3 bg-slate-100 hover:bg-slate-200 rounded-xl font-bold text-slate-700 transition-colors"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handlePairDevice}
                                disabled={!pairingCode || !pairingAgencyId}
                                className="flex-1 py-3 bg-indigo-600 hover:bg-indigo-700 disabled:bg-slate-200 disabled:text-slate-400 rounded-xl font-bold text-white transition-colors"
                            >
                                Appairer
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
