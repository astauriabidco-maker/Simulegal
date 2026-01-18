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
    AlertTriangle,
    ShieldCheck,
    Smartphone
} from 'lucide-react';

export default function FleetMonitor() {
    const [devices, setDevices] = useState<KioskDevice[]>([]);
    const [isLoading, setIsLoading] = useState(true);
    const [copiedId, setCopiedId] = useState<string | null>(null);

    const loadDevices = async () => {
        setIsLoading(true);
        try {
            const allDevices = await DeviceStore.getAllDevices();
            setDevices(allDevices);
        } catch (error) {
            console.error('[FLEET] Erreur chargement:', error);
        } finally {
            setIsLoading(false);
        }
    };

    useEffect(() => {
        loadDevices();
        const interval = setInterval(loadDevices, 15000); // Rafraîchir toutes les 15s
        return () => clearInterval(interval);
    }, []);

    const isOnline = (lastHeartbeat: string) => {
        const diff = Date.now() - new Date(lastHeartbeat).getTime();
        return diff < 2 * 60 * 1000;
    };

    const handleCopyId = (id: string) => {
        navigator.clipboard.writeText(id);
        setCopiedId(id);
        setTimeout(() => setCopiedId(null), 2000);
    };

    const stats = {
        total: devices.length,
        online: devices.filter(d => d.status === 'ACTIVE' && isOnline(d.lastHeartbeat)).length,
        offline: devices.filter(d => d.status === 'ACTIVE' && !isOnline(d.lastHeartbeat)).length,
        unpaired: devices.filter(d => d.status === 'UNPAIRED').length,
    };

    return (
        <div className="p-8 bg-slate-50 min-h-full">
            <div className="mb-8">
                <h1 className="text-3xl font-black text-slate-900 flex items-center gap-3">
                    <Smartphone className="text-indigo-600" size={32} />
                    Gestionnaire de Flotte
                </h1>
                <p className="text-slate-500 font-medium mt-1">
                    Surveillance et appairage des terminaux SimuLegal en temps réel
                </p>
            </div>

            {/* Dash Cards */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
                <StatCard label="Total Flotte" value={stats.total} icon={<Tablet />} color="indigo" />
                <StatCard label="En Ligne" value={stats.online} icon={<Wifi />} color="emerald" />
                <StatCard label="Hors Ligne" value={stats.offline} icon={<WifiOff />} color="red" />
                <StatCard label="À Appairer" value={stats.unpaired} icon={<AlertTriangle />} color="amber" />
            </div>

            {/* List */}
            <div className="bg-white rounded-3xl shadow-sm border border-slate-200 overflow-hidden">
                <div className="p-6 border-b border-slate-100 flex items-center justify-between bg-slate-50/50">
                    <h2 className="font-black text-slate-800 uppercase tracking-wider text-sm">Bornes & Tablettes</h2>
                    <button
                        onClick={loadDevices}
                        className="p-2 hover:bg-slate-200 rounded-xl transition-colors text-slate-500"
                    >
                        <RefreshCw size={20} className={isLoading ? 'animate-spin' : ''} />
                    </button>
                </div>

                <div className="divide-y divide-slate-100">
                    {devices.length === 0 ? (
                        <div className="p-12 text-center text-slate-400">
                            <Smartphone size={48} className="mx-auto mb-4 opacity-20" />
                            <p className="font-bold uppercase tracking-widest text-xs">Aucun terminal détecté</p>
                        </div>
                    ) : (
                        devices.map((device) => (
                            <div key={device.id} className="p-6 hover:bg-slate-50/50 transition-colors flex items-center gap-6">
                                {/* Visual Status Indicator */}
                                <div className={`w-14 h-14 rounded-2xl flex items-center justify-center relative ${device.status === 'ACTIVE'
                                        ? (isOnline(device.lastHeartbeat) ? 'bg-emerald-100 text-emerald-600' : 'bg-red-100 text-red-600')
                                        : 'bg-amber-100 text-amber-600'
                                    }`}>
                                    <Tablet size={28} />
                                    {isOnline(device.lastHeartbeat) && device.status === 'ACTIVE' && (
                                        <span className="absolute -top-1 -right-1 w-4 h-4 bg-emerald-500 border-2 border-white rounded-full animate-pulse" />
                                    )}
                                </div>

                                {/* Main Info */}
                                <div className="flex-1 min-w-0">
                                    <div className="flex items-center gap-3 mb-1">
                                        <h3 className="font-black text-slate-900 truncate">{device.name}</h3>
                                        <span className={`px-2 py-0.5 rounded-lg text-[10px] font-black uppercase tracking-tighter ${device.status === 'ACTIVE'
                                                ? (isOnline(device.lastHeartbeat) ? 'bg-emerald-500 text-white' : 'bg-red-500 text-white')
                                                : 'bg-amber-500 text-white'
                                            }`}>
                                            {device.status === 'ACTIVE'
                                                ? (isOnline(device.lastHeartbeat) ? 'Online' : 'Offline')
                                                : 'Waiting Pairing'}
                                        </span>
                                    </div>
                                    <div className="flex items-center gap-4 text-xs font-bold text-slate-400">
                                        <span className="flex items-center gap-1">
                                            <Building2 size={12} />
                                            {device.assignedAgency?.name || 'STOCK'}
                                        </span>
                                        <span className="flex items-center gap-1">
                                            <Clock size={12} />
                                            {new Date(device.lastHeartbeat).toLocaleTimeString('fr-FR')}
                                        </span>
                                        <span className="bg-slate-100 px-1.5 py-0.5 rounded">v{device.appVersion}</span>
                                    </div>
                                </div>

                                {/* Pairing Tooltip/Code */}
                                {device.status === 'UNPAIRED' && (
                                    <div className="bg-slate-900 text-white px-4 py-3 rounded-2xl flex flex-col items-center">
                                        <span className="text-[9px] font-black text-slate-400 uppercase leading-none mb-1">Pairing Code</span>
                                        <span className="text-xl font-mono font-black tracking-widest leading-none">
                                            {device.pairingCode}
                                        </span>
                                    </div>
                                )}

                                {/* Action Buttons */}
                                <div className="flex items-center gap-2">
                                    <button
                                        onClick={() => handleCopyId(device.id)}
                                        className="w-10 h-10 flex items-center justify-center bg-slate-100 hover:bg-slate-200 rounded-xl text-slate-500 transition-colors"
                                        title="Copy ID"
                                    >
                                        {copiedId === device.id ? <Check size={18} className="text-emerald-500" /> : <Copy size={18} />}
                                    </button>
                                    <button
                                        onClick={() => DeviceStore.removeDevice(device.id).then(loadDevices)}
                                        className="w-10 h-10 flex items-center justify-center bg-red-50 hover:bg-red-100 rounded-xl text-red-500 transition-colors"
                                        title="Remove Device"
                                    >
                                        <Trash2 size={18} />
                                    </button>
                                </div>
                            </div>
                        ))
                    )}
                </div>
            </div>
        </div>
    );
}

function StatCard({ label, value, icon, color }: { label: string, value: number, icon: React.ReactNode, color: string }) {
    const colorMap: Record<string, string> = {
        indigo: 'bg-indigo-500 shadow-indigo-200',
        emerald: 'bg-emerald-500 shadow-emerald-200',
        red: 'bg-red-500 shadow-red-200',
        amber: 'bg-amber-500 shadow-amber-200'
    };

    return (
        <div className="bg-white rounded-3xl p-6 shadow-sm border border-slate-200">
            <div className="flex items-center justify-between mb-4">
                <div className={`p-3 rounded-2xl text-white ${colorMap[color]} shadow-lg`}>
                    {icon}
                </div>
                <span className="text-3xl font-black text-slate-900">{value}</span>
            </div>
            <p className="text-sm font-bold text-slate-500 uppercase tracking-widest">{label}</p>
        </div>
    );
}
