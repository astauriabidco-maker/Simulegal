'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { DeviceStore, KioskDevice } from '../../../services/DeviceStore';
import { AgencyStore, AgencyExt } from '../../../services/AgencyStore';
import {
    Tablet, Wifi, WifiOff, CheckCircle, Plus, Trash2, RefreshCw, Building2,
    Copy, Check, Clock, AlertTriangle, X, Search, XCircle, AlertCircle,
    BarChart3, Eye, Loader2, Signal, Cpu, Server
} from 'lucide-react';

export default function DevicesPage() {
    const [devices, setDevices] = useState<KioskDevice[]>([]);
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [loading, setLoading] = useState(true);
    const [searchQuery, setSearchQuery] = useState('');
    const [statusFilter, setStatusFilter] = useState<string>('');
    const [showPairingModal, setShowPairingModal] = useState(false);
    const [showStatsPanel, setShowStatsPanel] = useState(false);
    const [fleetStats, setFleetStats] = useState<any>(null);
    const [pairingCode, setPairingCode] = useState('');
    const [pairingAgencyId, setPairingAgencyId] = useState('');
    const [pairingDeviceName, setPairingDeviceName] = useState('');
    const [copiedId, setCopiedId] = useState<string | null>(null);
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'warning' } | null>(null);
    const [selectedDevice, setSelectedDevice] = useState<KioskDevice | null>(null);

    const showToast = useCallback((message: string, type: 'success' | 'error' | 'warning' = 'success') => {
        setToast({ message, type });
        setTimeout(() => setToast(null), 4000);
    }, []);

    const loadDevices = async () => {
        setLoading(true);
        const [allDevices, allAgencies] = await Promise.all([
            DeviceStore.getAllDevices(),
            AgencyStore.getAllAgencies()
        ]);
        setDevices(allDevices);
        setAgencies(allAgencies);
        setLoading(false);
    };

    useEffect(() => {
        loadDevices();
        const interval = setInterval(() => DeviceStore.getAllDevices().then(setDevices), 15000);
        return () => clearInterval(interval);
    }, []);

    const loadFleetStats = async () => {
        const data = await DeviceStore.getFleetStats();
        if (data) { setFleetStats(data); setShowStatsPanel(true); }
    };

    const isOnline = (lastHeartbeat: string) => (Date.now() - new Date(lastHeartbeat).getTime()) < 2 * 60 * 1000;
    const getTimeSince = (d: string) => {
        const mins = Math.floor((Date.now() - new Date(d).getTime()) / 60000);
        if (mins < 1) return 'à l\'instant';
        if (mins < 60) return `il y a ${mins}min`;
        const hrs = Math.floor(mins / 60);
        if (hrs < 24) return `il y a ${hrs}h`;
        return `il y a ${Math.floor(hrs / 24)}j`;
    };

    const filteredDevices = devices.filter(d => {
        if (searchQuery) {
            const q = searchQuery.toLowerCase();
            if (!d.name.toLowerCase().includes(q) && !d.pairingCode.toLowerCase().includes(q) && !d.assignedAgency?.name?.toLowerCase().includes(q)) return false;
        }
        if (statusFilter === 'ONLINE') return d.status === 'ACTIVE' && isOnline(d.lastHeartbeat);
        if (statusFilter === 'OFFLINE') return d.status === 'ACTIVE' && !isOnline(d.lastHeartbeat);
        if (statusFilter === 'UNPAIRED') return d.status === 'UNPAIRED';
        return true;
    });

    const handlePair = async () => {
        if (!pairingCode || !pairingAgencyId) return;
        const agency = agencies.find(a => a.id === pairingAgencyId);
        const result = await DeviceStore.pairDevice(pairingCode, pairingAgencyId, agency?.name || '', pairingDeviceName || undefined);
        if (result) {
            showToast(`Terminal appairé → ${agency?.name || pairingAgencyId}`);
            setShowPairingModal(false);
            setPairingCode(''); setPairingAgencyId(''); setPairingDeviceName('');
            loadDevices();
        } else {
            showToast('Code d\'appairage invalide', 'error');
        }
    };

    const handleReset = async (id: string) => {
        if (!confirm('Réinitialiser ce terminal ?')) return;
        await DeviceStore.resetDevice(id);
        showToast('Terminal réinitialisé', 'warning');
        loadDevices();
    };

    const handleDelete = async (id: string) => {
        if (!confirm('Supprimer définitivement ce terminal ?')) return;
        await DeviceStore.removeDevice(id);
        showToast('Terminal supprimé', 'warning');
        loadDevices();
    };

    const handleCopyId = (id: string) => {
        navigator.clipboard.writeText(id);
        setCopiedId(id);
        setTimeout(() => setCopiedId(null), 2000);
    };

    const onlineCount = devices.filter(d => d.status === 'ACTIVE' && isOnline(d.lastHeartbeat)).length;
    const offlineCount = devices.filter(d => d.status === 'ACTIVE' && !isOnline(d.lastHeartbeat)).length;
    const unpairedCount = devices.filter(d => d.status === 'UNPAIRED').length;

    return (
        <div className="p-8 h-full flex flex-col">
            {/* Toast */}
            {toast && (
                <div className={`fixed top-6 right-6 z-50 px-5 py-3 rounded-xl shadow-xl font-bold text-sm flex items-center gap-2 ${toast.type === 'success' ? 'bg-emerald-500 text-white' : toast.type === 'error' ? 'bg-rose-500 text-white' : 'bg-amber-500 text-white'}`}>
                    {toast.type === 'success' && <CheckCircle size={16} />}
                    {toast.type === 'error' && <XCircle size={16} />}
                    {toast.type === 'warning' && <AlertCircle size={16} />}
                    {toast.message}
                    <button onClick={() => setToast(null)} className="ml-2 opacity-70 hover:opacity-100"><X size={14} /></button>
                </div>
            )}

            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Flotte des Terminaux</h1>
                    <p className="text-slate-500 text-sm">Tablettes, bornes kiosques et points relais</p>
                </div>
                <div className="flex items-center gap-3">
                    <button onClick={loadFleetStats} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors">
                        <BarChart3 size={16} /> Stats Flotte
                    </button>
                    <button onClick={loadDevices} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors">
                        <RefreshCw size={16} /> Actualiser
                    </button>
                    <button onClick={() => setShowPairingModal(true)} className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors shadow-lg shadow-indigo-200">
                        <Plus size={16} /> Appairer
                    </button>
                </div>
            </div>

            {/* Stats cards */}
            <div className="grid grid-cols-4 gap-4 mb-6">
                {[
                    { label: 'Total', value: devices.length, icon: Tablet, color: 'text-slate-600', bg: 'bg-slate-100', filter: '' },
                    { label: 'En ligne', value: onlineCount, icon: Wifi, color: 'text-emerald-600', bg: 'bg-emerald-100', filter: 'ONLINE' },
                    { label: 'Hors ligne', value: offlineCount, icon: WifiOff, color: 'text-rose-600', bg: 'bg-rose-100', filter: 'OFFLINE' },
                    { label: 'Non appairés', value: unpairedCount, icon: AlertTriangle, color: 'text-amber-600', bg: 'bg-amber-100', filter: 'UNPAIRED' },
                ].map(s => (
                    <button key={s.label} onClick={() => setStatusFilter(statusFilter === s.filter ? '' : s.filter)}
                        className={`bg-white rounded-2xl p-5 border shadow-sm flex items-center gap-3 text-left transition-all ${statusFilter === s.filter ? 'border-indigo-400 ring-2 ring-indigo-100' : 'border-slate-200 hover:border-slate-300'}`}>
                        <div className={`w-10 h-10 ${s.bg} rounded-xl flex items-center justify-center`}><s.icon size={20} className={s.color} /></div>
                        <div><p className="text-xs text-slate-500 font-medium">{s.label}</p><p className={`text-2xl font-black ${s.color}`}>{s.value}</p></div>
                    </button>
                ))}
            </div>

            {/* Filters */}
            <div className="flex items-center gap-4 mb-6 p-4 bg-white rounded-2xl border border-slate-200 shadow-sm">
                <div className="flex items-center gap-2 flex-1">
                    <Search size={18} className="text-slate-400" />
                    <input type="text" placeholder="Rechercher par nom, code ou agence..." value={searchQuery} onChange={(e) => setSearchQuery(e.target.value)} className="flex-1 outline-none text-sm font-medium" />
                </div>
                {statusFilter && (
                    <button onClick={() => setStatusFilter('')} className="text-xs text-indigo-600 font-bold hover:text-indigo-700">Effacer filtre ✕</button>
                )}
                <span className="text-xs text-slate-400 font-medium">{filteredDevices.length} terminal{filteredDevices.length > 1 ? 'ux' : ''}</span>
            </div>

            {/* Offline alert banner */}
            {offlineCount > 0 && !statusFilter && (
                <div className="mb-4 p-4 bg-gradient-to-r from-amber-50 to-orange-50 rounded-xl border border-amber-200 flex items-center gap-3">
                    <AlertTriangle size={20} className="text-amber-600" />
                    <span className="text-sm font-bold text-amber-800">{offlineCount} terminal{offlineCount > 1 ? 'ux' : ''} hors ligne</span>
                    <button onClick={() => setStatusFilter('OFFLINE')} className="ml-auto text-xs text-amber-700 font-bold hover:text-amber-900 underline">Voir →</button>
                </div>
            )}

            {/* Devices Grid */}
            <div className="grid grid-cols-3 gap-4 flex-1 overflow-auto">
                {filteredDevices.map(device => {
                    const online = device.status === 'ACTIVE' && isOnline(device.lastHeartbeat);
                    const isUnpaired = device.status === 'UNPAIRED';
                    return (
                        <div key={device.id} className={`bg-white rounded-2xl border shadow-sm p-5 transition-all hover:shadow-md ${online ? 'border-emerald-200' : isUnpaired ? 'border-amber-200' : 'border-rose-200'}`}>
                            <div className="flex items-start justify-between mb-3">
                                <div className="flex items-center gap-3">
                                    <div className={`w-12 h-12 rounded-xl flex items-center justify-center ${online ? 'bg-emerald-100' : isUnpaired ? 'bg-amber-100' : 'bg-rose-100'}`}>
                                        <Tablet className={online ? 'text-emerald-600' : isUnpaired ? 'text-amber-600' : 'text-rose-500'} size={24} />
                                    </div>
                                    <div>
                                        <h3 className="font-bold text-slate-800 text-sm">{device.name}</h3>
                                        <span className={`text-[10px] font-bold px-2 py-0.5 rounded-md ${online ? 'bg-emerald-100 text-emerald-700' : isUnpaired ? 'bg-amber-100 text-amber-700' : 'bg-rose-100 text-rose-600'}`}>
                                            {online ? '● En ligne' : isUnpaired ? '◎ Non appairé' : '● Hors ligne'}
                                        </span>
                                    </div>
                                </div>
                                {/* Live pulse for online */}
                                {online && <span className="w-3 h-3 rounded-full bg-emerald-400 animate-pulse" />}
                            </div>

                            <div className="space-y-1.5 mb-3">
                                <div className="flex items-center gap-2 text-xs text-slate-500">
                                    <Building2 size={12} /><span>{device.assignedAgency?.name || 'Non assigné'}</span>
                                </div>
                                <div className="flex items-center gap-2 text-xs text-slate-400">
                                    <Clock size={12} /><span>{getTimeSince(device.lastHeartbeat)}</span>
                                </div>
                                <div className="flex items-center gap-2 text-xs text-slate-400">
                                    <Cpu size={12} /><span>v{device.appVersion}</span>
                                </div>
                            </div>

                            {/* Pairing code for unpaired */}
                            {isUnpaired && (
                                <div className="bg-amber-50 border border-amber-200 rounded-xl p-3 text-center mb-3">
                                    <p className="text-[9px] text-amber-600 font-bold uppercase mb-0.5">Code d'appairage</p>
                                    <p className="font-mono text-xl font-black text-amber-800 tracking-widest">{device.pairingCode}</p>
                                </div>
                            )}

                            {/* Actions */}
                            <div className="flex items-center justify-between pt-3 border-t border-slate-100">
                                <button onClick={() => handleCopyId(device.id)} className="flex items-center gap-1 text-xs text-slate-400 hover:text-slate-600 font-mono">
                                    {copiedId === device.id ? <Check size={11} className="text-emerald-500" /> : <Copy size={11} />}
                                    {device.id.slice(0, 10)}…
                                </button>
                                <div className="flex gap-1">
                                    <button onClick={() => setSelectedDevice(device)} className="p-1.5 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-colors" title="Détail"><Eye size={14} /></button>
                                    {device.status === 'ACTIVE' && (
                                        <button onClick={() => handleReset(device.id)} className="p-1.5 text-slate-400 hover:text-amber-600 hover:bg-amber-50 rounded-lg transition-colors" title="Réinitialiser"><RefreshCw size={14} /></button>
                                    )}
                                    <button onClick={() => handleDelete(device.id)} className="p-1.5 text-slate-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition-colors" title="Supprimer"><Trash2 size={14} /></button>
                                </div>
                            </div>
                        </div>
                    );
                })}

                {filteredDevices.length === 0 && (
                    <div className="col-span-3 p-12 text-center">
                        <Tablet className="mx-auto mb-4 text-slate-300" size={48} />
                        <p className="text-slate-500 font-medium">Aucun terminal trouvé</p>
                        <p className="text-slate-400 text-sm">Les terminaux s'enregistrent automatiquement via /kiosk</p>
                    </div>
                )}
            </div>

            {/* ═══ FLEET STATS PANEL ═══ */}
            {showStatsPanel && fleetStats && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex justify-end" onClick={() => setShowStatsPanel(false)}>
                    <div className="bg-white w-full max-w-lg shadow-2xl overflow-y-auto" onClick={e => e.stopPropagation()}>
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center sticky top-0 bg-white z-10">
                            <h2 className="text-xl font-black text-slate-900">📊 Stats Flotte</h2>
                            <button onClick={() => setShowStatsPanel(false)} className="text-slate-400 hover:text-slate-600"><X size={24} /></button>
                        </div>
                        <div className="p-6 space-y-6">
                            {/* Uptime */}
                            <div className="bg-slate-50 rounded-xl p-4">
                                <div className="flex items-center justify-between mb-2">
                                    <span className="text-xs font-bold text-slate-500">Taux de disponibilité</span>
                                    <span className={`text-2xl font-black ${fleetStats.uptimeRate >= 80 ? 'text-emerald-600' : fleetStats.uptimeRate >= 50 ? 'text-amber-600' : 'text-rose-600'}`}>{fleetStats.uptimeRate}%</span>
                                </div>
                                <div className="w-full bg-slate-200 rounded-full h-3">
                                    <div className={`h-3 rounded-full ${fleetStats.uptimeRate >= 80 ? 'bg-emerald-500' : fleetStats.uptimeRate >= 50 ? 'bg-amber-500' : 'bg-rose-500'}`} style={{ width: `${fleetStats.uptimeRate}%` }} />
                                </div>
                            </div>

                            {/* Quick KPIs */}
                            <div className="grid grid-cols-3 gap-3">
                                {[
                                    { label: 'En ligne', value: fleetStats.online, color: 'text-emerald-600' },
                                    { label: 'Hors ligne', value: fleetStats.offline, color: 'text-rose-600' },
                                    { label: 'Non appairés', value: fleetStats.unpaired, color: 'text-amber-600' },
                                ].map(s => (
                                    <div key={s.label} className="bg-slate-50 rounded-xl p-3 text-center">
                                        <p className="text-xs text-slate-400">{s.label}</p>
                                        <p className={`text-xl font-black ${s.color}`}>{s.value}</p>
                                    </div>
                                ))}
                            </div>

                            {/* Agency Coverage */}
                            <div className="bg-slate-50 rounded-xl p-4">
                                <h3 className="text-xs font-bold text-slate-500 mb-2">Couverture agences</h3>
                                <div className="flex items-center justify-between">
                                    <span className="text-sm text-slate-600">{fleetStats.agencyCoverage?.covered} / {fleetStats.agencyCoverage?.total} agences équipées</span>
                                    <span className="font-black text-indigo-600">{fleetStats.agencyCoverage?.total > 0 ? Math.round((fleetStats.agencyCoverage.covered / fleetStats.agencyCoverage.total) * 100) : 0}%</span>
                                </div>
                            </div>

                            {/* Versions */}
                            <div>
                                <h3 className="text-xs font-bold text-slate-500 mb-2">Versions logicielles</h3>
                                <div className="space-y-2">
                                    {Object.entries(fleetStats.versions || {}).map(([version, count]: [string, any]) => (
                                        <div key={version} className="flex items-center justify-between py-2 px-3 bg-slate-50 rounded-lg">
                                            <span className="font-mono text-sm text-slate-700">v{version}</span>
                                            <span className="font-black text-sm text-indigo-600">{count} terminal{count > 1 ? 'ux' : ''}</span>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            {/* Offline Alerts */}
                            {(fleetStats.offlineAlerts || []).length > 0 && (
                                <div>
                                    <h3 className="text-xs font-bold text-rose-500 mb-2">⚠️ Alertes hors ligne (&gt; 1h)</h3>
                                    <div className="space-y-2">
                                        {fleetStats.offlineAlerts.map((a: any) => (
                                            <div key={a.id} className="flex items-center gap-3 p-3 bg-rose-50 border border-rose-200 rounded-xl">
                                                <WifiOff size={16} className="text-rose-500" />
                                                <div className="flex-1">
                                                    <p className="text-sm font-bold text-slate-800">{a.name}</p>
                                                    <p className="text-xs text-slate-400">{a.agency}</p>
                                                </div>
                                                <span className="text-xs font-black text-rose-600">{a.downtime}h</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ DEVICE DETAIL ═══ */}
            {selectedDevice && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4" onClick={() => setSelectedDevice(null)}>
                    <div className="bg-white rounded-2xl shadow-xl w-full max-w-md overflow-hidden" onClick={e => e.stopPropagation()}>
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                            <h2 className="text-xl font-black text-slate-900">Détail Terminal</h2>
                            <button onClick={() => setSelectedDevice(null)} className="text-slate-400 hover:text-slate-600"><X size={24} /></button>
                        </div>
                        <div className="p-6 space-y-4">
                            {[
                                ['Nom', selectedDevice.name],
                                ['ID', selectedDevice.id],
                                ['Code', selectedDevice.pairingCode],
                                ['Statut', selectedDevice.status],
                                ['Agence', selectedDevice.assignedAgency?.name || 'Non assigné'],
                                ['Version', `v${selectedDevice.appVersion}`],
                                ['Dernier signal', new Date(selectedDevice.lastHeartbeat).toLocaleString('fr-FR')],
                                ['Créé le', new Date(selectedDevice.createdAt).toLocaleDateString('fr-FR')],
                            ].map(([k, v]) => (
                                <div key={k as string} className="flex justify-between py-2 border-b border-slate-100">
                                    <span className="text-xs font-bold text-slate-400 uppercase">{k}</span>
                                    <span className="text-sm font-medium text-slate-700 font-mono">{v as any}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ PAIRING MODAL ═══ */}
            {showPairingModal && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-xl w-full max-w-md overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                            <h2 className="text-xl font-black text-slate-900">Appairer un terminal</h2>
                            <button onClick={() => setShowPairingModal(false)} className="text-slate-400 hover:text-slate-600"><X size={24} /></button>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Code d'appairage</label>
                                <input type="text" value={pairingCode} onChange={(e) => setPairingCode(e.target.value.toUpperCase())} placeholder="ABCD-1234"
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl text-center font-mono text-2xl tracking-widest focus:ring-2 focus:ring-indigo-500 outline-none" />
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Agence assignée</label>
                                <select value={pairingAgencyId} onChange={(e) => setPairingAgencyId(e.target.value)}
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none">
                                    <option value="">Sélectionner une agence</option>
                                    {agencies.filter(a => a.status === 'ACTIVE').map(a => (
                                        <option key={a.id} value={a.id}>{a.name} ({a.type})</option>
                                    ))}
                                </select>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Nom du terminal (optionnel)</label>
                                <input type="text" value={pairingDeviceName} onChange={(e) => setPairingDeviceName(e.target.value)} placeholder="Borne Entrée"
                                    className="w-full p-3 border border-slate-200 rounded-xl outline-none" />
                            </div>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button onClick={() => setShowPairingModal(false)} className="flex-1 py-3 bg-white border border-slate-200 text-slate-600 font-bold rounded-xl hover:bg-slate-100">Annuler</button>
                            <button onClick={handlePair} disabled={!pairingCode || !pairingAgencyId}
                                className="flex-1 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 disabled:bg-slate-200 disabled:text-slate-400 shadow-lg">Appairer</button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
