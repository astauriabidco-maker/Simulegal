'use client';

import React, { useEffect, useState, useMemo, useCallback } from 'react';
import { AgencyExt, AgencyStore } from '../../../services/AgencyStore';
import {
    Plus, Search, Building2, Store, MapPin, Download, Trash2, Edit2,
    ExternalLink, Users, TrendingUp, MoreHorizontal, Check, X, BarChart3,
    Kanban, AlertCircle, CheckCircle, XCircle, Eye, Loader2, LayoutGrid, List, Map
} from 'lucide-react';

type AgencyType = 'FRANCHISE' | 'CORNER' | 'OWNED' | 'HQ';
type ViewMode = 'TABLE' | 'CARDS' | 'MAP' | 'ANALYTICS';

const TYPE_CONFIG: Record<AgencyType, { label: string; color: string; bgColor: string; icon: any }> = {
    FRANCHISE: { label: 'Franchise', color: 'text-indigo-700', bgColor: 'bg-indigo-100', icon: Building2 },
    CORNER: { label: 'Corner', color: 'text-purple-700', bgColor: 'bg-purple-100', icon: Store },
    OWNED: { label: 'Propre', color: 'text-emerald-700', bgColor: 'bg-emerald-100', icon: Building2 },
    HQ: { label: 'Siège', color: 'text-slate-700', bgColor: 'bg-slate-100', icon: Building2 }
};

const REGIONS = ['IDF', 'AURA', 'PACA', 'HDF', 'NAQ', 'OCC', 'BRE', 'NOR', 'GES', 'PDL', 'BFC', 'CVL'];

export default function AgenciesPage() {
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [loading, setLoading] = useState(true);
    const [viewMode, setViewMode] = useState<ViewMode>('TABLE');
    const [searchQuery, setSearchQuery] = useState('');
    const [typeFilter, setTypeFilter] = useState<string>('');
    const [statusFilter, setStatusFilter] = useState<string>('');
    const [regionFilter, setRegionFilter] = useState<string>('');
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [editingAgency, setEditingAgency] = useState<AgencyExt | null>(null);
    const [selectedAgency, setSelectedAgency] = useState<any>(null);
    const [networkAnalytics, setNetworkAnalytics] = useState<any>(null);
    const [mapData, setMapData] = useState<any[]>([]);
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'warning' } | null>(null);
    const [formData, setFormData] = useState({
        name: '', type: 'FRANCHISE', contactEmail: '', region: 'IDF',
        city: 'Paris', zipCodes: '', commissionRate: 15, kioskUrl: ''
    });

    const showToast = useCallback((message: string, type: 'success' | 'error' | 'warning' = 'success') => {
        setToast({ message, type });
        setTimeout(() => setToast(null), 4000);
    }, []);

    useEffect(() => { loadAgencies(); }, []);
    useEffect(() => {
        if (viewMode === 'ANALYTICS') loadNetworkAnalytics();
        if (viewMode === 'MAP') loadMapData();
    }, [viewMode]);

    const loadAgencies = async () => {
        setLoading(true);
        const data = await AgencyStore.getAllAgencies();
        setAgencies(data);
        setLoading(false);
    };

    const loadNetworkAnalytics = async () => {
        const data = await AgencyStore.getNetworkAnalytics();
        if (data) setNetworkAnalytics(data);
    };

    const loadMapData = async () => {
        const data = await AgencyStore.getMapData();
        if (data) setMapData(data);
    };

    const openDetail = async (id: string) => {
        const data = await AgencyStore.getAgencyPerformance(id);
        if (data) setSelectedAgency(data);
    };

    const filteredAgencies = useMemo(() => {
        return agencies.filter(a => {
            if (searchQuery) {
                const q = searchQuery.toLowerCase();
                if (!a.name.toLowerCase().includes(q) && !a.contactEmail?.toLowerCase().includes(q)) return false;
            }
            if (typeFilter && a.type !== typeFilter) return false;
            if (statusFilter && a.status !== statusFilter) return false;
            if (regionFilter && a.region !== regionFilter) return false;
            return true;
        });
    }, [agencies, searchQuery, typeFilter, statusFilter, regionFilter]);

    const handleOpenModal = (agency: AgencyExt | null = null) => {
        if (agency) {
            setEditingAgency(agency);
            setFormData({
                name: agency.name, type: agency.type, contactEmail: agency.contactEmail || '',
                region: agency.region || 'IDF', city: '',
                zipCodes: Array.isArray(agency.zipCodes) ? agency.zipCodes.join(', ') : (agency.zipCodes || ''),
                commissionRate: agency.commissionRate || 15, kioskUrl: agency.kioskUrl || ''
            });
        } else {
            setEditingAgency(null);
            setFormData({ name: '', type: 'FRANCHISE', contactEmail: '', region: 'IDF', city: 'Paris', zipCodes: '', commissionRate: 15, kioskUrl: '' });
        }
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        if (!formData.name) { showToast('Le nom est requis', 'error'); return; }
        if (editingAgency) {
            await AgencyStore.updateAgency(editingAgency.id, formData);
            showToast('Agence mise à jour');
        } else {
            await AgencyStore.addAgency({
                ...formData,
                id: `${formData.type.substring(0, 3)}-${formData.region}-${Date.now().toString().slice(-4)}`,
                status: 'ACTIVE'
            });
            showToast('Agence créée avec succès');
        }
        setIsModalOpen(false);
        loadAgencies();
    };

    const handleDelete = async (id: string, name: string) => {
        if (!confirm(`Désactiver l'agence "${name}" ?`)) return;
        await AgencyStore.deleteAgency(id);
        showToast(`${name} désactivée`, 'warning');
        loadAgencies();
    };

    const getHealthColor = (score: number) => {
        if (score >= 70) return 'bg-emerald-500';
        if (score >= 40) return 'bg-amber-500';
        return 'bg-rose-500';
    };

    const activeCount = agencies.filter(a => a.status === 'ACTIVE').length;
    const franchiseCount = agencies.filter(a => a.type === 'FRANCHISE').length;
    const cornerCount = agencies.filter(a => a.type === 'CORNER').length;

    return (
        <div className="p-8 h-full flex flex-col">
            {/* Toast */}
            {toast && (
                <div className={`fixed top-6 right-6 z-50 px-5 py-3 rounded-xl shadow-xl font-bold text-sm flex items-center gap-2 transition-all ${toast.type === 'success' ? 'bg-emerald-500 text-white' : toast.type === 'error' ? 'bg-rose-500 text-white' : 'bg-amber-500 text-white'}`}>
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
                    <h1 className="text-2xl font-black text-slate-900">Réseau Agences & Points</h1>
                    <p className="text-slate-500 text-sm">Gestion du réseau de franchises, corners et agences propres</p>
                </div>
                <div className="flex items-center gap-3">
                    <div className="flex bg-slate-100 rounded-xl p-1">
                        {[
                            { mode: 'TABLE' as ViewMode, icon: List, label: 'Table' },
                            { mode: 'CARDS' as ViewMode, icon: LayoutGrid, label: 'Cards' },
                            { mode: 'MAP' as ViewMode, icon: Map, label: 'Carte' },
                            { mode: 'ANALYTICS' as ViewMode, icon: BarChart3, label: 'Analytics' },
                        ].map(v => (
                            <button key={v.mode} onClick={() => setViewMode(v.mode)}
                                className={`flex items-center gap-1.5 px-3 py-2 rounded-lg text-sm font-bold transition-all ${viewMode === v.mode ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                            ><v.icon size={14} /> {v.label}</button>
                        ))}
                    </div>
                    <button onClick={() => AgencyStore.downloadCSV()} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors">
                        <Download size={16} /> CSV
                    </button>
                    <button onClick={() => handleOpenModal()} className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors shadow-lg shadow-indigo-200">
                        <Plus size={16} /> Nouvelle Agence
                    </button>
                </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-4 gap-4 mb-6">
                {[
                    { label: 'Total', value: agencies.length, icon: Building2, color: 'text-slate-600', bg: 'bg-slate-100' },
                    { label: 'Actives', value: activeCount, icon: CheckCircle, color: 'text-emerald-600', bg: 'bg-emerald-100' },
                    { label: 'Franchises', value: franchiseCount, icon: Building2, color: 'text-indigo-600', bg: 'bg-indigo-100' },
                    { label: 'Corners', value: cornerCount, icon: Store, color: 'text-purple-600', bg: 'bg-purple-100' },
                ].map(s => (
                    <div key={s.label} className="bg-white rounded-2xl p-5 border border-slate-200 shadow-sm flex items-center gap-3">
                        <div className={`w-10 h-10 ${s.bg} rounded-xl flex items-center justify-center`}><s.icon size={20} className={s.color} /></div>
                        <div><p className="text-xs text-slate-500 font-medium">{s.label}</p><p className={`text-2xl font-black ${s.color}`}>{s.value}</p></div>
                    </div>
                ))}
            </div>

            {/* Filters */}
            {(viewMode === 'TABLE' || viewMode === 'CARDS') && (
                <div className="flex items-center gap-4 mb-6 p-4 bg-white rounded-2xl border border-slate-200 shadow-sm">
                    <div className="flex items-center gap-2 flex-1">
                        <Search size={18} className="text-slate-400" />
                        <input type="text" placeholder="Rechercher..." value={searchQuery} onChange={(e) => setSearchQuery(e.target.value)} className="flex-1 outline-none text-sm font-medium" />
                    </div>
                    <select value={typeFilter} onChange={(e) => setTypeFilter(e.target.value)} className="outline-none text-sm font-medium bg-transparent cursor-pointer">
                        <option value="">Tous types</option>
                        <option value="FRANCHISE">Franchises</option>
                        <option value="CORNER">Corners</option>
                        <option value="OWNED">Propres</option>
                    </select>
                    <select value={statusFilter} onChange={(e) => setStatusFilter(e.target.value)} className="outline-none text-sm font-medium bg-transparent cursor-pointer">
                        <option value="">Tous statuts</option>
                        <option value="ACTIVE">Actives</option>
                        <option value="INACTIVE">Inactives</option>
                    </select>
                    <select value={regionFilter} onChange={(e) => setRegionFilter(e.target.value)} className="outline-none text-sm font-medium bg-transparent cursor-pointer">
                        <option value="">Toutes régions</option>
                        {REGIONS.map(r => <option key={r} value={r}>{r}</option>)}
                    </select>
                    <span className="text-xs text-slate-400 font-medium">{filteredAgencies.length} résultat{filteredAgencies.length > 1 ? 's' : ''}</span>
                </div>
            )}

            {/* ═══ TABLE VIEW ═══ */}
            {viewMode === 'TABLE' && (
                <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden flex-1">
                    <table className="w-full">
                        <thead className="bg-slate-50 border-b border-slate-200">
                            <tr>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Agence</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Type</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Région</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Contact</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Commission</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Codes Postaux</th>
                                <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Statut</th>
                                <th className="text-right p-4 text-xs font-bold text-slate-500 uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody>
                            {filteredAgencies.map(agency => {
                                const config = TYPE_CONFIG[agency.type as AgencyType] || TYPE_CONFIG.FRANCHISE;
                                const Icon = config.icon;
                                return (
                                    <tr key={agency.id} className="border-b border-slate-100 hover:bg-slate-50 transition-colors">
                                        <td className="p-4">
                                            <div className="flex items-center gap-3">
                                                <div className={`w-10 h-10 ${config.bgColor} rounded-xl flex items-center justify-center`}><Icon size={20} className={config.color} /></div>
                                                <div>
                                                    <p className="font-bold text-slate-800">{agency.name}</p>
                                                    <p className="text-xs text-slate-400">{agency.id}</p>
                                                </div>
                                            </div>
                                        </td>
                                        <td className="p-4"><span className={`px-3 py-1 rounded-lg text-xs font-bold ${config.bgColor} ${config.color}`}>{config.label}</span></td>
                                        <td className="p-4 text-sm text-slate-600">{agency.region || '-'}</td>
                                        <td className="p-4 text-sm text-slate-700">{agency.contactEmail || '-'}</td>
                                        <td className="p-4 text-sm font-bold text-slate-700">{agency.commissionRate}%</td>
                                        <td className="p-4">
                                            <div className="flex flex-wrap gap-1">
                                                {(Array.isArray(agency.zipCodes) ? agency.zipCodes : []).slice(0, 3).map((z: string) => (
                                                    <span key={z} className="text-[10px] bg-slate-100 text-slate-600 px-2 py-0.5 rounded-md font-mono">{z}</span>
                                                ))}
                                                {(Array.isArray(agency.zipCodes) ? agency.zipCodes : []).length > 3 && <span className="text-[10px] text-slate-400">+{(agency.zipCodes as string[]).length - 3}</span>}
                                            </div>
                                        </td>
                                        <td className="p-4"><span className={`px-3 py-1 rounded-lg text-xs font-bold ${agency.status === 'ACTIVE' ? 'bg-emerald-100 text-emerald-700' : 'bg-red-100 text-red-700'}`}>{agency.status === 'ACTIVE' ? 'Active' : 'Inactive'}</span></td>
                                        <td className="p-4">
                                            <div className="flex items-center justify-end gap-1">
                                                <button onClick={() => openDetail(agency.id)} className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-colors" title="Détail"><Eye size={16} /></button>
                                                <button onClick={() => handleOpenModal(agency)} className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-colors"><Edit2 size={16} /></button>
                                                <button onClick={() => handleDelete(agency.id, agency.name)} className="p-2 text-slate-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition-colors"><Trash2 size={16} /></button>
                                            </div>
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ CARDS VIEW ═══ */}
            {viewMode === 'CARDS' && (
                <div className="grid grid-cols-3 gap-4 flex-1 overflow-auto">
                    {filteredAgencies.map(agency => {
                        const config = TYPE_CONFIG[agency.type as AgencyType] || TYPE_CONFIG.FRANCHISE;
                        const Icon = config.icon;
                        return (
                            <div key={agency.id} className="bg-white rounded-2xl border border-slate-200 shadow-sm p-5 hover:shadow-md transition-all cursor-pointer" onClick={() => openDetail(agency.id)}>
                                <div className="flex items-start justify-between mb-3">
                                    <div className="flex items-center gap-3">
                                        <div className={`w-12 h-12 ${config.bgColor} rounded-xl flex items-center justify-center`}><Icon size={22} className={config.color} /></div>
                                        <div>
                                            <h3 className="font-bold text-slate-800">{agency.name}</h3>
                                            <span className={`text-[10px] font-bold px-2 py-0.5 rounded-md ${config.bgColor} ${config.color}`}>{config.label}</span>
                                        </div>
                                    </div>
                                    <span className={`w-2.5 h-2.5 rounded-full ${agency.status === 'ACTIVE' ? 'bg-emerald-400' : 'bg-red-400'}`} />
                                </div>
                                <div className="space-y-1.5 mb-3">
                                    <div className="flex items-center gap-2 text-xs text-slate-500"><MapPin size={12} /><span>{agency.region || '-'}</span></div>
                                    <div className="flex items-center gap-2 text-xs text-slate-500"><Users size={12} /><span>{agency.contactEmail || '-'}</span></div>
                                </div>
                                <div className="flex items-center justify-between pt-3 border-t border-slate-100">
                                    <span className="text-xs text-slate-400">Commission</span>
                                    <span className="text-sm font-black text-indigo-600">{agency.commissionRate}%</span>
                                </div>
                                <div className="flex flex-wrap gap-1 mt-2">
                                    {(Array.isArray(agency.zipCodes) ? agency.zipCodes : []).slice(0, 4).map((z: string) => (
                                        <span key={z} className="text-[9px] bg-slate-100 text-slate-500 px-1.5 py-0.5 rounded font-mono">{z}</span>
                                    ))}
                                </div>
                            </div>
                        );
                    })}
                </div>
            )}

            {/* ═══ MAP VIEW ═══ */}
            {viewMode === 'MAP' && (
                <div className="flex-1 bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <div className="p-4 border-b border-slate-200 flex items-center justify-between">
                        <h2 className="text-lg font-black text-slate-900">Carte du Réseau</h2>
                        <div className="flex items-center gap-4 text-xs">
                            <span className="flex items-center gap-1"><span className="w-3 h-3 rounded-full bg-indigo-500" /> Franchises</span>
                            <span className="flex items-center gap-1"><span className="w-3 h-3 rounded-full bg-purple-500" /> Corners</span>
                            <span className="flex items-center gap-1"><span className="w-3 h-3 rounded-full bg-emerald-500" /> Propres</span>
                        </div>
                    </div>
                    <div className="relative" style={{ height: '500px', background: 'linear-gradient(135deg, #e0e7ff 0%, #f0f4ff 50%, #e8f5e9 100%)' }}>
                        {mapData.map(point => {
                            const x = ((point.lng + 5) / 15) * 100;
                            const y = ((51 - point.lat) / 10) * 100;
                            const dotColor = point.type === 'FRANCHISE' ? 'bg-indigo-500' : point.type === 'CORNER' ? 'bg-purple-500' : 'bg-emerald-500';
                            return (
                                <div key={point.id} className="absolute transform -translate-x-1/2 -translate-y-1/2 group cursor-pointer"
                                    style={{ left: `${Math.max(5, Math.min(95, x))}%`, top: `${Math.max(5, Math.min(95, y))}%` }}
                                    onClick={() => openDetail(point.id)}>
                                    <div className={`w-5 h-5 rounded-full shadow-lg border-2 border-white transition-transform group-hover:scale-150 ${dotColor} ${point.status !== 'ACTIVE' ? 'opacity-40' : ''}`} />
                                    <div className="absolute bottom-full left-1/2 -translate-x-1/2 mb-2 bg-slate-900 text-white text-[10px] px-2 py-1 rounded-lg whitespace-nowrap opacity-0 group-hover:opacity-100 transition-opacity z-10 pointer-events-none">
                                        <div className="font-bold">{point.name}</div>
                                        <div>{point.city} • {point.region} • {point.leadsCount} leads</div>
                                    </div>
                                </div>
                            );
                        })}
                        {Object.entries({ IDF: [52, 50], AURA: [53, 47], PACA: [68, 42], OCC: [52, 33], NAQ: [30, 40], HDF: [53, 58], GES: [72, 55], BRE: [10, 50], NOR: [38, 58], PDL: [18, 43], BFC: [64, 48], CVL: [38, 45] }).map(([region, pos]) => (
                            <div key={region} className="absolute text-[9px] font-black text-slate-300 uppercase" style={{ left: `${(pos as number[])[0]}%`, top: `${(pos as number[])[1]}%` }}>{region}</div>
                        ))}
                    </div>
                </div>
            )}

            {/* ═══ ANALYTICS VIEW ═══ */}
            {viewMode === 'ANALYTICS' && networkAnalytics && (
                <div className="grid grid-cols-4 gap-6 flex-1 overflow-auto">
                    <div className="col-span-4 grid grid-cols-5 gap-4">
                        {[
                            { label: 'Total Agences', value: networkAnalytics.totalAgencies, color: 'text-slate-700' },
                            { label: 'Actives', value: networkAnalytics.activeAgencies, color: 'text-emerald-600' },
                            { label: 'Total Leads', value: networkAnalytics.totalLeads, color: 'text-indigo-600' },
                            { label: 'CA Total', value: `${(networkAnalytics.totalRevenue || 0).toLocaleString()} €`, color: 'text-emerald-600' },
                            { label: 'Conversion', value: `${networkAnalytics.conversionRate}%`, color: 'text-amber-600' },
                        ].map(s => (
                            <div key={s.label} className="bg-white rounded-2xl p-5 border border-slate-200 shadow-sm">
                                <p className="text-xs text-slate-500 font-medium mb-1">{s.label}</p>
                                <p className={`text-2xl font-black ${s.color}`}>{s.value}</p>
                            </div>
                        ))}
                    </div>

                    {/* Top Performers */}
                    <div className="col-span-2 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">🏆 Top Performers</h3>
                        <div className="space-y-3">
                            {(networkAnalytics.topPerformers || []).map((a: any, i: number) => (
                                <div key={a.id} className="flex items-center gap-3 p-3 bg-slate-50 rounded-xl">
                                    <span className="text-lg font-black text-slate-300 w-8">#{i + 1}</span>
                                    <div className="flex-1">
                                        <p className="font-bold text-sm text-slate-800">{a.name}</p>
                                        <p className="text-xs text-slate-400">{a.region} • {a.leads} leads</p>
                                    </div>
                                    <div className="text-right">
                                        <p className="font-black text-sm text-emerald-600">{(a.revenue / 100).toLocaleString()} €</p>
                                        <div className="flex items-center gap-1"><div className="w-8 bg-slate-200 rounded-full h-1.5"><div className={`h-1.5 rounded-full ${getHealthColor(a.healthScore)}`} style={{ width: `${a.healthScore}%` }} /></div><span className="text-[10px] text-slate-500">{a.healthScore}</span></div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* By Type */}
                    <div className="col-span-1 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">Par type</h3>
                        {Object.entries(networkAnalytics.typeCounts || {}).map(([type, count]: [string, any]) => {
                            const cfg = TYPE_CONFIG[type as AgencyType];
                            return (
                                <div key={type} className="flex items-center justify-between py-2">
                                    <span className={`text-xs font-bold ${cfg?.color || 'text-slate-500'}`}>{cfg?.label || type}</span>
                                    <span className="font-black text-lg text-slate-800">{count}</span>
                                </div>
                            );
                        })}
                    </div>

                    {/* By Region */}
                    <div className="col-span-1 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">Par région</h3>
                        <div className="space-y-1">
                            {Object.entries(networkAnalytics.regionCounts || {}).sort((a: any, b: any) => b[1] - a[1]).map(([region, count]: [string, any]) => (
                                <div key={region} className="flex items-center justify-between py-1.5">
                                    <span className="text-xs font-bold text-slate-500">{region}</span>
                                    <span className="font-black text-sm text-indigo-600">{count}</span>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Low Performers Alert */}
                    {(networkAnalytics.lowPerformers || []).length > 0 && (
                        <div className="col-span-4 bg-gradient-to-r from-amber-50 to-orange-50 rounded-2xl p-6 border border-amber-200">
                            <h3 className="font-black text-amber-900 mb-3">⚠️ Agences nécessitant attention</h3>
                            <div className="grid grid-cols-5 gap-3">
                                {(networkAnalytics.lowPerformers || []).map((a: any) => (
                                    <div key={a.id} className="bg-white/80 rounded-xl p-3 text-center">
                                        <p className="font-bold text-sm text-slate-800">{a.name}</p>
                                        <p className="text-xs text-slate-400">{a.region} • Score {a.healthScore}/100</p>
                                    </div>
                                ))}
                            </div>
                        </div>
                    )}
                </div>
            )}

            {/* ═══ DETAIL SLIDE PANEL ═══ */}
            {selectedAgency && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex justify-end" onClick={() => setSelectedAgency(null)}>
                    <div className="bg-white w-full max-w-xl shadow-2xl overflow-y-auto" onClick={e => e.stopPropagation()}>
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center sticky top-0 bg-white z-10">
                            <div>
                                <h2 className="text-xl font-black text-slate-900">{selectedAgency.name}</h2>
                                <p className="text-xs text-slate-400">{selectedAgency.id}</p>
                            </div>
                            <div className="flex items-center gap-2">
                                <button onClick={() => handleOpenModal(selectedAgency)} className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg"><Edit2 size={18} /></button>
                                <button onClick={() => setSelectedAgency(null)} className="text-slate-400 hover:text-slate-600"><X size={24} /></button>
                            </div>
                        </div>
                        <div className="p-6 space-y-6">
                            {/* Quick stats */}
                            <div className="grid grid-cols-3 gap-3">
                                {[
                                    { label: 'Leads', value: selectedAgency.stats?.totalLeads || 0, color: 'text-indigo-600' },
                                    { label: 'CA', value: `${(selectedAgency.stats?.totalRevenue || 0).toLocaleString()} €`, color: 'text-emerald-600' },
                                    { label: 'Conversion', value: `${selectedAgency.stats?.conversion || 0}%`, color: 'text-amber-600' },
                                ].map(s => (
                                    <div key={s.label} className="bg-slate-50 rounded-xl p-4 text-center">
                                        <p className="text-xs text-slate-400 font-medium">{s.label}</p>
                                        <p className={`text-xl font-black ${s.color}`}>{s.value}</p>
                                    </div>
                                ))}
                            </div>

                            {/* Health Score */}
                            <div className="bg-slate-50 rounded-xl p-4">
                                <div className="flex items-center justify-between mb-2">
                                    <span className="text-xs font-bold text-slate-500">Score de santé</span>
                                    <span className="text-lg font-black text-slate-800">{selectedAgency.stats?.healthScore || 0}/100</span>
                                </div>
                                <div className="w-full bg-slate-200 rounded-full h-3">
                                    <div className={`h-3 rounded-full transition-all ${getHealthColor(selectedAgency.stats?.healthScore || 0)}`}
                                        style={{ width: `${selectedAgency.stats?.healthScore || 0}%` }} />
                                </div>
                            </div>

                            {/* Monthly Trend */}
                            {selectedAgency.stats?.monthlyTrend && (
                                <div>
                                    <h3 className="font-black text-slate-900 mb-3">Tendance mensuelle</h3>
                                    <div className="flex items-end gap-2 h-32">
                                        {selectedAgency.stats.monthlyTrend.map((m: any) => (
                                            <div key={m.month} className="flex-1 flex flex-col items-center gap-1">
                                                <div className="w-full flex flex-col items-center" style={{ height: '80px' }}>
                                                    <div className="w-full bg-indigo-500 rounded-t-lg transition-all" style={{ height: `${Math.max(4, (m.leads / Math.max(...selectedAgency.stats.monthlyTrend.map((x: any) => x.leads), 1)) * 100)}%` }} />
                                                </div>
                                                <span className="text-[9px] font-bold text-slate-400">{m.month}</span>
                                                <span className="text-[10px] font-black text-slate-700">{m.leads}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            )}

                            {/* Info */}
                            <div className="space-y-3">
                                <h3 className="font-black text-slate-900">Informations</h3>
                                {[
                                    ['Type', TYPE_CONFIG[selectedAgency.type as AgencyType]?.label || selectedAgency.type],
                                    ['Région', selectedAgency.region],
                                    ['Email', selectedAgency.contactEmail],
                                    ['Commission', `${selectedAgency.commissionRate}%`],
                                    ['Codes postaux', (Array.isArray(selectedAgency.zipCodes) ? selectedAgency.zipCodes : []).join(', ') || '-'],
                                    ['Users', selectedAgency.stats?.usersCount || 0],
                                    ['RDV', selectedAgency.stats?.appointmentsCount || 0],
                                ].map(([k, v]) => (
                                    <div key={k as string} className="flex justify-between py-2 border-b border-slate-100">
                                        <span className="text-xs font-bold text-slate-400 uppercase">{k}</span>
                                        <span className="text-sm font-medium text-slate-700">{v as any}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ CREATE/EDIT MODAL ═══ */}
            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-xl w-full max-w-lg overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                            <h2 className="text-xl font-black text-slate-900">{editingAgency ? 'Modifier l\'agence' : 'Nouvelle Agence'}</h2>
                            <button onClick={() => setIsModalOpen(false)} className="text-slate-400 hover:text-slate-600"><X size={24} /></button>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Nom</label>
                                <input type="text" value={formData.name} onChange={(e) => setFormData({ ...formData, name: e.target.value })} className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none" />
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Type</label>
                                    <select value={formData.type} onChange={(e) => setFormData({ ...formData, type: e.target.value })} className="w-full p-3 border border-slate-200 rounded-xl outline-none">
                                        <option value="FRANCHISE">Franchise</option><option value="CORNER">Corner</option><option value="OWNED">Propre</option>
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Commission %</label>
                                    <input type="number" value={formData.commissionRate} onChange={(e) => setFormData({ ...formData, commissionRate: parseInt(e.target.value) })} className="w-full p-3 border border-slate-200 rounded-xl outline-none" />
                                </div>
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Région</label>
                                    <select value={formData.region} onChange={(e) => setFormData({ ...formData, region: e.target.value })} className="w-full p-3 border border-slate-200 rounded-xl outline-none">
                                        {REGIONS.map(r => <option key={r} value={r}>{r}</option>)}
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Ville</label>
                                    <input type="text" value={formData.city} onChange={(e) => setFormData({ ...formData, city: e.target.value })} className="w-full p-3 border border-slate-200 rounded-xl outline-none" placeholder="Paris" />
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Email Contact</label>
                                <input type="email" value={formData.contactEmail} onChange={(e) => setFormData({ ...formData, contactEmail: e.target.value })} className="w-full p-3 border border-slate-200 rounded-xl outline-none" />
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Codes Postaux (séparés par virgule)</label>
                                <input type="text" value={formData.zipCodes} onChange={(e) => setFormData({ ...formData, zipCodes: e.target.value })} placeholder="75001, 75002" className="w-full p-3 border border-slate-200 rounded-xl outline-none" />
                            </div>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button onClick={() => setIsModalOpen(false)} className="flex-1 py-3 bg-white border border-slate-200 text-slate-600 font-bold rounded-xl hover:bg-slate-100">Annuler</button>
                            <button onClick={handleSave} className="flex-1 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 shadow-lg">{editingAgency ? 'Enregistrer' : 'Créer'}</button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
