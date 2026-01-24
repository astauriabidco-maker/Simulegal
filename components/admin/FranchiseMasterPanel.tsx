'use client';

import React, { useState, useEffect } from 'react';
import { Plus, Search, Building2, Store, QrCode, MapPin, TrendingUp, Percent, Check, X, FileText, Download, Copy, AlertTriangle, Layers, Settings, BarChart3, Users } from 'lucide-react';
import { AgencyExt, AgencyStore } from '../../services/AgencyStore';
import FranceMap from './FranceMap';

const PerformanceTrendChart = ({ data, agencyId }: { data: any[], agencyId: string }) => {
    const [hoveredIndex, setHoveredIndex] = useState<number | null>(null);

    if (!data || data.length === 0) return (
        <div className="h-64 flex flex-col items-center justify-center bg-slate-50 rounded-[32px] border-2 border-dashed border-slate-200">
            <BarChart3 className="text-slate-300 mb-2" size={32} />
            <p className="text-slate-400 font-bold text-sm uppercase tracking-widest">Pas de données historiques</p>
        </div>
    );

    const maxGMV = Math.max(...data.map(d => d.gmv), 100);
    const maxCount = Math.max(...data.map(d => d.count), 5);
    const height = 120;
    const width = 500;

    const downloadSepaSample = async () => {
        // Just as an example, for the current agency, 
        // in a real scenario we'd use a specific payout from a list
        window.open(`http://localhost:3001/finance/payouts/LATEST/sepa?agencyId=${agencyId}`, '_blank');
    };

    return (
        <div className="space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-700">
            {/* GMV Chart */}
            <div className="bg-white p-8 rounded-[32px] border border-slate-100 shadow-sm relative overflow-hidden group">
                <div className="absolute top-0 right-0 p-8 opacity-5 group-hover:opacity-10 transition-opacity">
                    <TrendingUp size={120} />
                </div>
                <div className="flex items-center justify-between mb-8">
                    <div>
                        <h4 className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1">Volume d'affaires (GMV)</h4>
                        <p className="text-2xl font-black text-slate-900 tracking-tighter uppercase">Evolution Mensuelle</p>
                    </div>
                </div>

                <div className="relative h-[160px] w-full">
                    <svg viewBox={`0 0 ${width} ${height + 40}`} className="w-full h-full overflow-visible">
                        <defs>
                            <linearGradient id="gmvGradient" x1="0" y1="0" x2="0" y2="1">
                                <stop offset="0%" stopColor="#6366f1" />
                                <stop offset="100%" stopColor="#818cf8" />
                            </linearGradient>
                            <filter id="shadow" x="-20%" y="-20%" width="140%" height="140%">
                                <feGaussianBlur in="SourceAlpha" stdDeviation="3" />
                                <feOffset dx="0" dy="4" result="offsetblur" />
                                <feComponentTransfer>
                                    <feFuncA type="linear" slope="0.2" />
                                </feComponentTransfer>
                                <feMerge>
                                    <feMergeNode />
                                    <feMergeNode in="SourceGraphic" />
                                </feMerge>
                            </filter>
                        </defs>

                        {/* Grid */}
                        {[0, 0.5, 1].map(p => (
                            <line key={p} x1="0" y1={height * (1 - p)} x2={width} y2={height * (1 - p)} stroke="#f1f5f9" strokeWidth="1" />
                        ))}

                        {/* Bars */}
                        {data.map((d, i) => {
                            const barHeight = (d.gmv / maxGMV) * height;
                            const x = (i * (width / data.length)) + (width / data.length / 4);
                            const barWidth = width / data.length / 2;
                            const isActive = hoveredIndex === i;

                            return (
                                <g
                                    key={i}
                                    className="group/bar cursor-pointer"
                                    onMouseEnter={() => setHoveredIndex(i)}
                                    onMouseLeave={() => setHoveredIndex(null)}
                                >
                                    <rect
                                        x={x} y={height - barHeight}
                                        width={barWidth} height={barHeight}
                                        fill="url(#gmvGradient)" rx="8"
                                        filter={isActive ? "url(#shadow)" : ""}
                                        className={`transition-all duration-300 ${isActive ? 'brightness-110 -translate-y-1' : 'brightness-100 opacity-80'}`}
                                    />
                                    <text x={x + barWidth / 2} y={height + 25} textAnchor="middle" className={`text-[10px] font-black transition-colors duration-300 uppercase ${isActive ? 'fill-indigo-600' : 'fill-slate-400'}`}>{d.period}</text>

                                    {/* Tooltip-like value */}
                                    <g className={`transition-all duration-300 transform ${isActive ? 'opacity-100 -translate-y-2' : 'opacity-0 translate-y-2'}`}>
                                        <rect
                                            x={x + barWidth / 2 - 35} y={height - barHeight - 40}
                                            width="70" height="25" rx="12" fill="#1e293b"
                                        />
                                        <text x={x + barWidth / 2} y={height - barHeight - 24} textAnchor="middle" className="text-[9px] font-bold fill-white">
                                            {d.gmv.toLocaleString()}€
                                        </text>
                                    </g>
                                </g>
                            );
                        })}
                    </svg>
                </div>
            </div>

            {/* Conversion Sparklines / Lead Volume */}
            <div className="grid grid-cols-2 gap-6">
                <div className="bg-slate-900 p-8 rounded-[32px] text-white overflow-hidden relative group">
                    <div className="absolute top-[-20%] right-[-10%] opacity-10 group-hover:scale-110 transition-transform duration-700">
                        <Users size={160} />
                    </div>
                    <p className="text-[10px] font-black text-slate-500 uppercase tracking-widest mb-4">Volume Dossiers</p>
                    <div className="flex items-end justify-between relative z-10">
                        <div>
                            <p className="text-4xl font-black tracking-tighter mb-1 leading-none">{data.reduce((sum, d) => sum + d.count, 0)}</p>
                            <p className="text-[10px] font-bold text-slate-400 uppercase tracking-widest">Total 6 derniers mois</p>
                        </div>
                        <div className="flex gap-1.5 items-end h-16">
                            {data.map((d, i) => {
                                const dotHeight = (d.count / maxCount) * 100;
                                return (
                                    <div
                                        key={i}
                                        className="w-2.5 bg-indigo-500 rounded-full transition-all duration-500 hover:brightness-125"
                                        style={{ height: `${Math.max(dotHeight, 15)}%` }}
                                        title={`${d.period}: ${d.count}`}
                                    />
                                );
                            })}
                        </div>
                    </div>
                </div>

                <div className="bg-indigo-600 p-8 rounded-[32px] text-white group hover:bg-indigo-700 transition-colors duration-300 cursor-pointer overflow-hidden relative">
                    <div className="absolute top-[-20%] left-[-10%] opacity-10 group-hover:rotate-12 transition-transform duration-700">
                        <DollarSign size={160} />
                    </div>
                    <p className="text-[10px] font-black text-indigo-300 uppercase tracking-widest mb-4">Moyenne Panier</p>
                    <div className="flex items-center justify-between relative z-10">
                        <div>
                            <p className="text-4xl font-black tracking-tighter mb-1 leading-none">
                                {Math.round(data.reduce((sum, d) => sum + d.gmv, 0) / Math.max(data.reduce((sum, d) => sum + d.count, 0), 1))}€
                            </p>
                            <p className="text-[10px] font-bold text-indigo-200 uppercase tracking-widest">Par dossier payé</p>
                        </div>
                        <div className="w-12 h-12 bg-white/20 rounded-2xl flex items-center justify-center backdrop-blur-sm group-hover:scale-110 transition-transform">
                            <Percent size={24} />
                        </div>
                    </div>
                </div>
            </div>

            {/* Export Actions (Simulated) */}
            {data.length > 0 && (
                <div className="bg-white border-2 border-slate-100 rounded-[32px] p-2 flex items-center">
                    <div className="px-6 flex-1">
                        <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest">Opérations bancaires</p>
                        <p className="text-xs font-bold text-slate-700">Générer le virement de commission de ce mois</p>
                    </div>
                    <button
                        onClick={downloadSepaSample}
                        className="bg-indigo-600 hover:bg-slate-900 text-white px-6 py-4 rounded-3xl font-black text-xs uppercase tracking-widest flex items-center gap-2 transition-all shadow-lg active:scale-95"
                    >
                        <Download size={16} />
                        Export SEPA XML
                    </button>
                </div>
            )}
        </div>
    );
};

export default function FranchiseMasterPanel() {
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [loading, setLoading] = useState(true);
    const [searchTerm, setSearchTerm] = useState('');
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [selectedAgency, setSelectedAgency] = useState<AgencyExt | null>(null);
    const [activeTab, setActiveTab] = useState<'general' | 'territory' | 'marketing' | 'performance'>('general');
    const [viewMode, setViewMode] = useState<'list' | 'map'>('list');
    const [copied, setCopied] = useState(false);
    const [territoryValidation, setTerritoryValidation] = useState<Record<string, { available: boolean, agencyName?: string, agencyId?: string }>>({});
    const [isValidating, setIsValidating] = useState(false);
    const [stats, setStats] = useState<Record<string, any>>({});
    const [performanceTrends, setPerformanceTrends] = useState<any[]>([]);
    const [loadingPerformance, setLoadingPerformance] = useState(false);

    // Form State
    const [formData, setFormData] = useState<Partial<AgencyExt>>({
        id: '',
        name: '',
        type: 'OWNED',
        commissionRate: 0,
        contactEmail: '',
        zipCodes: [],
        status: 'ACTIVE',
        region: ''
    });

    const [zipCodesString, setZipCodesString] = useState('');

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            setLoading(true);
            const allAgencies = await AgencyStore.getAllAgencies();
            setAgencies(allAgencies);

            const statsMap: Record<string, any> = {};
            await Promise.all(allAgencies.map(async (a) => {
                const s = await AgencyStore.getAgencyStats(a.id);
                statsMap[a.id] = s;
            }));
            setStats(statsMap);
        } catch (error) {
            console.error('[FranchiseMaster] Erreur chargement:', error);
        } finally {
            setLoading(false);
        }
    };

    const loadPerformance = async (agencyId: string) => {
        try {
            setLoadingPerformance(true);
            const trends = await AgencyStore.getPerformanceTrends(agencyId);
            setPerformanceTrends(trends);
        } catch (error) {
            console.error('[FranchiseMaster] Erreur performance:', error);
        } finally {
            setLoadingPerformance(false);
        }
    };

    const filteredAgencies = agencies.filter(a =>
        a.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
        a.id.toLowerCase().includes(searchTerm.toLowerCase())
    );

    const handleOpenModal = (agency: AgencyExt | null = null) => {
        if (agency) {
            setSelectedAgency(agency);
            setFormData(agency);
            setZipCodesString(Array.isArray(agency.zipCodes) ? agency.zipCodes.join(', ') : '');
            loadPerformance(agency.id);
        } else {
            setSelectedAgency(null);
            setPerformanceTrends([]);
            setZipCodesString('');
            setFormData({
                id: '',
                name: '',
                type: 'OWNED',
                commissionRate: 0,
                contactEmail: '',
                zipCodes: [],
                status: 'ACTIVE',
                region: ''
            });
        }
        setActiveTab('general');
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        try {
            const finalZipCodes = zipCodesString.split(',').map(s => s.trim()).filter(Boolean);
            const dataToSave = { ...formData, zipCodes: finalZipCodes };

            if (selectedAgency) {
                await AgencyStore.updateAgency(selectedAgency.id, dataToSave);
            } else {
                await AgencyStore.addAgency(dataToSave as any);
            }
            await loadData();
            setIsModalOpen(false);
        } catch (error) {
            console.error('[FranchiseMaster] Erreur sauvegarde:', error);
        }
    };

    const copyToClipboard = (text: string) => {
        navigator.clipboard.writeText(text);
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
    };

    const handleCheckTerritory = async () => {
        const codes = zipCodesString.split(',').map(s => s.trim()).filter(Boolean);
        if (codes.length === 0) return;

        setIsValidating(true);
        const results: Record<string, any> = {};
        for (const code of codes) {
            const res = await AgencyStore.checkTerritoryAvailability(code);
            results[code] = res;
        }
        setTerritoryValidation(results);
        setIsValidating(false);
    };

    const getTerritoryHighlights = () => {
        const highlights: Record<string, string> = {};

        if (selectedAgency && selectedAgency.region) {
            highlights[selectedAgency.region] = '#6366f1';
        }

        Object.entries(territoryValidation).forEach(([code, result]) => {
            if (!result.available && result.agencyId) {
                const otherAgency = agencies.find(a => a.id === result.agencyId);
                if (otherAgency && otherAgency.region) {
                    highlights[otherAgency.region] = '#ef4444';
                }
            }
        });

        if (formData.region) {
            highlights[formData.region] = highlights[formData.region] === '#ef4444' ? '#f43f5e' : '#f59e0b';
        }

        return highlights;
    };

    const networkStats = {
        totalCA: Object.values(stats).reduce((sum, s) => sum + (s.totalCA || 0), 0),
        totalCommission: Object.values(stats).reduce((sum, s) => sum + (s.totalCommission || 0), 0)
    };

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="flex items-center justify-between mb-8">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tighter uppercase">Franchise Master</h1>
                    <p className="text-slate-500 font-medium">Pilotage réseau et monitoring des performances</p>
                </div>
                <button
                    onClick={() => handleOpenModal()}
                    className="bg-indigo-600 hover:bg-indigo-700 text-white px-6 py-3 rounded-2xl font-black flex items-center gap-2 transition-all shadow-xl hover:scale-105"
                >
                    <Plus size={20} />
                    Ouvrir une franchise
                </button>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm transition-all hover:shadow-md">
                    <p className="text-xs font-black text-slate-400 uppercase tracking-widest mb-4">Total Réseau</p>
                    <div className="flex items-end justify-between">
                        <div>
                            <p className="text-4xl font-black text-slate-900">{agencies.length}</p>
                            <p className="text-sm text-slate-500 font-bold">Points de vente actifs</p>
                        </div>
                        <div className="w-12 h-12 bg-indigo-50 rounded-2xl flex items-center justify-center text-indigo-600">
                            <Building2 size={24} />
                        </div>
                    </div>
                </div>
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm transition-all hover:shadow-md">
                    <p className="text-xs font-black text-slate-400 uppercase tracking-widest mb-4">CA Total Réseau</p>
                    <div className="flex items-end justify-between">
                        <div>
                            <p className="text-4xl font-black text-emerald-600">
                                {networkStats.totalCA.toLocaleString()} €
                            </p>
                            <p className="text-sm text-slate-500 font-bold">Généré via franchises</p>
                        </div>
                        <div className="w-12 h-12 bg-emerald-50 rounded-2xl flex items-center justify-center text-emerald-600">
                            <TrendingUp size={24} />
                        </div>
                    </div>
                </div>
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm transition-all hover:shadow-md">
                    <p className="text-xs font-black text-slate-400 uppercase tracking-widest mb-4">Commissions Dues</p>
                    <div className="flex items-end justify-between">
                        <div>
                            <p className="text-4xl font-black text-amber-600">
                                {networkStats.totalCommission.toLocaleString()} €
                            </p>
                            <p className="text-sm text-slate-500 font-bold">À reverser aux partenaires</p>
                        </div>
                        <div className="w-12 h-12 bg-amber-50 rounded-2xl flex items-center justify-center text-amber-600">
                            <Percent size={24} />
                        </div>
                    </div>
                </div>
            </div>

            <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                <div className="p-6 border-b border-slate-50 flex items-center gap-4 bg-slate-50/50">
                    <div className="relative flex-1">
                        <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                        <input
                            type="text"
                            placeholder="Rechercher une agence, une ville, un ID..."
                            className="w-full h-12 pl-12 pr-4 bg-white border-2 border-slate-100 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 outline-none transition-all shadow-inner"
                            value={searchTerm}
                            onChange={(e) => setSearchTerm(e.target.value)}
                        />
                    </div>
                </div>

                <table className="w-full">
                    <thead className="bg-white border-b border-slate-100">
                        <tr>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Entité</th>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Type</th>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Taux</th>
                            <th className="text-right p-6 text-xs font-black text-slate-400 uppercase tracking-widest">CA Apporté</th>
                            <th className="text-right p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Com. Due</th>
                            <th className="text-center p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Statut</th>
                            <th className="text-right p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Actions</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {filteredAgencies.map((agency) => {
                            const agencyStat = stats[agency.id] || { totalCA: 0, totalCommission: 0 };
                            return (
                                <tr key={agency.id} className="hover:bg-slate-50/50 transition-colors group">
                                    <td className="p-6">
                                        <div className="flex items-center gap-4">
                                            <div className="w-12 h-12 bg-slate-900 text-white rounded-2xl flex items-center justify-center shadow-lg group-hover:scale-110 transition-transform">
                                                {agency.type === 'OWNED' && <Building2 size={20} />}
                                                {agency.type === 'FRANCHISE' && <Store size={20} />}
                                                {agency.type === 'CORNER' && <QrCode size={20} />}
                                            </div>
                                            <div>
                                                <p className="font-black text-slate-900 uppercase tracking-tight">{agency.name}</p>
                                                <p className="text-[10px] text-slate-400 font-bold uppercase tracking-widest flex items-center gap-1">
                                                    <MapPin size={10} /> {agency.zipCodes ? String(agency.zipCodes).split(/[ ,]+/)[0] : 'France'}
                                                </p>
                                            </div>
                                        </div>
                                    </td>
                                    <td className="p-6">
                                        <span className={`px-3 py-1 rounded-full text-[10px] font-black uppercase border ${agency.type === 'OWNED' ? 'bg-slate-100 text-slate-700 border-slate-200' :
                                            agency.type === 'FRANCHISE' ? 'bg-indigo-50 text-indigo-700 border-indigo-100' :
                                                'bg-purple-50 text-purple-700 border-purple-100'
                                            }`}>
                                            {agency.type}
                                        </span>
                                    </td>
                                    <td className="p-6 font-black text-slate-900">{agency.commissionRate}%</td>
                                    <td className="p-6 text-right font-black text-slate-900">{agencyStat.totalCA.toLocaleString()} €</td>
                                    <td className="p-6 text-right font-black text-emerald-600">{agencyStat.totalCommission.toLocaleString()} €</td>
                                    <td className="p-6 text-center">
                                        <div className={`inline-flex items-center gap-1 text-[10px] font-black uppercase px-2 py-1 rounded-lg ${agency.status === 'ACTIVE' ? 'text-emerald-600 bg-emerald-50' : 'text-red-400 bg-red-50'}`}>
                                            {agency.status === 'ACTIVE' ? <Check size={12} /> : <X size={12} />}
                                            {agency.status === 'ACTIVE' ? 'Actif' : 'Off'}
                                        </div>
                                    </td>
                                    <td className="p-6 text-right">
                                        <button onClick={() => handleOpenModal(agency)} className="p-3 hover:bg-slate-900 hover:text-white text-slate-400 rounded-2xl transition-all">
                                            <Settings size={20} />
                                        </button>
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
                {filteredAgencies.length === 0 && (
                    <div className="p-20 text-center">
                        <Building2 className="mx-auto text-slate-100 mb-4" size={64} />
                        <p className="text-slate-400 font-black uppercase tracking-widest text-sm">Aucune agence trouvée</p>
                    </div>
                )}
            </div>

            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-md z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-[40px] shadow-2xl w-full max-w-2xl overflow-hidden border border-white/20">
                        <div className="bg-slate-900 p-8 text-white">
                            <div className="flex justify-between items-center mb-6">
                                <div className="flex items-center gap-4">
                                    <div className="w-14 h-14 bg-indigo-500 rounded-2xl flex items-center justify-center shadow-lg shadow-indigo-500/40">
                                        {activeTab === 'performance' ? <BarChart3 size={28} /> : <Building2 size={28} />}
                                    </div>
                                    <div>
                                        <h2 className="text-2xl font-black uppercase tracking-tighter">
                                            {selectedAgency ? selectedAgency.name : 'Ouverture Franchise'}
                                        </h2>
                                        <p className="text-slate-400 text-xs font-bold uppercase tracking-widest">
                                            {activeTab === 'performance' ? 'Données de Performance' : 'Network Node Management'}
                                        </p>
                                    </div>
                                </div>
                                <button onClick={() => setIsModalOpen(false)} className="bg-white/10 hover:bg-white/20 p-3 rounded-2xl transition-all">
                                    <X size={24} />
                                </button>
                            </div>

                            <div className="flex gap-4 overflow-x-auto pb-2 scrollbar-hide">
                                {[
                                    { id: 'general', label: 'Infos', icon: <FileText size={16} /> },
                                    { id: 'performance', label: 'Performance', icon: <BarChart3 size={16} /> },
                                    { id: 'territory', label: 'Territoire', icon: <MapPin size={16} /> },
                                    { id: 'marketing', label: 'Kit', icon: <QrCode size={16} /> }
                                ].map(tab => (
                                    <button
                                        key={tab.id}
                                        onClick={() => setActiveTab(tab.id as any)}
                                        className={`flex items-center gap-2 px-6 py-2.5 rounded-xl text-sm font-black uppercase tracking-tight transition-all shrink-0 ${activeTab === tab.id ? 'bg-white text-slate-900 shadow-xl' : 'bg-white/10 text-white/60 hover:text-white'}`}
                                    >
                                        {tab.icon}
                                        {tab.label}
                                    </button>
                                ))}
                            </div>
                        </div>

                        <div className="p-10 max-h-[60vh] overflow-y-auto">
                            {activeTab === 'performance' && (
                                <div className="space-y-6">
                                    {loadingPerformance ? (
                                        <div className="h-64 flex flex-col items-center justify-center">
                                            <div className="w-12 h-12 border-4 border-indigo-500 border-t-transparent rounded-full animate-spin mb-4" />
                                            <p className="text-slate-400 font-bold text-xs uppercase tracking-widest">Agrégation des données...</p>
                                        </div>
                                    ) : (
                                        <PerformanceTrendChart data={performanceTrends} agencyId={selectedAgency?.id || ''} />
                                    )}
                                </div>
                            )}

                            {activeTab === 'general' && (
                                <div className="space-y-6">
                                    <div className="grid grid-cols-2 gap-6">
                                        <div>
                                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">ID Agence (Unique)</label>
                                            <input
                                                type="text"
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                                placeholder="ex: PARIS_OUEST"
                                                value={formData.id}
                                                onChange={(e) => setFormData({ ...formData, id: e.target.value })}
                                                disabled={!!selectedAgency}
                                            />
                                        </div>
                                        <div>
                                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Statut Réseau</label>
                                            <select
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 outline-none appearance-none cursor-pointer"
                                                value={formData.status}
                                                onChange={(e) => setFormData({ ...formData, status: e.target.value as any })}
                                            >
                                                <option value="ACTIVE">Actif (Ligne ouverte)</option>
                                                <option value="INACTIVE">Inactif (Suspendu)</option>
                                            </select>
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Nom Commercial</label>
                                        <input
                                            type="text"
                                            className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                            placeholder="ex: Simulegal Bordeaux Centre"
                                            value={formData.name}
                                            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                        />
                                    </div>
                                    <div className="grid grid-cols-2 gap-6">
                                        <div>
                                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Type d'Installation</label>
                                            <select
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 outline-none appearance-none cursor-pointer"
                                                value={formData.type}
                                                onChange={(e) => {
                                                    const newType = e.target.value as any;
                                                    let newRate = 0;
                                                    if (newType === 'FRANCHISE') newRate = 15;
                                                    if (newType === 'CORNER') newRate = 5;
                                                    setFormData({ ...formData, type: newType, commissionRate: newRate });
                                                }}
                                            >
                                                <option value="OWNED">Succursale (Integrated)</option>
                                                <option value="FRANCHISE">Franchise Partenaire</option>
                                                <option value="CORNER">Corner / Point Relais</option>
                                            </select>
                                        </div>
                                        <div>
                                            <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Taux de Commission (%)</label>
                                            <input
                                                type="number"
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                                value={formData.commissionRate}
                                                onChange={(e) => setFormData({ ...formData, commissionRate: Number(e.target.value) })}
                                            />
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Email Contact Franchise</label>
                                        <input
                                            type="email"
                                            className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                            placeholder="manager@franchise.fr"
                                            value={formData.contactEmail}
                                            onChange={(e) => setFormData({ ...formData, contactEmail: e.target.value })}
                                        />
                                    </div>
                                </div>
                            )}

                            {activeTab === 'territory' && (
                                <div className="space-y-6">
                                    <div className="flex items-center justify-between">
                                        <div className="bg-indigo-50 p-6 rounded-3xl border border-indigo-100 flex-1">
                                            <div className="flex gap-4 items-start">
                                                <MapPin className="text-indigo-600 mt-1" size={24} />
                                                <div>
                                                    <p className="font-black text-indigo-900 uppercase text-sm tracking-tight">Routing Intelligent</p>
                                                    <p className="text-indigo-600/70 text-xs font-bold leading-relaxed">Les leads "Rappel" provenant de ces zones seront assignés à cette agence.</p>
                                                </div>
                                            </div>
                                        </div>
                                        <div className="flex gap-2 ml-4">
                                            <button onClick={() => setViewMode('list')} className={`p-3 rounded-2xl transition-all ${viewMode === 'list' ? 'bg-slate-900 text-white shadow-lg' : 'bg-slate-50 text-slate-400'}`}>
                                                <Layers size={20} />
                                            </button>
                                            <button onClick={() => setViewMode('map')} className={`p-3 rounded-2xl transition-all ${viewMode === 'map' ? 'bg-indigo-600 text-white shadow-lg' : 'bg-slate-50 text-slate-400'}`}>
                                                <Layers size={20} className="rotate-90" />
                                            </button>
                                        </div>
                                    </div>

                                    {viewMode === 'map' ? (
                                        <div className="py-4">
                                            <FranceMap agencies={agencies} customHighlights={getTerritoryHighlights()} className="border-none shadow-none bg-transparent" />
                                        </div>
                                    ) : (
                                        <div className="space-y-4">
                                            <div className="flex items-center justify-between">
                                                <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest ml-1">Codes Postaux Couverts</label>
                                                <button onClick={handleCheckTerritory} disabled={isValidating} className="text-[10px] font-black text-indigo-600 uppercase hover:underline">
                                                    {isValidating ? 'Verification...' : 'Vérifier Dispo'}
                                                </button>
                                            </div>
                                            <textarea
                                                className="w-full h-32 bg-slate-50 border-2 border-slate-100 rounded-2xl p-4 font-bold text-slate-900 focus:border-indigo-500 outline-none resize-none"
                                                placeholder="75001, 75002..."
                                                value={zipCodesString}
                                                onChange={(e) => setZipCodesString(e.target.value)}
                                            />
                                            <div className="flex flex-wrap gap-2">
                                                {Object.entries(territoryValidation).map(([code, result]) => (
                                                    <div key={code} className={`px-3 py-1 rounded-full text-[10px] font-bold border flex items-center gap-1 ${result.available ? 'bg-emerald-50 border-emerald-100 text-emerald-700' : 'bg-red-50 border-red-100 text-red-700'}`}>
                                                        {code} {result.available ? <Check size={10} /> : <X size={10} />}
                                                    </div>
                                                ))}
                                            </div>
                                        </div>
                                    )}
                                </div>
                            )}

                            {activeTab === 'marketing' && (
                                <div className="space-y-8">
                                    <div className="bg-slate-900 p-8 rounded-3xl text-white flex gap-8 items-center">
                                        <div className="w-40 h-40 bg-white p-4 rounded-3xl flex items-center justify-center shrink-0 shadow-2xl">
                                            <QrCode size={120} className="text-slate-900" />
                                        </div>
                                        <div className="flex-1 space-y-6">
                                            <div>
                                                <p className="text-[10px] font-black text-indigo-400 uppercase tracking-widest mb-2">Borne Interactive / Lien Tracking</p>
                                                <div className="flex gap-2">
                                                    <input readOnly className="flex-1 bg-white/10 border border-white/10 rounded-xl px-4 py-3 font-mono text-xs text-indigo-200 outline-none" value={`https://app.simulegal.fr/?ref=${formData.id}`} />
                                                    <button onClick={() => copyToClipboard(`https://app.simulegal.fr/?ref=${formData.id}`)} className="bg-indigo-500 hover:bg-indigo-400 p-3 rounded-xl transition-all shadow-lg hover:scale-105">
                                                        {copied ? <Check size={18} /> : <Copy size={18} />}
                                                    </button>
                                                </div>
                                            </div>
                                            <div className="grid grid-cols-2 gap-3">
                                                <button className="bg-white/10 hover:bg-white/20 border border-white/10 rounded-xl px-4 py-3 text-[10px] font-black uppercase tracking-widest flex items-center justify-center gap-2 transition-all">
                                                    <Download size={14} /> PDF A4
                                                </button>
                                                <button className="bg-white/10 hover:bg-white/20 border border-white/10 rounded-xl px-4 py-3 text-[10px] font-black uppercase tracking-widest flex items-center justify-center gap-2 transition-all">
                                                    <Download size={14} /> Sticker QR
                                                </button>
                                            </div>
                                        </div>
                                    </div>
                                    <div className="p-6 bg-slate-50 rounded-3xl border border-slate-100 flex gap-4 items-start">
                                        <AlertTriangle className="text-amber-500 mt-1 shrink-0" size={24} />
                                        <div>
                                            <p className="font-black text-slate-900 uppercase text-xs mb-1">Tracking Direct</p>
                                            <p className="text-slate-500 text-[10px] font-bold leading-relaxed">
                                                Tout lead créé via ce QR code ou lien sera automatiquement rattaché à cette agence.
                                                La commission sera calculée sur le montant HT du dossier lors du paiement.
                                            </p>
                                        </div>
                                    </div>
                                </div>
                            )}
                        </div>

                        <div className="p-10 bg-slate-50 border-t border-slate-100 flex gap-4">
                            <button onClick={() => setIsModalOpen(false)} className="flex-1 h-14 bg-white border-2 border-slate-200 text-slate-400 rounded-2xl font-black uppercase tracking-widest transition-all hover:bg-slate-100">Fermer</button>
                            <button onClick={handleSave} className="flex-1 h-14 bg-slate-900 text-white rounded-2xl font-black uppercase tracking-widest hover:bg-indigo-600 transition-all shadow-xl flex items-center justify-center gap-2 hover:scale-[1.02] active:scale-95">
                                <Check size={20} /> Valider la config
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
