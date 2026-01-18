'use client';

import React, { useState, useEffect } from 'react';
import {
    Building2,
    Plus,
    MapPin,
    Percent,
    TrendingUp,
    Mail,
    Globe,
    QrCode,
    Copy,
    Download,
    Edit3,
    Search,
    X,
    Check,
    Settings,
    FileText,
    ExternalLink,
    Store
} from 'lucide-react';
import { AgencyExt, AgencyStore, AgencyStats } from '../../services/AgencyStore';

export default function FranchiseMasterPanel() {
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [stats, setStats] = useState<Record<string, AgencyStats>>({});
    const [searchTerm, setSearchTerm] = useState('');
    const [selectedAgency, setSelectedAgency] = useState<AgencyExt | null>(null);
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [activeTab, setActiveTab] = useState<'general' | 'territory' | 'marketing'>('general');
    const [copied, setCopied] = useState(false);

    // Form State
    const [formData, setFormData] = useState<Partial<AgencyExt>>({
        id: '',
        name: '',
        type: 'OWNED',
        commissionRate: 0,
        contactEmail: '',
        zipCodes: '',
        status: 'ACTIVE'
    });

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const allAgencies = await AgencyStore.getAllAgencies();
            setAgencies(allAgencies);

            // Charger les stats pour chaque agence
            const statsMap: Record<string, AgencyStats> = {};
            await Promise.all(allAgencies.map(async (a) => {
                const s = await AgencyStore.getAgencyStats(a.id);
                statsMap[a.id] = s;
            }));
            setStats(statsMap);
        } catch (error) {
            console.error('[FranchiseMaster] Erreur chargement:', error);
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
        } else {
            setSelectedAgency(null);
            setFormData({
                id: '',
                name: '',
                type: 'OWNED',
                commissionRate: 0,
                contactEmail: '',
                zipCodes: '', // Initialiser à une chaîne vide
                status: 'ACTIVE'
            });
        }
        setActiveTab('general');
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        try {
            if (selectedAgency) {
                await AgencyStore.updateAgency(selectedAgency.id, formData);
            } else {
                await AgencyStore.addAgency(formData as any);
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

    // Global Network Stats
    const networkStats = {
        totalCA: Object.values(stats).reduce((sum, s) => sum + s.totalCA, 0),
        totalCommission: Object.values(stats).reduce((sum, s) => sum + s.totalCommission, 0)
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

            {/* Quick Stats Network */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm">
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
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm">
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
                <div className="bg-white p-6 rounded-3xl border border-slate-100 shadow-sm">
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

            {/* Liste des Agences */}
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
                                                {!['OWNED', 'FRANCHISE', 'CORNER'].includes(agency.type) && <Building2 size={20} />}
                                            </div>
                                            <div>
                                                <p className="font-black text-slate-900 uppercase tracking-tight">{agency.name}</p>
                                                <p className="text-[10px] text-slate-400 font-bold uppercase tracking-widest flex items-center gap-1">
                                                    <MapPin size={10} /> {agency.zipCodes.length > 0 ? agency.zipCodes[0] : 'France'}
                                                </p>
                                            </div>
                                        </div>
                                    </td>
                                    <td className="p-6">
                                        {agency.type === 'OWNED' && (
                                            <span className="px-3 py-1 rounded-full text-[10px] font-black uppercase border bg-slate-100 text-slate-700 border-slate-200">
                                                Succursale
                                            </span>
                                        )}
                                        {agency.type === 'FRANCHISE' && (
                                            <span className="px-3 py-1 rounded-full text-[10px] font-black uppercase border bg-indigo-50 text-indigo-700 border-indigo-100">
                                                Franchise
                                            </span>
                                        )}
                                        {agency.type === 'CORNER' && (
                                            <span className="px-3 py-1 rounded-full text-[10px] font-black uppercase border bg-purple-50 text-purple-700 border-purple-100">
                                                Corner
                                            </span>
                                        )}
                                    </td>
                                    <td className="p-6">
                                        <div className="flex items-center gap-1 font-black text-slate-900">
                                            {agency.commissionRate}%
                                        </div>
                                    </td>
                                    <td className="p-6 text-right font-black text-slate-900">
                                        {agencyStat.totalCA.toLocaleString()} €
                                    </td>
                                    <td className="p-6 text-right">
                                        <span className="font-black text-emerald-600">
                                            {agencyStat.totalCommission.toLocaleString()} €
                                        </span>
                                    </td>
                                    <td className="p-6 text-center">
                                        <div className={`inline-flex items-center gap-1 text-[10px] font-black uppercase px-2 py-1 rounded-lg ${agency.status === 'ACTIVE' ? 'text-emerald-600 bg-emerald-50' : 'text-red-400 bg-red-50'
                                            }`}>
                                            {agency.status === 'ACTIVE' ? <Check size={12} /> : <X size={12} />}
                                            {agency.status === 'ACTIVE' ? 'Actif' : 'Off'}
                                        </div>
                                    </td>
                                    <td className="p-6 text-right">
                                        <button
                                            onClick={() => handleOpenModal(agency)}
                                            className="p-3 hover:bg-slate-900 hover:text-white text-slate-400 rounded-2xl transition-all shadow-sm hover:shadow-indigo-500/20"
                                        >
                                            <Settings size={20} />
                                        </button>
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>

            {/* Modale Configuration */}
            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-md z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-[40px] shadow-2xl w-full max-w-2xl overflow-hidden border border-white/20">
                        {/* Header */}
                        <div className="bg-slate-900 p-8 text-white">
                            <div className="flex justify-between items-center mb-6">
                                <div className="flex items-center gap-4">
                                    <div className="w-14 h-14 bg-indigo-500 rounded-2xl flex items-center justify-center shadow-lg shadow-indigo-500/40">
                                        <Building2 size={28} />
                                    </div>
                                    <div>
                                        <h2 className="text-2xl font-black uppercase tracking-tighter">
                                            {selectedAgency ? 'Configuration Franchise' : 'Ouverture Franchise'}
                                        </h2>
                                        <p className="text-slate-400 text-xs font-bold uppercase tracking-widest">Network Node Management</p>
                                    </div>
                                </div>
                                <button onClick={() => setIsModalOpen(false)} className="bg-white/10 hover:bg-white/20 p-3 rounded-2xl transition-all">
                                    <X size={24} />
                                </button>
                            </div>

                            {/* Tabs */}
                            <div className="flex gap-4">
                                {[
                                    { id: 'general', label: 'Infos Générales', icon: <FileText size={16} /> },
                                    { id: 'territory', label: 'Territoire', icon: <MapPin size={16} /> },
                                    { id: 'marketing', label: 'Kit Marketing', icon: <QrCode size={16} /> }
                                ].map(tab => (
                                    <button
                                        key={tab.id}
                                        onClick={() => setActiveTab(tab.id as any)}
                                        className={`flex items-center gap-2 px-6 py-2.5 rounded-xl text-sm font-black uppercase tracking-tight transition-all ${activeTab === tab.id ? 'bg-white text-slate-900 shadow-xl' : 'bg-white/10 text-white/60 hover:text-white'
                                            }`}
                                    >
                                        {tab.icon}
                                        {tab.label}
                                    </button>
                                ))}
                            </div>
                        </div>

                        {/* Body */}
                        <div className="p-10 max-h-[60vh] overflow-y-auto">
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
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 outline-none appearance-none"
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
                                                className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 outline-none appearance-none"
                                                value={formData.type}
                                                onChange={(e) => {
                                                    const newType = e.target.value as any;
                                                    let newRate = 0;
                                                    if (newType === 'FRANCHISE') newRate = 15;
                                                    if (newType === 'CORNER') newRate = 5;

                                                    setFormData({
                                                        ...formData,
                                                        type: newType,
                                                        commissionRate: newRate
                                                    });
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
                                    <div className="bg-indigo-50 p-6 rounded-3xl border border-indigo-100">
                                        <div className="flex gap-4 items-start">
                                            <MapPin className="text-indigo-600 mt-1" size={24} />
                                            <div>
                                                <p className="font-black text-indigo-900 uppercase text-sm tracking-tight">Routing Intelligent</p>
                                                <p className="text-indigo-600/70 text-xs font-bold leading-relaxed">
                                                    Les leads "Rappel" provenant de ces zones géographiques seront automatiquement
                                                    assignés à cette agence pour traitement immédiat.
                                                </p>
                                            </div>
                                        </div>
                                    </div>

                                    <div>
                                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 ml-1">Codes Postaux Couverts</label>
                                        <textarea
                                            className="w-full h-32 bg-slate-50 border-2 border-slate-100 rounded-2xl p-4 font-bold text-slate-900 focus:border-indigo-500 outline-none resize-none shadow-inner"
                                            placeholder="75001, 75002, 75010..."
                                            value={formData.zipCodes}
                                            onChange={(e) => setFormData({ ...formData, zipCodes: e.target.value })}
                                        />
                                        <p className="text-[10px] text-slate-400 font-bold mt-2 ml-2 italic">Séparez les codes postaux par une virgule.</p>
                                    </div>
                                </div>
                            )}

                            {activeTab === 'marketing' && (
                                <div className="space-y-8">
                                    <div className="bg-slate-900 p-8 rounded-3xl text-white">
                                        <div className="flex flex-col md:flex-row gap-8 items-center">
                                            <div className="w-40 h-40 bg-white p-4 rounded-3xl shadow-2xl flex items-center justify-center shrink-0">
                                                <QrCode size={120} className="text-slate-900" />
                                            </div>
                                            <div className="flex-1 space-y-4 text-center md:text-left">
                                                <div>
                                                    <p className="text-[10px] font-black text-indigo-400 uppercase tracking-widest mb-1">Borne Interactive / Lien Kiosque</p>
                                                    <div className="flex gap-2">
                                                        <input
                                                            readOnly
                                                            className="flex-1 bg-white/10 border border-white/10 rounded-xl px-4 py-2 font-mono text-xs text-indigo-200"
                                                            value={formData.kioskUrl || `https://app.simulegal.fr/?ref=${formData.id}`}
                                                        />
                                                        <button
                                                            onClick={() => copyToClipboard(formData.kioskUrl || `https://app.simulegal.fr/?ref=${formData.id}`)}
                                                            className="bg-indigo-500 hover:bg-indigo-400 p-2 rounded-xl transition-all relative"
                                                        >
                                                            {copied ? <Check size={18} /> : <Copy size={18} />}
                                                        </button>
                                                    </div>
                                                </div>
                                                <div className="flex flex-wrap gap-2">
                                                    <button className="flex-1 bg-white/10 hover:bg-white/20 border border-white/10 rounded-xl px-4 py-3 text-xs font-black uppercase tracking-tight flex items-center justify-center gap-2 transition-all">
                                                        <Download size={16} />
                                                        Affiche PDF (A4)
                                                    </button>
                                                    <button className="flex-1 bg-white/10 hover:bg-white/20 border border-white/10 rounded-xl px-4 py-3 text-xs font-black uppercase tracking-tight flex items-center justify-center gap-2 transition-all">
                                                        <Download size={16} />
                                                        QR Sticker (Petit)
                                                    </button>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <div className="p-6 bg-amber-50 rounded-3xl border border-amber-100 flex gap-4 items-start">
                                        <Settings className="text-amber-600 mt-1 shrink-0" size={24} />
                                        <div>
                                            <p className="font-black text-amber-900 uppercase text-sm">Tracking d'origine</p>
                                            <p className="text-amber-600 text-xs font-bold leading-relaxed">
                                                Toute personne utilisant ce lien/QR code sera automatiquement marquée comme
                                                provenant de cette agence. La commission est calculée instantanément lors du paiement.
                                            </p>
                                        </div>
                                    </div>
                                </div>
                            )}
                        </div>

                        {/* Footer */}
                        <div className="p-10 bg-slate-50 border-t border-slate-100 flex gap-4">
                            <button
                                onClick={() => setIsModalOpen(false)}
                                className="flex-1 h-14 bg-white border-2 border-slate-200 text-slate-400 rounded-2xl font-black uppercase tracking-widest hover:bg-slate-50 transition-all"
                            >
                                Fermer
                            </button>
                            <button
                                onClick={handleSave}
                                className="flex-1 h-14 bg-slate-900 text-white rounded-2xl font-black uppercase tracking-widest hover:bg-indigo-600 transition-all shadow-xl flex items-center justify-center gap-2"
                            >
                                <Check size={20} />
                                Valider la config
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
