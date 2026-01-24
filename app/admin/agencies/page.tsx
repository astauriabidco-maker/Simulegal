'use client';

import React, { useEffect, useState } from 'react';
import { AgencyExt, AgencyStore } from '../../../services/AgencyStore';
import {
    Plus, Search, Building2, Store, MapPin, Download, Trash2, Edit2,
    ExternalLink, Users, TrendingUp, MoreHorizontal, Check, X
} from 'lucide-react';

type AgencyType = 'FRANCHISE' | 'CORNER' | 'OWNED' | 'HQ';

const TYPE_CONFIG: Record<AgencyType, { label: string; color: string; bgColor: string; icon: any }> = {
    FRANCHISE: { label: 'Franchise', color: 'text-indigo-700', bgColor: 'bg-indigo-100', icon: Building2 },
    CORNER: { label: 'Corner', color: 'text-purple-700', bgColor: 'bg-purple-100', icon: Store },
    OWNED: { label: 'Propre', color: 'text-emerald-700', bgColor: 'bg-emerald-100', icon: Building2 },
    HQ: { label: 'Siège', color: 'text-slate-700', bgColor: 'bg-slate-100', icon: Building2 }
};

export default function AgenciesPage() {
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [loading, setLoading] = useState(true);
    const [searchQuery, setSearchQuery] = useState('');
    const [typeFilter, setTypeFilter] = useState<string>('');
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [editingAgency, setEditingAgency] = useState<AgencyExt | null>(null);
    const [formData, setFormData] = useState({
        name: '', type: 'FRANCHISE', contactEmail: '', region: 'IDF',
        zipCodes: '', commissionRate: 15, kioskUrl: ''
    });

    useEffect(() => {
        loadAgencies();
    }, []);

    const loadAgencies = async () => {
        setLoading(true);
        const data = await AgencyStore.getAllAgencies();
        setAgencies(data);
        setLoading(false);
    };

    const filteredAgencies = agencies.filter(a => {
        if (searchQuery) {
            const q = searchQuery.toLowerCase();
            if (!a.name.toLowerCase().includes(q) && !a.contactEmail?.toLowerCase().includes(q)) return false;
        }
        if (typeFilter && a.type !== typeFilter) return false;
        return true;
    });

    const handleOpenModal = (agency: AgencyExt | null = null) => {
        if (agency) {
            setEditingAgency(agency);
            setFormData({
                name: agency.name,
                type: agency.type,
                contactEmail: agency.contactEmail || '',
                region: '',
                zipCodes: Array.isArray(agency.zipCodes) ? agency.zipCodes.join(', ') : (agency.zipCodes || ''),
                commissionRate: agency.commissionRate || 15,
                kioskUrl: agency.kioskUrl || ''
            });
        } else {
            setEditingAgency(null);
            setFormData({ name: '', type: 'FRANCHISE', contactEmail: '', region: 'IDF', zipCodes: '', commissionRate: 15, kioskUrl: '' });
        }
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        if (!formData.name) {
            alert('Le nom est requis');
            return;
        }

        if (editingAgency) {
            await AgencyStore.updateAgency(editingAgency.id, formData);
        } else {
            await AgencyStore.addAgency({
                ...formData,
                id: `${formData.name.substring(0, 3).toUpperCase()}-${Date.now().toString().slice(-4)}`,
                status: 'ACTIVE'
            });
        }

        setIsModalOpen(false);
        loadAgencies();
    };

    const handleDelete = async (id: string, name: string) => {
        if (!confirm(`Désactiver l'agence "${name}" ? Elle sera marquée comme inactive.`)) return;
        await AgencyStore.deleteAgency(id);
        loadAgencies();
    };

    const activeCount = agencies.filter(a => a.status === 'ACTIVE').length;
    const franchiseCount = agencies.filter(a => a.type === 'FRANCHISE').length;
    const cornerCount = agencies.filter(a => a.type === 'CORNER').length;

    return (
        <div className="p-8">
            {/* Header */}
            <div className="flex justify-between items-center mb-8">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Réseau Agences</h1>
                    <p className="text-slate-500 text-sm">Gestion du réseau de franchises et corners</p>
                </div>
                <div className="flex items-center gap-3">
                    <button
                        onClick={() => AgencyStore.downloadCSV()}
                        className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors"
                    >
                        <Download size={16} /> Export CSV
                    </button>
                    <button
                        onClick={() => handleOpenModal()}
                        className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors shadow-lg shadow-indigo-200"
                    >
                        <Plus size={16} /> Nouvelle Agence
                    </button>
                </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-4 gap-4 mb-8">
                <div className="bg-white rounded-2xl p-5 border border-slate-200">
                    <p className="text-xs text-slate-500 font-medium mb-1">Total Agences</p>
                    <p className="text-2xl font-black text-slate-900">{agencies.length}</p>
                </div>
                <div className="bg-white rounded-2xl p-5 border border-slate-200">
                    <p className="text-xs text-slate-500 font-medium mb-1">Actives</p>
                    <p className="text-2xl font-black text-emerald-600">{activeCount}</p>
                </div>
                <div className="bg-white rounded-2xl p-5 border border-slate-200">
                    <p className="text-xs text-slate-500 font-medium mb-1">Franchises</p>
                    <p className="text-2xl font-black text-indigo-600">{franchiseCount}</p>
                </div>
                <div className="bg-white rounded-2xl p-5 border border-slate-200">
                    <p className="text-xs text-slate-500 font-medium mb-1">Corners</p>
                    <p className="text-2xl font-black text-purple-600">{cornerCount}</p>
                </div>
            </div>

            {/* Filters */}
            <div className="flex items-center gap-4 mb-6 p-4 bg-white rounded-2xl border border-slate-200">
                <div className="flex items-center gap-2 flex-1">
                    <Search size={18} className="text-slate-400" />
                    <input
                        type="text"
                        placeholder="Rechercher par nom ou email..."
                        value={searchQuery}
                        onChange={(e) => setSearchQuery(e.target.value)}
                        className="flex-1 outline-none text-sm font-medium"
                    />
                </div>
                <div className="h-6 w-px bg-slate-200" />
                <select
                    value={typeFilter}
                    onChange={(e) => setTypeFilter(e.target.value)}
                    className="outline-none text-sm font-medium bg-transparent cursor-pointer"
                >
                    <option value="">Tous types</option>
                    <option value="FRANCHISE">Franchises</option>
                    <option value="CORNER">Corners</option>
                    <option value="OWNED">Propres</option>
                </select>
                <div className="text-xs text-slate-400 font-medium">
                    {filteredAgencies.length} résultat{filteredAgencies.length > 1 ? 's' : ''}
                </div>
            </div>

            {/* Table */}
            <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
                <table className="w-full">
                    <thead className="bg-slate-50 border-b border-slate-200">
                        <tr>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Agence</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Type</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Contact</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Commission</th>
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
                                            <div className={`w-10 h-10 ${config.bgColor} rounded-xl flex items-center justify-center`}>
                                                <Icon size={20} className={config.color} />
                                            </div>
                                            <div>
                                                <p className="font-bold text-slate-800">{agency.name}</p>
                                                <p className="text-xs text-slate-400">{agency.id}</p>
                                            </div>
                                        </div>
                                    </td>
                                    <td className="p-4">
                                        <span className={`px-3 py-1 rounded-lg text-xs font-bold ${config.bgColor} ${config.color}`}>
                                            {config.label}
                                        </span>
                                    </td>
                                    <td className="p-4">
                                        <p className="text-sm text-slate-700">{agency.contactEmail || '-'}</p>
                                    </td>
                                    <td className="p-4">
                                        <p className="text-sm font-bold text-slate-700">{agency.commissionRate}%</p>
                                    </td>
                                    <td className="p-4">
                                        <span className={`px-3 py-1 rounded-lg text-xs font-bold ${agency.status === 'ACTIVE' ? 'bg-emerald-100 text-emerald-700' : 'bg-red-100 text-red-700'}`}>
                                            {agency.status === 'ACTIVE' ? 'Active' : 'Inactive'}
                                        </span>
                                    </td>
                                    <td className="p-4">
                                        <div className="flex items-center justify-end gap-2">
                                            <button
                                                onClick={() => handleOpenModal(agency)}
                                                className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-colors"
                                            >
                                                <Edit2 size={16} />
                                            </button>
                                            {agency.kioskUrl && (
                                                <a
                                                    href={agency.kioskUrl}
                                                    target="_blank"
                                                    className="p-2 text-slate-400 hover:text-purple-600 hover:bg-purple-50 rounded-lg transition-colors"
                                                >
                                                    <ExternalLink size={16} />
                                                </a>
                                            )}
                                            <button
                                                onClick={() => handleDelete(agency.id, agency.name)}
                                                className="p-2 text-slate-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition-colors"
                                            >
                                                <Trash2 size={16} />
                                            </button>
                                        </div>
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>

            {/* Modal */}
            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-xl w-full max-w-lg overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                            <h2 className="text-xl font-black text-slate-900">
                                {editingAgency ? 'Modifier l\'agence' : 'Nouvelle Agence'}
                            </h2>
                            <button onClick={() => setIsModalOpen(false)} className="text-slate-400 hover:text-slate-600">
                                <X size={24} />
                            </button>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Nom</label>
                                <input
                                    type="text"
                                    value={formData.name}
                                    onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                />
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Type</label>
                                    <select
                                        value={formData.type}
                                        onChange={(e) => setFormData({ ...formData, type: e.target.value })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    >
                                        <option value="FRANCHISE">Franchise</option>
                                        <option value="CORNER">Corner</option>
                                        <option value="OWNED">Propre</option>
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Commission %</label>
                                    <input
                                        type="number"
                                        value={formData.commissionRate}
                                        onChange={(e) => setFormData({ ...formData, commissionRate: parseInt(e.target.value) })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Email Contact</label>
                                <input
                                    type="email"
                                    value={formData.contactEmail}
                                    onChange={(e) => setFormData({ ...formData, contactEmail: e.target.value })}
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                />
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Codes Postaux (JSON ou séparés par virgule)</label>
                                <input
                                    type="text"
                                    value={formData.zipCodes}
                                    onChange={(e) => setFormData({ ...formData, zipCodes: e.target.value })}
                                    placeholder='["75001", "75002"] ou 75001,75002'
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                />
                            </div>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button
                                onClick={() => setIsModalOpen(false)}
                                className="flex-1 py-3 bg-white border border-slate-200 text-slate-600 font-bold rounded-xl hover:bg-slate-100 transition-colors"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleSave}
                                className="flex-1 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 transition-colors shadow-lg"
                            >
                                {editingAgency ? 'Enregistrer' : 'Créer'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
