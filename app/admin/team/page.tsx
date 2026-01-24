'use client';

import React, { useEffect, useState, useMemo } from 'react';
import { UserStore } from '../../../services/UserStore';
import { AgencyStore, AgencyExt } from '../../../services/AgencyStore';
import { StaffUser } from '../../../types/backoffice';
import {
    Plus, Search, Edit2, Trash2, Download, X, Users, Shield, Briefcase,
    Building2, Phone, Mail, UserCog, Scale, Headphones
} from 'lucide-react';

const ROLE_CONFIG: Record<string, { label: string; color: string; bgColor: string; icon: any }> = {
    SUPER_ADMIN: { label: 'Super Admin', color: 'text-red-700', bgColor: 'bg-red-100', icon: Shield },
    HQ_ADMIN: { label: 'Admin Siège', color: 'text-indigo-700', bgColor: 'bg-indigo-100', icon: UserCog },
    AGENCY_MANAGER: { label: 'Gérant Agence', color: 'text-emerald-700', bgColor: 'bg-emerald-100', icon: Building2 },
    SALES_REP: { label: 'Commercial', color: 'text-blue-700', bgColor: 'bg-blue-100', icon: Phone },
    LAWYER: { label: 'Juriste', color: 'text-purple-700', bgColor: 'bg-purple-100', icon: Scale },
    SUPPORT: { label: 'Support', color: 'text-amber-700', bgColor: 'bg-amber-100', icon: Headphones },
    EMPLOYEE: { label: 'Employé', color: 'text-slate-700', bgColor: 'bg-slate-100', icon: Briefcase }
};

export default function TeamPage() {
    const [users, setUsers] = useState<StaffUser[]>([]);
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [loading, setLoading] = useState(true);
    const [searchQuery, setSearchQuery] = useState('');
    const [roleFilter, setRoleFilter] = useState('');
    const [agencyFilter, setAgencyFilter] = useState('');
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [editingUser, setEditingUser] = useState<StaffUser | null>(null);
    const [formData, setFormData] = useState({
        name: '', email: '', role: 'EMPLOYEE', homeAgencyId: '', phone: '', password: ''
    });

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        setLoading(true);
        const [usersData, agenciesData] = await Promise.all([
            UserStore.getAllUsers(),
            AgencyStore.getAllAgencies()
        ]);
        setUsers(usersData);
        setAgencies(agenciesData);
        setLoading(false);
    };

    const filteredUsers = useMemo(() => {
        return users.filter(u => {
            if (searchQuery) {
                const q = searchQuery.toLowerCase();
                if (!u.name.toLowerCase().includes(q) && !u.email.toLowerCase().includes(q)) return false;
            }
            if (roleFilter && u.role !== roleFilter) return false;
            if (agencyFilter && u.homeAgencyId !== agencyFilter) return false;
            return true;
        });
    }, [users, searchQuery, roleFilter, agencyFilter]);

    const handleOpenModal = (user: StaffUser | null = null) => {
        if (user) {
            setEditingUser(user);
            setFormData({
                name: user.name,
                email: user.email,
                role: user.role,
                homeAgencyId: user.homeAgencyId || '',
                phone: user.phone || '',
                password: ''
            });
        } else {
            setEditingUser(null);
            setFormData({ name: '', email: '', role: 'EMPLOYEE', homeAgencyId: '', phone: '', password: '' });
        }
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        if (!formData.name || !formData.email) {
            alert('Nom et email requis');
            return;
        }

        if (editingUser) {
            const updateData: any = { ...formData };
            if (!updateData.password) delete updateData.password;
            await UserStore.updateUser(editingUser.id, updateData);
        } else {
            if (!formData.password) {
                alert('Mot de passe requis pour un nouvel utilisateur');
                return;
            }
            await UserStore.createUser(formData as any);
        }

        setIsModalOpen(false);
        loadData();
    };

    const handleDelete = async (id: string, name: string) => {
        if (!confirm(`Supprimer l'utilisateur "${name}" ?`)) return;
        await UserStore.deleteUser(id);
        loadData();
    };

    const handleExportCSV = () => {
        const headers = ['Nom', 'Email', 'Rôle', 'Téléphone', 'Agence'];
        const rows = filteredUsers.map(u => [
            u.name,
            u.email,
            ROLE_CONFIG[u.role]?.label || u.role,
            u.phone || '',
            agencies.find(a => a.id === u.homeAgencyId)?.name || ''
        ].map(v => `"${v}"`).join(';'));

        const csv = '\uFEFF' + [headers.join(';'), ...rows].join('\n');
        const blob = new Blob([csv], { type: 'text/csv;charset=utf-8' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'equipe.csv';
        a.click();
    };

    const roleStats = useMemo(() => {
        const stats: Record<string, number> = {};
        users.forEach(u => { stats[u.role] = (stats[u.role] || 0) + 1; });
        return stats;
    }, [users]);

    return (
        <div className="p-8">
            {/* Header */}
            <div className="flex justify-between items-center mb-8">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Gestion d'Équipe</h1>
                    <p className="text-slate-500 text-sm">Gérez les membres de votre équipe</p>
                </div>
                <div className="flex items-center gap-3">
                    <button
                        onClick={handleExportCSV}
                        className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors"
                    >
                        <Download size={16} /> Export CSV
                    </button>
                    <button
                        onClick={() => handleOpenModal()}
                        className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors shadow-lg shadow-indigo-200"
                    >
                        <Plus size={16} /> Ajouter
                    </button>
                </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-6 gap-4 mb-8">
                <div className="bg-white rounded-2xl p-5 border border-slate-200">
                    <div className="flex items-center gap-3">
                        <div className="w-10 h-10 bg-slate-100 rounded-xl flex items-center justify-center">
                            <Users size={20} className="text-slate-600" />
                        </div>
                        <div>
                            <p className="text-xs text-slate-500 font-medium">Total</p>
                            <p className="text-xl font-black text-slate-900">{users.length}</p>
                        </div>
                    </div>
                </div>
                {Object.entries(ROLE_CONFIG).slice(0, 5).map(([role, config]) => (
                    <div key={role} className="bg-white rounded-2xl p-5 border border-slate-200">
                        <div className="flex items-center gap-3">
                            <div className={`w-10 h-10 ${config.bgColor} rounded-xl flex items-center justify-center`}>
                                <config.icon size={20} className={config.color} />
                            </div>
                            <div>
                                <p className="text-xs text-slate-500 font-medium">{config.label}</p>
                                <p className="text-xl font-black text-slate-900">{roleStats[role] || 0}</p>
                            </div>
                        </div>
                    </div>
                ))}
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
                    value={roleFilter}
                    onChange={(e) => setRoleFilter(e.target.value)}
                    className="outline-none text-sm font-medium bg-transparent cursor-pointer"
                >
                    <option value="">Tous les rôles</option>
                    {Object.entries(ROLE_CONFIG).map(([role, config]) => (
                        <option key={role} value={role}>{config.label}</option>
                    ))}
                </select>
                <div className="h-6 w-px bg-slate-200" />
                <select
                    value={agencyFilter}
                    onChange={(e) => setAgencyFilter(e.target.value)}
                    className="outline-none text-sm font-medium bg-transparent cursor-pointer"
                >
                    <option value="">Toutes agences</option>
                    {agencies.map(a => <option key={a.id} value={a.id}>{a.name}</option>)}
                </select>
                <div className="text-xs text-slate-400 font-medium">
                    {filteredUsers.length} résultat{filteredUsers.length > 1 ? 's' : ''}
                </div>
            </div>

            {/* Table */}
            <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
                <table className="w-full">
                    <thead className="bg-slate-50 border-b border-slate-200">
                        <tr>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Membre</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Rôle</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Contact</th>
                            <th className="text-left p-4 text-xs font-bold text-slate-500 uppercase">Agence</th>
                            <th className="text-right p-4 text-xs font-bold text-slate-500 uppercase">Actions</th>
                        </tr>
                    </thead>
                    <tbody>
                        {filteredUsers.map(user => {
                            const config = ROLE_CONFIG[user.role] || ROLE_CONFIG.EMPLOYEE;
                            const Icon = config.icon;
                            const agency = agencies.find(a => a.id === user.homeAgencyId);
                            return (
                                <tr key={user.id} className="border-b border-slate-100 hover:bg-slate-50 transition-colors">
                                    <td className="p-4">
                                        <div className="flex items-center gap-3">
                                            <div className={`w-10 h-10 ${config.bgColor} rounded-xl flex items-center justify-center`}>
                                                <Icon size={20} className={config.color} />
                                            </div>
                                            <div>
                                                <p className="font-bold text-slate-800">{user.name}</p>
                                                <p className="text-xs text-slate-400">{user.email}</p>
                                            </div>
                                        </div>
                                    </td>
                                    <td className="p-4">
                                        <span className={`px-3 py-1 rounded-lg text-xs font-bold ${config.bgColor} ${config.color}`}>
                                            {config.label}
                                        </span>
                                    </td>
                                    <td className="p-4">
                                        <div className="flex items-center gap-4 text-sm text-slate-600">
                                            {user.phone && (
                                                <span className="flex items-center gap-1">
                                                    <Phone size={14} className="text-slate-400" />
                                                    {user.phone}
                                                </span>
                                            )}
                                        </div>
                                    </td>
                                    <td className="p-4">
                                        {agency ? (
                                            <span className="text-sm text-slate-700">{agency.name}</span>
                                        ) : (
                                            <span className="text-sm text-slate-400">Siège</span>
                                        )}
                                    </td>
                                    <td className="p-4">
                                        <div className="flex items-center justify-end gap-2">
                                            <button
                                                onClick={() => handleOpenModal(user)}
                                                className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg transition-colors"
                                            >
                                                <Edit2 size={16} />
                                            </button>
                                            <button
                                                onClick={() => handleDelete(user.id, user.name)}
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
                                {editingUser ? 'Modifier le membre' : 'Nouveau membre'}
                            </h2>
                            <button onClick={() => setIsModalOpen(false)} className="text-slate-400 hover:text-slate-600">
                                <X size={24} />
                            </button>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Nom complet</label>
                                <input
                                    type="text"
                                    value={formData.name}
                                    onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                />
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Email</label>
                                    <input
                                        type="email"
                                        value={formData.email}
                                        onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Téléphone</label>
                                    <input
                                        type="tel"
                                        value={formData.phone}
                                        onChange={(e) => setFormData({ ...formData, phone: e.target.value })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Rôle</label>
                                    <select
                                        value={formData.role}
                                        onChange={(e) => setFormData({ ...formData, role: e.target.value })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    >
                                        {Object.entries(ROLE_CONFIG).map(([role, config]) => (
                                            <option key={role} value={role}>{config.label}</option>
                                        ))}
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Agence</label>
                                    <select
                                        value={formData.homeAgencyId}
                                        onChange={(e) => setFormData({ ...formData, homeAgencyId: e.target.value })}
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    >
                                        <option value="">Siège (aucune agence)</option>
                                        {agencies.map(a => <option key={a.id} value={a.id}>{a.name}</option>)}
                                    </select>
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">
                                    {editingUser ? 'Nouveau mot de passe (laisser vide pour conserver)' : 'Mot de passe'}
                                </label>
                                <input
                                    type="password"
                                    value={formData.password}
                                    onChange={(e) => setFormData({ ...formData, password: e.target.value })}
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
                                {editingUser ? 'Enregistrer' : 'Créer'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
