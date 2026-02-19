'use client';

import React, { useState, useEffect } from 'react';
import {
    Users, Plus, Search, Edit2, Trash2, Key,
    Shield, Server, UserCog, Check, X, Loader2,
    Eye, EyeOff, AlertCircle
} from 'lucide-react';
import { AuthStore } from '../../../services/authStore';

interface SystemUser {
    id: string;
    name: string;
    email: string;
    role: 'SUPER_ADMIN' | 'HQ_ADMIN' | 'API_PARTNER';
    isSystemUser: boolean;
    lastLogin: string | null;
    createdAt: string;
}

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000';

const ROLE_CONFIG = {
    SUPER_ADMIN: { label: 'Super Admin', icon: Shield, color: 'text-red-600 bg-red-50 border-red-100' },
    HQ_ADMIN: { label: 'Admin Siège', icon: UserCog, color: 'text-indigo-600 bg-indigo-50 border-indigo-100' },
    API_PARTNER: { label: 'Partenaire API', icon: Server, color: 'text-amber-600 bg-amber-50 border-amber-100' },
};

export default function SystemUsersTab() {
    const [users, setUsers] = useState<SystemUser[]>([]);
    const [loading, setLoading] = useState(true);
    const [searchTerm, setSearchTerm] = useState('');
    const [roleFilter, setRoleFilter] = useState<string | null>(null);
    const [showModal, setShowModal] = useState(false);
    const [editingUser, setEditingUser] = useState<SystemUser | null>(null);
    const [formData, setFormData] = useState({ name: '', email: '', role: 'HQ_ADMIN', password: '' });
    const [showPassword, setShowPassword] = useState(false);
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        fetchUsers();
    }, []);

    const fetchUsers = async () => {
        setLoading(true);
        try {
            const response = await fetch(`${API_URL}/users/system`, {
                headers: { 'Authorization': `Bearer ${AuthStore.getToken()}` }
            });
            if (response.ok) {
                const data = await response.json();
                setUsers(data);
            }
        } catch (err) {
            console.error('Failed to fetch system users:', err);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async () => {
        setSaving(true);
        setError(null);
        try {
            const url = editingUser
                ? `${API_URL}/users/${editingUser.id}`
                : `${API_URL}/users`;
            const method = editingUser ? 'PATCH' : 'POST';

            const payload: any = {
                name: formData.name,
                email: formData.email,
                role: formData.role,
                isSystemUser: true,
                permissions: '[]'
            };

            if (!editingUser || formData.password) {
                payload.password = formData.password || 'tempPass123!';
            }

            const response = await fetch(url, {
                method,
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${AuthStore.getToken()}`
                },
                body: JSON.stringify(payload)
            });

            if (response.ok) {
                setShowModal(false);
                setEditingUser(null);
                setFormData({ name: '', email: '', role: 'HQ_ADMIN', password: '' });
                fetchUsers();
            } else {
                const err = await response.json();
                setError(err.message || 'Une erreur est survenue');
            }
        } catch (err) {
            setError('Erreur de connexion au serveur');
        } finally {
            setSaving(false);
        }
    };

    const handleDelete = async (id: string) => {
        if (!confirm('Supprimer cet utilisateur système ?')) return;
        try {
            await fetch(`${API_URL}/users/${id}`, {
                method: 'DELETE',
                headers: { 'Authorization': `Bearer ${AuthStore.getToken()}` }
            });
            fetchUsers();
        } catch (err) {
            console.error('Delete failed:', err);
        }
    };

    const openEditModal = (user: SystemUser) => {
        setEditingUser(user);
        setFormData({ name: user.name, email: user.email, role: user.role, password: '' });
        setShowModal(true);
    };

    const openCreateModal = () => {
        setEditingUser(null);
        setFormData({ name: '', email: '', role: 'HQ_ADMIN', password: '' });
        setShowModal(true);
    };

    const filteredUsers = users.filter(u => {
        const matchesSearch = u.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
            u.email.toLowerCase().includes(searchTerm.toLowerCase());
        const matchesRole = !roleFilter || u.role === roleFilter;
        return matchesSearch && matchesRole;
    });

    const formatDate = (date: string | null) => {
        if (!date) return 'Jamais connecté';
        return new Date(date).toLocaleDateString('fr-FR', {
            day: 'numeric', month: 'short', year: 'numeric', hour: '2-digit', minute: '2-digit'
        });
    };

    return (
        <div className="space-y-6 animate-in fade-in slide-in-from-bottom-6 duration-700">
            {/* Header */}
            <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
                <div className="relative flex-1 max-w-md">
                    <Search size={18} className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" />
                    <input
                        type="text"
                        placeholder="Rechercher un utilisateur..."
                        value={searchTerm}
                        onChange={e => setSearchTerm(e.target.value)}
                        className="w-full h-12 pl-12 pr-4 bg-slate-50 border-2 border-slate-100 rounded-2xl font-medium text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all"
                    />
                </div>
                <button
                    onClick={openCreateModal}
                    className="h-12 px-6 bg-slate-900 text-white rounded-2xl font-black text-sm flex items-center gap-2 hover:bg-slate-800 transition-all active:scale-95 shadow-lg"
                >
                    <Plus size={18} />
                    Nouvel Utilisateur
                </button>
            </div>

            {/* Filters */}
            <div className="flex gap-2 flex-wrap">
                <button
                    onClick={() => setRoleFilter(null)}
                    className={`px-4 py-2 rounded-xl text-xs font-black transition-all ${!roleFilter ? 'bg-slate-900 text-white' : 'bg-slate-100 text-slate-600 hover:bg-slate-200'}`}
                >
                    Tous ({users.length})
                </button>
                {Object.entries(ROLE_CONFIG).map(([role, config]) => {
                    const count = users.filter(u => u.role === role).length;
                    return (
                        <button
                            key={role}
                            onClick={() => setRoleFilter(roleFilter === role ? null : role)}
                            className={`px-4 py-2 rounded-xl text-xs font-black transition-all flex items-center gap-2 ${roleFilter === role ? 'bg-slate-900 text-white' : 'bg-slate-100 text-slate-600 hover:bg-slate-200'}`}
                        >
                            <config.icon size={14} />
                            {config.label} ({count})
                        </button>
                    );
                })}
            </div>

            {/* User List */}
            <div className="bg-white rounded-[2rem] border-2 border-slate-100 shadow-sm overflow-hidden">
                {loading ? (
                    <div className="p-12 flex items-center justify-center">
                        <Loader2 size={32} className="animate-spin text-slate-400" />
                    </div>
                ) : filteredUsers.length === 0 ? (
                    <div className="p-12 text-center">
                        <Users size={48} className="mx-auto text-slate-300 mb-4" />
                        <p className="text-slate-500 font-medium">Aucun utilisateur système trouvé</p>
                    </div>
                ) : (
                    <div className="divide-y divide-slate-100">
                        {filteredUsers.map(user => {
                            const roleConfig = ROLE_CONFIG[user.role] || ROLE_CONFIG.HQ_ADMIN;
                            const RoleIcon = roleConfig.icon;
                            return (
                                <div key={user.id} className="p-6 flex items-center justify-between hover:bg-slate-50/50 transition-colors">
                                    <div className="flex items-center gap-4">
                                        <div className="w-12 h-12 bg-gradient-to-br from-slate-100 to-slate-200 rounded-2xl flex items-center justify-center font-black text-slate-600 text-lg">
                                            {user.name.charAt(0).toUpperCase()}
                                        </div>
                                        <div>
                                            <h4 className="font-black text-slate-900">{user.name}</h4>
                                            <p className="text-sm text-slate-500">{user.email}</p>
                                        </div>
                                    </div>
                                    <div className="flex items-center gap-4">
                                        <div className={`px-3 py-1.5 rounded-xl border flex items-center gap-2 ${roleConfig.color}`}>
                                            <RoleIcon size={14} />
                                            <span className="text-[10px] font-black uppercase tracking-wider">{roleConfig.label}</span>
                                        </div>
                                        <div className="text-right hidden md:block">
                                            <p className="text-[10px] font-bold text-slate-400 uppercase tracking-widest">Dernière connexion</p>
                                            <p className="text-xs font-medium text-slate-600">{formatDate(user.lastLogin)}</p>
                                        </div>
                                        <div className="flex items-center gap-1">
                                            <button
                                                onClick={() => openEditModal(user)}
                                                className="p-2 rounded-xl text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 transition-all"
                                                title="Modifier"
                                            >
                                                <Edit2 size={16} />
                                            </button>
                                            <button
                                                onClick={() => handleDelete(user.id)}
                                                className="p-2 rounded-xl text-slate-400 hover:text-red-600 hover:bg-red-50 transition-all"
                                                title="Supprimer"
                                            >
                                                <Trash2 size={16} />
                                            </button>
                                        </div>
                                    </div>
                                </div>
                            );
                        })}
                    </div>
                )}
            </div>

            {/* Modal */}
            {showModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 backdrop-blur-sm animate-in fade-in duration-200">
                    <div className="bg-white rounded-[2rem] p-8 w-full max-w-md shadow-2xl animate-in zoom-in-95 duration-300">
                        <h3 className="text-xl font-black text-slate-900 mb-6">
                            {editingUser ? 'Modifier l\'utilisateur' : 'Nouvel Utilisateur Système'}
                        </h3>

                        {error && (
                            <div className="mb-4 p-3 bg-red-50 border border-red-100 rounded-xl text-red-600 text-sm font-medium flex items-center gap-2">
                                <AlertCircle size={16} />
                                {error}
                            </div>
                        )}

                        <div className="space-y-4">
                            <div>
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Nom complet</label>
                                <input
                                    type="text"
                                    value={formData.name}
                                    onChange={e => setFormData(prev => ({ ...prev, name: e.target.value }))}
                                    className="w-full h-12 px-4 bg-slate-50 border-2 border-slate-100 rounded-xl font-medium text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all mt-1"
                                    placeholder="Jean Dupont"
                                />
                            </div>
                            <div>
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Email</label>
                                <input
                                    type="email"
                                    value={formData.email}
                                    onChange={e => setFormData(prev => ({ ...prev, email: e.target.value }))}
                                    className="w-full h-12 px-4 bg-slate-50 border-2 border-slate-100 rounded-xl font-medium text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all mt-1"
                                    placeholder="admin@simulegal.fr"
                                />
                            </div>
                            <div>
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">Rôle</label>
                                <select
                                    value={formData.role}
                                    onChange={e => setFormData(prev => ({ ...prev, role: e.target.value }))}
                                    className="w-full h-12 px-4 bg-slate-50 border-2 border-slate-100 rounded-xl font-medium text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all mt-1"
                                >
                                    <option value="SUPER_ADMIN">Super Admin</option>
                                    <option value="HQ_ADMIN">Admin Siège</option>
                                    <option value="API_PARTNER">Partenaire API</option>
                                </select>
                            </div>
                            <div>
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 ml-1">
                                    {editingUser ? 'Nouveau mot de passe (optionnel)' : 'Mot de passe'}
                                </label>
                                <div className="relative">
                                    <input
                                        type={showPassword ? 'text' : 'password'}
                                        value={formData.password}
                                        onChange={e => setFormData(prev => ({ ...prev, password: e.target.value }))}
                                        className="w-full h-12 px-4 pr-12 bg-slate-50 border-2 border-slate-100 rounded-xl font-medium text-sm focus:border-indigo-600 focus:bg-white outline-none transition-all mt-1"
                                        placeholder="••••••••"
                                    />
                                    <button
                                        type="button"
                                        onClick={() => setShowPassword(!showPassword)}
                                        className="absolute right-4 top-1/2 -translate-y-1/2 text-slate-400 hover:text-indigo-600"
                                    >
                                        {showPassword ? <EyeOff size={18} /> : <Eye size={18} />}
                                    </button>
                                </div>
                            </div>
                        </div>

                        <div className="flex gap-3 mt-8">
                            <button
                                onClick={() => setShowModal(false)}
                                className="flex-1 h-12 bg-slate-100 text-slate-600 rounded-xl font-black text-sm hover:bg-slate-200 transition-all"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleSave}
                                disabled={saving || !formData.name || !formData.email}
                                className="flex-1 h-12 bg-slate-900 text-white rounded-xl font-black text-sm flex items-center justify-center gap-2 hover:bg-slate-800 transition-all disabled:bg-slate-300"
                            >
                                {saving ? <Loader2 size={18} className="animate-spin" /> : <Check size={18} />}
                                {editingUser ? 'Enregistrer' : 'Créer'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
