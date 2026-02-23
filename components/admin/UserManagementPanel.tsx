'use client';

import React, { useState, useEffect } from 'react';
import {
    Users,
    UserPlus,
    Shield,
    Mail,
    Building2,
    CheckCircle2,
    XCircle,
    MoreVertical,
    Edit2,
    Trash2,
    X,
    Lock,
    Save
} from 'lucide-react';
import { AgencyStore } from '../../services/AgencyStore';
import { Agency } from '../../types/backoffice';
import { StaffUser, UserStore } from '../../services/UserStore';
import { UserRole, ROLE_LABELS } from '../../config/Permissions';
import { AuthStore } from '../../services/authStore';

export default function UserManagementPanel() {
    const [users, setUsers] = useState<StaffUser[]>([]);
    const [agencies, setAgencies] = useState<Agency[]>([]);
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [editingUser, setEditingUser] = useState<StaffUser | null>(null);
    const [accessDenied, setAccessDenied] = useState(false);

    // État local pour le type de staff (UI only)
    const [staffType, setStaffType] = useState<'AGENCY' | 'HQ'>('AGENCY');

    const [formData, setFormData] = useState<Partial<StaffUser>>({
        name: '',
        email: '',
        role: 'CASE_WORKER',
        isActive: true, // Note: StaffUser n'a pas isActive dans l'interface, on assume que le backend le gère ou que l'interface est incomplète
        homeAgencyId: '',
        scopeAgencyIds: [] // Pour le siège
    });

    useEffect(() => {
        // Check role before loading
        const currentUser = AuthStore.getCurrentUser();
        if (!currentUser || !['SUPERADMIN', 'SUPER_ADMIN', 'HQ', 'HQ_ADMIN'].includes(currentUser.role)) {
            setAccessDenied(true);
            return;
        }
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const [allUsers, allAgencies] = await Promise.all([
                UserStore.getAllUsers(),
                AgencyStore.getAllAgencies()
            ]);
            setUsers(allUsers);
            setAgencies(allAgencies as any);
        } catch (error) {
            console.error('[UserManagement] Erreur chargement:', error);
        }
    };

    const handleOpenModal = (user: StaffUser | null = null) => {
        if (user) {
            setEditingUser(user);
            const isHQ = !user.homeAgencyId || user.homeAgencyId === 'HQ';
            setStaffType(isHQ ? 'HQ' : 'AGENCY');

            setFormData({
                name: user.name,
                email: user.email,
                role: user.role,
                isActive: (user as any).isActive ?? true,
                homeAgencyId: user.homeAgencyId || '',
                scopeAgencyIds: user.scopeAgencyIds || []
            });
        } else {
            setEditingUser(null);
            setStaffType('AGENCY');
            setFormData({
                name: '',
                email: '',
                role: 'CASE_WORKER',
                isActive: true,
                homeAgencyId: '',
                scopeAgencyIds: []
            });
        }
        setIsModalOpen(true);
    };

    const handleSave = async () => {
        try {
            if (editingUser) {
                await UserStore.updateUser(editingUser.id, formData as any);
            } else {
                await UserStore.createUser(formData as any);
            }
            await loadData();
            setIsModalOpen(false);
        } catch (error) {
            console.error('[UserManagement] Erreur sauvegarde:', error);
        }
    };

    const handleDelete = async (id: string) => {
        if (confirm('Voulez-vous vraiment désactiver cet utilisateur ?')) {
            try {
                await UserStore.deleteUser(id);
                await loadData();
            } catch (error) {
                console.error('[UserManagement] Erreur suppression:', error);
            }
        }
    };

    const getRoleBadgeColor = (role: string) => {
        switch (role) {
            case 'SUPER_ADMIN': return 'bg-red-100 text-red-700 border-red-200';
            case 'HQ_ADMIN': return 'bg-amber-100 text-amber-700 border-amber-200';
            case 'CASE_WORKER': return 'bg-blue-100 text-blue-700 border-blue-200';
            case 'AGENCY_MANAGER': return 'bg-emerald-100 text-emerald-700 border-emerald-200';
            case 'SALES': return 'bg-violet-100 text-violet-700 border-violet-200';
            case 'KIOSK_AGENT': return 'bg-purple-100 text-purple-700 border-purple-200';
            default: return 'bg-slate-100 text-slate-700 border-slate-200';
        }
    };

    const toggleScopeAgency = (agencyId: string) => {
        const currentScopes = formData.scopeAgencyIds || [];
        if (currentScopes.includes(agencyId)) {
            setFormData({ ...formData, scopeAgencyIds: currentScopes.filter(id => id !== agencyId) });
        } else {
            setFormData({ ...formData, scopeAgencyIds: [...currentScopes, agencyId] });
        }
    };

    const isGlobalScope = !formData.scopeAgencyIds || formData.scopeAgencyIds.length === 0;

    // Access denied UI
    if (accessDenied) {
        return (
            <div className="p-8 flex items-center justify-center min-h-[400px]">
                <div className="text-center space-y-4">
                    <div className="w-20 h-20 bg-red-50 rounded-3xl flex items-center justify-center mx-auto">
                        <Shield className="text-red-500" size={40} />
                    </div>
                    <h2 className="text-2xl font-black text-slate-900">Accès Refusé</h2>
                    <p className="text-slate-500 max-w-md">
                        Cette fonctionnalité est réservée aux <strong>Administrateurs du Siège</strong> (HQ_ADMIN ou SUPER_ADMIN).
                    </p>
                    <button
                        onClick={() => window.location.href = '/staff-login'}
                        className="px-6 py-3 bg-slate-900 text-white rounded-2xl font-black text-sm"
                    >
                        Se reconnecter
                    </button>
                </div>
            </div>
        );
    }

    return (
        <div className="p-8">
            <div className="flex items-center justify-between mb-8">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tighter uppercase">Gestion des Équipes</h1>
                    <p className="text-slate-500 font-medium">Contrôlez les accès du personnel et des partenaires</p>
                </div>
                <button
                    onClick={() => handleOpenModal()}
                    className="bg-slate-900 hover:bg-slate-800 text-white px-6 py-3 rounded-2xl font-black flex items-center gap-2 transition-all shadow-xl hover:scale-105"
                >
                    <UserPlus size={20} />
                    Ajouter un membre
                </button>
            </div>

            {/* Liste des utilisateurs */}
            <div className="bg-white rounded-3xl shadow-sm border border-slate-100 overflow-hidden">
                <table className="w-full">
                    <thead className="bg-slate-50 border-b border-slate-100">
                        <tr>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Utilisateur</th>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Rôle</th>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Rattachement</th>
                            <th className="text-left p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Statut</th>
                            <th className="text-right p-6 text-xs font-black text-slate-400 uppercase tracking-widest">Actions</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {users.map((user) => (
                            <tr key={user.id} className="hover:bg-slate-50/50 transition-colors">
                                <td className="p-6">
                                    <div className="flex items-center gap-4">
                                        <div className="w-10 h-10 bg-slate-900 text-white rounded-full flex items-center justify-center font-black">
                                            {user.name.charAt(0)}
                                        </div>
                                        <div>
                                            <p className="font-bold text-slate-900">{user.name}</p>
                                            <p className="text-xs text-slate-400 flex items-center gap-1">
                                                <Mail size={12} /> {user.email}
                                            </p>
                                        </div>
                                    </div>
                                </td>
                                <td className="p-6">
                                    <span className={`inline-flex items-center gap-1.5 px-3 py-1 rounded-full text-[10px] font-black uppercase border ${getRoleBadgeColor(user.role)}`}>
                                        <Shield size={12} />
                                        {ROLE_LABELS[user.role as UserRole] || user.role}
                                    </span>
                                </td>
                                <td className="p-6">
                                    {user.homeAgencyId && user.homeAgencyId !== 'HQ' ? (
                                        <div className="flex items-center gap-2 text-sm text-slate-600 font-medium">
                                            <Building2 size={16} className="text-slate-400" />
                                            {(user as any).homeAgencyName || agencies.find(a => a.id === user.homeAgencyId)?.name || `Agence ${user.homeAgencyId}`}
                                        </div>
                                    ) : (
                                        <div className="flex flex-col gap-1">
                                            <span className="text-xs font-black text-indigo-600 uppercase tracking-wider flex items-center gap-1">
                                                <Building2 size={12} /> Siège (HQ)
                                            </span>
                                            {(!user.scopeAgencyIds || user.scopeAgencyIds.length === 0) ? (
                                                <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded text-[10px] font-bold bg-slate-100 text-slate-500 border border-slate-200 w-fit">
                                                    🌍 Accès Global
                                                </span>
                                            ) : (
                                                <div className="group relative w-fit">
                                                    <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded text-[10px] font-bold bg-amber-50 text-amber-600 border border-amber-100 cursor-help">
                                                        🔍 {user.scopeAgencyIds.length} Agences
                                                    </span>
                                                    {/* Tooltip simple */}
                                                    <div className="absolute left-0 bottom-full mb-1 hidden group-hover:block bg-slate-900 text-white text-[10px] p-2 rounded w-48 z-10">
                                                        {user.scopeAgencyIds.map(id => agencies.find(a => a.id === id)?.name).join(', ')}
                                                    </div>
                                                </div>
                                            )}
                                        </div>
                                    )}
                                </td>
                                <td className="p-6">
                                    {user.isActive ? (
                                        <span className="inline-flex items-center gap-1 text-emerald-600 text-xs font-bold">
                                            <CheckCircle2 size={14} /> Actif
                                        </span>
                                    ) : (
                                        <span className="inline-flex items-center gap-1 text-red-400 text-xs font-bold">
                                            <XCircle size={14} /> Inactif
                                        </span>
                                    )}
                                </td>
                                <td className="p-6 text-right">
                                    <div className="flex items-center justify-end gap-2">
                                        <button
                                            onClick={() => handleOpenModal(user)}
                                            className="p-2 hover:bg-indigo-50 text-slate-400 hover:text-indigo-600 rounded-lg transition-colors"
                                        >
                                            <Edit2 size={18} />
                                        </button>
                                        <button
                                            onClick={() => handleDelete(user.id)}
                                            className="p-2 hover:bg-red-50 text-slate-400 hover:text-red-500 rounded-lg transition-colors"
                                        >
                                            <Trash2 size={18} />
                                        </button>
                                    </div>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>

            {/* Modal de Gestion */}
            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/40 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-[32px] shadow-2xl w-full max-w-lg overflow-hidden border border-white/20 animate-in fade-in zoom-in duration-200">
                        {/* Modal Header */}
                        <div className="bg-slate-900 p-8 text-white flex justify-between items-center">
                            <div className="flex items-center gap-3">
                                <div className="w-12 h-12 bg-indigo-500 rounded-2xl flex items-center justify-center shadow-lg shadow-indigo-500/30">
                                    <UserPlus size={24} />
                                </div>
                                <div>
                                    <h2 className="text-2xl font-black tracking-tighter uppercase">
                                        {editingUser ? 'Éditer Membre' : 'Nouveau Membre'}
                                    </h2>
                                    <p className="text-slate-400 text-xs font-bold uppercase tracking-widest">IAM Configuration</p>
                                </div>
                            </div>
                            <button onClick={() => setIsModalOpen(false)} className="text-slate-400 hover:text-white transition-colors p-2">
                                <X size={24} />
                            </button>
                        </div>

                        {/* Modal Body */}
                        <div className="p-8 space-y-6">
                            <div className="space-y-4">
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5 ml-1">Nom Complet</label>
                                    <input
                                        type="text"
                                        className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none transition-all"
                                        placeholder="ex: Jean de la Fontaine"
                                        value={formData.name}
                                        onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                    />
                                </div>
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5 ml-1">Email Professionnel</label>
                                    <input
                                        type="email"
                                        className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none transition-all"
                                        placeholder="contact@simulegal.fr"
                                        value={formData.email}
                                        onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                                    />
                                </div>
                                <div className="grid grid-cols-2 gap-4">
                                    <div>
                                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5 ml-1">Rôle Système</label>
                                        <select
                                            className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none transition-all appearance-none"
                                            value={formData.role}
                                            onChange={(e) => setFormData({ ...formData, role: e.target.value as StaffUser['role'] })}
                                        >
                                            {Object.entries(ROLE_LABELS).map(([val, label]) => (
                                                <option key={val} value={val}>{label}</option>
                                            ))}
                                        </select>
                                    </div>
                                </div>
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5 ml-1">Statut</label>
                                    <select
                                        className="w-full h-14 bg-slate-50 border-2 border-slate-100 rounded-2xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none transition-all appearance-none"
                                        value={(formData as any).isActive ? '1' : '0'}
                                        onChange={(e) => setFormData({ ...formData, isActive: e.target.value === '1' })}
                                    >
                                        <option value="1">Actif</option>
                                        <option value="0">Inactif</option>
                                    </select>
                                </div>
                            </div>

                            {/* Sélecteur de Type de Staff */}
                            <div className="bg-slate-100 p-1 rounded-xl flex">
                                <button
                                    onClick={() => {
                                        setStaffType('AGENCY');
                                        setFormData({ ...formData, homeAgencyId: agencies[0]?.id || '', role: 'CASE_WORKER' });
                                    }}
                                    className={`flex-1 py-2 text-xs font-black uppercase tracking-wider rounded-lg transition-all ${staffType === 'AGENCY' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                                >
                                    Staff Agence
                                </button>
                                <button
                                    onClick={() => {
                                        setStaffType('HQ');
                                        setFormData({ ...formData, homeAgencyId: 'HQ', role: 'HQ_ADMIN', scopeAgencyIds: [] });
                                    }}
                                    className={`flex-1 py-2 text-xs font-black uppercase tracking-wider rounded-lg transition-all ${staffType === 'HQ' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400 hover:text-slate-600'}`}
                                >
                                    Staff Siège
                                </button>
                            </div>

                            {/* Formulaire Conditionnel */}
                            <div className="animate-in slide-in-from-top-2 duration-300">
                                {staffType === 'AGENCY' ? (
                                    <div>
                                        <label className="block text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1.5 ml-1">Agence de Rattachement</label>
                                        <select
                                            className="w-full h-14 bg-indigo-50 border-2 border-indigo-100 rounded-2xl px-4 font-bold text-indigo-900 focus:border-indigo-500 outline-none transition-all appearance-none"
                                            value={formData.homeAgencyId}
                                            onChange={(e) => setFormData({ ...formData, homeAgencyId: e.target.value })}
                                        >
                                            <option value="">Sélectionner une agence...</option>
                                            {agencies.map(agency => (
                                                <option key={agency.id} value={agency.id}>{agency.name} ({agency.type})</option>
                                            ))}
                                        </select>
                                    </div>
                                ) : (
                                    <div className="space-y-4">
                                        {/* Rôle Siège Spécifique */}
                                        <div className="p-4 bg-slate-50 border border-slate-100 rounded-2xl">
                                            <div className="flex items-center justify-between mb-4">
                                                <span className="text-xs font-black text-slate-900 uppercase">Périmètre de Supervision</span>
                                                <label className="flex items-center gap-2 cursor-pointer">
                                                    <input
                                                        type="checkbox"
                                                        checked={isGlobalScope}
                                                        onChange={(e) => setFormData({ ...formData, scopeAgencyIds: e.target.checked ? [] : [agencies[0]?.id] })}
                                                        className="w-4 h-4 rounded border-slate-300 text-indigo-600 focus:ring-indigo-500"
                                                    />
                                                    <span className="text-xs font-bold text-slate-600">🌍 Accès Global</span>
                                                </label>
                                            </div>

                                            {!isGlobalScope && (
                                                <div className="grid grid-cols-2 gap-2 max-h-48 overflow-y-auto">
                                                    {agencies.map(agency => (
                                                        <label key={agency.id} className={`flex items-center gap-2 p-2 rounded-lg border cursor-pointer transition-all ${formData.scopeAgencyIds?.includes(agency.id) ? 'bg-indigo-50 border-indigo-200' : 'bg-white border-slate-100 hover:border-slate-300'}`}>
                                                            <input
                                                                type="checkbox"
                                                                checked={formData.scopeAgencyIds?.includes(agency.id)}
                                                                onChange={() => toggleScopeAgency(agency.id)}
                                                                className="rounded border-slate-300 text-indigo-600 focus:ring-indigo-500"
                                                            />
                                                            <span className="text-xs font-bold text-slate-700 truncate">{agency.name}</span>
                                                        </label>
                                                    ))}
                                                </div>
                                            )}
                                        </div>
                                    </div>
                                )}
                            </div>
                        </div>
                    </div>

                    {/* Modal Footer */}
                    <div className="p-8 bg-slate-50 border-t border-slate-100 flex gap-4">
                        <button
                            onClick={() => setIsModalOpen(false)}
                            className="flex-1 h-14 bg-white border-2 border-slate-200 text-slate-400 rounded-2xl font-black uppercase tracking-widest hover:bg-slate-50 transition-all"
                        >
                            Annuler
                        </button>
                        <button
                            onClick={handleSave}
                            className="flex-1 h-14 bg-slate-900 text-white rounded-2xl font-black uppercase tracking-widest hover:bg-slate-800 transition-all shadow-xl flex items-center justify-center gap-2"
                        >
                            <Save size={20} />
                            Sauvegarder
                        </button>
                    </div>
                </div>
            )}
        </div>
    );
}
