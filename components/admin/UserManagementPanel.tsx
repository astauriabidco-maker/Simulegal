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
    Edit2,
    Trash2,
    X,
    Save,
    Key,
    Power,
    Copy,
    Search,
    RefreshCw,
    AlertTriangle,
    Eye,
    EyeOff
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
    const [searchQuery, setSearchQuery] = useState('');
    const [loading, setLoading] = useState(true);

    // Toast notification
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'info' } | null>(null);

    // Temp password modal
    const [tempPasswordModal, setTempPasswordModal] = useState<{ name: string; email: string; password: string } | null>(null);
    const [showTempPassword, setShowTempPassword] = useState(false);

    // Confirm dialog
    const [confirmDialog, setConfirmDialog] = useState<{ title: string; message: string; onConfirm: () => void } | null>(null);

    // Staff type toggle
    const [staffType, setStaffType] = useState<'AGENCY' | 'HQ'>('AGENCY');

    const [formData, setFormData] = useState<Partial<StaffUser>>({
        name: '',
        email: '',
        role: 'CASE_WORKER',
        isActive: true,
        homeAgencyId: '',
        scopeAgencyIds: []
    });

    const showToast = (message: string, type: 'success' | 'error' | 'info' = 'success') => {
        setToast({ message, type });
        setTimeout(() => setToast(null), 4000);
    };

    useEffect(() => {
        const currentUser = AuthStore.getCurrentUser();
        if (!currentUser || !['SUPERADMIN', 'SUPER_ADMIN', 'HQ', 'HQ_ADMIN'].includes(currentUser.role)) {
            setAccessDenied(true);
            return;
        }
        loadData();
    }, []);

    const loadData = async () => {
        setLoading(true);
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
        setLoading(false);
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
                showToast(`${formData.name} mis à jour avec succès`);
            } else {
                const result = await UserStore.createUser(formData as any);
                if (result?.tempPassword) {
                    setTempPasswordModal({
                        name: result.name,
                        email: result.email,
                        password: result.tempPassword
                    });
                    setShowTempPassword(false);
                }
                showToast(`${formData.name} créé avec succès`);
            }
            await loadData();
            setIsModalOpen(false);
        } catch (error) {
            showToast('Erreur lors de la sauvegarde', 'error');
        }
    };

    const handleToggleActive = (user: StaffUser) => {
        const isActive = (user as any).isActive !== false;
        setConfirmDialog({
            title: isActive ? 'Désactiver le compte' : 'Réactiver le compte',
            message: isActive
                ? `${user.name} ne pourra plus se connecter. Voulez-vous continuer ?`
                : `${user.name} pourra à nouveau accéder au système. Voulez-vous continuer ?`,
            onConfirm: async () => {
                const result = await UserStore.toggleActive(user.id);
                if (result) {
                    showToast(`Compte ${result.isActive ? 'activé' : 'désactivé'} : ${user.name}`);
                    await loadData();
                } else {
                    showToast('Erreur lors du changement de statut', 'error');
                }
                setConfirmDialog(null);
            }
        });
    };

    const handleResetPassword = (user: StaffUser) => {
        setConfirmDialog({
            title: 'Réinitialiser le mot de passe',
            message: `Un nouveau mot de passe temporaire sera généré pour ${user.name}. L'ancien mot de passe sera invalidé.`,
            onConfirm: async () => {
                const result = await UserStore.resetPassword(user.id);
                if (result?.tempPassword) {
                    setTempPasswordModal({
                        name: user.name,
                        email: user.email,
                        password: result.tempPassword
                    });
                    setShowTempPassword(false);
                    showToast('Mot de passe réinitialisé', 'info');
                } else {
                    showToast('Erreur lors de la réinitialisation', 'error');
                }
                setConfirmDialog(null);
            }
        });
    };

    const handleDelete = (user: StaffUser) => {
        setConfirmDialog({
            title: 'Désactiver cet utilisateur',
            message: `Le compte de ${user.name} sera désactivé. Cette action ne supprime pas les données.`,
            onConfirm: async () => {
                await UserStore.deleteUser(user.id);
                showToast(`${user.name} désactivé`, 'info');
                await loadData();
                setConfirmDialog(null);
            }
        });
    };

    const copyToClipboard = (text: string) => {
        navigator.clipboard.writeText(text);
        showToast('Copié dans le presse-papiers', 'info');
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

    // Filtered users
    const filteredUsers = users.filter(u =>
        u.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        u.email.toLowerCase().includes(searchQuery.toLowerCase()) ||
        u.role.toLowerCase().includes(searchQuery.toLowerCase())
    );

    // Stats
    const activeCount = users.filter(u => (u as any).isActive !== false).length;
    const inactiveCount = users.length - activeCount;

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
                </div>
            </div>
        );
    }

    return (
        <div className="p-8 max-w-[1400px] mx-auto">
            {/* Header */}
            <div className="flex items-center justify-between mb-8">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tight">Gestion des Équipes</h1>
                    <p className="text-slate-500 text-sm mt-1">
                        {users.length} membres · <span className="text-emerald-600">{activeCount} actifs</span>
                        {inactiveCount > 0 && <span className="text-rose-500"> · {inactiveCount} inactifs</span>}
                    </p>
                </div>
                <div className="flex gap-3">
                    <button onClick={loadData} className="p-3 bg-slate-100 hover:bg-slate-200 text-slate-600 rounded-xl transition">
                        <RefreshCw size={18} />
                    </button>
                    <button
                        onClick={() => handleOpenModal()}
                        className="bg-slate-900 hover:bg-slate-800 text-white px-6 py-3 rounded-xl font-bold flex items-center gap-2 transition shadow-lg"
                    >
                        <UserPlus size={18} />
                        Nouveau membre
                    </button>
                </div>
            </div>

            {/* Search */}
            <div className="mb-6 relative">
                <Search size={18} className="absolute left-4 top-1/2 transform -translate-y-1/2 text-slate-400" />
                <input
                    type="text"
                    placeholder="Rechercher par nom, email ou rôle..."
                    className="w-full h-12 pl-12 pr-4 bg-white border border-slate-200 rounded-xl text-sm font-medium text-slate-900 outline-none focus:border-indigo-400 transition"
                    value={searchQuery}
                    onChange={(e) => setSearchQuery(e.target.value)}
                />
            </div>

            {/* Users Table */}
            {loading ? (
                <div className="flex items-center justify-center h-40">
                    <RefreshCw size={24} className="animate-spin text-indigo-500" />
                </div>
            ) : (
                <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                    <table className="w-full">
                        <thead className="bg-slate-50 border-b border-slate-100">
                            <tr>
                                <th className="text-left p-5 text-[10px] font-black text-slate-400 uppercase tracking-widest">Utilisateur</th>
                                <th className="text-left p-5 text-[10px] font-black text-slate-400 uppercase tracking-widest">Rôle</th>
                                <th className="text-left p-5 text-[10px] font-black text-slate-400 uppercase tracking-widest">Rattachement</th>
                                <th className="text-center p-5 text-[10px] font-black text-slate-400 uppercase tracking-widest">Statut</th>
                                <th className="text-right p-5 text-[10px] font-black text-slate-400 uppercase tracking-widest">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-slate-50">
                            {filteredUsers.map((user) => {
                                const isActive = (user as any).isActive !== false;
                                return (
                                    <tr key={user.id} className={`transition-colors ${isActive ? 'hover:bg-slate-50/50' : 'opacity-50 bg-slate-50/30'}`}>
                                        <td className="p-5">
                                            <div className="flex items-center gap-3">
                                                <div className={`w-9 h-9 rounded-full flex items-center justify-center font-black text-sm ${isActive ? 'bg-slate-900 text-white' : 'bg-slate-300 text-slate-500'}`}>
                                                    {user.name.charAt(0)}
                                                </div>
                                                <div>
                                                    <p className="font-bold text-slate-900 text-sm">{user.name}</p>
                                                    <p className="text-[11px] text-slate-400 flex items-center gap-1">
                                                        <Mail size={10} /> {user.email}
                                                    </p>
                                                </div>
                                            </div>
                                        </td>
                                        <td className="p-5">
                                            <span className={`inline-flex items-center gap-1 px-2.5 py-1 rounded-lg text-[10px] font-black uppercase border ${getRoleBadgeColor(user.role)}`}>
                                                <Shield size={10} />
                                                {ROLE_LABELS[user.role as UserRole] || user.role}
                                            </span>
                                        </td>
                                        <td className="p-5">
                                            {user.homeAgencyId && user.homeAgencyId !== 'HQ' ? (
                                                <div className="flex items-center gap-1.5 text-xs text-slate-600 font-medium">
                                                    <Building2 size={14} className="text-slate-400" />
                                                    {(user as any).homeAgencyName || agencies.find(a => a.id === user.homeAgencyId)?.name || 'Agence'}
                                                </div>
                                            ) : (
                                                <span className="text-[10px] font-black text-indigo-600 uppercase flex items-center gap-1">
                                                    <Building2 size={10} /> Siège
                                                    {user.scopeAgencyIds && user.scopeAgencyIds.length > 0 && (
                                                        <span className="text-slate-400 font-bold normal-case"> · {user.scopeAgencyIds.length} agences</span>
                                                    )}
                                                </span>
                                            )}
                                        </td>
                                        <td className="p-5 text-center">
                                            <button
                                                onClick={() => handleToggleActive(user)}
                                                className={`inline-flex items-center gap-1.5 px-3 py-1 rounded-lg text-[10px] font-bold transition cursor-pointer ${isActive
                                                    ? 'bg-emerald-50 text-emerald-600 hover:bg-emerald-100 border border-emerald-100'
                                                    : 'bg-rose-50 text-rose-500 hover:bg-rose-100 border border-rose-100'
                                                    }`}
                                            >
                                                {isActive ? <><CheckCircle2 size={12} /> Actif</> : <><XCircle size={12} /> Inactif</>}
                                            </button>
                                        </td>
                                        <td className="p-5 text-right">
                                            <div className="flex items-center justify-end gap-1">
                                                <button onClick={() => handleOpenModal(user)} title="Éditer"
                                                    className="p-2 hover:bg-indigo-50 text-slate-400 hover:text-indigo-600 rounded-lg transition">
                                                    <Edit2 size={16} />
                                                </button>
                                                <button onClick={() => handleResetPassword(user)} title="Réinitialiser MDP"
                                                    className="p-2 hover:bg-amber-50 text-slate-400 hover:text-amber-600 rounded-lg transition">
                                                    <Key size={16} />
                                                </button>
                                                <button onClick={() => handleDelete(user)} title="Désactiver"
                                                    className="p-2 hover:bg-red-50 text-slate-400 hover:text-red-500 rounded-lg transition">
                                                    <Trash2 size={16} />
                                                </button>
                                            </div>
                                        </td>
                                    </tr>
                                );
                            })}
                            {filteredUsers.length === 0 && (
                                <tr><td colSpan={5} className="p-12 text-center text-slate-400">Aucun utilisateur trouvé</td></tr>
                            )}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ CREATE/EDIT MODAL ═══ */}
            {isModalOpen && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-lg overflow-hidden">
                        <div className="bg-slate-900 p-6 text-white flex justify-between items-center">
                            <div className="flex items-center gap-3">
                                <div className="w-10 h-10 bg-indigo-500 rounded-xl flex items-center justify-center">
                                    <UserPlus size={20} />
                                </div>
                                <div>
                                    <h2 className="text-lg font-black">{editingUser ? 'Éditer' : 'Nouveau'} membre</h2>
                                    <p className="text-slate-400 text-[10px] font-bold uppercase tracking-wider">
                                        {editingUser ? 'Modifier les informations' : 'Un mot de passe temporaire sera généré'}
                                    </p>
                                </div>
                            </div>
                            <button onClick={() => setIsModalOpen(false)} className="p-2 hover:bg-white/10 rounded-lg"><X size={20} /></button>
                        </div>

                        <div className="p-6 space-y-5">
                            <div className="grid grid-cols-2 gap-4">
                                <div className="col-span-2">
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-wider mb-1.5">Nom complet</label>
                                    <input type="text" className="w-full h-12 bg-slate-50 border border-slate-200 rounded-xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                        placeholder="Jean Dupont" value={formData.name} onChange={(e) => setFormData({ ...formData, name: e.target.value })} />
                                </div>
                                <div className="col-span-2">
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-wider mb-1.5">Email</label>
                                    <input type="email" className="w-full h-12 bg-slate-50 border border-slate-200 rounded-xl px-4 font-bold text-slate-900 focus:border-indigo-500 outline-none"
                                        placeholder="jean@simulegal.fr" value={formData.email} onChange={(e) => setFormData({ ...formData, email: e.target.value })} />
                                </div>
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-wider mb-1.5">Rôle</label>
                                    <select className="w-full h-12 bg-slate-50 border border-slate-200 rounded-xl px-4 font-bold text-slate-900 outline-none appearance-none"
                                        value={formData.role} onChange={(e) => setFormData({ ...formData, role: e.target.value as StaffUser['role'] })}>
                                        {Object.entries(ROLE_LABELS).map(([val, label]) => (
                                            <option key={val} value={val}>{label}</option>
                                        ))}
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-wider mb-1.5">Statut</label>
                                    <select className="w-full h-12 bg-slate-50 border border-slate-200 rounded-xl px-4 font-bold text-slate-900 outline-none appearance-none"
                                        value={(formData as any).isActive ? '1' : '0'} onChange={(e) => setFormData({ ...formData, isActive: e.target.value === '1' })}>
                                        <option value="1">✅ Actif</option>
                                        <option value="0">🚫 Inactif</option>
                                    </select>
                                </div>
                            </div>

                            {/* Staff type toggle */}
                            <div className="bg-slate-100 p-1 rounded-lg flex">
                                <button onClick={() => { setStaffType('AGENCY'); setFormData({ ...formData, homeAgencyId: agencies[0]?.id || '' }); }}
                                    className={`flex-1 py-2 text-xs font-bold rounded-md transition ${staffType === 'AGENCY' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400'}`}>
                                    Staff Agence
                                </button>
                                <button onClick={() => { setStaffType('HQ'); setFormData({ ...formData, homeAgencyId: 'HQ', scopeAgencyIds: [] }); }}
                                    className={`flex-1 py-2 text-xs font-bold rounded-md transition ${staffType === 'HQ' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-400'}`}>
                                    Staff Siège
                                </button>
                            </div>

                            {staffType === 'AGENCY' ? (
                                <div>
                                    <label className="block text-[10px] font-black text-slate-400 uppercase tracking-wider mb-1.5">Agence</label>
                                    <select className="w-full h-12 bg-indigo-50 border border-indigo-200 rounded-xl px-4 font-bold text-indigo-900 outline-none appearance-none"
                                        value={formData.homeAgencyId} onChange={(e) => setFormData({ ...formData, homeAgencyId: e.target.value })}>
                                        <option value="">Sélectionner...</option>
                                        {agencies.map(a => <option key={a.id} value={a.id}>{a.name}</option>)}
                                    </select>
                                </div>
                            ) : (
                                <div className="p-4 bg-slate-50 border border-slate-100 rounded-xl">
                                    <div className="flex items-center justify-between mb-3">
                                        <span className="text-[10px] font-black text-slate-900 uppercase">Périmètre</span>
                                        <label className="flex items-center gap-2 cursor-pointer">
                                            <input type="checkbox" checked={isGlobalScope}
                                                onChange={(e) => setFormData({ ...formData, scopeAgencyIds: e.target.checked ? [] : [agencies[0]?.id] })}
                                                className="w-4 h-4 rounded border-slate-300 text-indigo-600" />
                                            <span className="text-xs font-bold text-slate-600">🌍 Global</span>
                                        </label>
                                    </div>
                                    {!isGlobalScope && (
                                        <div className="grid grid-cols-2 gap-2 max-h-36 overflow-y-auto">
                                            {agencies.map(a => (
                                                <label key={a.id} className={`flex items-center gap-2 p-2 rounded-lg border cursor-pointer text-xs font-bold transition ${formData.scopeAgencyIds?.includes(a.id) ? 'bg-indigo-50 border-indigo-200 text-indigo-700' : 'bg-white border-slate-100 text-slate-600'}`}>
                                                    <input type="checkbox" checked={formData.scopeAgencyIds?.includes(a.id)} onChange={() => toggleScopeAgency(a.id)} className="rounded border-slate-300 text-indigo-600" />
                                                    {a.name}
                                                </label>
                                            ))}
                                        </div>
                                    )}
                                </div>
                            )}
                        </div>

                        <div className="p-6 bg-slate-50 border-t border-slate-100 flex gap-3">
                            <button onClick={() => setIsModalOpen(false)} className="flex-1 h-12 bg-white border border-slate-200 text-slate-500 rounded-xl font-bold hover:bg-slate-50 transition">
                                Annuler
                            </button>
                            <button onClick={handleSave} className="flex-1 h-12 bg-slate-900 text-white rounded-xl font-bold hover:bg-slate-800 transition flex items-center justify-center gap-2">
                                <Save size={16} /> Sauvegarder
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ TEMP PASSWORD MODAL ═══ */}
            {tempPasswordModal && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-[60] flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-md overflow-hidden">
                        <div className="bg-amber-500 p-6 text-white">
                            <div className="flex items-center gap-3">
                                <div className="w-10 h-10 bg-white/20 rounded-xl flex items-center justify-center"><Key size={20} /></div>
                                <div>
                                    <h2 className="text-lg font-black">Mot de passe temporaire</h2>
                                    <p className="text-amber-100 text-xs font-bold">⚠️ Ce mot de passe ne sera affiché qu'une seule fois</p>
                                </div>
                            </div>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <p className="text-xs text-slate-400 font-bold mb-1">Utilisateur</p>
                                <p className="font-bold text-slate-900">{tempPasswordModal.name}</p>
                                <p className="text-xs text-slate-500">{tempPasswordModal.email}</p>
                            </div>
                            <div>
                                <p className="text-xs text-slate-400 font-bold mb-2">Mot de passe temporaire</p>
                                <div className="flex items-center gap-2">
                                    <div className="flex-1 h-12 bg-slate-900 text-white rounded-xl px-4 flex items-center font-mono font-bold text-lg tracking-wider">
                                        {showTempPassword ? tempPasswordModal.password : '••••••••••••'}
                                    </div>
                                    <button onClick={() => setShowTempPassword(!showTempPassword)}
                                        className="p-3 bg-slate-100 hover:bg-slate-200 rounded-xl transition">
                                        {showTempPassword ? <EyeOff size={18} /> : <Eye size={18} />}
                                    </button>
                                    <button onClick={() => copyToClipboard(tempPasswordModal.password)}
                                        className="p-3 bg-indigo-100 hover:bg-indigo-200 text-indigo-600 rounded-xl transition">
                                        <Copy size={18} />
                                    </button>
                                </div>
                            </div>
                            <div className="p-3 bg-amber-50 border border-amber-100 rounded-xl">
                                <p className="text-xs text-amber-700 font-bold flex items-center gap-1.5">
                                    <AlertTriangle size={14} />
                                    Communiquez ce mot de passe à l'utilisateur de manière sécurisée. Il devra le changer à sa première connexion.
                                </p>
                            </div>
                        </div>
                        <div className="p-6 bg-slate-50 border-t border-slate-100">
                            <button onClick={() => setTempPasswordModal(null)}
                                className="w-full h-12 bg-slate-900 text-white rounded-xl font-bold hover:bg-slate-800 transition">
                                J'ai noté le mot de passe
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ CONFIRM DIALOG ═══ */}
            {confirmDialog && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-[60] flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-sm p-6 space-y-4">
                        <h3 className="text-lg font-black text-slate-900">{confirmDialog.title}</h3>
                        <p className="text-sm text-slate-500">{confirmDialog.message}</p>
                        <div className="flex gap-3">
                            <button onClick={() => setConfirmDialog(null)} className="flex-1 h-11 bg-slate-100 text-slate-600 rounded-xl font-bold hover:bg-slate-200 transition">
                                Annuler
                            </button>
                            <button onClick={confirmDialog.onConfirm} className="flex-1 h-11 bg-slate-900 text-white rounded-xl font-bold hover:bg-slate-800 transition">
                                Confirmer
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ TOAST ═══ */}
            {toast && (
                <div className={`fixed bottom-6 right-6 z-[70] px-6 py-3 rounded-xl shadow-lg font-bold text-sm flex items-center gap-2 animate-in slide-in-from-bottom-4 duration-300
                    ${toast.type === 'success' ? 'bg-emerald-600 text-white' : toast.type === 'error' ? 'bg-red-600 text-white' : 'bg-slate-900 text-white'}`}>
                    {toast.type === 'success' && <CheckCircle2 size={16} />}
                    {toast.type === 'error' && <XCircle size={16} />}
                    {toast.message}
                </div>
            )}
        </div>
    );
}
