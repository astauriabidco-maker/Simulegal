'use client';

import React, { useState, useEffect } from 'react';
import {
    Shield,
    Plus,
    Trash2,
    Save,
    ChevronRight,
    CheckCircle2,
    AlertCircle,
    Info,
    Lock
} from 'lucide-react';
import { RoleStore, Role } from '../../../services/RoleStore';
import { PERMISSION_GROUPS, PermissionKey, PERMISSION_LABELS } from '../../../config/PermissionRegistry';

export default function RoleManager() {
    const [roles, setRoles] = useState<Role[]>([]);
    const [selectedRole, setSelectedRole] = useState<Role | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [isSaving, setIsSaving] = useState(false);
    const [showSuccess, setShowSuccess] = useState(false);

    useEffect(() => {
        const fetchRoles = async () => {
            const data = await RoleStore.getAllRoles();
            setRoles(data);
            setIsLoading(false);
        };
        fetchRoles();
    }, []);

    const handleSelectRole = (role: Role) => {
        setSelectedRole({ ...role });
    };

    const handleTogglePermission = (key: PermissionKey) => {
        if (!selectedRole || selectedRole.id === 'SUPER_ADMIN') return;

        const newPermissions = selectedRole.permissions.includes(key)
            ? selectedRole.permissions.filter(p => p !== key)
            : [...selectedRole.permissions, key];

        setSelectedRole({ ...selectedRole, permissions: newPermissions });
    };

    const handleCreateRole = () => {
        const newRole: Role = {
            id: '',
            label: 'Nouveau Rôle',
            description: 'Description du rôle...',
            permissions: [],
            isSystem: false
        };
        setSelectedRole(newRole);
    };

    const handleSave = async () => {
        if (!selectedRole) return;
        setIsSaving(true);
        try {
            await RoleStore.saveRole(selectedRole);
            const updatedRoles = await RoleStore.getAllRoles();
            setRoles(updatedRoles);
            setIsSaving(false);
            setShowSuccess(true);
            setTimeout(() => setShowSuccess(false), 3000);
        } catch (error) {
            console.error('Save error:', error);
            setIsSaving(false);
        }
    };

    return (
        <div className="flex bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden min-h-[700px]">
            {/* Sidebar: Liste des Rôles */}
            <div className="w-80 border-r border-slate-100 flex flex-col bg-slate-50/50">
                <div className="p-8 border-b border-slate-100 bg-white">
                    <div className="flex items-center justify-between mb-6">
                        <h3 className="text-xl font-black text-slate-900 tracking-tight uppercase px-1">Rôles</h3>
                        <button
                            onClick={handleCreateRole}
                            className="w-10 h-10 bg-indigo-600 text-white rounded-xl flex items-center justify-center hover:bg-slate-900 transition-all shadow-lg shadow-indigo-200"
                        >
                            <Plus size={20} />
                        </button>
                    </div>
                </div>

                <div className="flex-1 overflow-y-auto p-4 space-y-2">
                    {roles.map(role => (
                        <button
                            key={role.id}
                            onClick={() => handleSelectRole(role)}
                            className={`w-full p-4 rounded-2xl flex items-center justify-between transition-all text-left ${selectedRole?.id === role.id
                                ? 'bg-white shadow-xl shadow-slate-200/50 border-2 border-indigo-600'
                                : 'hover:bg-white border-2 border-transparent'
                                }`}
                        >
                            <div>
                                <div className="flex items-center gap-2 mb-1">
                                    <p className="font-black text-slate-900 text-sm uppercase tracking-tight">{role.label}</p>
                                    {role.isSystem && <Lock size={12} className="text-slate-300" />}
                                </div>
                                <p className="text-[10px] text-slate-400 font-bold uppercase tracking-widest leading-relaxed line-clamp-1">
                                    {role.permissions.length} Permissions
                                </p>
                            </div>
                            <ChevronRight size={16} className={selectedRole?.id === role.id ? 'text-indigo-600' : 'text-slate-200'} />
                        </button>
                    ))}
                </div>
            </div>

            {/* Main: Matrice de Permissions */}
            <div className="flex-1 flex flex-col bg-white">
                {!selectedRole ? (
                    <div className="flex-1 flex flex-col items-center justify-center p-20 text-center">
                        <div className="w-24 h-24 bg-slate-50 rounded-[32px] flex items-center justify-center mb-8 text-slate-200">
                            <Shield size={48} />
                        </div>
                        <h4 className="text-2xl font-black text-slate-900 uppercase tracking-tighter mb-4">Configuration RBAC</h4>
                        <p className="text-slate-400 max-w-sm font-medium leading-relaxed">
                            Sélectionnez un rôle dans la liste pour modifier ses accès ou créez-en un nouveau.
                        </p>
                    </div>
                ) : (
                    <>
                        <div className="p-8 border-b border-slate-50 flex items-center justify-between sticky top-0 bg-white z-10">
                            <div>
                                <input
                                    value={selectedRole.label}
                                    onChange={(e) => setSelectedRole({ ...selectedRole, label: e.target.value })}
                                    disabled={selectedRole.isSystem}
                                    className="text-2xl font-black text-slate-900 tracking-tighter uppercase bg-transparent border-none focus:ring-0 w-full mb-2 disabled:opacity-100"
                                />
                                <div className="flex items-center gap-4">
                                    <input
                                        value={selectedRole.description}
                                        onChange={(e) => setSelectedRole({ ...selectedRole, description: e.target.value })}
                                        disabled={selectedRole.isSystem}
                                        className="text-xs text-slate-400 font-bold uppercase tracking-widest bg-transparent border-none focus:ring-0 w-full disabled:opacity-100"
                                    />
                                </div>
                            </div>
                            <button
                                onClick={handleSave}
                                disabled={isSaving || selectedRole.id === 'SUPER_ADMIN'}
                                className="bg-indigo-600 hover:bg-slate-900 text-white px-8 py-3.5 rounded-2xl font-black text-xs uppercase tracking-widest transition-all shadow-xl shadow-indigo-100 flex items-center gap-2 disabled:bg-slate-100 disabled:text-slate-300 disabled:shadow-none"
                            >
                                {isSaving ? 'Enregistrement...' : (
                                    <>
                                        <Save size={18} />
                                        Appliquer les changements
                                    </>
                                )}
                            </button>
                        </div>

                        <div className="flex-1 overflow-y-auto p-12 space-y-12 pb-32">
                            {showSuccess && (
                                <div className="bg-emerald-50 border border-emerald-100 p-4 rounded-2xl flex items-center gap-3 animate-in fade-in slide-in-from-top-4">
                                    <CheckCircle2 className="text-emerald-500" size={20} />
                                    <p className="text-emerald-700 text-sm font-black uppercase">Changements enregistrés avec succès !</p>
                                </div>
                            )}

                            {selectedRole.id === 'SUPER_ADMIN' && (
                                <div className="bg-indigo-50 border border-indigo-100 p-6 rounded-3xl flex items-start gap-4">
                                    <Info className="text-indigo-600 mt-1" size={20} />
                                    <div>
                                        <p className="font-black text-indigo-900 uppercase text-xs mb-1">Rôle Système Inaltérable</p>
                                        <p className="text-indigo-800/70 text-sm leading-relaxed">
                                            Le rôle Super Admin possède intrinsèquement toutes les permissions.
                                            Il ne peut être ni modifié ni supprimé pour garantir l'accès permanent au système.
                                        </p>
                                    </div>
                                </div>
                            )}

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                {Object.entries(PERMISSION_GROUPS).map(([group, groupData]) => (
                                    <div key={group} className="space-y-6">
                                        <h5 className="text-xs font-black text-slate-400 uppercase tracking-[0.2em] px-1 border-l-4 border-indigo-600 pl-4">
                                            {groupData.label}
                                        </h5>
                                        <div className="space-y-3">
                                            {groupData.permissions.map(key => (
                                                <div
                                                    key={key}
                                                    onClick={() => handleTogglePermission(key)}
                                                    className={`p-5 rounded-2xl border-2 transition-all cursor-pointer flex items-center justify-between ${selectedRole.permissions.includes(key)
                                                        ? 'bg-indigo-50/50 border-indigo-200'
                                                        : 'bg-white border-slate-50 hover:border-slate-100'
                                                        } ${selectedRole.id === 'SUPER_ADMIN' ? 'pointer-events-none' : ''}`}
                                                >
                                                    <div className="flex items-center gap-4">
                                                        <div className={`w-3 h-3 rounded-full ${selectedRole.permissions.includes(key) ? 'bg-indigo-600 animate-pulse' : 'bg-slate-200'}`} />
                                                        <p className={`text-sm font-black uppercase tracking-tight ${selectedRole.permissions.includes(key) ? 'text-indigo-900' : 'text-slate-400'}`}>
                                                            {PERMISSION_LABELS[key as PermissionKey]}
                                                        </p>
                                                    </div>
                                                    <div className={`w-12 h-6 rounded-full p-1 transition-all ${selectedRole.permissions.includes(key) ? 'bg-indigo-600' : 'bg-slate-200'}`}>
                                                        <div className={`w-4 h-4 bg-white rounded-full transition-all ${selectedRole.permissions.includes(key) ? 'translate-x-6' : 'translate-x-0'}`} />
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </>
                )}
            </div>
        </div>
    );
}
