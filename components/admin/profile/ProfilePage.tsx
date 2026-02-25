'use client';

import React, { useState, useEffect } from 'react';
import {
    User, Shield, Mail, Key, Save, Camera,
    Building2, Calendar, Clock, CheckCircle, AlertCircle,
    Eye, EyeOff, Briefcase, MapPin, Award, Activity,
    LogOut, Settings, ChevronRight, Zap, Lock
} from 'lucide-react';
import { AuthStore } from '../../../services/authStore';
import { ROLE_LABELS, ROLE_DESCRIPTIONS, ROLE_COLORS, ROLE_PERMISSIONS, normalizeRole } from '../../../config/Permissions';
import { PERMISSION_LABELS, PERMISSION_GROUPS } from '../../../config/PermissionRegistry';
import type { PermissionKey } from '../../../config/PermissionRegistry';
import type { UserRole } from '../../../config/Permissions';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

interface UserProfile {
    id: string;
    name: string;
    email: string;
    role: string;
    agencyId?: string;
    agencyName?: string;
    permissions: string;
    expertises: string[];
    createdAt: string;
    lastLogin?: string;
    agency?: { name: string; city?: string };
    roleRef?: { label: string; description: string; permissions: string };
}

export default function ProfilePage() {
    const [profile, setProfile] = useState<UserProfile | null>(null);
    const [loading, setLoading] = useState(true);
    const [activeTab, setActiveTab] = useState<'info' | 'security' | 'permissions' | 'activity'>('info');
    const [editing, setEditing] = useState(false);
    const [saving, setSaving] = useState(false);
    const [saveSuccess, setSaveSuccess] = useState(false);
    const [editForm, setEditForm] = useState({ name: '', email: '', expertises: [] as string[] });
    const [passwordForm, setPasswordForm] = useState({ current: '', new: '', confirm: '' });
    const [showPassword, setShowPassword] = useState(false);
    const [passwordError, setPasswordError] = useState('');

    useEffect(() => {
        loadProfile();
    }, []);

    const loadProfile = async () => {
        try {
            const token = AuthStore.getToken();
            const res = await fetch(`${API_URL}/users/me`, {
                headers: { 'Authorization': `Bearer ${token}` }
            });
            if (res.ok) {
                const data = await res.json();
                setProfile(data);
                setEditForm({
                    name: data.name,
                    email: data.email,
                    expertises: data.expertises || [],
                });
            } else {
                // Fallback to AuthStore data
                const user = AuthStore.getCurrentUser();
                if (user) {
                    setProfile({
                        id: user.id,
                        name: user.name,
                        email: user.email,
                        role: user.role,
                        agencyId: user.agencyId,
                        agencyName: user.agencyName,
                        permissions: user.permissions?.join(',') || '',
                        expertises: [],
                        createdAt: new Date().toISOString(),
                    });
                    setEditForm({ name: user.name, email: user.email, expertises: [] });
                }
            }
        } catch {
            const user = AuthStore.getCurrentUser();
            if (user) {
                setProfile({
                    id: user.id, name: user.name, email: user.email, role: user.role,
                    permissions: user.permissions?.join(',') || '', expertises: [], createdAt: new Date().toISOString(),
                });
                setEditForm({ name: user.name, email: user.email, expertises: [] });
            }
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async () => {
        if (!profile) return;
        setSaving(true);
        try {
            const token = AuthStore.getToken();
            const res = await fetch(`${API_URL}/users/me`, {
                method: 'PATCH',
                headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                body: JSON.stringify(editForm),
            });
            if (res.ok) {
                const updated = await res.json();
                setProfile(updated);
                setEditing(false);
                setSaveSuccess(true);
                setTimeout(() => setSaveSuccess(false), 3000);
            }
        } catch (e) {
            console.error('Save failed', e);
        } finally {
            setSaving(false);
        }
    };

    const handlePasswordChange = async () => {
        if (passwordForm.new !== passwordForm.confirm) {
            setPasswordError('Les mots de passe ne correspondent pas.');
            return;
        }
        if (passwordForm.new.length < 6) {
            setPasswordError('Le mot de passe doit contenir au moins 6 caractères.');
            return;
        }
        setPasswordError('');
        setSaving(true);
        try {
            const token = AuthStore.getToken();
            const res = await fetch(`${API_URL}/users/me`, {
                method: 'PATCH',
                headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${token}` },
                body: JSON.stringify({ password: passwordForm.new }),
            });
            if (res.ok) {
                setPasswordForm({ current: '', new: '', confirm: '' });
                setSaveSuccess(true);
                setTimeout(() => setSaveSuccess(false), 3000);
            }
        } catch (e) {
            console.error('Password change failed', e);
        } finally {
            setSaving(false);
        }
    };

    if (loading || !profile) {
        return (
            <div className="flex items-center justify-center min-h-[60vh]">
                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full" />
            </div>
        );
    }

    const normalizedRole = normalizeRole(profile.role);
    const roleColor = ROLE_COLORS[normalizedRole];
    const roleLabel = ROLE_LABELS[normalizedRole];
    const roleDesc = ROLE_DESCRIPTIONS[normalizedRole];
    const permissions = ROLE_PERMISSIONS[normalizedRole] || [];
    const initials = profile.name.split(' ').map(n => n[0]).join('').toUpperCase().slice(0, 2);
    const memberSince = new Date(profile.createdAt).toLocaleDateString('fr-FR', { year: 'numeric', month: 'long' });

    return (
        <div className="max-w-5xl mx-auto space-y-8">
            {/* Header Card */}
            <div className="relative bg-gradient-to-br from-slate-900 via-slate-800 to-indigo-900 rounded-[2.5rem] p-10 text-white overflow-hidden shadow-2xl">
                {/* Background decoration */}
                <div className="absolute top-0 right-0 w-80 h-80 bg-indigo-500/10 rounded-full -mr-20 -mt-20" />
                <div className="absolute bottom-0 left-0 w-60 h-60 bg-violet-500/10 rounded-full -ml-10 -mb-10" />

                <div className="relative flex items-start gap-8">
                    {/* Avatar */}
                    <div className="relative group">
                        <div className="w-28 h-28 rounded-[2rem] bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center text-4xl font-black shadow-xl shadow-indigo-500/30">
                            {initials}
                        </div>
                        <button className="absolute -bottom-2 -right-2 w-10 h-10 bg-white rounded-xl flex items-center justify-center shadow-lg opacity-0 group-hover:opacity-100 transition-all">
                            <Camera size={16} className="text-slate-700" />
                        </button>
                    </div>

                    {/* User Info */}
                    <div className="flex-1 space-y-3">
                        <div className="flex items-center gap-4">
                            <h1 className="text-3xl font-black tracking-tight">{profile.name}</h1>
                            <span className={`px-4 py-1.5 rounded-full text-xs font-black uppercase tracking-wider border ${roleColor.bg} ${roleColor.text} ${roleColor.border}`}>
                                {roleLabel}
                            </span>
                        </div>
                        <p className="text-slate-300 font-medium">{roleDesc}</p>
                        <div className="flex items-center gap-6 text-sm text-slate-400">
                            <span className="flex items-center gap-2"><Mail size={14} /> {profile.email}</span>
                            {(profile.agency?.name || profile.agencyName) && (
                                <span className="flex items-center gap-2"><Building2 size={14} /> {profile.agency?.name || profile.agencyName}</span>
                            )}
                            <span className="flex items-center gap-2"><Calendar size={14} /> Membre depuis {memberSince}</span>
                        </div>
                    </div>

                    {/* Quick actions */}
                    <div className="flex gap-3">
                        <button onClick={() => setActiveTab('security')} className="px-5 py-3 bg-white/10 hover:bg-white/20 rounded-xl font-bold text-sm transition-all border border-white/5 flex items-center gap-2">
                            <Key size={14} /> Sécurité
                        </button>
                        <button onClick={() => AuthStore.logout()} className="px-5 py-3 bg-red-500/20 hover:bg-red-500/30 rounded-xl font-bold text-sm transition-all border border-red-500/20 text-red-200 flex items-center gap-2">
                            <LogOut size={14} /> Déconnexion
                        </button>
                    </div>
                </div>
            </div>

            {/* Success Toast */}
            {saveSuccess && (
                <div className="fixed top-6 right-6 z-50 flex items-center gap-3 px-6 py-4 bg-emerald-600 text-white rounded-2xl shadow-2xl shadow-emerald-500/30 animate-in slide-in-from-top-4 duration-500">
                    <CheckCircle size={20} /> Profil mis à jour avec succès
                </div>
            )}

            {/* Tabs */}
            <div className="flex p-1 bg-slate-100 rounded-2xl w-fit">
                {([
                    { id: 'info', label: 'Informations', icon: User },
                    { id: 'security', label: 'Sécurité', icon: Lock },
                    { id: 'permissions', label: 'Droits & Accès', icon: Shield },
                    { id: 'activity', label: 'Activité', icon: Activity },
                ] as const).map(tab => (
                    <button
                        key={tab.id}
                        onClick={() => setActiveTab(tab.id)}
                        className={`flex items-center gap-2 px-6 py-3 rounded-xl font-black text-sm transition-all ${activeTab === tab.id
                            ? 'bg-white text-indigo-600 shadow-sm'
                            : 'text-slate-500 hover:text-slate-700'
                            }`}
                    >
                        <tab.icon size={16} />
                        {tab.label}
                    </button>
                ))}
            </div>

            {/* TAB: Informations personnelles */}
            {activeTab === 'info' && (
                <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                    <div className="flex items-center justify-between">
                        <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Informations personnelles</h2>
                        {!editing ? (
                            <button onClick={() => setEditing(true)} className="px-5 py-2.5 bg-indigo-600 text-white rounded-xl font-bold text-sm hover:bg-indigo-700 transition-all flex items-center gap-2 shadow-lg shadow-indigo-100">
                                <Settings size={14} /> Modifier
                            </button>
                        ) : (
                            <div className="flex gap-2">
                                <button onClick={() => setEditing(false)} className="px-5 py-2.5 bg-slate-100 text-slate-600 rounded-xl font-bold text-sm hover:bg-slate-200 transition-all">
                                    Annuler
                                </button>
                                <button onClick={handleSave} disabled={saving} className="px-5 py-2.5 bg-emerald-600 text-white rounded-xl font-bold text-sm hover:bg-emerald-700 transition-all flex items-center gap-2 shadow-lg shadow-emerald-100">
                                    <Save size={14} /> {saving ? 'Enregistrement...' : 'Sauvegarder'}
                                </button>
                            </div>
                        )}
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Nom complet</label>
                            {editing ? (
                                <input value={editForm.name} onChange={e => setEditForm({ ...editForm, name: e.target.value })} className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-200 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 outline-none transition-all" />
                            ) : (
                                <p className="px-5 py-4 bg-slate-50 rounded-2xl font-bold text-slate-900 border-2 border-transparent">{profile.name}</p>
                            )}
                        </div>
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Email</label>
                            {editing ? (
                                <input type="email" value={editForm.email} onChange={e => setEditForm({ ...editForm, email: e.target.value })} className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-200 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 outline-none transition-all" />
                            ) : (
                                <p className="px-5 py-4 bg-slate-50 rounded-2xl font-bold text-slate-900 border-2 border-transparent">{profile.email}</p>
                            )}
                        </div>
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Rôle</label>
                            <div className={`px-5 py-4 rounded-2xl font-bold flex items-center gap-3 border-2 ${roleColor.bg} ${roleColor.text} ${roleColor.border}`}>
                                <Shield size={18} /> {roleLabel}
                            </div>
                        </div>
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Agence</label>
                            <p className="px-5 py-4 bg-slate-50 rounded-2xl font-bold text-slate-900 border-2 border-transparent flex items-center gap-3">
                                <Building2 size={18} className="text-slate-400" /> {profile.agency?.name || profile.agencyName || 'Siège (HQ)'}
                            </p>
                        </div>
                    </div>

                    {/* Expertises */}
                    <div className="space-y-3">
                        <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Expertises / Spécialisations</label>
                        <div className="flex flex-wrap gap-2">
                            {(profile.expertises || []).length > 0 ? (
                                profile.expertises.map((exp, i) => (
                                    <span key={i} className="px-4 py-2 bg-indigo-50 text-indigo-700 rounded-xl text-sm font-bold border border-indigo-100 flex items-center gap-2">
                                        <Award size={12} /> {exp}
                                    </span>
                                ))
                            ) : (
                                <span className="text-slate-400 text-sm font-medium italic">Aucune expertise définie</span>
                            )}
                        </div>
                    </div>

                    {/* Meta info */}
                    <div className="pt-6 border-t border-slate-100 grid grid-cols-3 gap-6">
                        <div className="text-center p-4 bg-slate-50 rounded-2xl">
                            <p className="text-xs font-black uppercase tracking-widest text-slate-400 mb-1">ID Utilisateur</p>
                            <p className="font-mono text-xs text-slate-600">{profile.id}</p>
                        </div>
                        <div className="text-center p-4 bg-slate-50 rounded-2xl">
                            <p className="text-xs font-black uppercase tracking-widest text-slate-400 mb-1">Créé le</p>
                            <p className="font-bold text-sm text-slate-700">{new Date(profile.createdAt).toLocaleDateString('fr-FR')}</p>
                        </div>
                        <div className="text-center p-4 bg-slate-50 rounded-2xl">
                            <p className="text-xs font-black uppercase tracking-widest text-slate-400 mb-1">Dernière connexion</p>
                            <p className="font-bold text-sm text-slate-700">{profile.lastLogin ? new Date(profile.lastLogin).toLocaleDateString('fr-FR') : 'Aujourd\'hui'}</p>
                        </div>
                    </div>
                </div>
            )}

            {/* TAB: Sécurité */}
            {activeTab === 'security' && (
                <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                    <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Sécurité du compte</h2>

                    <div className="max-w-md space-y-6">
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Mot de passe actuel</label>
                            <div className="relative">
                                <input
                                    type={showPassword ? 'text' : 'password'}
                                    value={passwordForm.current}
                                    onChange={e => setPasswordForm({ ...passwordForm, current: e.target.value })}
                                    className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-200 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 outline-none transition-all pr-14"
                                    placeholder="••••••••"
                                />
                                <button onClick={() => setShowPassword(!showPassword)} className="absolute right-4 top-1/2 -translate-y-1/2 text-slate-400 hover:text-slate-600">
                                    {showPassword ? <EyeOff size={18} /> : <Eye size={18} />}
                                </button>
                            </div>
                        </div>
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Nouveau mot de passe</label>
                            <input
                                type={showPassword ? 'text' : 'password'}
                                value={passwordForm.new}
                                onChange={e => setPasswordForm({ ...passwordForm, new: e.target.value })}
                                className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-200 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 outline-none transition-all"
                                placeholder="Minimum 6 caractères"
                            />
                        </div>
                        <div className="space-y-2">
                            <label className="block text-xs font-black uppercase tracking-widest text-slate-400">Confirmer le mot de passe</label>
                            <input
                                type={showPassword ? 'text' : 'password'}
                                value={passwordForm.confirm}
                                onChange={e => setPasswordForm({ ...passwordForm, confirm: e.target.value })}
                                className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-200 rounded-2xl font-bold text-slate-900 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 outline-none transition-all"
                                placeholder="Retapez le mot de passe"
                            />
                        </div>

                        {passwordError && (
                            <div className="flex items-center gap-2 text-red-600 text-sm font-bold p-3 bg-red-50 rounded-xl">
                                <AlertCircle size={16} /> {passwordError}
                            </div>
                        )}

                        <button
                            onClick={handlePasswordChange}
                            disabled={!passwordForm.new || !passwordForm.confirm || saving}
                            className="w-full px-6 py-4 bg-indigo-600 text-white rounded-2xl font-black text-sm hover:bg-indigo-700 transition-all disabled:opacity-40 disabled:cursor-not-allowed flex items-center justify-center gap-2 shadow-lg shadow-indigo-100"
                        >
                            <Key size={16} /> {saving ? 'Modification en cours...' : 'Changer le mot de passe'}
                        </button>
                    </div>

                    {/* Security info */}
                    <div className="pt-6 border-t border-slate-100 space-y-3">
                        <h3 className="font-black text-slate-900 uppercase text-xs tracking-widest">Informations de sécurité</h3>
                        <div className="grid grid-cols-2 gap-4">
                            <div className="p-5 bg-emerald-50 rounded-2xl border border-emerald-100 flex items-center gap-4">
                                <CheckCircle className="text-emerald-500 shrink-0" size={24} />
                                <div>
                                    <p className="font-bold text-emerald-800 text-sm">Authentification active</p>
                                    <p className="text-emerald-600 text-xs">Connexion sécurisée JWT</p>
                                </div>
                            </div>
                            <div className="p-5 bg-slate-50 rounded-2xl border border-slate-100 flex items-center gap-4">
                                <Shield className="text-slate-400 shrink-0" size={24} />
                                <div>
                                    <p className="font-bold text-slate-700 text-sm">2FA non activée</p>
                                    <p className="text-slate-500 text-xs">Activez l'authentification double facteur</p>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* TAB: Permissions */}
            {activeTab === 'permissions' && (
                <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                    <div className="flex items-center justify-between">
                        <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Droits & permissions</h2>
                        <div className={`px-4 py-2 rounded-xl text-xs font-black border ${roleColor.bg} ${roleColor.text} ${roleColor.border}`}>
                            {permissions.length} permissions actives
                        </div>
                    </div>

                    <div className="space-y-6">
                        {Object.entries(PERMISSION_GROUPS).map(([groupKey, group]) => {
                            const groupPerms = group.permissions;
                            const activeCount = groupPerms.filter(p => permissions.includes(p)).length;

                            return (
                                <div key={groupKey} className="space-y-3">
                                    <div className="flex items-center justify-between">
                                        <h3 className="font-black text-sm text-slate-700 flex items-center gap-2">
                                            {group.label}
                                        </h3>
                                        <span className="text-xs font-bold text-slate-400">{activeCount}/{groupPerms.length}</span>
                                    </div>
                                    <div className="grid grid-cols-2 gap-2">
                                        {groupPerms.map(perm => {
                                            const has = permissions.includes(perm);
                                            return (
                                                <div
                                                    key={perm}
                                                    className={`flex items-center gap-3 px-4 py-3 rounded-xl border text-sm transition-all ${has
                                                        ? 'bg-emerald-50 border-emerald-100 text-emerald-700'
                                                        : 'bg-slate-50 border-slate-100 text-slate-400'
                                                        }`}
                                                >
                                                    {has ? <CheckCircle size={14} className="shrink-0" /> : <Lock size={14} className="shrink-0 opacity-30" />}
                                                    <span className={`font-bold text-xs ${has ? '' : 'line-through opacity-50'}`}>
                                                        {PERMISSION_LABELS[perm] || perm}
                                                    </span>
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>
                            );
                        })}
                    </div>

                    <div className="pt-4 border-t border-slate-100">
                        <p className="text-xs text-slate-400 font-medium flex items-center gap-2">
                            <Shield size={12} /> Les permissions sont définies par votre rôle <strong>{roleLabel}</strong>. Contactez un administrateur pour les modifier.
                        </p>
                    </div>
                </div>
            )}

            {/* TAB: Activité */}
            {activeTab === 'activity' && (
                <div className="bg-white rounded-[2rem] p-8 border-2 border-slate-100 shadow-sm space-y-6 animate-in fade-in slide-in-from-bottom-4 duration-500">
                    <h2 className="font-black text-slate-900 uppercase text-xs tracking-widest">Activité récente</h2>

                    <div className="space-y-3">
                        {[
                            { icon: <Zap size={16} className="text-indigo-500" />, label: 'Connexion au back-office', time: 'Aujourd\'hui', type: 'auth' },
                            { icon: <CheckCircle size={16} className="text-emerald-500" />, label: 'Profil consulté', time: 'Maintenant', type: 'profile' },
                        ].map((item, i) => (
                            <div key={i} className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 hover:bg-slate-100 transition-all">
                                <div className="w-10 h-10 rounded-xl bg-white border border-slate-200 flex items-center justify-center shadow-sm">
                                    {item.icon}
                                </div>
                                <div className="flex-1">
                                    <p className="font-bold text-sm text-slate-800">{item.label}</p>
                                    <p className="text-xs text-slate-400 font-medium">{item.time}</p>
                                </div>
                                <ChevronRight size={16} className="text-slate-300" />
                            </div>
                        ))}
                    </div>

                    <div className="pt-4 text-center">
                        <p className="text-xs text-slate-400 font-medium">L'historique d'activité complet sera disponible dans une prochaine version.</p>
                    </div>
                </div>
            )}
        </div>
    );
}
