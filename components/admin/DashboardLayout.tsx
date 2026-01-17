'use client';

import React, { ReactNode } from 'react';
import {
    LayoutDashboard,
    FolderKanban,
    Building2,
    Wallet,
    PhoneCall,
    FileSearch,
    DollarSign,
    ChevronRight,
    LogOut,
    User,
    Shield,
    Crown,
    Settings,
    Brain
} from 'lucide-react';


type UserRoleType = 'SUPER_ADMIN' | 'HQ' | 'AGENCY' | 'CASE_WORKER' | 'AGENCY_MANAGER';

interface MenuItem {
    id: string;
    label: string;
    icon: ReactNode;
    href: string;
}

interface DashboardLayoutProps {
    children: ReactNode;
    currentUser: {
        name: string;
        role: UserRoleType;
        agencyName?: string;
    };
    activeMenuItem?: string;
    onMenuClick?: (menuId: string) => void;
    onLogout?: () => void;
}

// Menus selon le rôle
const MENU_CONFIG: Record<UserRoleType, MenuItem[]> = {
    SUPER_ADMIN: [
        { id: 'overview', label: 'Vue Globale', icon: <LayoutDashboard size={20} />, href: '/admin' },
        { id: 'dossiers', label: 'Tous les Dossiers', icon: <FolderKanban size={20} />, href: '/admin/dossiers' },
        { id: 'network', label: 'Réseau Agences', icon: <Building2 size={20} />, href: '/admin/network' },
        { id: 'finances', label: 'Finances', icon: <Wallet size={20} />, href: '/admin/finances' },
        { id: 'settings', label: '⚙️ Paramètres Services', icon: <Settings size={20} />, href: '/admin/settings' },
        { id: 'eligibility', label: '🎓 Éligibilité', icon: <Brain size={20} />, href: '/admin/eligibility' },
    ],
    HQ: [
        { id: 'overview', label: 'Vue Globale', icon: <LayoutDashboard size={20} />, href: '/admin' },
        { id: 'dossiers', label: 'Tous les Dossiers', icon: <FolderKanban size={20} />, href: '/admin/dossiers' },
        { id: 'network', label: 'Réseau Agences', icon: <Building2 size={20} />, href: '/admin/network' },
        { id: 'settings', label: '⚙️ Paramètres Services', icon: <Settings size={20} />, href: '/admin/settings' },
        { id: 'eligibility', label: '🎓 Éligibilité', icon: <Brain size={20} />, href: '/admin/eligibility' },
    ],
    AGENCY: [
        { id: 'overview', label: 'Tableau de Bord', icon: <LayoutDashboard size={20} />, href: '/admin' },
        { id: 'leads', label: 'Mes Leads à rappeler', icon: <PhoneCall size={20} />, href: '/admin/leads' },
        { id: 'dossiers', label: 'Mes Dossiers', icon: <FileSearch size={20} />, href: '/admin/dossiers' },
        { id: 'commissions', label: 'Mes Commissions', icon: <DollarSign size={20} />, href: '/admin/commissions' },
    ],
    CASE_WORKER: [
        { id: 'overview', label: 'Mon Tableau de Bord', icon: <LayoutDashboard size={20} />, href: '/admin' },
        { id: 'dossiers', label: 'Mes Dossiers', icon: <FolderKanban size={20} />, href: '/admin/dossiers' },
    ],
    AGENCY_MANAGER: [
        { id: 'leads', label: 'Mes Leads à rappeler', icon: <PhoneCall size={20} />, href: '/admin/leads' },
        { id: 'dossiers', label: 'Mes Dossiers en cours', icon: <FileSearch size={20} />, href: '/admin/dossiers' },
        { id: 'commissions', label: 'Mes Commissions', icon: <DollarSign size={20} />, href: '/admin/commissions' },
    ],
};


export default function DashboardLayout({
    children,
    currentUser,
    activeMenuItem = 'overview',
    onMenuClick,
    onLogout
}: DashboardLayoutProps) {
    const menuItems = MENU_CONFIG[currentUser.role] || MENU_CONFIG['AGENCY'];

    const getRoleBadge = (role: UserRoleType) => {
        switch (role) {
            case 'SUPER_ADMIN':
                return { label: 'Super Admin', color: 'bg-amber-100 text-amber-700', icon: <Crown size={10} /> };
            case 'HQ':
                return { label: 'Siège', color: 'bg-indigo-100 text-indigo-700', icon: <Building2 size={10} /> };
            case 'AGENCY':
                return { label: 'Agence', color: 'bg-emerald-100 text-emerald-700', icon: <Building2 size={10} /> };
            case 'CASE_WORKER':
                return { label: 'Juriste', color: 'bg-blue-100 text-blue-700', icon: <User size={10} /> };
            case 'AGENCY_MANAGER':
                return { label: 'Manager Agence', color: 'bg-emerald-100 text-emerald-700', icon: <Building2 size={10} /> };
            default:
                return { label: 'Utilisateur', color: 'bg-slate-100 text-slate-700', icon: <User size={10} /> };
        }
    };

    const badge = getRoleBadge(currentUser.role);

    return (
        <div className="min-h-screen bg-slate-100 flex">
            {/* Sidebar */}
            <aside className="w-64 bg-slate-900 text-white flex flex-col">
                {/* Logo */}
                <div className="p-6 border-b border-slate-700">
                    <div className="flex items-center gap-3">
                        <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center font-black text-lg">
                            S
                        </div>
                        <div>
                            <h1 className="font-bold text-lg tracking-tight">SimuLegal</h1>
                            <p className="text-[10px] text-slate-400 uppercase tracking-widest">Back-Office</p>
                        </div>
                    </div>
                </div>

                {/* Menu */}
                <nav className="flex-1 p-4 space-y-1">
                    {menuItems.map((item) => (
                        <button
                            key={item.id}
                            onClick={() => onMenuClick?.(item.id)}
                            className={`w-full flex items-center gap-3 px-4 py-3 rounded-xl text-left transition-all group ${activeMenuItem === item.id
                                ? 'bg-indigo-600 text-white shadow-lg shadow-indigo-600/30'
                                : 'text-slate-400 hover:bg-slate-800 hover:text-white'
                                }`}
                        >
                            {item.icon}
                            <span className="font-medium flex-1">{item.label}</span>
                            {activeMenuItem === item.id && (
                                <ChevronRight size={16} className="opacity-60" />
                            )}
                        </button>
                    ))}
                </nav>

                {/* User Info */}
                <div className="p-4 border-t border-slate-700">
                    <div className="bg-slate-800 rounded-xl p-4">
                        <div className="flex items-center gap-3 mb-3">
                            <div className="w-10 h-10 bg-slate-700 rounded-full flex items-center justify-center">
                                <User size={20} className="text-slate-400" />
                            </div>
                            <div className="flex-1 min-w-0">
                                <p className="font-bold text-sm truncate">{currentUser.name}</p>
                                {currentUser.agencyName && (
                                    <p className="text-[10px] text-slate-400 truncate">{currentUser.agencyName}</p>
                                )}
                            </div>
                        </div>
                        <div className="flex items-center justify-between">
                            <span className={`flex items-center gap-1 text-[10px] font-bold px-2 py-1 rounded-full ${badge.color}`}>
                                {badge.icon}
                                {badge.label}
                            </span>
                            {onLogout && (
                                <button
                                    onClick={onLogout}
                                    className="text-slate-400 hover:text-red-400 transition-colors"
                                    title="Se déconnecter"
                                >
                                    <LogOut size={16} />
                                </button>
                            )}
                        </div>
                    </div>
                </div>
            </aside>

            {/* Main Content */}
            <main className="flex-1 overflow-auto">
                {children}
            </main>
        </div>
    );
}

