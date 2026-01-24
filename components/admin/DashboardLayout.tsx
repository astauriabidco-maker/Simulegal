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
    Users,
    Shield,
    Crown,
    Settings,
    Brain,
    Eye,
    Tablet,
    Calendar as CalendarIcon,
    TrendingUp,
    FileText,
    ArrowRightLeft,
    MessageSquare
} from 'lucide-react';
import Link from 'next/link';
import { usePermission } from '../../hooks/usePermission';
import { PermissionKey } from '../../config/PermissionRegistry';

type UserRoleType = 'SUPERADMIN' | 'HQ' | 'AGENCY' | 'CASE_WORKER' | 'AGENCY_MANAGER';

interface MenuItem {
    id: string;
    label: string;
    icon: ReactNode;
    href?: string;
    permission?: PermissionKey;
    category?: 'PILOTAGE' | 'COMMERCIAL' | 'RESEAU' | 'SYSTEME';
    subItems?: MenuItem[];
}

interface DashboardLayoutProps {
    children: ReactNode;
    currentUser: {
        name: string;
        role: string; // Utilise maintenant l'ID du rôle
        agencyName?: string;
    };
    activeMenuItem?: string;
    onMenuClick?: (menuId: string) => void;
    onLogout?: () => void;
}

// Menus selon le rôle (Servira de fallback si besoin, mais on filtre surtout par permission)
const DEFAULT_MENU: MenuItem[] = [
    // BLOC 1: PILOTAGE
    { id: 'overview', label: 'Vue Globale', icon: <LayoutDashboard size={20} />, href: '/admin', category: 'PILOTAGE' },
    { id: 'agenda-dashboard', label: 'Cockpit Temporel', icon: <TrendingUp size={20} />, href: '/admin/dashboard-agenda', permission: 'crm.view_agency', category: 'PILOTAGE' },
    { id: 'calendar', label: 'Agenda & RDV', icon: <CalendarIcon size={20} />, href: '/admin/calendar', permission: 'crm.view_agency', category: 'PILOTAGE' },

    // BLOC 2: COMMERCIAL & CLIENTS
    { id: 'sales', label: 'Sales Hub', icon: <TrendingUp size={20} />, href: '/admin/sales', permission: 'crm.view_agency', category: 'COMMERCIAL' },
    { id: 'dossiers', label: 'Dossiers', icon: <FolderKanban size={20} />, href: '/admin/dossiers', permission: 'crm.view_agency', category: 'COMMERCIAL' },
    { id: 'franchise-leads', label: 'Franchises (CRM)', icon: <Users size={20} />, href: '/admin/franchise-leads', permission: 'network.manage', category: 'COMMERCIAL' },
    { id: 'inbox', label: 'WhatsApp Inbox', icon: <MessageSquare size={20} />, href: '/admin/inbox', permission: 'crm.view_agency', category: 'COMMERCIAL' },

    // BLOC 3: LOGISTIQUE RÉSEAU
    { id: 'network', label: 'Réseau Agences', icon: <Building2 size={20} />, href: '/admin/network', permission: 'network.manage', category: 'RESEAU' },
    { id: 'devices', label: 'Terminaux', icon: <Tablet size={20} />, href: '/admin/devices', permission: 'fleet.manage', category: 'RESEAU' },

    // BLOC 4: CONFIGURATION & ADMIN
    { id: 'staff', label: 'Gestion Équipe', icon: <Users size={20} />, href: '/admin/staff', permission: 'users.manage', category: 'SYSTEME' },
    {
        id: 'finances',
        label: 'Finances',
        icon: <Wallet size={20} />,
        permission: 'finance.view_agency',
        category: 'SYSTEME',
        subItems: [
            { id: 'finances-overview', label: 'Synthèse', icon: <TrendingUp size={16} />, href: '/admin/finances', permission: 'finance.view_agency' },
            { id: 'finances-invoices', label: 'Factures', icon: <FileText size={16} />, href: '/admin/finances/invoices', permission: 'finance.view_agency' },
            { id: 'finances-transactions', label: 'Règlements', icon: <ArrowRightLeft size={16} />, href: '/admin/finances/transactions', permission: 'finance.view_agency' },
            { id: 'finances-credit-notes', label: 'Avoirs', icon: <ArrowRightLeft size={16} className="rotate-180" />, href: '/admin/finances/credit-notes', permission: 'finance.view_agency' },
            { id: 'finances-payouts', label: 'Reversements', icon: <Building2 size={16} />, href: '/admin/finances/payouts', permission: 'finance.view_agency' },
        ]
    },
    { id: 'roles', label: 'Rôles & Droits', icon: <Shield size={20} />, href: '/admin/rbac', permission: 'roles.manage', category: 'SYSTEME' },
    { id: 'audit-veille', label: 'Audit et Veille', icon: <Eye size={20} />, href: '/admin/audit', permission: 'settings.manage', category: 'SYSTEME' },
    { id: 'settings', label: 'Paramètres Généraux', icon: <Settings size={20} />, href: '/admin/settings', permission: 'settings.manage', category: 'SYSTEME' },
];

export default function DashboardLayout({
    children,
    currentUser,
    activeMenuItem = 'overview',
    onMenuClick,
    onLogout
}: DashboardLayoutProps) {
    const { can, role } = usePermission();

    // Filtre les menus en fonction des permissions de l'utilisateur
    const menuItems = DEFAULT_MENU.filter(item => {
        if (!item.permission) return true;
        return can(item.permission);
    });

    const getRoleBadge = (roleId: string) => {
        if (roleId === 'SUPERADMIN') return { label: 'Super Admin', color: 'bg-amber-100 text-amber-700', icon: <Crown size={10} /> };
        if (roleId === 'HQ') return { label: 'Siège', color: 'bg-indigo-100 text-indigo-700', icon: <Building2 size={10} /> };
        if (roleId.startsWith('AGENCY')) return { label: 'Agence', color: 'bg-emerald-100 text-emerald-700', icon: <Building2 size={10} /> };
        return { label: role?.label || roleId || 'Utilisateur', color: 'bg-slate-100 text-slate-700', icon: <User size={10} /> };
    };

    const badge = getRoleBadge(currentUser.role);

    // Group items by category
    const categories: { id: MenuItem['category']; label: string }[] = [
        { id: 'PILOTAGE', label: 'Pilotage & RDV' },
        { id: 'COMMERCIAL', label: 'Commercial & Clients' },
        { id: 'RESEAU', label: 'Logistique Réseau' },
        { id: 'SYSTEME', label: 'Système & Config' },
    ];

    return (
        <div className="min-h-screen bg-slate-100 flex">
            {/* Sidebar */}
            <aside className="w-64 bg-slate-900 text-white flex flex-col">
                {/* Logo */}
                <div className="p-6 border-b border-slate-700">
                    <div className="flex items-center gap-3">
                        <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center font-black text-lg shadow-lg shadow-indigo-600/20">
                            S
                        </div>
                        <div>
                            <h1 className="font-bold text-lg tracking-tight">SimuLegal</h1>
                            <p className="text-[10px] text-slate-400 uppercase tracking-widest font-black">Back-Office</p>
                        </div>
                    </div>
                </div>

                {/* Menu */}
                <nav className="flex-1 p-4 space-y-8 overflow-y-auto custom-scrollbar">
                    {categories.map((cat) => {
                        const itemsInCat = menuItems.filter(i => i.category === cat.id);
                        if (itemsInCat.length === 0) return null;

                        return (
                            <div key={cat.id} className="space-y-2">
                                <h3 className="px-4 text-[10px] font-black text-slate-500 uppercase tracking-[0.2em]">
                                    {cat.label}
                                </h3>
                                <div className="space-y-1">
                                    {itemsInCat.map((item) => (
                                        <div key={item.id}>
                                            {item.subItems ? (
                                                // Parent item with subitems
                                                <div className="space-y-1">
                                                    <div className={`w-full flex items-center gap-3 px-4 py-2.5 rounded-xl text-left transition-all ${activeMenuItem?.startsWith(item.id)
                                                        ? 'text-white'
                                                        : 'text-slate-400'
                                                        }`}>
                                                        <div className={`${activeMenuItem?.startsWith(item.id) ? 'text-indigo-400' : 'text-slate-500'} transition-colors`}>
                                                            {item.icon}
                                                        </div>
                                                        <span className="font-bold text-sm flex-1">{item.label}</span>
                                                    </div>
                                                    <div className="pl-11 space-y-1">
                                                        {item.subItems.map((subItem) => (
                                                            <Link
                                                                key={subItem.id}
                                                                href={subItem.href!}
                                                                className={`block text-xs font-bold py-2 px-3 rounded-lg transition-colors ${activeMenuItem === subItem.id || (subItem.id === 'finances-overview' && activeMenuItem === 'finances')
                                                                    ? 'bg-indigo-600/10 text-indigo-400' // Active subitem
                                                                    : 'text-slate-500 hover:text-slate-300'
                                                                    }`}
                                                            >
                                                                {subItem.label}
                                                            </Link>
                                                        ))}
                                                    </div>
                                                </div>
                                            ) : (
                                                // Standard item
                                                <Link
                                                    href={item.href!}
                                                    className={`w-full flex items-center gap-3 px-4 py-2.5 rounded-xl text-left transition-all group ${activeMenuItem === item.id
                                                        ? 'bg-indigo-600 text-white shadow-lg shadow-indigo-600/30'
                                                        : 'text-slate-400 hover:bg-slate-800/50 hover:text-white'
                                                        }`}
                                                >
                                                    <div className={`${activeMenuItem === item.id ? 'text-white' : 'text-slate-500 group-hover:text-indigo-400'} transition-colors`}>
                                                        {item.icon}
                                                    </div>
                                                    <span className="font-bold text-sm flex-1">{item.label}</span>
                                                    {activeMenuItem === item.id && (
                                                        <ChevronRight size={14} className="opacity-60" />
                                                    )}
                                                </Link>
                                            )}
                                        </div>
                                    ))}
                                </div>
                            </div>
                        );
                    })}
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

