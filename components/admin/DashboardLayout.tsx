'use client';

import React, { ReactNode } from 'react';
import {
    LayoutDashboard,
    FolderKanban,
    Building2,
    Wallet,
    DollarSign,
    ChevronRight,
    LogOut,
    User,
    Users,
    Shield,
    Crown,
    Settings,
    Eye,
    Tablet,
    Calendar as CalendarIcon,
    TrendingUp,
    FileText,
    ArrowRightLeft,
    MessageSquare,
    Scale,
    Briefcase,
    BarChart3,
    Network,
    Handshake,
    UserCog,
    Gauge,
    Newspaper,
    LayoutGrid
} from 'lucide-react';
import Link from 'next/link';
import { usePermission } from '../../hooks/usePermission';
import { PermissionKey } from '../../config/PermissionRegistry';

interface MenuItem {
    id: string;
    label: string;
    icon: ReactNode;
    href?: string;
    permission?: PermissionKey;
    category?: 'DASHBOARD' | 'ACTIVITE' | 'RESEAU' | 'FINANCES' | 'ADMIN';
    subItems?: MenuItem[];
}

interface DashboardLayoutProps {
    children: ReactNode;
    currentUser: {
        name: string;
        role: string;
        agencyName?: string;
    };
    activeMenuItem?: string;
    onMenuClick?: (menuId: string) => void;
    onLogout?: () => void;
}

const DEFAULT_MENU: MenuItem[] = [
    // ─── BLOC 1: TABLEAU DE BORD ─── Vue d'ensemble & monitoring
    { id: 'overview', label: 'Vue d\'ensemble', icon: <Gauge size={20} />, href: '/admin', category: 'DASHBOARD' },
    { id: 'custom-dashboard', label: 'Mon Dashboard', icon: <LayoutGrid size={20} />, href: '/admin/custom-dashboard', category: 'DASHBOARD' },
    { id: 'agenda-dashboard', label: 'Cockpit Temporel', icon: <BarChart3 size={20} />, href: '/admin/dashboard-agenda', permission: 'crm.view_agency', category: 'DASHBOARD' },

    // ─── BLOC 2: ACTIVITÉ ─── Opérations quotidiennes (agences + siège)
    { id: 'calendar', label: 'Agenda & RDV', icon: <CalendarIcon size={20} />, href: '/admin/calendar', permission: 'crm.view_agency', category: 'ACTIVITE' },
    { id: 'sales', label: 'Pipeline Ventes', icon: <TrendingUp size={20} />, href: '/admin/sales', permission: 'crm.view_agency', category: 'ACTIVITE' },
    { id: 'sales-tracking', label: 'Suivi Commercial', icon: <BarChart3 size={20} />, href: '/admin/sales-tracking', permission: 'crm.view_agency', category: 'ACTIVITE' },
    { id: 'dossiers', label: 'Dossiers Clients', icon: <FolderKanban size={20} />, href: '/admin/dossiers', permission: 'crm.view_agency', category: 'ACTIVITE' },
    { id: 'inbox', label: 'Messagerie', icon: <MessageSquare size={20} />, href: '/admin/inbox', permission: 'inbox.view', category: 'ACTIVITE' },
    { id: 'blog', label: 'Blog & Insights', icon: <Newspaper size={20} />, href: '/admin/blog', permission: 'settings.manage', category: 'ACTIVITE' },

    // ─── BLOC 3: RÉSEAU & FRANCHISE ─── Gestion du réseau d'agences
    { id: 'franchise-leads', label: 'CRM Franchise', icon: <Handshake size={20} />, href: '/admin/franchise-leads', permission: 'network.manage', category: 'RESEAU' },
    { id: 'network', label: 'Agences & Points', icon: <Network size={20} />, href: '/admin/network', permission: 'network.manage', category: 'RESEAU' },
    { id: 'devices', label: 'Flotte Terminaux', icon: <Tablet size={20} />, href: '/admin/devices', permission: 'fleet.manage', category: 'RESEAU' },
    { id: 'commissions', label: 'Commissions Réseau', icon: <ArrowRightLeft size={20} />, href: '/admin/finances/payouts', permission: 'network.manage', category: 'RESEAU' },

    // ─── BLOC 4: FINANCES ─── Facturation clients, encaissements et avoirs
    {
        id: 'finances',
        label: 'Facturation',
        icon: <Wallet size={20} />,
        permission: 'finance.view_agency',
        category: 'FINANCES',
        subItems: [
            { id: 'finances-overview', label: 'Tableau de bord', icon: <BarChart3 size={16} />, href: '/admin/finances', permission: 'finance.view_agency' },
            { id: 'finances-invoices', label: 'Factures Clients', icon: <FileText size={16} />, href: '/admin/finances/invoices', permission: 'finance.view_agency' },
            { id: 'finances-transactions', label: 'Transactions', icon: <ArrowRightLeft size={16} />, href: '/admin/finances/transactions', permission: 'finance.view_agency' },
            { id: 'finances-credit-notes', label: 'Avoirs', icon: <ArrowRightLeft size={16} className="rotate-180" />, href: '/admin/finances/credit-notes', permission: 'finance.view_agency' },
        ]
    },

    // ─── BLOC 5: ADMINISTRATION ─── Configuration & système
    { id: 'staff', label: 'Utilisateurs', icon: <UserCog size={20} />, href: '/admin/staff', permission: 'users.manage', category: 'ADMIN' },
    { id: 'roles', label: 'Rôles & Droits', icon: <Shield size={20} />, href: '/admin/rbac', permission: 'roles.manage', category: 'ADMIN' },
    { id: 'audit-veille', label: 'Audit & Veille', icon: <Eye size={20} />, href: '/admin/audit', permission: 'settings.manage', category: 'ADMIN' },
    {
        id: 'settings',
        label: 'Paramètres',
        icon: <Settings size={20} />,
        permission: 'settings.manage',
        category: 'ADMIN',
        subItems: [
            { id: 'settings-general', label: 'Général', icon: <Settings size={16} />, href: '/admin/settings', permission: 'settings.manage' },
            { id: 'settings-pricing', label: 'Tarifs Services', icon: <DollarSign size={16} />, href: '/admin/settings?tab=SERVICE_PRICING', permission: 'settings.manage' },
            { id: 'settings-legal', label: 'CGV & Contrats', icon: <Scale size={16} />, href: '/admin/settings?tab=LEGAL_DOCS', permission: 'settings.manage' },
        ]
    },
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
        const badges: Record<string, { label: string; color: string; icon: ReactNode }> = {
            'SUPERADMIN': { label: 'Super Admin', color: 'bg-red-100 text-red-700', icon: <Crown size={10} /> },
            'SUPER_ADMIN': { label: 'Super Admin', color: 'bg-red-100 text-red-700', icon: <Crown size={10} /> },
            'HQ': { label: 'Siège', color: 'bg-amber-100 text-amber-700', icon: <Building2 size={10} /> },
            'HQ_ADMIN': { label: 'Siège', color: 'bg-amber-100 text-amber-700', icon: <Building2 size={10} /> },
            'CASE_WORKER': { label: 'Juriste', color: 'bg-blue-100 text-blue-700', icon: <User size={10} /> },
            'AGENCY': { label: 'Agence', color: 'bg-emerald-100 text-emerald-700', icon: <Building2 size={10} /> },
            'AGENCY_MANAGER': { label: 'Agence', color: 'bg-emerald-100 text-emerald-700', icon: <Building2 size={10} /> },
            'SALES': { label: 'Commercial', color: 'bg-violet-100 text-violet-700', icon: <User size={10} /> },
            'KIOSK_AGENT': { label: 'Kiosque', color: 'bg-purple-100 text-purple-700', icon: <Tablet size={10} /> },
        };
        return badges[roleId] || { label: role?.label || roleId || 'Utilisateur', color: 'bg-slate-100 text-slate-700', icon: <User size={10} /> };
    };

    const badge = getRoleBadge(currentUser.role);

    // Group items by category — 5 blocs fonctionnels cohérents
    const categories: { id: MenuItem['category']; label: string }[] = [
        { id: 'DASHBOARD', label: 'Tableau de bord' },
        { id: 'ACTIVITE', label: 'Activité' },
        { id: 'RESEAU', label: 'Réseau & Franchise' },
        { id: 'FINANCES', label: 'Finances' },
        { id: 'ADMIN', label: 'Administration' },
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
                <nav className="flex-1 p-4 space-y-6 overflow-y-auto custom-scrollbar">
                    {categories.map((cat) => {
                        const itemsInCat = menuItems.filter(i => i.category === cat.id);
                        if (itemsInCat.length === 0) return null;

                        return (
                            <div key={cat.id} className="space-y-1.5">
                                <h3 className="px-4 text-[10px] font-black text-slate-500 uppercase tracking-[0.2em] mb-2">
                                    {cat.label}
                                </h3>
                                <div className="space-y-0.5">
                                    {itemsInCat.map((item) => (
                                        <div key={item.id}>
                                            {item.subItems ? (
                                                // Parent item with subitems
                                                <div className="space-y-0.5">
                                                    <div className={`w-full flex items-center gap-3 px-4 py-2.5 rounded-xl text-left transition-all ${activeMenuItem?.startsWith(item.id)
                                                        ? 'text-white'
                                                        : 'text-slate-400'
                                                        }`}>
                                                        <div className={`${activeMenuItem?.startsWith(item.id) ? 'text-indigo-400' : 'text-slate-500'} transition-colors`}>
                                                            {item.icon}
                                                        </div>
                                                        <span className="font-bold text-sm flex-1">{item.label}</span>
                                                    </div>
                                                    <div className="pl-11 space-y-0.5">
                                                        {item.subItems.map((subItem) => (
                                                            <Link
                                                                key={subItem.id}
                                                                href={subItem.href!}
                                                                className={`block text-xs font-bold py-2 px-3 rounded-lg transition-colors ${activeMenuItem === subItem.id || (subItem.id === 'finances-overview' && activeMenuItem === 'finances')
                                                                    ? 'bg-indigo-600/10 text-indigo-400'
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
                        <a href="/admin/profile" className="flex items-center gap-3 mb-3 hover:opacity-80 transition-opacity cursor-pointer">
                            <div className="w-10 h-10 bg-gradient-to-br from-indigo-500 to-violet-600 rounded-full flex items-center justify-center text-white font-black text-xs">
                                {currentUser.name.split(' ').map(n => n[0]).join('').slice(0, 2)}
                            </div>
                            <div className="flex-1 min-w-0">
                                <p className="font-bold text-sm truncate">{currentUser.name}</p>
                                {currentUser.agencyName && (
                                    <p className="text-[10px] text-slate-400 truncate">{currentUser.agencyName}</p>
                                )}
                            </div>
                        </a>
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

