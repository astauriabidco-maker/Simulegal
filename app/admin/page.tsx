'use client';

import React, { useState, useEffect } from 'react';
import DashboardLayout from '../../components/admin/DashboardLayout';
import HQDashboard from '../../components/backoffice/HQDashboard';
import AgencyDashboard from '../../components/backoffice/AgencyDashboard';
import ServiceConfigPanel from '../../components/backoffice/ServiceConfigPanel';
import EligibilityConfigPanel from '../../components/backoffice/EligibilityConfigPanel';
import AdminLogin from '../../components/auth/AdminLogin';
import { AuthStore, AdminUser } from '../../services/authStore';
import { CRM } from '../../services/crmStore';
import { Lead } from '../../services/crmStore';
import { MOCK_AGENCIES } from '../../types/backoffice';
import {
    TrendingUp,
    Users,
    FileCheck,
    Clock,
    ArrowUpRight,
    Building2,
    AlertCircle,
    LayoutDashboard,
    FolderKanban,
    Store,
    LogOut,
    Shield,
    Settings
} from 'lucide-react';

type AdminView = 'overview' | 'hq-kanban' | 'agency-view' | 'settings' | 'eligibility';

export default function AdminPage() {
    const [isAuthenticated, setIsAuthenticated] = useState(false);
    const [currentUser, setCurrentUser] = useState<AdminUser | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [activeMenu, setActiveMenu] = useState('overview');
    const [currentView, setCurrentView] = useState<AdminView>('overview');
    const [leads, setLeads] = useState<Lead[]>([]);
    const [stats, setStats] = useState({
        totalLeads: 0,
        totalRevenue: 0,
        pendingDossiers: 0,
        completedToday: 0
    });

    // Vérifie l'authentification au chargement
    useEffect(() => {
        const user = AuthStore.getCurrentUser();
        if (user) {
            setCurrentUser(user);
            setIsAuthenticated(true);
            // Définit la vue par défaut selon le rôle
            if (user.role === 'AGENCY') {
                setCurrentView('agency-view');
            }
        }
        setIsLoading(false);
    }, []);

    // Charge les données quand authentifié
    useEffect(() => {
        if (isAuthenticated) {
            const allLeads = CRM.getAllLeads();

            // Filtre les leads selon le rôle
            let filteredLeads = allLeads;
            if (currentUser?.role === 'AGENCY' && currentUser.agencyId) {
                // Agence voit seulement ses leads
                filteredLeads = allLeads.filter(l => l.originAgencyId === currentUser.agencyId);
            }

            setLeads(filteredLeads);

            const today = new Date().toDateString();
            setStats({
                totalLeads: filteredLeads.length,
                totalRevenue: filteredLeads.reduce((sum, l) => sum + (l.amountPaid || 0), 0),
                pendingDossiers: filteredLeads.filter(l => l.status === 'PAID' || l.status === 'PROCESSING').length,
                completedToday: filteredLeads.filter(l => new Date(l.createdAt).toDateString() === today).length
            });
        }
    }, [isAuthenticated, currentUser]);

    const handleLoginSuccess = (user: AdminUser) => {
        setCurrentUser(user);
        setIsAuthenticated(true);
        // Vue par défaut selon le rôle
        if (user.role === 'AGENCY') {
            setCurrentView('agency-view');
        } else {
            setCurrentView('overview');
        }
    };

    const handleLogout = () => {
        AuthStore.logout();
        setCurrentUser(null);
        setIsAuthenticated(false);
        setCurrentView('overview');
    };

    const handleMenuClick = (menuId: string) => {
        setActiveMenu(menuId);
        if (menuId === 'dossiers') {
            setCurrentView(currentUser?.role === 'AGENCY' ? 'agency-view' : 'hq-kanban');
        } else if (menuId === 'overview') {
            setCurrentView('overview');
        } else if (menuId === 'settings') {
            setCurrentView('settings');
        } else if (menuId === 'eligibility') {
            setCurrentView('eligibility');
        }
    };

    // Loading state
    if (isLoading) {
        return (
            <div className="min-h-screen bg-slate-100 flex items-center justify-center">
                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full"></div>
            </div>
        );
    }

    // Non authentifié → Login
    if (!isAuthenticated || !currentUser) {
        return <AdminLogin onLoginSuccess={handleLoginSuccess} />;
    }

    // Rendu conditionnel selon la vue et le rôle
    const renderContent = () => {
        // Agence ne peut voir que son dashboard
        if (currentUser.role === 'AGENCY') {
            return <AgencyDashboard />;
        }

        // HQ et SuperAdmin peuvent switcher
        switch (currentView) {
            case 'hq-kanban':
                return <HQDashboard />;
            case 'agency-view':
                return <AgencyDashboard />;
            case 'settings':
                return <ServiceConfigPanel />;
            case 'eligibility':
                return <EligibilityConfigPanel />;
            default:
                return renderOverview();
        }
    };

    // Vérifie si l'utilisateur peut voir le switcher de vue
    const canSwitchViews = currentUser.role === 'HQ' || currentUser.role === 'SUPERADMIN';

    const renderOverview = () => (
        <div className="p-8">
            {/* Header avec Switcher */}
            <div className="mb-8 flex items-center justify-between">
                <div>
                    <h1 className="text-3xl font-black text-slate-900">Vue Globale</h1>
                    <p className="text-slate-500 font-medium">
                        {currentUser.role === 'SUPERADMIN' ? 'Super Administration' :
                            currentUser.role === 'HQ' ? 'Tableau de bord du réseau SimuLegal' :
                                `Tableau de bord - ${currentUser.agencyName}`}
                    </p>
                </div>

                {/* Switcher de Vue (seulement pour HQ et SuperAdmin) */}
                {canSwitchViews && (
                    <div className="flex gap-2 bg-slate-100 p-1.5 rounded-xl">
                        <button
                            onClick={() => setCurrentView('overview')}
                            className={`flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-bold transition-all ${currentView === 'overview'
                                ? 'bg-white text-slate-900 shadow-sm'
                                : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            <LayoutDashboard size={16} />
                            Overview
                        </button>
                        <button
                            onClick={() => setCurrentView('hq-kanban')}
                            className={`flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-bold transition-all ${currentView === 'hq-kanban'
                                ? 'bg-indigo-600 text-white shadow-sm'
                                : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            <FolderKanban size={16} />
                            Vue HQ
                        </button>
                        <button
                            onClick={() => setCurrentView('agency-view')}
                            className={`flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-bold transition-all ${currentView === 'agency-view'
                                ? 'bg-emerald-600 text-white shadow-sm'
                                : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            <Store size={16} />
                            Vue Agence
                        </button>
                    </div>
                )}
            </div>

            {/* Stats Cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
                <StatCard
                    title="Total Dossiers"
                    value={stats.totalLeads}
                    icon={<Users className="text-indigo-600" />}
                    trend="+12%"
                    trendUp={true}
                />
                <StatCard
                    title="Chiffre d'Affaires"
                    value={`${(stats.totalRevenue / 100).toFixed(0)} €`}
                    icon={<TrendingUp className="text-emerald-600" />}
                    trend="+8%"
                    trendUp={true}
                />
                <StatCard
                    title="Dossiers en cours"
                    value={stats.pendingDossiers}
                    icon={<Clock className="text-amber-600" />}
                    trend=""
                    trendUp={true}
                />
                <StatCard
                    title="Signés Aujourd'hui"
                    value={stats.completedToday}
                    icon={<FileCheck className="text-blue-600" />}
                    trend=""
                    trendUp={true}
                />
            </div>

            {/* Main Grid */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                {/* Recent Leads */}
                <div className="lg:col-span-2 bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                    <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                        <h2 className="font-bold text-slate-900">Derniers Dossiers</h2>
                        <button
                            onClick={() => setCurrentView(currentUser.role === 'AGENCY' ? 'agency-view' : 'hq-kanban')}
                            className="text-sm text-indigo-600 font-bold hover:underline"
                        >
                            Voir tout →
                        </button>
                    </div>
                    <div className="divide-y divide-slate-50">
                        {leads.length === 0 ? (
                            <div className="p-8 text-center text-slate-400">
                                <AlertCircle className="mx-auto mb-2" size={32} />
                                <p>Aucun dossier pour le moment</p>
                            </div>
                        ) : (
                            leads.slice(0, 5).map((lead) => (
                                <div key={lead.id} className="p-4 hover:bg-slate-50 transition-colors flex items-center gap-4">
                                    <div className="w-10 h-10 bg-indigo-100 rounded-full flex items-center justify-center text-indigo-600 font-bold">
                                        {lead.name.charAt(0).toUpperCase()}
                                    </div>
                                    <div className="flex-1 min-w-0">
                                        <p className="font-bold text-slate-900 truncate">{lead.name}</p>
                                        <p className="text-xs text-slate-400">{lead.serviceName}</p>
                                    </div>
                                    <div className="text-right">
                                        <p className="font-bold text-slate-900">{(lead.amountPaid / 100).toFixed(0)} €</p>
                                        <span className={`text-[10px] font-bold px-2 py-0.5 rounded-full ${lead.status === 'PAID' ? 'bg-emerald-100 text-emerald-700' :
                                            lead.status === 'PROCESSING' ? 'bg-amber-100 text-amber-700' :
                                                'bg-slate-100 text-slate-600'
                                            }`}>
                                            {lead.status}
                                        </span>
                                    </div>
                                </div>
                            ))
                        )}
                    </div>
                </div>

                {/* Network Overview (seulement pour HQ/SuperAdmin) */}
                {canSwitchViews && (
                    <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                        <div className="p-6 border-b border-slate-100">
                            <h2 className="font-bold text-slate-900">Réseau d'Agences</h2>
                        </div>
                        <div className="p-4 space-y-3">
                            {MOCK_AGENCIES.map((agency) => (
                                <div key={agency.id} className="flex items-center gap-3 p-3 rounded-xl bg-slate-50 hover:bg-slate-100 transition-colors cursor-pointer"
                                    onClick={() => setCurrentView('agency-view')}
                                >
                                    <div className={`w-8 h-8 rounded-lg flex items-center justify-center ${agency.type === 'HQ' ? 'bg-red-100 text-red-600' :
                                        agency.type === 'OWNED' ? 'bg-blue-100 text-blue-600' :
                                            agency.type === 'FRANCHISE' ? 'bg-emerald-100 text-emerald-600' :
                                                'bg-amber-100 text-amber-600'
                                        }`}>
                                        <Building2 size={16} />
                                    </div>
                                    <div className="flex-1 min-w-0">
                                        <p className="font-bold text-sm text-slate-900 truncate">{agency.name}</p>
                                        <p className="text-[10px] text-slate-400">{agency.zipCodes.length} zones</p>
                                    </div>
                                    <span className={`text-[10px] font-bold px-2 py-0.5 rounded-full ${agency.type === 'HQ' ? 'bg-red-100 text-red-700' :
                                        agency.type === 'OWNED' ? 'bg-blue-100 text-blue-700' :
                                            agency.type === 'FRANCHISE' ? 'bg-emerald-100 text-emerald-700' :
                                                'bg-amber-100 text-amber-700'
                                        }`}>
                                        {agency.type}
                                    </span>
                                </div>
                            ))}
                        </div>
                    </div>
                )}

                {/* User Info Card (pour les agences) */}
                {!canSwitchViews && (
                    <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                        <div className="p-6 border-b border-slate-100">
                            <h2 className="font-bold text-slate-900">Mon Agence</h2>
                        </div>
                        <div className="p-6 text-center">
                            <div className="w-16 h-16 bg-emerald-100 rounded-2xl flex items-center justify-center mx-auto mb-4">
                                <Building2 className="text-emerald-600" size={32} />
                            </div>
                            <h3 className="font-bold text-slate-900 text-lg">{currentUser.agencyName}</h3>
                            <p className="text-sm text-slate-500 mb-4">{currentUser.email}</p>
                            <div className="flex flex-wrap gap-2 justify-center">
                                {currentUser.permissions.map((perm) => (
                                    <span key={perm} className="text-[10px] font-bold px-2 py-1 rounded-full bg-slate-100 text-slate-600">
                                        {perm.replace('_', ' ')}
                                    </span>
                                ))}
                            </div>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );

    // Construit l'objet user pour DashboardLayout
    const layoutUser = {
        name: currentUser.name,
        role: currentUser.role === 'SUPERADMIN' ? 'SUPER_ADMIN' as const : currentUser.role as 'HQ' | 'AGENCY',
        agencyName: currentUser.agencyName || 'SimuLegal HQ'
    };

    return (
        <DashboardLayout
            currentUser={layoutUser}
            activeMenuItem={activeMenu}
            onMenuClick={handleMenuClick}
            onLogout={handleLogout}
        >
            {renderContent()}
        </DashboardLayout>
    );
}


// Composant StatCard
function StatCard({
    title,
    value,
    icon,
    trend,
    trendUp
}: {
    title: string;
    value: string | number;
    icon: React.ReactNode;
    trend: string;
    trendUp: boolean;
}) {
    return (
        <div className="bg-white rounded-2xl p-6 shadow-sm border border-slate-100 hover:shadow-md transition-shadow">
            <div className="flex items-center justify-between mb-4">
                <div className="w-12 h-12 bg-slate-50 rounded-xl flex items-center justify-center">
                    {icon}
                </div>
                {trend && (
                    <span className={`flex items-center gap-1 text-xs font-bold ${trendUp ? 'text-emerald-600' : 'text-red-600'}`}>
                        {trend}
                        <ArrowUpRight size={14} className={!trendUp ? 'rotate-90' : ''} />
                    </span>
                )}
            </div>
            <p className="text-3xl font-black text-slate-900 mb-1">{value}</p>
            <p className="text-sm text-slate-400 font-medium">{title}</p>
        </div>
    );
}

