'use client';

import React, { useState, useEffect } from 'react';
import DashboardLayout from '../../components/admin/DashboardLayout';
import HQDashboard from '../../components/backoffice/HQDashboard';
import AgencyDashboard from '../../components/backoffice/AgencyDashboard';
import AuditVeillePanel from '../../components/backoffice/AuditVeillePanel';
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
    Settings,
    Eye
} from 'lucide-react';
import RoleGuard from '../../components/auth/RoleGuard';
import UserManagementPanel from '../../components/admin/UserManagementPanel';
import FranchiseMasterPanel from '../../components/admin/FranchiseMasterPanel';
import GlobalFinancePanel from '../../components/admin/finance/GlobalFinancePanel';
import AgencyFinanceView from '../../components/backoffice/AgencyFinanceView';
import DeviceManagementPanel from '../../components/admin/DeviceManagementPanel';
import FleetMonitor from '../../components/admin/FleetMonitor';
import RoleManager from '../../components/admin/rbac/RoleManager';
import { usePermission } from '../../hooks/usePermission';

type AdminView = 'overview' | 'hq-kanban' | 'agency-view' | 'audit-veille' | 'staff-management' | 'franchise-master' | 'finance' | 'device-management' | 'fleet-monitor' | 'rbac';

export default function AdminPage() {
    const { can, user: currentUser } = usePermission();
    const [leads, setLeads] = useState<Lead[]>([]);
    const [isLoading, setIsLoading] = useState(true);
    const [stats, setStats] = useState({
        totalLeads: 0,
        totalRevenue: 0,
        conversionRate: 0,
        activeAgencies: 0,
        pendingDossiers: 0,
        completedToday: 0
    });

    // Handle View View Logic - but now strictly for Dashboard Widgets
    // We default to 'overview' content.
    // If we want to show AgencyView vs HQView depending on permission inside the dashboard WIDGETS, we can keep some logic.

    useEffect(() => {
        const initDashboard = async () => {
            if (currentUser) {
                const allLeads = await CRM.getAllLeads();
                setLeads(allLeads);
                calculateStats(allLeads);
            }
            setIsLoading(false);
        };

        if (currentUser !== undefined) {
            initDashboard();
        }
    }, [currentUser]);

    const calculateStats = (allLeads: Lead[]) => {
        const revenues = allLeads.reduce((acc, lead) => acc + (lead.amountPaid || 0), 0);
        const signed = allLeads.filter(l => l.status === 'SIGNED' || l.status === 'PAID').length;
        const pending = allLeads.filter(l => !['SIGNED', 'PAID', 'CANCELLED', 'ARCHIVED'].includes(l.status)).length;

        // Mock daily stats
        const today = new Date().toISOString().split('T')[0];
        const signedToday = allLeads.filter(l => (l.status === 'SIGNED' || l.status === 'PAID') && l.updatedAt?.startsWith(today)).length;

        setStats({
            totalLeads: allLeads.length,
            totalRevenue: revenues,
            conversionRate: allLeads.length ? Math.round((signed / allLeads.length) * 100) : 0,
            activeAgencies: 5, // Mock
            pendingDossiers: pending,
            completedToday: signedToday
        });
    };

    if (isLoading || !currentUser) return null; // Layout handles loading

    // Render Overview Content Only
    // This is what is shown at /admin
    return (
        <RoleGuard allowedRoles={['SUPERADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'AGENCY']}>
            <div className="p-8">
                <div className="mb-8 flex items-center justify-between">
                    <div>
                        <h1 className="text-3xl font-black text-slate-900">Vue Globale</h1>
                        <p className="text-slate-500 font-medium">
                            {currentUser.role === 'SUPERADMIN' ? 'Super Administration' :
                                'Tableau de bord du réseau SimuLegal'}
                        </p>
                    </div>
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

                {/* Main Grid: AgencyDashboard or HQ Dashboard Widgets */}
                {!can('crm.view_all') ? (
                    <AgencyDashboard />
                ) : (
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                        {/* Recent Leads */}
                        <div className="lg:col-span-2 bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                            <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                                <h2 className="font-bold text-slate-900">Derniers Dossiers</h2>
                                <button
                                    className="text-sm text-indigo-600 font-bold hover:underline"
                                    onClick={() => window.location.href = '/admin/dossiers'}
                                >
                                    Voir tout →
                                </button>
                            </div>
                            <div className="divide-y divide-slate-50">
                                {leads.slice(0, 5).map((lead) => (
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
                                            <span className={`text-[10px] font-bold px-2 py-0.5 rounded-full ${lead.currentStage === 'NEW' ? 'bg-emerald-100 text-emerald-700' :
                                                lead.currentStage === 'COLLECTING' ? 'bg-amber-100 text-amber-700' :
                                                    'bg-slate-100 text-slate-600'
                                                }`}>
                                                {lead.currentStage}
                                            </span>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        {/* Network Quick View */}
                        <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                            <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                                <h2 className="font-bold text-slate-900">Réseau</h2>
                                <button
                                    className="text-sm text-indigo-600 font-bold hover:underline"
                                    onClick={() => window.location.href = '/admin/network'}
                                >
                                    Gérer →
                                </button>
                            </div>
                            <div className="p-4 space-y-3">
                                {MOCK_AGENCIES.slice(0, 5).map((agency) => (
                                    <div key={agency.id} className="flex items-center gap-3 p-3 rounded-xl bg-slate-50">
                                        <div className={`w-8 h-8 rounded-lg flex items-center justify-center ${agency.type === 'HQ' ? 'bg-red-100 text-red-600' :
                                            agency.type === 'INTEGRATED' ? 'bg-blue-100 text-blue-600' :
                                                agency.type === 'FRANCHISE' ? 'bg-emerald-100 text-emerald-600' :
                                                    'bg-amber-100 text-amber-600'
                                            }`}>
                                            <Building2 size={16} />
                                        </div>
                                        <div className="flex-1 min-w-0">
                                            <p className="font-bold text-sm text-slate-900 truncate">{agency.name}</p>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                )}
            </div>
        </RoleGuard>
    );
}

// Composant StatCard stays same...



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

