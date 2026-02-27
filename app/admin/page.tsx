'use client';

import React, { useState, useEffect } from 'react';
import {
    Users, TrendingUp, Clock, FileCheck,
    ArrowUpRight, Building2, Shield,
    Activity, Network, UserPlus,
    BarChart3, CheckCircle2, AlertCircle,
    Smartphone, Search, ChevronRight,
    MapPin, Mail, Zap, Award, ActivitySquare
} from 'lucide-react';
import { usePermission } from '../../hooks/usePermission';
import { AuthStore } from '../../services/authStore';
import { CRM, Lead } from '../../services/crmStore';
import { FranchiseLeadStore } from '../../services/FranchiseLeadStore';
import { DeviceStore } from '../../services/DeviceStore';
import { MOCK_AGENCIES } from '../../types/backoffice';
import RoleGuard from '../../components/auth/RoleGuard';
import AgencyDashboard from '../../components/backoffice/AgencyDashboard';
import FranceMap from '../../components/admin/FranceMap';
import AiReportingChat from '../../components/admin/dashboard/AiReportingChat';
import AiWidgetGrid from '../../components/admin/dashboard/AiWidgetGrid';

export default function AdminPage() {
    const { can, user: currentUser } = usePermission();
    const [leads, setLeads] = useState<Lead[]>([]);
    const [franchiseLeads, setFranchiseLeads] = useState<any>({ new: 0, meeting: 0, signed: 0 });
    const [networkAgencies, setNetworkAgencies] = useState<any[]>([]);
    const [devices, setDevices] = useState<any[]>([]);
    const [isLoading, setIsLoading] = useState(true);
    const [stats, setStats] = useState({
        totalLeads: 0,
        totalRevenue: 0,
        conversionRate: 0,
        activeAgencies: 0,
        pendingDossiers: 0,
        completedToday: 0,
        fleetHealth: 0,
        franchisePipeline: 0
    });

    useEffect(() => {
        const initDashboard = async () => {
            if (currentUser) {
                try {
                    const token = AuthStore.getToken();
                    const baseUrl = (process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3005');
                    const response = await fetch(`${baseUrl}/admin/dashboard/stats`, {
                        headers: { 'Authorization': `Bearer ${token}` }
                    });

                    if (response.ok) {
                        const data = await response.json();
                        setStats(data.overview);
                        setNetworkAgencies(data.network);
                        setFranchiseLeads(data.franchiseStats);

                        // We still fetch leads for the detailed list below
                        const allLeads = await CRM.getAllLeads();
                        setLeads(allLeads);
                    }
                } catch (err) {
                    console.error("Dashboard Init Error", err);
                }
            }
            setIsLoading(false);
        };

        if (currentUser !== undefined) {
            initDashboard();
        }
    }, [currentUser]);

    // calculateStats removed, now coming from backend.

    if (isLoading || !currentUser) return null;

    return (
        <RoleGuard allowedRoles={['SUPERADMIN', 'HQ_ADMIN', 'AGENCY_MANAGER', 'AGENCY']}>
            <div className="p-8 max-w-[1600px] mx-auto space-y-10">
                {/* Header Section */}
                <div className="flex flex-col md:flex-row md:items-end justify-between gap-6">
                    <div>
                        <div className="flex items-center gap-2 mb-2">
                            <span className="px-3 py-1 bg-indigo-50 text-indigo-600 text-[10px] font-black uppercase tracking-widest rounded-full border border-indigo-100">
                                {currentUser.role}
                            </span>
                            <span className="text-slate-300">/</span>
                            <span className="text-slate-400 text-[10px] font-bold uppercase tracking-widest">Dashboard Global</span>
                        </div>
                        <h1 className="text-4xl font-black text-slate-900 tracking-tight">Vue Globale <span className="text-indigo-600">360°</span></h1>
                        <p className="text-slate-500 font-medium mt-1 italic italic">"Le Pilotage Intelligent du Réseau Simulegal"</p>
                    </div>

                    <div className="flex items-center gap-4">
                        <div className="bg-white rounded-2xl p-4 shadow-sm border border-slate-100 flex items-center gap-4">
                            <div className="w-10 h-10 bg-emerald-50 text-emerald-600 rounded-xl flex items-center justify-center">
                                <Zap size={20} fill="currentColor" />
                            </div>
                            <div>
                                <p className="text-[10px] font-black text-slate-400 uppercase">Santé du Parc</p>
                                <p className="text-xl font-black text-slate-900">{stats.fleetHealth}% <span className="text-xs text-emerald-500 font-bold ml-1">Optimal</span></p>
                            </div>
                        </div>
                    </div>
                </div>

                {/* AI BI Chat Agent */}
                {can('crm.view_all') && (
                    <div className="mb-6">
                        <AiReportingChat />
                        <AiWidgetGrid />
                    </div>
                )}

                {/* Top Stat Cards */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                    <StatCard
                        title="Dossiers Totaux"
                        value={stats.totalLeads}
                        label="Volumétrie Réseau"
                        icon={<Users className="text-indigo-600" />}
                        trend="+14%"
                        color="indigo"
                    />
                    <StatCard
                        title="Chiffre d'Affaires"
                        value={`${(stats.totalRevenue / 100).toLocaleString()} €`}
                        label="Revenus Bruts"
                        icon={<TrendingUp className="text-emerald-600" />}
                        trend="+2.4k"
                        color="emerald"
                    />
                    <StatCard
                        title="Conversion"
                        value={`${stats.conversionRate}%`}
                        label="Efficacité Commerciale"
                        icon={<Award className="text-amber-600" />}
                        trend="+5%"
                        color="amber"
                    />
                    <StatCard
                        title="Dossiers Actifs"
                        value={stats.pendingDossiers}
                        label="Files d'attente"
                        icon={<ActivitySquare className="text-blue-600" />}
                        trend="-3"
                        color="blue"
                    />
                </div>

                {/* Main Content Grid */}
                {!can('crm.view_all') ? (
                    <AgencyDashboard />
                ) : (
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                        {/* RECENT ACTIVITY & LEADS */}
                        <div className="lg:col-span-2 space-y-8">
                            <section className="bg-white rounded-[2.5rem] shadow-xl shadow-slate-200/50 border border-slate-100 overflow-hidden">
                                <div className="p-8 border-b border-slate-50 flex justify-between items-center">
                                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                                        <BarChart3 className="text-indigo-600" size={24} />
                                        Performance Opérationnelle
                                    </h2>
                                    <button
                                        onClick={() => window.location.href = '/admin/dossiers'}
                                        className="p-2 hover:bg-slate-100 rounded-xl text-slate-400 transition-colors"
                                    >
                                        <ChevronRight />
                                    </button>
                                </div>
                                <div className="p-4 grid grid-cols-1 md:grid-cols-2 gap-4">
                                    {leads.filter(l => l.status === 'NEW' || l.status === 'SIGNED').slice(0, 4).map(lead => (
                                        <div key={lead.id} className="p-5 rounded-3xl bg-slate-50 border border-slate-100 hover:border-indigo-200 transition-all group">
                                            <div className="flex items-center gap-4">
                                                <div className="w-12 h-12 bg-white rounded-2xl shadow-sm flex items-center justify-center text-xl font-black text-indigo-600">
                                                    {lead.name.charAt(0)}
                                                </div>
                                                <div className="flex-1 min-w-0">
                                                    <p className="font-black text-slate-900 truncate group-hover:text-indigo-600 transition-colors">{lead.name}</p>
                                                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-tighter">{lead.serviceName}</p>
                                                </div>
                                                <div className="text-right">
                                                    <p className="font-black text-slate-900">{(lead.amountPaid / 100).toFixed(0)}€</p>
                                                    <span className={`text-[8px] font-black px-2 py-0.5 rounded-full ${lead.status === 'PAID' ? 'bg-emerald-100 text-emerald-700' : 'bg-indigo-100 text-indigo-700'}`}>
                                                        {lead.status}
                                                    </span>
                                                </div>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </section>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                                {/* FLEET MONITORING MINI */}
                                <div className="bg-slate-900 rounded-[2.5rem] p-8 text-white shadow-2xl relative overflow-hidden group">
                                    <div className="absolute -right-10 -bottom-10 opacity-10 group-hover:scale-110 transition-transform duration-700">
                                        <Network size={200} />
                                    </div>
                                    <div className="relative z-10 space-y-6">
                                        <div className="flex justify-between items-start">
                                            <h3 className="font-black uppercase text-xs tracking-[0.2em] text-indigo-400">Status du Parc</h3>
                                            <Network className="text-indigo-400" />
                                        </div>
                                        <div>
                                            <p className="text-5xl font-black tracking-tighter">{devices.length}</p>
                                            <p className="text-xs font-bold text-slate-400 uppercase">Terminaux Enregistrés</p>
                                        </div>
                                        <div className="flex items-center gap-4">
                                            <div className="flex-1 h-2 bg-white/10 rounded-full overflow-hidden">
                                                <div className="h-full bg-emerald-400" style={{ width: `${stats.fleetHealth}%` }} />
                                            </div>
                                            <span className="text-xs font-black text-emerald-400">{stats.fleetHealth}% UP</span>
                                        </div>
                                        <button
                                            onClick={() => window.location.href = '/admin/devices'}
                                            className="w-full py-3 bg-white/5 border border-white/10 rounded-2xl text-[10px] font-black uppercase tracking-widest hover:bg-white/10 transition-colors"
                                        >
                                            Ouvrir Fleet Monitor
                                        </button>
                                    </div>
                                </div>

                                {/* FRANCHISE PIPELINE MINI */}
                                <div className="bg-white rounded-[2.5rem] p-8 border border-slate-100 shadow-xl shadow-slate-200/50 space-y-6">
                                    <div className="flex justify-between items-start">
                                        <h3 className="font-black uppercase text-xs tracking-[0.2em] text-amber-500">Pipeline Franchise</h3>
                                        <UserPlus className="text-amber-500" />
                                    </div>
                                    <div className="flex items-end gap-3">
                                        <p className="text-5xl font-black text-slate-900 tracking-tighter">{stats.franchisePipeline}</p>
                                        <span className="text-xs font-black text-emerald-500 mb-2">+2 cette semaine</span>
                                    </div>
                                    <div className="space-y-3">
                                        <div className="flex items-center justify-between text-[10px] font-bold">
                                            <span className="text-slate-400 uppercase">Nouveaux</span>
                                            <div className="flex-1 mx-4 h-1 bg-slate-100 rounded-full">
                                                <div className="h-full bg-slate-900" style={{ width: `${(franchiseLeads.new / (stats.franchisePipeline || 1) * 100)}%` }} />
                                            </div>
                                            <span className="text-slate-900">{franchiseLeads.new}</span>
                                        </div>
                                        <div className="flex items-center justify-between text-[10px] font-bold">
                                            <span className="text-slate-400 uppercase">RDV</span>
                                            <div className="flex-1 mx-4 h-1 bg-slate-100 rounded-full">
                                                <div className="h-full bg-slate-900" style={{ width: `${(franchiseLeads.meeting / (stats.franchisePipeline || 1) * 100)}%` }} />
                                            </div>
                                            <span className="text-slate-900">{franchiseLeads.meeting}</span>
                                        </div>
                                        <div className="flex items-center justify-between text-[10px] font-bold">
                                            <span className="text-slate-400 uppercase">Signés</span>
                                            <div className="flex-1 mx-4 h-1 bg-slate-100 rounded-full">
                                                <div className="h-full bg-slate-900" style={{ width: `${(franchiseLeads.signed / (stats.franchisePipeline || 1) * 100)}%` }} />
                                            </div>
                                            <span className="text-slate-900">{franchiseLeads.signed}</span>
                                        </div>
                                    </div>
                                    <button
                                        onClick={() => window.location.href = '/admin/franchise-leads'}
                                        className="w-full py-3 bg-slate-50 border border-slate-100 rounded-2xl text-[10px] font-black uppercase tracking-widest hover:bg-slate-100 transition-colors mt-2"
                                    >
                                        Pipeline Complet
                                    </button>
                                </div>
                            </div>
                        </div>

                        {/* RIGHT SIDEBAR - NETWORK VITALITY & SECURITY */}
                        <div className="space-y-8">
                            <section className="bg-white rounded-[2.5rem] shadow-xl shadow-slate-200/50 border border-slate-100 overflow-hidden">
                                <div className="p-8 border-b border-slate-50 flex justify-between items-center">
                                    <h2 className="text-xl font-black text-slate-900">Implantation</h2>
                                    <button
                                        onClick={() => window.location.href = '/admin/network'}
                                        className="text-xs font-black text-indigo-600 uppercase tracking-widest"
                                    >
                                        Gérer
                                    </button>
                                </div>
                                <div className="p-2">
                                    <FranceMap agencies={networkAgencies} />
                                </div>
                            </section>

                            <section className="bg-indigo-600 rounded-[2.5rem] p-8 text-white shadow-xl shadow-indigo-200 relative overflow-hidden">
                                <div className="absolute top-0 right-0 p-8 opacity-10">
                                    <Shield size={120} />
                                </div>
                                <div className="relative z-10 space-y-6">
                                    <h3 className="font-black uppercase text-xs tracking-widest text-indigo-200">Sécurité & Accès</h3>
                                    <div className="flex items-center gap-4">
                                        <div className="w-12 h-12 bg-white/10 rounded-2xl flex items-center justify-center">
                                            <Shield className="text-white" />
                                        </div>
                                        <div>
                                            <p className="text-lg font-black tracking-tight">RBAC Active</p>
                                            <p className="text-[10px] font-bold text-indigo-200 uppercase tracking-widest">Contrôle de conformité</p>
                                        </div>
                                    </div>
                                    <div className="p-4 bg-white/5 rounded-2xl border border-white/10 space-y-4">
                                        <div className="flex justify-between items-center text-[10px] font-bold">
                                            <span>Rôles Configurés</span>
                                            <span className="px-2 py-0.5 bg-white/10 rounded-full">6</span>
                                        </div>
                                        <div className="flex justify-between items-center text-[10px] font-bold">
                                            <span>Actions Critiques (24h)</span>
                                            <span className="text-amber-400">12 Logged</span>
                                        </div>
                                    </div>
                                    <button
                                        onClick={() => window.location.href = '/admin/rbac'}
                                        className="w-full py-3 bg-white text-indigo-600 rounded-2xl text-[10px] font-black uppercase tracking-widest shadow-xl"
                                    >
                                        Gérer les Permissions
                                    </button>
                                </div>
                            </section>
                        </div>
                    </div>
                )}
            </div>
        </RoleGuard>
    );
}

function StatCard({ title, value, label, icon, trend, color }: {
    title: string;
    value: string | number;
    label: string;
    icon: React.ReactNode;
    trend: string;
    color: string;
}) {
    const bgColor = {
        indigo: 'bg-indigo-50',
        emerald: 'bg-emerald-50',
        amber: 'bg-amber-50',
        blue: 'bg-blue-50'
    }[color];

    return (
        <div className="bg-white rounded-[2rem] p-8 shadow-sm border border-slate-100 hover:shadow-xl transition-all hover:translate-y-[-4px] group">
            <div className="flex justify-between items-start mb-6">
                <div className={`w-14 h-14 ${bgColor} rounded-2xl flex items-center justify-center group-hover:scale-110 transition-transform`}>
                    {icon}
                </div>
                <div className="flex items-center gap-1 text-[10px] font-black uppercase px-2 py-1 bg-slate-50 text-slate-500 rounded-lg">
                    {trend}
                    <ArrowUpRight size={12} className="text-emerald-500" />
                </div>
            </div>
            <div>
                <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1">{label}</p>
                <p className="text-3xl font-black text-slate-900 tracking-tight">{value}</p>
                <p className="text-sm font-bold text-slate-500 mt-2">{title}</p>
            </div>
        </div>
    );
}
