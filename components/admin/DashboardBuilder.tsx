'use client';

import React, { useState, useEffect, useCallback } from 'react';
import {
    Plus, X, GripVertical, Settings, Save, RotateCcw,
    TrendingUp, TrendingDown, Wallet, FileText, Users,
    BarChart3, Clock, CheckCircle2, Target, Building2,
    DollarSign, ArrowRightLeft, Calendar, Briefcase,
    PieChart, AlertTriangle, RefreshCw, LayoutGrid, XCircle
} from 'lucide-react';
import { PermissionKey } from '../../config/PermissionRegistry';
import { FinanceStore } from '../../services/FinanceStore';
import { CRM } from '../../services/crmStore';
import { AgencyStore } from '../../services/AgencyStore';
import { AuthStore } from '../../services/authStore';

// ═══════════════════════════════════════════════
// Simple permission check without hooks (avoids re-render loops)
// ═══════════════════════════════════════════════
function canAccess(permission: PermissionKey): boolean {
    const user = AuthStore.getCurrentUser();
    if (!user) return false;
    if (user.role === 'SUPERADMIN' || user.role === 'SUPER_ADMIN') return true;
    const perms = (user.permissions as string[]) || [];
    return perms.includes(permission);
}

// ═══════════════════════════════════════════════
// WIDGET CATALOG — Each widget has an id, permission, and data fetcher
// ═══════════════════════════════════════════════

interface WidgetDef {
    id: string;
    title: string;
    description: string;
    icon: React.ElementType;
    category: 'finance' | 'crm' | 'network' | 'performance';
    permission: PermissionKey;
    size: 'sm' | 'md' | 'lg';  // sm=1col, md=2col, lg=3col
    color: string;
}

const WIDGET_CATALOG: WidgetDef[] = [
    // ─── FINANCE ───
    { id: 'kpi_gmv', title: 'Chiffre d\'Affaires', description: 'CA total encaissé', icon: TrendingUp, category: 'finance', permission: 'finance.view_agency', size: 'sm', color: 'indigo' },
    { id: 'kpi_margin', title: 'Marge Nette', description: 'Marge après commissions', icon: Wallet, category: 'finance', permission: 'finance.view_global', size: 'sm', color: 'emerald' },
    { id: 'kpi_month', title: 'CA du Mois', description: 'CA mensuel avec tendance MoM', icon: BarChart3, category: 'finance', permission: 'finance.view_agency', size: 'sm', color: 'blue' },
    { id: 'kpi_pending', title: 'Commissions en Attente', description: 'Montant dû aux agences', icon: Clock, category: 'finance', permission: 'finance.view_global', size: 'sm', color: 'amber' },
    { id: 'kpi_refunds', title: 'Total Avoirs', description: 'Montant total des avoirs émis', icon: RotateCcw, category: 'finance', permission: 'finance.view_global', size: 'sm', color: 'rose' },
    { id: 'chart_revenue_12m', title: 'CA 12 Mois', description: 'Graphique barres CA mensuel', icon: BarChart3, category: 'finance', permission: 'finance.view_agency', size: 'lg', color: 'indigo' },
    { id: 'list_recent_invoices', title: 'Dernières Factures', description: '5 dernières factures émises', icon: FileText, category: 'finance', permission: 'finance.view_agency', size: 'md', color: 'indigo' },
    { id: 'list_recent_transactions', title: 'Dernières Transactions', description: '5 dernières transactions', icon: ArrowRightLeft, category: 'finance', permission: 'finance.view_agency', size: 'md', color: 'emerald' },
    { id: 'breakdown_service', title: 'CA par Service', description: 'Répartition CA par type de service', icon: PieChart, category: 'finance', permission: 'finance.view_global', size: 'md', color: 'purple' },

    // ─── CRM ───
    { id: 'kpi_total_leads', title: 'Total Dossiers', description: 'Nombre total de leads actifs', icon: Briefcase, category: 'crm', permission: 'crm.view_agency', size: 'sm', color: 'slate' },
    { id: 'kpi_deals_month', title: 'Deals ce Mois', description: 'Nombre de dossiers signés ce mois', icon: CheckCircle2, category: 'crm', permission: 'crm.view_agency', size: 'sm', color: 'emerald' },
    { id: 'kpi_avg_deal', title: 'Valeur Moyenne Dossier', description: 'Montant moyen par dossier', icon: Target, category: 'crm', permission: 'crm.view_agency', size: 'sm', color: 'violet' },
    { id: 'list_pipeline', title: 'Pipeline par Étape', description: 'Répartition des leads par étape', icon: BarChart3, category: 'crm', permission: 'crm.view_agency', size: 'md', color: 'blue' },

    // ─── NETWORK ───
    { id: 'kpi_agencies', title: 'Nombre d\'Agences', description: 'Agences actives du réseau', icon: Building2, category: 'network', permission: 'network.manage', size: 'sm', color: 'teal' },
    { id: 'list_top_agencies', title: 'Top Agences', description: 'Classement des agences par CA', icon: TrendingUp, category: 'network', permission: 'network.manage', size: 'md', color: 'teal' },

    // ─── PERFORMANCE ───
    { id: 'kpi_conversion', title: 'Taux de Conversion', description: '% de prospects convertis', icon: Target, category: 'performance', permission: 'crm.view_agency', size: 'sm', color: 'amber' },
];

const CATEGORY_LABELS: Record<string, string> = {
    finance: '💰 Finance',
    crm: '📋 CRM & Dossiers',
    network: '🏢 Réseau',
    performance: '📈 Performance',
};

const STORAGE_KEY = 'simulegal_custom_dashboard';

export default function DashboardBuilder() {
    const [activeWidgets, setActiveWidgets] = useState<string[]>([]);
    const [isEditing, setIsEditing] = useState(false);
    const [showPicker, setShowPicker] = useState(false);
    const [pickerCategory, setPickerCategory] = useState<string>('all');
    const [data, setData] = useState<Record<string, any>>({});
    const [loading, setLoading] = useState(true);
    const [dragIdx, setDragIdx] = useState<number | null>(null);

    // Load saved layout (once)
    useEffect(() => {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            try { setActiveWidgets(JSON.parse(saved)); } catch { /* ignore */ }
        }
    }, []);

    // Get available widgets based on permissions (synchronous, no re-renders)
    const availableWidgets = WIDGET_CATALOG.filter(w => canAccess(w.permission));

    // Safe fetch with 5s timeout — never hangs
    const safeFetch = async <T,>(fn: () => Promise<T>, fallback: T): Promise<T> => {
        try {
            return await Promise.race([
                fn(),
                new Promise<T>((_, reject) => setTimeout(() => reject(new Error('timeout')), 5000))
            ]);
        } catch { return fallback; }
    };

    // Fetch all data — runs once on mount, always finishes
    const fetchData = useCallback(async () => {
        setLoading(true);

        const [summary, breakdown, invoices, transactions, creditNotes, leads, agencies] = await Promise.all([
            safeFetch(() => FinanceStore.getFinancialSummary(), null),
            safeFetch(() => FinanceStore.getRevenueBreakdown(), null),
            safeFetch(() => FinanceStore.getInvoices(), []),
            safeFetch(() => FinanceStore.getTransactions(), []),
            safeFetch(() => FinanceStore.getCreditNotes(), []),
            safeFetch(() => CRM.getAllLeads(), []),
            safeFetch(() => AgencyStore.getAllAgencies(), []),
        ]);

        setData({ summary, breakdown, invoices, transactions, creditNotes, leads, agencies });
        setLoading(false);
    }, []);

    // Fetch once on mount
    useEffect(() => { fetchData(); }, [fetchData]);

    const saveLayout = () => {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(activeWidgets));
        setIsEditing(false);
    };

    const addWidget = (id: string) => {
        if (!activeWidgets.includes(id)) {
            setActiveWidgets([...activeWidgets, id]);
        }
    };

    const removeWidget = (id: string) => {
        setActiveWidgets(activeWidgets.filter(w => w !== id));
    };

    const resetLayout = () => {
        // Default layout: first 6 available widgets
        setActiveWidgets(availableWidgets.slice(0, 6).map(w => w.id));
    };

    // Drag and drop reorder
    const handleDragStart = (idx: number) => setDragIdx(idx);
    const handleDragOver = (e: React.DragEvent, idx: number) => {
        e.preventDefault();
        if (dragIdx === null || dragIdx === idx) return;
        const items = [...activeWidgets];
        const [removed] = items.splice(dragIdx, 1);
        items.splice(idx, 0, removed);
        setActiveWidgets(items);
        setDragIdx(idx);
    };
    const handleDragEnd = () => setDragIdx(null);

    const fmt = (v: number) => (v / 100).toLocaleString('fr-FR', { minimumFractionDigits: 0 });

    // ═══ WIDGET RENDER ═══
    const renderWidget = (widgetId: string) => {
        const def = WIDGET_CATALOG.find(w => w.id === widgetId);
        if (!def) return null;

        const s = data.summary || {};
        const bk = data.breakdown || {};
        const invoices = data.invoices || [];
        const transactions = data.transactions || [];
        const creditNotes = data.creditNotes || [];
        const leads = data.leads || [];
        const agencies = data.agencies || [];

        const colorMap: Record<string, string> = {
            indigo: 'border-l-indigo-500', emerald: 'border-l-emerald-500', blue: 'border-l-blue-500',
            amber: 'border-l-amber-500', rose: 'border-l-rose-500', purple: 'border-l-purple-500',
            slate: 'border-l-slate-500', violet: 'border-l-violet-500', teal: 'border-l-teal-500',
        };

        // KPI Widgets
        const renderKpi = (value: string, sub: string, trend?: number) => (
            <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl p-5 shadow-sm relative overflow-hidden h-full`}>
                {isEditing && (
                    <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>
                )}
                <div className="flex items-start justify-between">
                    <div>
                        <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">{def.title}</p>
                        <p className="text-2xl font-black text-slate-900">{value}</p>
                        <p className="text-xs text-slate-500 mt-0.5">{sub}</p>
                    </div>
                    <def.icon size={28} className="text-slate-200" />
                </div>
                {trend !== undefined && (
                    <p className={`text-xs font-bold mt-1 flex items-center gap-1 ${trend >= 0 ? 'text-emerald-600' : 'text-rose-600'}`}>
                        {trend >= 0 ? <TrendingUp size={12} /> : <TrendingDown size={12} />}
                        {trend >= 0 ? '+' : ''}{trend}%
                    </p>
                )}
            </div>
        );

        switch (widgetId) {
            case 'kpi_gmv':
                return renderKpi(`${fmt(s.totalGMV || 0)} €`, `${s.totalDeals || invoices.length} dossiers`);
            case 'kpi_margin':
                return renderKpi(`${fmt(s.netRevenue || 0)} €`, `${s.totalGMV ? Math.round((s.netRevenue / s.totalGMV) * 100) : 0}% de marge`);
            case 'kpi_month':
                return renderKpi(`${fmt(s.monthGMV || 0)} €`, 'ce mois', s.momGrowth || 0);
            case 'kpi_pending':
                return renderKpi(`${fmt(s.totalCommissionsPending || 0)} €`, 'à reverser aux agences');
            case 'kpi_refunds':
                return renderKpi(`-${fmt(creditNotes.reduce((a: number, c: any) => a + (c.amount || 0), 0))} €`, `${creditNotes.length} avoir(s)`);
            case 'kpi_total_leads':
                return renderKpi(`${leads.length}`, 'dossiers actifs');
            case 'kpi_deals_month': {
                const now = new Date();
                const thisMonth = leads.filter((l: any) => new Date(l.createdAt).getMonth() === now.getMonth() && new Date(l.createdAt).getFullYear() === now.getFullYear());
                return renderKpi(`${thisMonth.length}`, 'nouveaux ce mois');
            }
            case 'kpi_avg_deal':
                return renderKpi(`${fmt(s.avgDealValue || 0)} €`, 'valeur moyenne');
            case 'kpi_agencies':
                return renderKpi(`${agencies.length}`, 'agences actives');
            case 'kpi_conversion': {
                const total = leads.length;
                const converted = leads.filter((l: any) => l.currentStage === 'PAID' || l.currentStage === 'CLOSED').length;
                return renderKpi(`${total > 0 ? Math.round((converted / total) * 100) : 0}%`, `${converted}/${total} convertis`);
            }

            // CHART: Revenue 12 months
            case 'chart_revenue_12m':
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl p-6 shadow-sm relative h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">{def.title}</h3>
                        {bk.monthly ? (
                            <div className="flex items-end gap-1.5 h-36">
                                {bk.monthly.map((m: any, i: number) => {
                                    const maxG = Math.max(...bk.monthly.map((x: any) => x.gmv), 1);
                                    const h = Math.max((m.gmv / maxG) * 100, 3);
                                    return (
                                        <div key={i} className="flex-1 flex flex-col items-center group relative">
                                            <div className="absolute -top-9 hidden group-hover:block bg-slate-800 text-white text-[9px] py-1 px-2 rounded-lg font-bold whitespace-nowrap z-10">{fmt(m.gmv)}€ · {m.count} deals</div>
                                            <div className="w-full rounded-t-sm bg-gradient-to-t from-indigo-600 to-indigo-400" style={{ height: `${h}%`, minHeight: '4px' }} />
                                            <p className="text-[8px] text-slate-400 mt-1 font-bold">{m.month}</p>
                                        </div>
                                    );
                                })}
                            </div>
                        ) : <p className="text-slate-400 text-sm">Données non disponibles</p>}
                    </div>
                );

            // LIST: Recent invoices
            case 'list_recent_invoices':
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl shadow-sm relative overflow-hidden h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <div className="p-5 border-b border-slate-100"><h3 className="text-xs font-black text-slate-400 uppercase tracking-wider">{def.title}</h3></div>
                        <div className="divide-y divide-slate-50">
                            {invoices.slice(0, 5).map((inv: any) => (
                                <div key={inv.id} className="px-5 py-2.5 flex justify-between items-center">
                                    <div>
                                        <p className="text-sm font-bold text-slate-800">{inv.name}</p>
                                        <p className="text-[10px] text-slate-400">{inv.invoiceNumber}</p>
                                    </div>
                                    <p className="font-black text-sm text-slate-900">{fmt(inv.amountPaid)} €</p>
                                </div>
                            ))}
                            {invoices.length === 0 && <div className="p-8 text-center text-slate-400 text-sm">Aucune facture</div>}
                        </div>
                    </div>
                );

            // LIST: Recent transactions
            case 'list_recent_transactions':
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl shadow-sm relative overflow-hidden h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <div className="p-5 border-b border-slate-100"><h3 className="text-xs font-black text-slate-400 uppercase tracking-wider">{def.title}</h3></div>
                        <div className="divide-y divide-slate-50">
                            {transactions.slice(0, 5).map((tx: any) => (
                                <div key={tx.id} className="px-5 py-2.5 flex justify-between items-center">
                                    <div className="flex items-center gap-2">
                                        <span className={`w-6 h-6 rounded flex items-center justify-center text-[10px] ${tx.type === 'PAYMENT' ? 'bg-emerald-100 text-emerald-600' : 'bg-rose-100 text-rose-600'}`}>
                                            {tx.type === 'PAYMENT' ? <DollarSign size={12} /> : <RotateCcw size={12} />}
                                        </span>
                                        <p className="text-sm font-bold text-slate-800">{tx.method || tx.type}</p>
                                    </div>
                                    <p className={`font-black text-sm ${tx.type === 'REFUND' ? 'text-rose-600' : 'text-slate-900'}`}>{tx.type === 'REFUND' ? '-' : '+'}{fmt(tx.amount)} €</p>
                                </div>
                            ))}
                            {transactions.length === 0 && <div className="p-8 text-center text-slate-400 text-sm">Aucune transaction</div>}
                        </div>
                    </div>
                );

            // Breakdown by service
            case 'breakdown_service':
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl p-6 shadow-sm relative h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">{def.title}</h3>
                        {bk.byService ? (
                            <div className="space-y-3">
                                {Object.entries(bk.byService).sort((a: any, b: any) => b[1] - a[1]).slice(0, 5).map(([svc, amt]: any) => {
                                    const total = Object.values(bk.byService).reduce((s: any, v: any) => s + v, 0) as number;
                                    const pct = total > 0 ? Math.round((amt / total) * 100) : 0;
                                    return (
                                        <div key={svc}>
                                            <div className="flex justify-between text-xs mb-1"><span className="font-bold text-slate-600 truncate max-w-[160px]">{svc}</span><span className="font-black text-slate-900">{pct}%</span></div>
                                            <div className="w-full bg-slate-100 rounded-full h-2"><div className="h-2 rounded-full bg-purple-500" style={{ width: `${pct}%` }} /></div>
                                        </div>
                                    );
                                })}
                            </div>
                        ) : <p className="text-slate-400 text-sm">Données non disponibles</p>}
                    </div>
                );

            // Pipeline by stage
            case 'list_pipeline': {
                const stages: Record<string, number> = {};
                leads.forEach((l: any) => { stages[l.currentStage || 'UNKNOWN'] = (stages[l.currentStage || 'UNKNOWN'] || 0) + 1; });
                const stageColors: Record<string, string> = { NEW: 'bg-blue-500', CONTACTED: 'bg-indigo-500', RDV: 'bg-purple-500', QUALIFIED: 'bg-violet-500', PAID: 'bg-emerald-500', CLOSED: 'bg-slate-500' };
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl p-6 shadow-sm relative h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">{def.title}</h3>
                        <div className="space-y-2">
                            {Object.entries(stages).sort((a, b) => b[1] - a[1]).map(([stage, count]) => (
                                <div key={stage} className="flex items-center gap-3">
                                    <div className={`w-3 h-3 rounded-full ${stageColors[stage] || 'bg-slate-300'}`} />
                                    <span className="text-xs font-bold text-slate-600 flex-1">{stage}</span>
                                    <span className="text-xs font-black text-slate-900">{count}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                );
            }

            // Top agencies
            case 'list_top_agencies':
                return (
                    <div className={`bg-white border border-slate-200 border-l-4 ${colorMap[def.color]} rounded-2xl shadow-sm relative overflow-hidden h-full`}>
                        {isEditing && <button onClick={() => removeWidget(def.id)} className="absolute top-2 right-2 p-1 text-slate-300 hover:text-rose-500 z-10"><XCircle size={16} /></button>}
                        <div className="p-5 border-b border-slate-100"><h3 className="text-xs font-black text-slate-400 uppercase tracking-wider">{def.title}</h3></div>
                        <div className="divide-y divide-slate-50">
                            {agencies.slice(0, 5).map((a: any, i: number) => (
                                <div key={a.id} className="px-5 py-2.5 flex items-center gap-3">
                                    <span className="w-6 h-6 bg-slate-900 text-white rounded-md flex items-center justify-center text-[10px] font-black">{i + 1}</span>
                                    <span className="text-sm font-bold text-slate-800 flex-1">{a.name}</span>
                                    <span className="text-xs font-bold text-slate-400">{a.commissionRate}%</span>
                                </div>
                            ))}
                            {agencies.length === 0 && <div className="p-8 text-center text-slate-400 text-sm">Aucune agence</div>}
                        </div>
                    </div>
                );

            default:
                return <div className="bg-white border border-slate-200 rounded-2xl p-6">Widget non implémenté</div>;
        }
    };

    // Grid size mapping
    const sizeClass = (id: string) => {
        const def = WIDGET_CATALOG.find(w => w.id === id);
        if (!def) return 'col-span-1';
        return def.size === 'lg' ? 'col-span-3' : def.size === 'md' ? 'col-span-2' : 'col-span-1';
    };

    if (loading) return (
        <div className="flex items-center justify-center h-[60vh]">
            <RefreshCw size={32} className="animate-spin text-indigo-500" />
        </div>
    );

    return (
        <div className="p-8 max-w-[1400px] mx-auto">
            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tight">Mon Tableau de Bord</h1>
                    <p className="text-slate-500 text-sm">
                        {isEditing ? '🔧 Mode édition — Glissez, ajoutez ou supprimez des widgets' : `${activeWidgets.length} widget${activeWidgets.length > 1 ? 's' : ''} · ${availableWidgets.length} disponibles`}
                    </p>
                </div>
                <div className="flex gap-2">
                    {isEditing ? (
                        <>
                            <button onClick={() => setShowPicker(true)} className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition">
                                <Plus size={16} /> Ajouter
                            </button>
                            <button onClick={resetLayout} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-600 rounded-xl text-sm font-bold hover:bg-slate-200 transition">
                                <RotateCcw size={16} /> Réinitialiser
                            </button>
                            <button onClick={saveLayout} className="flex items-center gap-2 px-4 py-2 bg-emerald-600 text-white rounded-xl text-sm font-bold hover:bg-emerald-700 transition">
                                <Save size={16} /> Enregistrer
                            </button>
                        </>
                    ) : (
                        <>
                            <button onClick={fetchData} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition">
                                <RefreshCw size={16} /> Actualiser
                            </button>
                            <button onClick={() => setIsEditing(true)} className="flex items-center gap-2 px-4 py-2 bg-slate-900 text-white rounded-xl text-sm font-bold hover:bg-slate-800 transition">
                                <Settings size={16} /> Personnaliser
                            </button>
                        </>
                    )}
                </div>
            </div>

            {/* Empty state */}
            {activeWidgets.length === 0 && (
                <div className="bg-white border-2 border-dashed border-slate-200 rounded-2xl p-20 text-center">
                    <LayoutGrid size={48} className="mx-auto mb-4 text-slate-300" />
                    <h2 className="text-xl font-black text-slate-900 mb-2">Construisez votre tableau de bord</h2>
                    <p className="text-slate-500 text-sm mb-6 max-w-md mx-auto">
                        Sélectionnez les widgets qui vous intéressent. Seuls les widgets autorisés par vos droits d'accès sont proposés.
                    </p>
                    <button onClick={() => { setIsEditing(true); setShowPicker(true); }} className="px-6 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 transition flex items-center gap-2 mx-auto">
                        <Plus size={18} /> Commencer
                    </button>
                </div>
            )}

            {/* Widget Grid */}
            {activeWidgets.length > 0 && (
                <div className="grid grid-cols-3 gap-4 auto-rows-auto">
                    {activeWidgets.map((wid, idx) => (
                        <div
                            key={wid}
                            className={`${sizeClass(wid)} ${isEditing ? 'cursor-grab active:cursor-grabbing' : ''} ${dragIdx === idx ? 'opacity-50' : ''}`}
                            draggable={isEditing}
                            onDragStart={() => handleDragStart(idx)}
                            onDragOver={e => handleDragOver(e, idx)}
                            onDragEnd={handleDragEnd}
                        >
                            {isEditing && (
                                <div className="flex items-center gap-1 mb-1 text-slate-300">
                                    <GripVertical size={14} />
                                    <span className="text-[9px] font-bold uppercase tracking-wider">
                                        {WIDGET_CATALOG.find(w => w.id === wid)?.title}
                                    </span>
                                </div>
                            )}
                            {renderWidget(wid)}
                        </div>
                    ))}
                </div>
            )}

            {/* ═══ WIDGET PICKER MODAL ═══ */}
            {showPicker && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-3xl max-h-[80vh] overflow-hidden flex flex-col">
                        <div className="bg-slate-900 p-6 text-white">
                            <div className="flex justify-between items-center">
                                <div>
                                    <h2 className="text-xl font-black">Catalogue de Widgets</h2>
                                    <p className="text-slate-400 text-xs font-bold mt-1">{availableWidgets.length} widgets disponibles selon vos droits</p>
                                </div>
                                <button onClick={() => setShowPicker(false)} className="p-2 hover:bg-white/10 rounded-lg"><X size={20} /></button>
                            </div>

                            {/* Category filter */}
                            <div className="flex gap-2 mt-4">
                                <button onClick={() => setPickerCategory('all')}
                                    className={`px-3 py-1.5 rounded-lg text-xs font-bold transition ${pickerCategory === 'all' ? 'bg-white text-slate-900' : 'bg-white/10 text-slate-300 hover:bg-white/20'}`}>
                                    Tous
                                </button>
                                {Object.entries(CATEGORY_LABELS).map(([cat, label]) => (
                                    <button key={cat} onClick={() => setPickerCategory(cat)}
                                        className={`px-3 py-1.5 rounded-lg text-xs font-bold transition ${pickerCategory === cat ? 'bg-white text-slate-900' : 'bg-white/10 text-slate-300 hover:bg-white/20'}`}>
                                        {label}
                                    </button>
                                ))}
                            </div>
                        </div>

                        <div className="flex-1 overflow-y-auto p-6">
                            <div className="grid grid-cols-2 gap-3">
                                {availableWidgets
                                    .filter(w => pickerCategory === 'all' || w.category === pickerCategory)
                                    .map(w => {
                                        const isActive = activeWidgets.includes(w.id);
                                        return (
                                            <button
                                                key={w.id}
                                                onClick={() => isActive ? removeWidget(w.id) : addWidget(w.id)}
                                                className={`p-4 rounded-xl border-2 text-left transition-all ${isActive ? 'border-indigo-500 bg-indigo-50' : 'border-slate-200 hover:border-slate-300'}`}
                                            >
                                                <div className="flex items-center gap-3">
                                                    <div className={`w-10 h-10 rounded-lg flex items-center justify-center ${isActive ? 'bg-indigo-600 text-white' : 'bg-slate-100 text-slate-500'}`}>
                                                        <w.icon size={20} />
                                                    </div>
                                                    <div className="flex-1">
                                                        <p className="font-bold text-sm text-slate-900">{w.title}</p>
                                                        <p className="text-[10px] text-slate-400">{w.description}</p>
                                                    </div>
                                                    {isActive ? (
                                                        <CheckCircle2 size={20} className="text-indigo-600" />
                                                    ) : (
                                                        <Plus size={20} className="text-slate-300" />
                                                    )}
                                                </div>
                                                <div className="flex items-center gap-2 mt-2">
                                                    <span className="text-[9px] font-bold text-slate-400 bg-slate-100 px-2 py-0.5 rounded">{CATEGORY_LABELS[w.category]}</span>
                                                    <span className="text-[9px] font-bold text-slate-400 bg-slate-100 px-2 py-0.5 rounded">{w.size === 'sm' ? 'Petit' : w.size === 'md' ? 'Moyen' : 'Large'}</span>
                                                </div>
                                            </button>
                                        );
                                    })}
                            </div>
                        </div>

                        <div className="p-4 border-t border-slate-100 bg-slate-50 flex justify-between items-center">
                            <p className="text-xs text-slate-400 font-bold">{activeWidgets.length} widgets sélectionnés</p>
                            <button onClick={() => setShowPicker(false)} className="px-6 py-2 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700">Terminé</button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
