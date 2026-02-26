'use client';

import React, { useState, useEffect, useCallback } from 'react';
import {
    TrendingUp, TrendingDown, Wallet, Clock, CheckCircle2, FileText, ArrowRightLeft,
    Building2, Download, DollarSign, X, Search, BarChart3, PieChart, Target,
    AlertTriangle, ChevronUp, ChevronDown, RefreshCw, XCircle, AlertCircle
} from 'lucide-react';
import { FinanceStore, Payout } from '../../../services/FinanceStore';
import { AgencyStore } from '../../../services/AgencyStore';
import { AuthStore } from '../../../services/authStore';

type Tab = 'dashboard' | 'invoices' | 'transactions' | 'credit-notes' | 'payouts' | 'breakdown' | 'projection';

export default function FinancePage() {
    const [tab, setTab] = useState<Tab>('dashboard');
    const [summary, setSummary] = useState<any>(null);
    const [breakdown, setBreakdown] = useState<any>(null);
    const [projection, setProjection] = useState<any>(null);
    const [payouts, setPayouts] = useState<Payout[]>([]);
    const [invoices, setInvoices] = useState<any[]>([]);
    const [transactions, setTransactions] = useState<any[]>([]);
    const [creditNotes, setCreditNotes] = useState<any[]>([]);
    const [debts, setDebts] = useState<{ id: string; name: string; balance: number; rate: number }[]>([]);
    const [loading, setLoading] = useState(true);
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'warning' } | null>(null);
    const [payoutModal, setPayoutModal] = useState<{ id: string; name: string; balance: number } | null>(null);

    const showToast = useCallback((msg: string, type: 'success' | 'error' | 'warning' = 'success') => {
        setToast({ message: msg, type }); setTimeout(() => setToast(null), 4000);
    }, []);

    const loadData = async () => {
        setLoading(true);
        try {
            const [s, b, p, pays, inv, tx, cn] = await Promise.all([
                FinanceStore.getFinancialSummary(),
                FinanceStore.getRevenueBreakdown(),
                FinanceStore.getCashFlowProjection(),
                FinanceStore.getAllPayouts(),
                FinanceStore.getInvoices(),
                FinanceStore.getTransactions(),
                FinanceStore.getCreditNotes(),
            ]);
            if (s) setSummary(s);
            if (b) setBreakdown(b);
            if (p) setProjection(p);
            setPayouts(pays.sort((a: any, b: any) => b.createdAt.localeCompare(a.createdAt)));
            setInvoices(inv);
            setTransactions(tx);
            setCreditNotes(cn);

            // Load debts
            const agencies = await AgencyStore.getAllAgencies();
            const ds = await Promise.all(agencies.map(async a => ({
                id: a.id, name: a.name,
                balance: await FinanceStore.getAgencyBalance(a.id),
                rate: a.commissionRate
            })));
            setDebts(ds.filter(d => d.balance > 0));
        } catch (err) {
            console.error('[Finance] load error:', err);
        }
        setLoading(false);
    };

    useEffect(() => { loadData(); }, []);

    const handlePayout = async () => {
        if (!payoutModal) return;
        await FinanceStore.createPayout(payoutModal.id, payoutModal.balance, new Date().toISOString().substring(0, 7));
        showToast(`Virement de ${payoutModal.balance.toLocaleString()}€ vers ${payoutModal.name}`);
        setPayoutModal(null);
        loadData();
    };

    const handleDownloadSepa = async (payoutId: string, ref: string) => {
        const blob = await FinanceStore.downloadSepaXml(payoutId);
        if (blob) {
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url; a.download = `SEPA-${ref}.xml`; a.click();
            showToast('Fichier SEPA téléchargé');
        } else {
            showToast('Erreur téléchargement SEPA', 'error');
        }
    };

    const fmt = (v: number) => (v / 100).toLocaleString('fr-FR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
    const fmtRaw = (v: number) => v.toLocaleString('fr-FR');

    const tabs: { id: Tab; label: string; icon: any }[] = [
        { id: 'dashboard', label: 'Tableau de bord', icon: BarChart3 },
        { id: 'breakdown', label: 'Ventilation CA', icon: PieChart },
        { id: 'projection', label: 'Projections', icon: Target },
        { id: 'invoices', label: 'Factures', icon: FileText },
        { id: 'transactions', label: 'Transactions', icon: ArrowRightLeft },
        { id: 'credit-notes', label: 'Avoirs', icon: ArrowRightLeft },
        { id: 'payouts', label: 'Reversements', icon: Building2 },
    ];

    if (loading || !summary) return (
        <div className="flex items-center justify-center h-[60vh]">
            <RefreshCw size={32} className="animate-spin text-indigo-500" />
        </div>
    );

    return (
        <div className="p-8 max-w-[1400px] mx-auto">
            {/* Toast */}
            {toast && (
                <div className={`fixed top-6 right-6 z-50 px-5 py-3 rounded-xl shadow-xl font-bold text-sm flex items-center gap-2 ${toast.type === 'success' ? 'bg-emerald-500 text-white' : toast.type === 'error' ? 'bg-rose-500 text-white' : 'bg-amber-500 text-white'}`}>
                    {toast.type === 'success' && <CheckCircle2 size={16} />}
                    {toast.type === 'error' && <XCircle size={16} />}
                    {toast.type === 'warning' && <AlertCircle size={16} />}
                    {toast.message}
                </div>
            )}

            {/* Header */}
            <div className="flex justify-between items-center mb-8">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tight">Finances & Trésorerie</h1>
                    <p className="text-slate-500 text-sm">Pilotage financier du réseau SimuLegal</p>
                </div>
                <button onClick={loadData} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200">
                    <RefreshCw size={16} /> Actualiser
                </button>
            </div>

            {/* Tabs */}
            <div className="flex gap-2 mb-8 overflow-x-auto pb-2">
                {tabs.map(t => (
                    <button key={t.id} onClick={() => setTab(t.id)}
                        className={`flex items-center gap-2 px-5 py-2.5 rounded-xl text-xs font-black uppercase tracking-wider whitespace-nowrap transition-all ${tab === t.id ? 'bg-slate-900 text-white shadow-lg' : 'bg-white text-slate-400 hover:text-slate-600 border border-slate-200'}`}>
                        <t.icon size={14} />{t.label}
                    </button>
                ))}
            </div>

            {/* ═══ DASHBOARD ═══ */}
            {tab === 'dashboard' && (
                <>
                    {/* KPI Cards */}
                    <div className="grid grid-cols-5 gap-4 mb-8">
                        {[
                            { label: 'Volume Total (GMV)', value: `${fmt(summary.totalGMV)} €`, sub: `${summary.totalDeals} dossiers`, icon: TrendingUp, color: 'emerald', accent: 'bg-emerald-500' },
                            { label: 'Ce Mois', value: `${fmt(summary.thisMonthGMV)} €`, sub: `${summary.momGrowth >= 0 ? '+' : ''}${summary.momGrowth}% MoM`, icon: summary.momGrowth >= 0 ? ChevronUp : ChevronDown, color: summary.momGrowth >= 0 ? 'emerald' : 'rose', accent: summary.momGrowth >= 0 ? 'bg-emerald-500' : 'bg-rose-500' },
                            { label: 'Marge Nette', value: `${fmt(summary.netRevenue)} €`, sub: `${summary.marginPct}% de marge`, icon: Wallet, color: 'indigo', accent: 'bg-indigo-500' },
                            { label: 'Dette Partenaire', value: `${fmtRaw(summary.totalCommissionsPending)} €`, sub: `${debts.length} créanciers`, icon: Clock, color: 'amber', accent: 'bg-amber-500' },
                            { label: 'Déjà Versé', value: `${fmtRaw(summary.totalCommissionsPaid)} €`, sub: `${payouts.length} virements`, icon: CheckCircle2, color: 'blue', accent: 'bg-blue-500' },
                        ].map(kpi => (
                            <div key={kpi.label} className={`relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm hover:shadow-md transition-all`}>
                                <div className={`absolute top-0 left-0 w-1 h-full ${kpi.accent}`} />
                                <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">{kpi.label}</p>
                                <p className={`text-xl font-black text-slate-900`}>{kpi.value}</p>
                                <p className={`text-xs font-medium text-${kpi.color}-600 mt-0.5`}>{kpi.sub}</p>
                                <kpi.icon size={28} className={`absolute top-4 right-4 text-${kpi.color}-200`} />
                            </div>
                        ))}
                    </div>

                    {/* Margin + Top Agency */}
                    <div className="grid grid-cols-3 gap-6 mb-8">
                        {/* Margin Gauge */}
                        <div className="bg-gradient-to-br from-indigo-600 to-purple-700 rounded-2xl p-6 text-white shadow-lg col-span-1">
                            <p className="text-indigo-200 text-xs font-bold uppercase tracking-wider mb-2">Taux de Marge</p>
                            <p className="text-5xl font-black mb-3">{summary.marginPct}%</p>
                            <div className="w-full bg-white/20 rounded-full h-2.5">
                                <div className="h-2.5 rounded-full bg-white" style={{ width: `${summary.marginPct}%` }} />
                            </div>
                            <p className="text-indigo-200 text-xs mt-2">GMV {fmt(summary.totalGMV)}€ → Net {fmt(summary.netRevenue)}€</p>
                        </div>

                        {/* Revenue bar chart (last 12 months) */}
                        {breakdown && (
                            <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm col-span-2">
                                <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">CA Mensuel (12 mois)</h3>
                                <div className="flex items-end gap-1.5 h-32">
                                    {breakdown.monthly.map((m: any, i: number) => {
                                        const maxGMV = Math.max(...breakdown.monthly.map((x: any) => x.gmv), 1);
                                        const h = (m.gmv / maxGMV) * 100;
                                        return (
                                            <div key={i} className="flex-1 flex flex-col items-center group relative">
                                                <div className="absolute -top-7 hidden group-hover:block bg-slate-800 text-white text-[9px] py-1 px-2 rounded-lg font-bold whitespace-nowrap z-10">{fmt(m.gmv)}€ · {m.count} deals</div>
                                                <div className="w-full rounded-t-md bg-indigo-500 hover:bg-indigo-600 transition-all" style={{ height: `${Math.max(h, 2)}%` }} />
                                                <p className="text-[8px] text-slate-400 mt-1 font-bold">{m.month}</p>
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>
                        )}
                    </div>

                    {/* Debts + Recent */}
                    <div className="grid grid-cols-3 gap-6">
                        <div className="col-span-2 bg-white border border-slate-200 rounded-2xl shadow-sm overflow-hidden">
                            <div className="p-5 border-b border-slate-100 flex justify-between items-center">
                                <div>
                                    <h3 className="font-black text-slate-900 text-sm">Dette Partenaire</h3>
                                    <p className="text-[10px] text-slate-400 font-bold uppercase">Soldes à régler</p>
                                </div>
                                <span className="bg-amber-100 text-amber-700 text-[10px] px-2.5 py-1 rounded-full font-black">{debts.length} créanciers</span>
                            </div>
                            {debts.length === 0 ? (
                                <div className="p-12 text-center"><CheckCircle2 size={32} className="mx-auto mb-3 text-slate-200" /><p className="text-slate-400 text-sm font-bold">Tous les soldes sont à jour ✓</p></div>
                            ) : (
                                <div className="divide-y divide-slate-50 max-h-[280px] overflow-y-auto">
                                    {debts.map(d => (
                                        <div key={d.id} className="px-5 py-3 flex items-center justify-between hover:bg-slate-50 transition-colors">
                                            <div className="flex items-center gap-3">
                                                <div className="w-9 h-9 bg-slate-900 text-white rounded-lg flex items-center justify-center text-xs font-black">{d.name.charAt(0)}</div>
                                                <div><p className="font-bold text-slate-800 text-sm">{d.name}</p><p className="text-[10px] text-slate-400">Com. {d.rate}%</p></div>
                                            </div>
                                            <div className="flex items-center gap-4">
                                                <p className="font-black text-slate-900">{d.balance.toLocaleString()} €</p>
                                                <button onClick={() => setPayoutModal(d)} className="px-3 py-1.5 bg-indigo-600 text-white text-xs font-bold rounded-lg hover:bg-indigo-700 transition-colors">Régler</button>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            )}
                        </div>

                        {/* Quick stats sidebar */}
                        <div className="space-y-4">
                            <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                                <p className="text-[10px] font-bold text-slate-400 uppercase mb-2">Valeur Moyenne Dossier</p>
                                <p className="text-2xl font-black text-slate-900">{fmt(summary.avgDealValue)} €</p>
                            </div>
                            <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                                <p className="text-[10px] font-bold text-slate-400 uppercase mb-2">Remboursements Total</p>
                                <p className="text-2xl font-black text-rose-600">{fmt(summary.totalRefunds)} €</p>
                            </div>
                            {summary.topAgency && (
                                <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                                    <p className="text-[10px] font-bold text-slate-400 uppercase mb-2">Top Agence ce mois</p>
                                    <p className="text-sm font-black text-indigo-600">{summary.topAgency.name}</p>
                                    <p className="text-xs text-slate-500">{fmt(summary.topAgency.gmv)} € de CA</p>
                                </div>
                            )}
                        </div>
                    </div>
                </>
            )}

            {/* ═══ BREAKDOWN ═══ */}
            {tab === 'breakdown' && breakdown && (
                <div className="space-y-6">
                    <div className="grid grid-cols-3 gap-6">
                        {/* By Type */}
                        <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                            <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">Par Type d'Agence</h3>
                            <div className="space-y-3">
                                {Object.entries(breakdown.byType).sort((a: any, b: any) => b[1] - a[1]).map(([type, amount]: any) => {
                                    const total = Object.values(breakdown.byType).reduce((s: any, v: any) => s + v, 0) as number;
                                    const pct = total > 0 ? Math.round((amount / total) * 100) : 0;
                                    return (
                                        <div key={type}>
                                            <div className="flex justify-between text-xs mb-1"><span className="font-bold text-slate-600">{type}</span><span className="font-black text-slate-900">{fmt(amount)} € ({pct}%)</span></div>
                                            <div className="w-full bg-slate-100 rounded-full h-2"><div className="h-2 rounded-full bg-indigo-500" style={{ width: `${pct}%` }} /></div>
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* By Service */}
                        <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                            <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">Par Service</h3>
                            <div className="space-y-3">
                                {Object.entries(breakdown.byService).sort((a: any, b: any) => b[1] - a[1]).slice(0, 6).map(([svc, amount]: any) => {
                                    const total = Object.values(breakdown.byService).reduce((s: any, v: any) => s + v, 0) as number;
                                    const pct = total > 0 ? Math.round((amount / total) * 100) : 0;
                                    return (
                                        <div key={svc}>
                                            <div className="flex justify-between text-xs mb-1"><span className="font-bold text-slate-600 truncate max-w-[120px]">{svc}</span><span className="font-black text-slate-900">{fmt(amount)} € ({pct}%)</span></div>
                                            <div className="w-full bg-slate-100 rounded-full h-2"><div className="h-2 rounded-full bg-emerald-500" style={{ width: `${pct}%` }} /></div>
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* By Region */}
                        <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                            <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">Par Région</h3>
                            <div className="space-y-3">
                                {Object.entries(breakdown.byRegion).sort((a: any, b: any) => b[1] - a[1]).slice(0, 8).map(([region, amount]: any) => {
                                    const total = Object.values(breakdown.byRegion).reduce((s: any, v: any) => s + v, 0) as number;
                                    const pct = total > 0 ? Math.round((amount / total) * 100) : 0;
                                    return (
                                        <div key={region}>
                                            <div className="flex justify-between text-xs mb-1"><span className="font-bold text-slate-600">{region}</span><span className="font-black text-slate-900">{fmt(amount)} € ({pct}%)</span></div>
                                            <div className="w-full bg-slate-100 rounded-full h-2"><div className="h-2 rounded-full bg-amber-500" style={{ width: `${pct}%` }} /></div>
                                        </div>
                                    );
                                })}
                            </div>
                        </div>
                    </div>

                    {/* Monthly Net Revenue chart */}
                    <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">Marge Nette Mensuelle (12 mois)</h3>
                        <div className="flex items-end gap-2 h-40">
                            {breakdown.monthly.map((m: any, i: number) => {
                                const maxNet = Math.max(...breakdown.monthly.map((x: any) => x.net), 1);
                                const h = Math.max((m.net / maxNet) * 100, 2);
                                return (
                                    <div key={i} className="flex-1 flex flex-col items-center group relative">
                                        <div className="absolute -top-8 hidden group-hover:block bg-slate-800 text-white text-[9px] py-1 px-2 rounded-lg font-bold whitespace-nowrap z-10">Net: {fmtRaw(m.net)}€ · {m.count} deals</div>
                                        <div className="w-full flex flex-col gap-0.5">
                                            <div className="w-full rounded-t-sm bg-emerald-400" style={{ height: `${h}%`, minHeight: '4px' }} />
                                        </div>
                                        <p className="text-[8px] text-slate-400 mt-1 font-bold">{m.month}</p>
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                </div>
            )}

            {/* ═══ PROJECTION ═══ */}
            {tab === 'projection' && projection && (
                <div className="space-y-6">
                    <div className="grid grid-cols-3 gap-4">
                        <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                            <p className="text-[10px] font-bold text-slate-400 uppercase mb-1">GMV Mensuelle Moyenne</p>
                            <p className="text-2xl font-black text-slate-900">{fmt(projection.avgMonthlyGMV)} €</p>
                        </div>
                        <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                            <p className="text-[10px] font-bold text-slate-400 uppercase mb-1">Marge Nette Moyenne</p>
                            <p className="text-2xl font-black text-emerald-600">{fmt(projection.avgMonthlyNet)} €</p>
                        </div>
                        <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                            <p className="text-[10px] font-bold text-slate-400 uppercase mb-1">Dette en Cours</p>
                            <p className="text-2xl font-black text-amber-600">{fmtRaw(projection.outstandingDebt)} €</p>
                        </div>
                    </div>

                    <div className="bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">Projections 3 Mois (basées sur la moyenne)</h3>
                        <div className="space-y-4">
                            {projection.projection.map((p: any, i: number) => (
                                <div key={i} className="flex items-center gap-4 p-4 bg-slate-50 rounded-xl">
                                    <div className="w-36"><p className="font-bold text-slate-800 text-sm">{p.month}</p></div>
                                    <div className="flex-1 grid grid-cols-3 gap-4">
                                        <div><p className="text-[9px] text-slate-400 font-bold uppercase">GMV Est.</p><p className="text-sm font-black text-slate-800">{fmt(p.estGMV)} €</p></div>
                                        <div><p className="text-[9px] text-slate-400 font-bold uppercase">Commissions Est.</p><p className="text-sm font-black text-amber-600">{fmtRaw(p.estCommission)} €</p></div>
                                        <div><p className="text-[9px] text-slate-400 font-bold uppercase">Marge Est.</p><p className="text-sm font-black text-emerald-600">{fmtRaw(p.estNet)} €</p></div>
                                    </div>
                                </div>
                            ))}
                        </div>
                        <p className="text-[10px] text-slate-400 mt-4 italic">* Hypothèse : croissance de 2% par mois sur la base de la moyenne des 3 derniers mois</p>
                    </div>
                </div>
            )}

            {/* ═══ INVOICES ═══ */}
            {tab === 'invoices' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">N° Facture</th><th className="p-4">Date</th><th className="p-4">Client</th><th className="p-4">Service</th><th className="p-4">Agence</th><th className="p-4 text-right">Montant</th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {invoices.length === 0 ? <tr><td colSpan={6} className="p-16 text-center text-slate-400 font-bold">Aucune facture</td></tr> :
                                invoices.map(inv => (
                                    <tr key={inv.id} className="hover:bg-slate-50 transition-colors">
                                        <td className="p-4 font-black text-indigo-600 text-sm">{inv.invoiceNumber}</td>
                                        <td className="p-4 text-sm text-slate-500">{inv.paymentDate ? new Date(inv.paymentDate).toLocaleDateString('fr-FR') : '-'}</td>
                                        <td className="p-4 font-bold text-slate-700 text-sm">{inv.name}</td>
                                        <td className="p-4 text-xs font-bold text-slate-500">{inv.serviceName}</td>
                                        <td className="p-4 text-xs text-slate-400">{inv.originAgency?.name || 'Direct'}</td>
                                        <td className="p-4 text-right font-black text-slate-900">{fmt(inv.amountPaid)} €</td>
                                    </tr>
                                ))}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ TRANSACTIONS ═══ */}
            {tab === 'transactions' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">ID</th><th className="p-4">Date</th><th className="p-4">Type</th><th className="p-4">Méthode</th><th className="p-4">Référence</th><th className="p-4 text-right">Montant</th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {transactions.length === 0 ? <tr><td colSpan={6} className="p-16 text-center text-slate-400 font-bold">Aucune transaction</td></tr> :
                                transactions.map(tx => (
                                    <tr key={tx.id} className="hover:bg-slate-50 transition-colors">
                                        <td className="p-4 font-mono text-xs text-slate-400">{tx.id.slice(0, 12)}…</td>
                                        <td className="p-4 text-sm text-slate-500">{new Date(tx.createdAt).toLocaleDateString('fr-FR')}</td>
                                        <td className="p-4"><span className={`px-2.5 py-1 rounded-full text-[10px] font-black ${tx.type === 'PAYMENT' ? 'bg-emerald-100 text-emerald-700' : 'bg-rose-100 text-rose-700'}`}>{tx.type}</span></td>
                                        <td className="p-4 text-xs text-slate-500">{tx.method || '-'}</td>
                                        <td className="p-4 text-xs text-slate-400">{tx.reference || '-'}</td>
                                        <td className={`p-4 text-right font-black ${tx.type === 'REFUND' ? 'text-rose-600' : 'text-slate-900'}`}>{tx.type === 'REFUND' ? '-' : ''}{fmt(tx.amount)} €</td>
                                    </tr>
                                ))}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ CREDIT NOTES ═══ */}
            {tab === 'credit-notes' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">N° Avoir</th><th className="p-4">Date</th><th className="p-4">Client</th><th className="p-4">Raison</th><th className="p-4 text-right">Montant</th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {creditNotes.length === 0 ? <tr><td colSpan={5} className="p-16 text-center text-slate-400 font-bold">Aucun avoir émis</td></tr> :
                                creditNotes.map(cn => (
                                    <tr key={cn.id} className="hover:bg-slate-50 transition-colors">
                                        <td className="p-4 font-black text-slate-900 text-sm">{cn.number}</td>
                                        <td className="p-4 text-sm text-slate-500">{new Date(cn.createdAt).toLocaleDateString('fr-FR')}</td>
                                        <td className="p-4 font-bold text-slate-700">{cn.lead?.name}</td>
                                        <td className="p-4 text-sm text-slate-500">{cn.reason}</td>
                                        <td className="p-4 text-right font-black text-rose-600">{fmt(cn.amount)} €</td>
                                    </tr>
                                ))}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ PAYOUTS ═══ */}
            {tab === 'payouts' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">Date</th><th className="p-4">Agence</th><th className="p-4">Période</th><th className="p-4">Référence</th><th className="p-4 text-right">Montant</th><th className="p-4">Statut</th><th className="p-4"></th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {payouts.map(p => (
                                <tr key={p.id} className="hover:bg-slate-50 transition-colors">
                                    <td className="p-4 text-sm text-slate-500">{new Date(p.createdAt).toLocaleDateString('fr-FR')}</td>
                                    <td className="p-4 font-bold text-slate-800">{(p as any).agency?.name || p.agencyId}</td>
                                    <td className="p-4 text-sm text-slate-500">{p.period}</td>
                                    <td className="p-4 font-mono text-xs text-slate-400">{p.reference}</td>
                                    <td className="p-4 text-right font-black text-indigo-600">{p.amount.toLocaleString()} €</td>
                                    <td className="p-4"><span className="bg-emerald-100 text-emerald-700 px-2.5 py-1 rounded-full text-[10px] font-black uppercase">{p.status}</span></td>
                                    <td className="p-4">
                                        <button onClick={() => handleDownloadSepa(p.id, p.reference || p.id)} className="p-1.5 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg" title="SEPA XML"><Download size={14} /></button>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ PAYOUT MODAL ═══ */}
            {payoutModal && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-md overflow-hidden">
                        <div className="bg-slate-900 p-6 text-white text-center">
                            <div className="w-16 h-16 bg-emerald-500 rounded-2xl flex items-center justify-center mx-auto mb-4 shadow-lg"><DollarSign size={32} /></div>
                            <h2 className="text-xl font-black">Règlement Solde</h2>
                            <p className="text-slate-400 text-xs font-bold uppercase mt-1">Confirmation de reversement</p>
                        </div>
                        <div className="p-8 text-center space-y-4">
                            <div><p className="text-[10px] text-slate-400 font-bold uppercase">Montant à transférer</p><p className="text-4xl font-black text-slate-900">{payoutModal.balance.toLocaleString()} €</p></div>
                            <div className="bg-slate-50 p-4 rounded-xl"><p className="text-sm text-slate-500">Destinataire</p><p className="font-black text-slate-900">{payoutModal.name}</p></div>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button onClick={() => setPayoutModal(null)} className="flex-1 py-3 bg-white border border-slate-200 text-slate-500 font-bold rounded-xl hover:bg-slate-100">Annuler</button>
                            <button onClick={handlePayout} className="flex-1 py-3 bg-emerald-600 text-white font-bold rounded-xl hover:bg-emerald-700 flex items-center justify-center gap-2"><CheckCircle2 size={18} /> Confirmer</button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
