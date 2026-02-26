'use client';

import React, { useState, useEffect, useCallback } from 'react';
import {
    TrendingUp, TrendingDown, Wallet, FileText, ArrowRightLeft,
    RefreshCw, DollarSign, RotateCcw, BarChart3, CheckCircle2,
    Clock, AlertTriangle, ChevronRight, XCircle
} from 'lucide-react';
import { FinanceStore } from '../../../services/FinanceStore';
import Link from 'next/link';

export default function FinanceOverview() {
    const [summary, setSummary] = useState<any>(null);
    const [breakdown, setBreakdown] = useState<any>(null);
    const [invoices, setInvoices] = useState<any[]>([]);
    const [transactions, setTransactions] = useState<any[]>([]);
    const [creditNotes, setCreditNotes] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);

    const loadData = useCallback(async () => {
        setLoading(true);
        try {
            const [s, b, inv, tx, cn] = await Promise.all([
                FinanceStore.getFinancialSummary(),
                FinanceStore.getRevenueBreakdown(),
                FinanceStore.getInvoices(),
                FinanceStore.getTransactions(),
                FinanceStore.getCreditNotes(),
            ]);
            if (s) setSummary(s);
            if (b) setBreakdown(b);
            setInvoices(inv);
            setTransactions(tx);
            setCreditNotes(cn);
        } catch (err) {
            console.error('[Finance] Erreur chargement:', err);
        }
        setLoading(false);
    }, []);

    useEffect(() => { loadData(); }, [loadData]);

    const fmt = (v: number) => (v / 100).toLocaleString('fr-FR', { minimumFractionDigits: 0 });

    if (loading || !summary) return (
        <div className="flex items-center justify-center h-[60vh]">
            <RefreshCw size={32} className="animate-spin text-indigo-500" />
        </div>
    );

    const marginPct = summary.totalGMV > 0 ? Math.round((summary.netRevenue / summary.totalGMV) * 100) : 0;
    const momGrowth = summary.momGrowth || 0;
    const totalRefunds = creditNotes.reduce((s: number, cn: any) => s + (cn.amount || 0), 0);

    // Shortcut modules
    const modules = [
        {
            title: 'Factures Clients',
            desc: 'Émettre et suivre les factures',
            icon: FileText,
            href: '/admin/finances/invoices',
            count: invoices.length,
            color: 'bg-indigo-600',
            lightColor: 'bg-indigo-50 text-indigo-600 border-indigo-100'
        },
        {
            title: 'Transactions',
            desc: 'Paiements et remboursements',
            icon: ArrowRightLeft,
            href: '/admin/finances/transactions',
            count: transactions.length,
            color: 'bg-emerald-600',
            lightColor: 'bg-emerald-50 text-emerald-600 border-emerald-100'
        },
        {
            title: 'Avoirs',
            desc: 'Notes de crédit émises',
            icon: RotateCcw,
            href: '/admin/finances/credit-notes',
            count: creditNotes.length,
            color: 'bg-rose-600',
            lightColor: 'bg-rose-50 text-rose-600 border-rose-100'
        },
    ];

    // Last 5 transactions
    const recentTx = transactions.slice(0, 5);
    // Last 5 invoices
    const recentInv = invoices.slice(0, 5);

    return (
        <div className="p-8 max-w-[1400px] mx-auto">
            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tight">Tableau de bord Facturation</h1>
                    <p className="text-slate-500 text-sm">Vue d'ensemble de la facturation, des encaissements et des avoirs</p>
                </div>
                <button onClick={loadData} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition">
                    <RefreshCw size={16} /> Actualiser
                </button>
            </div>

            {/* ═══ KPIs ═══ */}
            <div className="grid grid-cols-5 gap-4 mb-6">
                {/* GMV */}
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-slate-900" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Chiffre d'Affaires Total</p>
                    <p className="text-2xl font-black text-slate-900">{fmt(summary.totalGMV)} €</p>
                    <p className="text-xs text-slate-500 mt-0.5">{summary.totalDeals || invoices.length} dossiers</p>
                    <TrendingUp size={28} className="absolute top-4 right-4 text-slate-200" />
                </div>

                {/* This month + MoM */}
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className={`absolute top-0 left-0 w-1 h-full ${momGrowth >= 0 ? 'bg-emerald-500' : 'bg-rose-500'}`} />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Ce Mois</p>
                    <p className="text-2xl font-black text-slate-900">{fmt(summary.monthGMV || 0)} €</p>
                    <p className={`text-xs font-bold mt-0.5 flex items-center gap-1 ${momGrowth >= 0 ? 'text-emerald-600' : 'text-rose-600'}`}>
                        {momGrowth >= 0 ? <TrendingUp size={12} /> : <TrendingDown size={12} />}
                        {momGrowth >= 0 ? '+' : ''}{momGrowth}% vs mois préc.
                    </p>
                </div>

                {/* Margin */}
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-indigo-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Marge Nette</p>
                    <p className="text-2xl font-black text-indigo-600">{fmt(summary.netRevenue)} €</p>
                    <div className="flex items-center gap-2 mt-1">
                        <div className="flex-1 bg-slate-100 rounded-full h-1.5">
                            <div className="h-1.5 rounded-full bg-indigo-500" style={{ width: `${Math.min(100, marginPct)}%` }} />
                        </div>
                        <span className="text-[10px] font-black text-indigo-600">{marginPct}%</span>
                    </div>
                </div>

                {/* Encaissé en attente */}
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-amber-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Commissions en attente</p>
                    <p className="text-2xl font-black text-amber-600">{fmt(summary.totalCommissionsPending || 0)} €</p>
                    <p className="text-xs text-slate-500 mt-0.5">À reverser aux agences</p>
                    <Clock size={28} className="absolute top-4 right-4 text-amber-200" />
                </div>

                {/* Avoirs total */}
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-rose-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Total Avoirs</p>
                    <p className="text-2xl font-black text-rose-600">-{fmt(totalRefunds)} €</p>
                    <p className="text-xs text-slate-500 mt-0.5">{creditNotes.length} avoir{creditNotes.length > 1 ? 's' : ''} émis</p>
                    <RotateCcw size={28} className="absolute top-4 right-4 text-rose-200" />
                </div>
            </div>

            {/* ═══ MODULE SHORTCUTS ═══ */}
            <div className="grid grid-cols-3 gap-4 mb-6">
                {modules.map(m => (
                    <Link key={m.title} href={m.href} className="group bg-white border border-slate-200 rounded-2xl p-5 shadow-sm hover:shadow-md hover:border-slate-300 transition-all">
                        <div className="flex items-center gap-4">
                            <div className={`w-12 h-12 ${m.color} text-white rounded-xl flex items-center justify-center shadow-lg group-hover:scale-110 transition-transform`}>
                                <m.icon size={22} />
                            </div>
                            <div className="flex-1">
                                <p className="font-black text-slate-900 text-sm">{m.title}</p>
                                <p className="text-[10px] text-slate-400">{m.desc}</p>
                            </div>
                            <div className="flex items-center gap-2">
                                <span className={`px-2.5 py-1 rounded-full text-[10px] font-black border ${m.lightColor}`}>{m.count}</span>
                                <ChevronRight size={16} className="text-slate-300 group-hover:text-slate-500 group-hover:translate-x-1 transition-all" />
                            </div>
                        </div>
                    </Link>
                ))}
            </div>

            {/* ═══ MAIN CONTENT ═══ */}
            <div className="grid grid-cols-12 gap-6">
                {/* Revenue chart - 12 months */}
                {breakdown?.monthly && (
                    <div className="col-span-8 bg-white border border-slate-200 rounded-2xl p-6 shadow-sm">
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider mb-4">CA Mensuel (12 derniers mois)</h3>
                        <div className="flex items-end gap-1.5 h-44">
                            {breakdown.monthly.map((m: any, i: number) => {
                                const maxGMV = Math.max(...breakdown.monthly.map((x: any) => x.gmv), 1);
                                const h = Math.max((m.gmv / maxGMV) * 100, 3);
                                return (
                                    <div key={i} className="flex-1 flex flex-col items-center group relative">
                                        <div className="absolute -top-10 hidden group-hover:block bg-slate-800 text-white text-[9px] py-1.5 px-3 rounded-lg font-bold whitespace-nowrap z-10 shadow-lg">
                                            <div>CA: {fmt(m.gmv)}€</div>
                                            <div className="text-slate-400">Net: {fmt(m.net)}€ · {m.count} deals</div>
                                        </div>
                                        <div className="w-full rounded-t-md bg-gradient-to-t from-indigo-600 to-indigo-400 group-hover:from-indigo-500 group-hover:to-indigo-300 transition-colors"
                                            style={{ height: `${h}%`, minHeight: '6px' }} />
                                        <p className="text-[8px] text-slate-400 mt-1.5 font-bold">{m.month}</p>
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                )}

                {/* Quick stats sidebar */}
                <div className="col-span-4 space-y-4">
                    {/* Avg deal */}
                    <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                        <p className="text-[10px] font-bold text-slate-400 uppercase mb-1">Valeur Moyenne Dossier</p>
                        <p className="text-2xl font-black text-slate-900">{fmt(summary.avgDealValue || 0)} €</p>
                    </div>

                    {/* Top agency */}
                    {summary.topAgency && (
                        <div className="bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                            <p className="text-[10px] font-bold text-slate-400 uppercase mb-1">Top Agence (ce mois)</p>
                            <p className="text-lg font-black text-slate-900">{summary.topAgency.name}</p>
                            <p className="text-xs text-emerald-600 font-bold">{fmt(summary.topAgency.revenue)} € de CA</p>
                        </div>
                    )}

                    {/* Ratio */}
                    <div className="bg-gradient-to-br from-slate-900 to-slate-800 rounded-2xl p-5 text-white">
                        <p className="text-[10px] font-bold text-slate-400 uppercase mb-2">Ratio Facturation</p>
                        <div className="grid grid-cols-2 gap-3 text-center">
                            <div>
                                <p className="text-xl font-black">{invoices.length}</p>
                                <p className="text-[9px] text-slate-400 uppercase">Factures</p>
                            </div>
                            <div>
                                <p className="text-xl font-black">{transactions.length}</p>
                                <p className="text-[9px] text-slate-400 uppercase">Transactions</p>
                            </div>
                            <div>
                                <p className="text-xl font-black">{creditNotes.length}</p>
                                <p className="text-[9px] text-slate-400 uppercase">Avoirs</p>
                            </div>
                            <div>
                                <p className="text-xl font-black text-emerald-400">
                                    {invoices.length > 0 ? Math.round(((invoices.length - creditNotes.length) / invoices.length) * 100) : 100}%
                                </p>
                                <p className="text-[9px] text-slate-400 uppercase">Taux sain</p>
                            </div>
                        </div>
                    </div>
                </div>

                {/* Recent invoices */}
                <div className="col-span-6 bg-white border border-slate-200 rounded-2xl shadow-sm overflow-hidden">
                    <div className="p-5 border-b border-slate-100 flex justify-between items-center">
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider">Dernières Factures</h3>
                        <Link href="/admin/finances/invoices" className="text-[10px] font-bold text-indigo-600 hover:underline">Voir toutes →</Link>
                    </div>
                    <div className="divide-y divide-slate-50">
                        {recentInv.length === 0 ? (
                            <div className="p-10 text-center text-slate-400 text-sm font-bold">Aucune facture</div>
                        ) : recentInv.map(inv => (
                            <div key={inv.id} className="px-5 py-3 flex items-center justify-between hover:bg-slate-50 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 bg-indigo-100 text-indigo-600 rounded-lg flex items-center justify-center">
                                        <FileText size={14} />
                                    </div>
                                    <div>
                                        <p className="text-sm font-bold text-slate-800">{inv.name}</p>
                                        <p className="text-[10px] text-slate-400">{inv.invoiceNumber} · {inv.serviceName}</p>
                                    </div>
                                </div>
                                <div className="text-right">
                                    <p className="font-black text-slate-900 text-sm">{fmt(inv.amountPaid)} €</p>
                                    <p className="text-[9px] text-slate-400">
                                        {inv.paymentDate ? new Date(inv.paymentDate).toLocaleDateString('fr-FR') : '-'}
                                    </p>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Recent transactions */}
                <div className="col-span-6 bg-white border border-slate-200 rounded-2xl shadow-sm overflow-hidden">
                    <div className="p-5 border-b border-slate-100 flex justify-between items-center">
                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-wider">Dernières Transactions</h3>
                        <Link href="/admin/finances/transactions" className="text-[10px] font-bold text-indigo-600 hover:underline">Voir toutes →</Link>
                    </div>
                    <div className="divide-y divide-slate-50">
                        {recentTx.length === 0 ? (
                            <div className="p-10 text-center text-slate-400 text-sm font-bold">Aucune transaction</div>
                        ) : recentTx.map(tx => (
                            <div key={tx.id} className="px-5 py-3 flex items-center justify-between hover:bg-slate-50 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className={`w-8 h-8 rounded-lg flex items-center justify-center ${tx.type === 'PAYMENT' ? 'bg-emerald-100 text-emerald-600' : 'bg-rose-100 text-rose-600'}`}>
                                        {tx.type === 'PAYMENT' ? <DollarSign size={14} /> : <RotateCcw size={14} />}
                                    </div>
                                    <div>
                                        <p className="text-sm font-bold text-slate-800">{tx.type === 'PAYMENT' ? 'Paiement' : 'Remboursement'}</p>
                                        <p className="text-[10px] text-slate-400">{tx.method || '-'} · {tx.reference?.substring(0, 16) || '-'}</p>
                                    </div>
                                </div>
                                <div className="text-right">
                                    <p className={`font-black text-sm ${tx.type === 'REFUND' ? 'text-rose-600' : 'text-slate-900'}`}>
                                        {tx.type === 'REFUND' ? '-' : '+'}{fmt(tx.amount)} €
                                    </p>
                                    <p className="text-[9px] text-slate-400">{new Date(tx.createdAt).toLocaleDateString('fr-FR')}</p>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
