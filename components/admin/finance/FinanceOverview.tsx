'use client';

import React, { useState, useEffect } from 'react';
import {
    Wallet,
    TrendingUp,
    Clock,
    CheckCircle2,
    Building2,
    DollarSign,
    ArrowRightLeft
} from 'lucide-react';
import { CRM, Lead } from '../../../services/crmStore';
import { AgencyStore } from '../../../services/AgencyStore';
import { FinanceStore, FinancialStats } from '../../../services/FinanceStore';
import Link from 'next/link';

export default function FinanceOverview() {
    const [stats, setStats] = useState<FinancialStats | null>(null);
    const [agenciesWithDebt, setAgenciesWithDebt] = useState<{ id: string, name: string, balance: number, rate: number }[]>([]);
    const [recentDeals, setRecentDeals] = useState<Lead[]>([]);
    const [isPayoutModalOpen, setIsPayoutModalOpen] = useState(false);
    const [selectedAgencyForPayout, setSelectedAgencyForPayout] = useState<{ id: string, name: string, balance: number } | null>(null);

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const globalStats = await FinanceStore.getGlobalStats();
            setStats(globalStats);

            const agencies = await AgencyStore.getAllAgencies();
            const debts = await Promise.all(agencies.map(async (a) => ({
                id: a.id,
                name: a.name,
                balance: await FinanceStore.getAgencyBalance(a.id),
                rate: a.commissionRate
            })));

            setAgenciesWithDebt(debts.filter(a => a.balance > 0));

            const leads = await CRM.getAllLeads();
            setRecentDeals(leads.filter(l => l.currentStage === 'CLOSED' || l.currentStage === 'DECISION_WAIT').slice(0, 10));
        } catch (error) {
            console.error('[Finance] Erreur chargement:', error);
        }
    };

    const handleSettleBalance = (agency: { id: string, name: string, balance: number }) => {
        setSelectedAgencyForPayout(agency);
        setIsPayoutModalOpen(true);
    };

    const confirmPayout = async () => {
        if (!selectedAgencyForPayout) return;
        const period = new Date().toISOString().substring(0, 7);
        await FinanceStore.createPayout(selectedAgencyForPayout.id, selectedAgencyForPayout.balance, period);
        await loadData();
        setIsPayoutModalOpen(false);
    };

    if (!stats) return <div className="p-8">Chargement...</div>;

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10">
                <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Synthèse Financière</h1>
                <p className="text-slate-500 font-medium">Pilotage de la marge et des reversements partenaires</p>
            </div>

            {/* KPIs */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-12">
                <FinanceCard
                    title="Volume Total (GMV)"
                    value={`${stats.totalGMV.toLocaleString()} €`}
                    icon={<TrendingUp className="text-emerald-600" />}
                    color="emerald"
                />
                <FinanceCard
                    title="Dette Partenaire"
                    value={`${stats.totalPartnerDebt.toLocaleString()} €`}
                    icon={<Clock className="text-amber-600" />}
                    color="amber"
                />
                <FinanceCard
                    title="Déjà Versé"
                    value={`${stats.totalCommissionsPaid.toLocaleString()} €`}
                    icon={<CheckCircle2 className="text-blue-600" />}
                    color="blue"
                />
                <FinanceCard
                    title="Marge Nette (EBITDA)"
                    value={`${stats.netRevenue.toLocaleString()} €`}
                    icon={<Wallet className="text-indigo-600" />}
                    color="indigo"
                    isMain={true}
                />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                {/* Gestion des reversements */}
                <div className="lg:col-span-2 space-y-8">
                    <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                        <div className="p-8 border-b border-slate-50 flex items-center justify-between">
                            <div>
                                <h3 className="text-xl font-black text-slate-900 tracking-tight uppercase">Dette Partenaire</h3>
                                <p className="text-slate-400 text-xs font-bold uppercase tracking-widest mt-1">Soldes à régler aux agences</p>
                            </div>
                            <span className="bg-amber-100 text-amber-700 text-[10px] font-black px-3 py-1 rounded-full uppercase">
                                {agenciesWithDebt.length} Créanciers
                            </span>
                        </div>

                        <div className="divide-y divide-slate-100">
                            {agenciesWithDebt.length === 0 ? (
                                <div className="p-20 text-center">
                                    <div className="w-16 h-16 bg-slate-50 rounded-2xl flex items-center justify-center mx-auto mb-4 text-slate-200">
                                        <CheckCircle2 size={32} />
                                    </div>
                                    <p className="text-slate-400 font-bold uppercase text-xs tracking-widest">Tous les soldes sont à jour</p>
                                </div>
                            ) : (
                                agenciesWithDebt.map(agency => (
                                    <div key={agency.id} className="p-6 flex items-center justify-between hover:bg-slate-50/50 transition-colors">
                                        <div className="flex items-center gap-4">
                                            <div className="w-12 h-12 bg-slate-900 text-white rounded-2xl flex items-center justify-center shadow-lg">
                                                <Building2 size={20} />
                                            </div>
                                            <div>
                                                <p className="font-black text-slate-900 uppercase tracking-tight">{agency.name}</p>
                                                <p className="text-[10px] text-slate-400 font-bold uppercase tracking-widest">Com. {agency.rate}%</p>
                                            </div>
                                        </div>
                                        <div className="flex items-center gap-8">
                                            <div className="text-right">
                                                <p className="text-xl font-black text-slate-900">{agency.balance.toLocaleString()} €</p>
                                                <p className="text-[10px] text-amber-600 font-black uppercase tracking-widest">Solde ouvert</p>
                                            </div>
                                            <button
                                                onClick={() => handleSettleBalance(agency)}
                                                className="bg-indigo-600 hover:bg-slate-900 text-white px-5 py-2.5 rounded-xl font-black text-xs uppercase tracking-widest transition-all shadow-lg hover:shadow-indigo-500/20"
                                            >
                                                Régler
                                            </button>
                                        </div>
                                    </div>
                                ))
                            )}
                        </div>
                    </div>
                </div>

                {/* Recent History Sidebar */}
                <div className="space-y-8">
                    <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden p-8">
                        <h4 className="text-xs font-black text-slate-900 uppercase tracking-widest mb-6 border-b border-slate-50 pb-4">Activité Récente</h4>
                        <div className="space-y-6">
                            {recentDeals.slice(0, 5).map(deal => (
                                <div key={deal.id} className="flex justify-between items-start">
                                    <div>
                                        <p className="text-xs font-black text-slate-800 truncate max-w-[120px]">{deal.name}</p>
                                        <p className="text-[9px] text-slate-400 font-bold">{new Date(deal.createdAt).toLocaleDateString()}</p>
                                    </div>
                                    <span className="text-xs font-black text-emerald-600">+{(deal.amountPaid / 100).toLocaleString()} €</span>
                                </div>
                            ))}
                        </div>
                        <Link href="/admin/finances/invoices" className="block text-center w-full mt-8 py-3 bg-slate-50 text-[10px] font-black uppercase tracking-widest hover:bg-slate-100 transition-colors rounded-xl text-slate-400">
                            Voir tout
                        </Link>
                    </div>
                </div>
            </div>

            {/* Modal Settlement */}
            {isPayoutModalOpen && selectedAgencyForPayout && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-md z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-[40px] shadow-2xl w-full max-w-md overflow-hidden animate-in zoom-in duration-200">
                        <div className="bg-slate-900 p-8 text-white text-center">
                            <div className="w-20 h-20 bg-emerald-500 rounded-[28px] flex items-center justify-center mx-auto mb-6 shadow-xl shadow-emerald-500/30">
                                <DollarSign size={40} />
                            </div>
                            <h2 className="text-2xl font-black uppercase tracking-tighter mb-2">Règlement Solde</h2>
                            <p className="text-slate-400 text-xs font-bold uppercase tracking-widest">Confirmation de reversement</p>
                        </div>
                        <div className="p-10 space-y-8 text-center">
                            <div>
                                <p className="text-slate-400 font-bold uppercase text-[10px] tracking-widest mb-2">Montant à transférer</p>
                                <p className="text-5xl font-black text-slate-900 tracking-tighter">{selectedAgencyForPayout.balance.toLocaleString()} €</p>
                            </div>
                            <div className="bg-slate-50 p-6 rounded-3xl border border-slate-100">
                                <p className="text-sm font-bold text-slate-600 mb-1">Destinataire :</p>
                                <p className="text-lg font-black text-slate-900 uppercase">{selectedAgencyForPayout.name}</p>
                            </div>
                            <p className="text-xs text-slate-400 font-medium px-4">
                                En validant, vous confirmez que l'ordre de virement bancaire a été exécuté.
                                Ce montant sera déduit de la dette HQ et apparaîtra dans l'espace partenaire.
                            </p>
                        </div>
                        <div className="p-10 bg-slate-50 border-t border-slate-100 flex gap-4">
                            <button
                                onClick={() => setIsPayoutModalOpen(false)}
                                className="flex-1 h-14 bg-white border-2 border-slate-200 text-slate-400 rounded-2xl font-black uppercase tracking-widest hover:bg-slate-50 transition-all"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={confirmPayout}
                                className="flex-1 h-14 bg-emerald-600 text-white rounded-2xl font-black uppercase tracking-widest hover:bg-emerald-700 transition-all shadow-xl flex items-center justify-center gap-2"
                            >
                                <CheckCircle2 size={20} />
                                Confirmer
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}

function FinanceCard({ title, value, icon, color, isMain = false }: {
    title: string, value: string, icon: React.ReactNode, color: string, isMain?: boolean
}) {
    const colorClasses: Record<string, string> = {
        emerald: "bg-emerald-50 border-emerald-100 text-emerald-600",
        amber: "bg-amber-50 border-amber-100 text-amber-600",
        blue: "bg-blue-50 border-blue-100 text-blue-600",
        indigo: "bg-indigo-600 border-indigo-700 text-white shadow-xl shadow-indigo-600/30"
    };

    return (
        <div className={`p-6 rounded-[32px] border ${colorClasses[color]} relative overflow-hidden transition-all hover:scale-105`}>
            <p className={`text-[10px] font-black uppercase tracking-widest mb-1 ${isMain ? 'text-indigo-200' : 'text-slate-400'}`}>{title}</p>
            <p className="text-3xl font-black tracking-tight">{value}</p>
            <div className={`absolute top-6 right-6 ${isMain ? 'text-white/20' : 'opacity-40'}`}>
                {React.cloneElement(icon as any, { size: 32 })}
            </div>
            {isMain && (
                <div className="absolute -bottom-4 -right-4 w-24 h-24 bg-white/10 rounded-full blur-3xl"></div>
            )}
        </div>
    );
}
