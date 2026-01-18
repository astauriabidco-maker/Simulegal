'use client';

import React, { useState, useEffect } from 'react';
import {
    Wallet,
    ArrowUpRight,
    CheckCircle2,
    Clock,
    ArrowRightLeft,
    FileText,
    Download,
    Building2,
    Calendar,
    ChevronRight,
    Search,
    AlertCircle,
    BadgeEuro
} from 'lucide-react';
import { CRM, Lead } from '../../services/crmStore';
import { AgencyStore } from '../../services/AgencyStore';
import { FinanceStore, Payout } from '../../services/FinanceStore';
import AuthStore from '../../services/authStore';

export default function AgencyFinanceView() {
    const [balance, setBalance] = useState(0);
    const [stats, setStats] = useState({ totalGenerated: 0, totalPaid: 0, dealsCount: 0 });
    const [payouts, setPayouts] = useState<Payout[]>([]);
    const [myDeals, setMyDeals] = useState<Lead[]>([]);
    const [agencyId, setAgencyId] = useState<string | null>(null);
    const [agencyCommissionRate, setAgencyCommissionRate] = useState(15);

    useEffect(() => {
        const user = AuthStore.getCurrentUser();
        if (user && user.agencyId) {
            setAgencyId(user.agencyId);
            loadData(user.agencyId);
        }
    }, []);

    const loadData = async (aid: string) => {
        try {
            const [currentBalance, currentPayouts, allAgencies, agencyLeads] = await Promise.all([
                FinanceStore.getAgencyBalance(aid),
                FinanceStore.getAgencyPayouts(aid),
                AgencyStore.getAllAgencies(),
                CRM.getAllLeads(aid)
            ]);

            setBalance(currentBalance);
            setPayouts(currentPayouts.sort((a, b) => b.createdAt.localeCompare(a.createdAt)));

            const agency = allAgencies.find(a => a.id === aid);

            if (agency) {
                setAgencyCommissionRate(agency.commissionRate);
                setMyDeals(agencyLeads.slice(0, 10));

                const totalGenerated = agencyLeads.reduce((sum, l) => sum + ((l.amountPaid || 0) / 100 * agency.commissionRate / 100), 0);
                const totalPaid = currentPayouts.filter(p => p.status === 'PAID').reduce((sum, p) => sum + p.amount, 0);

                setStats({
                    totalGenerated,
                    totalPaid,
                    dealsCount: agencyLeads.length
                });
            }
        } catch (error) {
            console.error('[AgencyFinance] Erreur chargement:', error);
        }
    };

    if (!agencyId) return null;

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10 flex flex-col md:flex-row md:items-center justify-between gap-6">
                <div>
                    <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Ma Trésorerie</h1>
                    <p className="text-slate-500 font-medium">Suivi de mes commissions et de mes reversements</p>
                </div>
                <div className="bg-white p-4 px-8 rounded-3xl border border-slate-100 shadow-sm flex items-center gap-6">
                    <div className="text-right">
                        <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1">Moyen de paiement Principal</p>
                        <p className="font-bold text-slate-900 flex items-center gap-2 justify-end">
                            Virement Bancaire <span className="w-2 h-2 bg-emerald-500 rounded-full animate-pulse"></span>
                        </p>
                    </div>
                    <div className="w-12 h-12 bg-slate-900 text-white rounded-2xl flex items-center justify-center">
                        <BadgeEuro size={24} />
                    </div>
                </div>
            </div>

            {/* Partner KPIs */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-12">
                <div className="bg-indigo-600 rounded-[32px] p-8 text-white shadow-2xl relative overflow-hidden group">
                    <p className="text-indigo-200 text-xs font-black uppercase tracking-widest mb-2">Commissions à recevoir</p>
                    <p className="text-5xl font-black tracking-tighter">{balance.toLocaleString()} €</p>
                    <div className="mt-4 flex items-center gap-2 text-indigo-200 text-xs font-bold">
                        <Clock size={16} /> Versement estimé fin de mois
                    </div>
                    <Wallet size={80} className="absolute -bottom-8 -right-8 text-white/10 group-hover:rotate-12 transition-transform duration-500" />
                </div>

                <div className="bg-white rounded-[32px] p-8 border border-slate-100 shadow-sm hover:shadow-md transition-all">
                    <p className="text-slate-400 text-xs font-black uppercase tracking-widest mb-2">Généré ce mois-ci</p>
                    <p className="text-3xl font-black text-slate-900">{(stats.totalGenerated).toLocaleString()} €</p>
                    <div className="mt-4 flex items-center gap-2 text-slate-500 text-xs font-bold">
                        <ArrowUpRight size={16} className="text-emerald-500" /> Basé sur {stats.dealsCount} dossiers signés
                    </div>
                </div>

                <div className="bg-white rounded-[32px] p-8 border border-slate-100 shadow-sm hover:shadow-md transition-all">
                    <p className="text-slate-400 text-xs font-black uppercase tracking-widest mb-2">Total Déjà Perçu</p>
                    <p className="text-3xl font-black text-slate-900">{stats.totalPaid.toLocaleString()} €</p>
                    <div className="mt-4 flex items-center gap-2 text-slate-500 text-xs font-bold">
                        <CheckCircle2 size={16} className="text-blue-500" /> {payouts.length} virements reçus au total
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                {/* Historique des virements */}
                <div className="lg:col-span-2 space-y-8">
                    <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                        <div className="p-8 border-b border-slate-50 flex items-center justify-between">
                            <div>
                                <h3 className="text-lg font-black text-slate-900 tracking-tight uppercase">Historique des Virements Siège</h3>
                                <p className="text-slate-400 text-xs font-bold uppercase tracking-widest mt-1">Transparence totale sur vos revenus</p>
                            </div>
                        </div>

                        <div className="divide-y divide-slate-100">
                            {payouts.length === 0 ? (
                                <div className="p-20 text-center">
                                    <ArrowRightLeft className="mx-auto mb-4 text-slate-200" size={48} />
                                    <p className="text-slate-400 font-bold uppercase text-xs tracking-widest">Aucun virement pour le moment</p>
                                </div>
                            ) : (
                                payouts.map(payout => (
                                    <div key={payout.id} className="p-6 flex items-center justify-between hover:bg-slate-50/50 transition-colors">
                                        <div className="flex items-center gap-4">
                                            <div className="w-12 h-12 bg-emerald-50 text-emerald-600 rounded-2xl flex items-center justify-center">
                                                <ArrowRightLeft size={20} />
                                            </div>
                                            <div>
                                                <p className="font-black text-slate-900 uppercase tracking-tight">Virement Reçu</p>
                                                <p className="text-[10px] text-slate-400 font-bold uppercase tracking-widest">
                                                    Ref: {payout.reference} • {new Date(payout.paidAt || '').toLocaleDateString('fr-FR')}
                                                </p>
                                            </div>
                                        </div>
                                        <div className="flex items-center gap-6">
                                            <div className="text-right">
                                                <p className="text-lg font-black text-emerald-600">+{payout.amount.toLocaleString()} €</p>
                                                <span className="text-[10px] bg-emerald-100 text-emerald-700 px-2 py-0.5 rounded-full font-black uppercase">Exécuté</span>
                                            </div>
                                            <button className="p-2 text-slate-400 hover:text-slate-900 transition-colors">
                                                <Download size={20} />
                                            </button>
                                        </div>
                                    </div>
                                ))
                            )}
                        </div>
                    </div>

                    <div className="bg-amber-50 border border-amber-100 rounded-[32px] p-8 flex gap-6 items-start">
                        <AlertCircle className="text-amber-600 mt-1 shrink-0" size={24} />
                        <div>
                            <p className="font-black text-amber-900 uppercase text-sm tracking-tight mb-2">Comprendre vos revenus</p>
                            <p className="text-amber-800/80 text-xs font-bold leading-relaxed space-y-2">
                                Les commissions sont créditées sur votre solde dès que le dossier client passe au statut "PAYÉ".<br />
                                Les virements sont packagés mensuellement et déclenchés par le service comptabilité du siège.<br />
                                <strong className="text-amber-900">Une question ?</strong> Contactez le service finance à <a href="mailto:finance@simulegal.fr" className="underline font-black">finance@simulegal.fr</a>
                            </p>
                        </div>
                    </div>
                </div>

                {/* Dernières ventes side */}
                <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden h-fit">
                    <div className="p-8 border-b border-slate-50 text-center">
                        <h4 className="text-xs font-black text-slate-400 uppercase tracking-widest mb-1">Dernières Commissions</h4>
                        <p className="text-2xl font-black text-slate-900 tracking-tighter uppercase">Mes Ventes Live</p>
                    </div>
                    <div className="divide-y divide-slate-100">
                        {myDeals.map(deal => (
                            <div key={deal.id} className="p-4 px-6 flex items-center justify-between group">
                                <div className="min-w-0">
                                    <p className="font-bold text-slate-900 truncate">{deal.name}</p>
                                    <p className="text-[10px] text-slate-400 font-bold">{new Date(deal.createdAt).toLocaleDateString()}</p>
                                </div>
                                <div className="text-right shrink-0">
                                    <p className="font-black text-emerald-600">+{((deal.amountPaid || 0) / 100 * (agencyCommissionRate / 100)).toFixed(2)} €</p>
                                    <p className="text-[8px] text-slate-400 font-black uppercase">Apport</p>
                                </div>
                            </div>
                        ))}
                    </div>
                    <div className="p-6 bg-slate-50 text-center">
                        <button className="text-[10px] font-black text-indigo-600 uppercase tracking-widest flex items-center gap-2 mx-auto hover:gap-3 transition-all">
                            Voir tous mes dossiers <ArrowRightLeft size={12} />
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}
