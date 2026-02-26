'use client';

import React, { useState, useEffect, useCallback } from 'react';
import {
    ArrowRightLeft, Building2, Download, CheckCircle2, Clock, DollarSign, X,
    TrendingUp, AlertTriangle, RefreshCw, XCircle, Search
} from 'lucide-react';
import { FinanceStore, Payout } from '../../../services/FinanceStore';
import { AgencyStore } from '../../../services/AgencyStore';

export default function FinancePayouts() {
    const [payouts, setPayouts] = useState<Payout[]>([]);
    const [debts, setDebts] = useState<{ id: string; name: string; balance: number; totalEarned: number; totalPaid: number; rate: number }[]>([]);
    const [loading, setLoading] = useState(true);
    const [toast, setToast] = useState<{ msg: string; type: 'success' | 'error' } | null>(null);
    const [payoutModal, setPayoutModal] = useState<{ id: string; name: string; balance: number } | null>(null);
    const [search, setSearch] = useState('');
    const [tabView, setTabView] = useState<'debts' | 'history'>('debts');

    const showToast = useCallback((msg: string, type: 'success' | 'error' = 'success') => {
        setToast({ msg, type }); setTimeout(() => setToast(null), 4000);
    }, []);

    const loadData = async () => {
        setLoading(true);
        try {
            const [pays, agencies] = await Promise.all([
                FinanceStore.getAllPayouts(),
                AgencyStore.getAllAgencies()
            ]);
            setPayouts(pays.sort((a: any, b: any) => b.createdAt.localeCompare(a.createdAt)));

            const ds = await Promise.all(agencies.map(async a => {
                const bal = await FinanceStore.getAgencyBalance(a.id);
                return {
                    id: a.id,
                    name: a.name,
                    balance: typeof bal === 'object' ? (bal as any).balance || 0 : bal,
                    totalEarned: typeof bal === 'object' ? (bal as any).totalEarned || 0 : 0,
                    totalPaid: typeof bal === 'object' ? (bal as any).totalPaid || 0 : 0,
                    rate: a.commissionRate
                };
            }));
            setDebts(ds.sort((a, b) => b.balance - a.balance));
        } catch (err) {
            console.error('[Commissions] Erreur:', err);
        }
        setLoading(false);
    };

    useEffect(() => { loadData(); }, []);

    const handlePayout = async () => {
        if (!payoutModal) return;
        await FinanceStore.createPayout(payoutModal.id, payoutModal.balance, new Date().toISOString().substring(0, 7));
        showToast(`✅ Virement de ${payoutModal.balance.toLocaleString()}€ confirmé pour ${payoutModal.name}`);
        setPayoutModal(null);
        loadData();
    };

    const handleDownloadSepa = async (payoutId: string, ref: string) => {
        const blob = await FinanceStore.downloadSepaXml(payoutId);
        if (blob) {
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url; a.download = `SEPA-${ref}.xml`; a.click();
            showToast('Fichier SEPA XML téléchargé');
        } else {
            showToast('Erreur téléchargement SEPA', 'error');
        }
    };

    const totalDebt = debts.reduce((s, d) => s + Math.max(0, d.balance), 0);
    const totalPaid = debts.reduce((s, d) => s + d.totalPaid, 0);
    const totalEarned = debts.reduce((s, d) => s + d.totalEarned, 0);
    const activeDebts = debts.filter(d => d.balance > 0);
    const filteredDebts = debts.filter(d => d.name.toLowerCase().includes(search.toLowerCase()));
    const filteredPayouts = payouts.filter(p => {
        const name = (p as any).agency?.name || p.agencyId;
        return name.toLowerCase().includes(search.toLowerCase());
    });

    if (loading) return (
        <div className="flex items-center justify-center h-[60vh]">
            <RefreshCw size={32} className="animate-spin text-indigo-500" />
        </div>
    );

    return (
        <div className="p-8 max-w-[1400px] mx-auto">
            {/* Toast */}
            {toast && (
                <div className={`fixed top-6 right-6 z-50 px-5 py-3 rounded-xl shadow-xl font-bold text-sm flex items-center gap-2 ${toast.type === 'success' ? 'bg-emerald-500 text-white' : 'bg-rose-500 text-white'}`}>
                    {toast.type === 'success' ? <CheckCircle2 size={16} /> : <XCircle size={16} />}
                    {toast.msg}
                </div>
            )}

            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-3xl font-black text-slate-900 tracking-tight">Commissions Réseau</h1>
                    <p className="text-slate-500 text-sm">Gestion des commissions et virements aux agences partenaires</p>
                </div>
                <button onClick={loadData} className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200">
                    <RefreshCw size={16} /> Actualiser
                </button>
            </div>

            {/* KPIs */}
            <div className="grid grid-cols-4 gap-4 mb-6">
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-indigo-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Total Commissions Gagnées</p>
                    <p className="text-xl font-black text-slate-900">{totalEarned.toLocaleString()} €</p>
                    <p className="text-xs text-slate-500">{debts.length} agences</p>
                    <TrendingUp size={28} className="absolute top-4 right-4 text-indigo-200" />
                </div>
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-emerald-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Déjà Versé</p>
                    <p className="text-xl font-black text-emerald-600">{totalPaid.toLocaleString()} €</p>
                    <p className="text-xs text-slate-500">{payouts.length} virements effectués</p>
                    <CheckCircle2 size={28} className="absolute top-4 right-4 text-emerald-200" />
                </div>
                <div className="relative overflow-hidden bg-white border border-slate-200 rounded-2xl p-5 shadow-sm">
                    <div className="absolute top-0 left-0 w-1 h-full bg-amber-500" />
                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Solde à Régler</p>
                    <p className="text-xl font-black text-amber-600">{totalDebt.toLocaleString()} €</p>
                    <p className="text-xs text-slate-500">{activeDebts.length} agences créancières</p>
                    <Clock size={28} className="absolute top-4 right-4 text-amber-200" />
                </div>
                <div className="relative overflow-hidden bg-gradient-to-br from-indigo-600 to-purple-700 rounded-2xl p-5 shadow-lg text-white">
                    <p className="text-[10px] font-bold text-indigo-200 uppercase tracking-wider mb-1">Taux de Règlement</p>
                    <p className="text-xl font-black">{totalEarned > 0 ? Math.round((totalPaid / totalEarned) * 100) : 0}%</p>
                    <div className="w-full bg-white/20 rounded-full h-2 mt-2">
                        <div className="h-2 rounded-full bg-white" style={{ width: `${totalEarned > 0 ? Math.min(100, (totalPaid / totalEarned) * 100) : 0}%` }} />
                    </div>
                </div>
            </div>

            {/* Search + Tabs */}
            <div className="flex items-center justify-between mb-4">
                <div className="flex gap-2">
                    <button onClick={() => setTabView('debts')} className={`px-5 py-2 rounded-xl text-xs font-black uppercase tracking-wider ${tabView === 'debts' ? 'bg-slate-900 text-white' : 'bg-white text-slate-400 border border-slate-200 hover:text-slate-600'}`}>
                        <span className="flex items-center gap-2"><Clock size={14} /> Soldes Agences ({activeDebts.length})</span>
                    </button>
                    <button onClick={() => setTabView('history')} className={`px-5 py-2 rounded-xl text-xs font-black uppercase tracking-wider ${tabView === 'history' ? 'bg-slate-900 text-white' : 'bg-white text-slate-400 border border-slate-200 hover:text-slate-600'}`}>
                        <span className="flex items-center gap-2"><CheckCircle2 size={14} /> Historique Virements ({payouts.length})</span>
                    </button>
                </div>
                <div className="relative">
                    <Search size={16} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" />
                    <input type="text" value={search} onChange={e => setSearch(e.target.value)} placeholder="Rechercher une agence..." className="pl-9 pr-4 h-10 border border-slate-200 rounded-xl text-sm outline-none focus:ring-2 focus:ring-indigo-500 w-64" />
                </div>
            </div>

            {/* ═══ SOLDES AGENCES ═══ */}
            {tabView === 'debts' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">Agence</th><th className="p-4">Taux Com.</th><th className="p-4 text-right">Commissions Gagnées</th><th className="p-4 text-right">Déjà Versé</th><th className="p-4 text-right">Solde Dû</th><th className="p-4">Statut</th><th className="p-4"></th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {filteredDebts.length === 0 ? (
                                <tr><td colSpan={7} className="p-16 text-center text-slate-400 font-bold">Aucune agence trouvée</td></tr>
                            ) : (
                                filteredDebts.map(d => (
                                    <tr key={d.id} className="hover:bg-slate-50 transition-colors">
                                        <td className="p-4">
                                            <div className="flex items-center gap-3">
                                                <div className="w-9 h-9 bg-slate-900 text-white rounded-lg flex items-center justify-center text-xs font-black">{d.name.charAt(0)}</div>
                                                <span className="font-bold text-slate-800">{d.name}</span>
                                            </div>
                                        </td>
                                        <td className="p-4 text-sm font-bold text-slate-500">{d.rate}%</td>
                                        <td className="p-4 text-right font-bold text-slate-700">{d.totalEarned.toLocaleString()} €</td>
                                        <td className="p-4 text-right font-bold text-emerald-600">{d.totalPaid.toLocaleString()} €</td>
                                        <td className="p-4 text-right font-black text-slate-900">{Math.max(0, d.balance).toLocaleString()} €</td>
                                        <td className="p-4">
                                            {d.balance <= 0 ? (
                                                <span className="bg-emerald-100 text-emerald-700 px-2.5 py-1 rounded-full text-[10px] font-black uppercase">À jour</span>
                                            ) : d.balance > 1000 ? (
                                                <span className="bg-rose-100 text-rose-700 px-2.5 py-1 rounded-full text-[10px] font-black uppercase flex items-center gap-1 w-fit"><AlertTriangle size={10} /> Urgent</span>
                                            ) : (
                                                <span className="bg-amber-100 text-amber-700 px-2.5 py-1 rounded-full text-[10px] font-black uppercase">En attente</span>
                                            )}
                                        </td>
                                        <td className="p-4">
                                            {d.balance > 0 && (
                                                <button onClick={() => setPayoutModal({ id: d.id, name: d.name, balance: d.balance })}
                                                    className="px-4 py-2 bg-indigo-600 text-white text-xs font-bold rounded-lg hover:bg-indigo-700 transition-colors flex items-center gap-1.5">
                                                    <DollarSign size={14} /> Régler
                                                </button>
                                            )}
                                        </td>
                                    </tr>
                                ))
                            )}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ HISTORIQUE VIREMENTS ═══ */}
            {tabView === 'history' && (
                <div className="bg-white rounded-2xl border border-slate-200 shadow-sm overflow-hidden">
                    <table className="w-full text-left">
                        <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-wider text-slate-400">
                            <tr><th className="p-4">Date</th><th className="p-4">Agence</th><th className="p-4">Période</th><th className="p-4">Référence</th><th className="p-4 text-right">Montant</th><th className="p-4">Statut</th><th className="p-4"></th></tr>
                        </thead>
                        <tbody className="divide-y divide-slate-100">
                            {filteredPayouts.length === 0 ? (
                                <tr><td colSpan={7} className="p-16 text-center text-slate-400 font-bold">Aucun virement effectué</td></tr>
                            ) : (
                                filteredPayouts.map(p => (
                                    <tr key={p.id} className="hover:bg-slate-50 transition-colors">
                                        <td className="p-4 text-sm text-slate-500">{new Date(p.createdAt).toLocaleDateString('fr-FR')}</td>
                                        <td className="p-4">
                                            <div className="flex items-center gap-3">
                                                <div className="w-8 h-8 bg-slate-100 text-slate-600 rounded-lg flex items-center justify-center text-xs font-bold"><Building2 size={14} /></div>
                                                <span className="font-bold text-slate-800">{(p as any).agency?.name || p.agencyId}</span>
                                            </div>
                                        </td>
                                        <td className="p-4 text-sm text-slate-500">{p.period}</td>
                                        <td className="p-4 font-mono text-xs text-slate-400">{p.reference}</td>
                                        <td className="p-4 text-right font-black text-indigo-600">{p.amount.toLocaleString()} €</td>
                                        <td className="p-4"><span className="bg-emerald-100 text-emerald-700 px-2.5 py-1 rounded-full text-[10px] font-black uppercase">{p.status}</span></td>
                                        <td className="p-4">
                                            <button onClick={() => handleDownloadSepa(p.id, p.reference || p.id)} className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-lg" title="Télécharger SEPA XML">
                                                <Download size={16} />
                                            </button>
                                        </td>
                                    </tr>
                                ))
                            )}
                        </tbody>
                    </table>
                </div>
            )}

            {/* ═══ PAYOUT MODAL ═══ */}
            {payoutModal && (
                <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-2xl w-full max-w-md overflow-hidden">
                        <div className="bg-slate-900 p-6 text-white text-center">
                            <div className="w-16 h-16 bg-indigo-500 rounded-2xl flex items-center justify-center mx-auto mb-4 shadow-lg"><ArrowRightLeft size={28} /></div>
                            <h2 className="text-xl font-black">Virement Commission</h2>
                            <p className="text-slate-400 text-xs font-bold uppercase mt-1">Confirmation de transfert</p>
                        </div>
                        <div className="p-8 text-center space-y-4">
                            <div>
                                <p className="text-[10px] text-slate-400 font-bold uppercase">Montant à virer</p>
                                <p className="text-4xl font-black text-slate-900">{payoutModal.balance.toLocaleString()} €</p>
                            </div>
                            <div className="bg-slate-50 p-4 rounded-xl">
                                <p className="text-sm text-slate-500">Agence bénéficiaire</p>
                                <p className="font-black text-slate-900 text-lg">{payoutModal.name}</p>
                            </div>
                            <p className="text-xs text-slate-400">
                                En confirmant, vous validez que l'ordre de virement a été exécuté.
                                Un fichier SEPA XML sera générable pour la banque.
                            </p>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button onClick={() => setPayoutModal(null)} className="flex-1 py-3 bg-white border border-slate-200 text-slate-500 font-bold rounded-xl hover:bg-slate-100">Annuler</button>
                            <button onClick={handlePayout} className="flex-1 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 flex items-center justify-center gap-2">
                                <CheckCircle2 size={18} /> Confirmer le virement
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
