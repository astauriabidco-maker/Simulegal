'use client';

import React, { useState, useEffect } from 'react';
import { Building2 } from 'lucide-react';
import { FinanceStore, Payout } from '../../../services/FinanceStore';

export default function FinancePayouts() {
    const [recentPayouts, setRecentPayouts] = useState<Payout[]>([]);

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const payouts = await FinanceStore.getAllPayouts();
            setRecentPayouts(payouts.sort((a, b) => b.createdAt.localeCompare(a.createdAt)));
        } catch (error) {
            console.error('[Finance] Erreur chargement reversements:', error);
        }
    };

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10">
                <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Reversements Partenaires</h1>
                <p className="text-slate-500 font-medium">Historique des commissions versées aux agences</p>
            </div>

            <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                <table className="w-full text-left">
                    <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-widest text-slate-400">
                        <tr>
                            <th className="p-6">Date</th>
                            <th className="p-6">Agence</th>
                            <th className="p-6">Période</th>
                            <th className="p-6">Référence</th>
                            <th className="p-6 text-right">Montant Versé</th>
                            <th className="p-6">Statut</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {recentPayouts.map(payout => (
                            <tr key={payout.id} className="hover:bg-slate-50/50 transition-colors">
                                <td className="p-6 text-sm text-slate-500">{new Date(payout.createdAt).toLocaleDateString()}</td>
                                <td className="p-6 font-black text-slate-900 uppercase">{(payout as any).agency?.name || payout.agencyId}</td>
                                <td className="p-6 text-sm font-bold text-slate-500">{payout.period}</td>
                                <td className="p-6 font-mono text-xs text-slate-400">{payout.reference}</td>
                                <td className="p-6 text-right font-black text-indigo-600">{payout.amount.toLocaleString()} €</td>
                                <td className="p-6">
                                    <span className="bg-emerald-100 text-emerald-700 px-3 py-1 rounded-full text-[10px] font-black uppercase">
                                        Effectué
                                    </span>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
        </div>
    );
}
