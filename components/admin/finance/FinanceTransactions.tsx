'use client';

import React, { useState, useEffect } from 'react';
import { ArrowRightLeft } from 'lucide-react';
import { FinanceStore } from '../../../services/FinanceStore';

export default function FinanceTransactions() {
    const [transactions, setTransactions] = useState<any[]>([]);

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const allTransactions = await FinanceStore.getTransactions();
            setTransactions(allTransactions);
        } catch (error) {
            console.error('[Finance] Erreur chargement transactions:', error);
        }
    };

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10">
                <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Règlements & Flux</h1>
                <p className="text-slate-500 font-medium">Historique complet des paiements et remboursements</p>
            </div>

            <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                <table className="w-full text-left">
                    <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-widest text-slate-400">
                        <tr>
                            <th className="p-6">ID Transaction</th>
                            <th className="p-6">Date</th>
                            <th className="p-6">Type</th>
                            <th className="p-6">Méthode</th>
                            <th className="p-6">Référence</th>
                            <th className="p-6 text-right">Montant</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {transactions.length === 0 ? (
                            <tr><td colSpan={6} className="p-20 text-center text-slate-400 font-bold italic">Aucune transaction enregistrée</td></tr>
                        ) : (
                            transactions.map(tx => (
                                <tr key={tx.id} className="hover:bg-slate-50/50 transition-colors">
                                    <td className="p-6 font-mono text-xs text-slate-400">{tx.id}</td>
                                    <td className="p-6 text-sm text-slate-500">{new Date(tx.createdAt).toLocaleDateString()}</td>
                                    <td className="p-6 uppercase">
                                        <span className={`px-3 py-1 rounded-full text-[10px] font-black ${tx.type === 'PAYMENT' ? 'bg-emerald-100 text-emerald-700' : 'bg-red-100 text-red-700'}`}>
                                            {tx.type}
                                        </span>
                                    </td>
                                    <td className="p-6 text-xs font-bold text-slate-500">{tx.method || '-'}</td>
                                    <td className="p-6 text-xs font-bold text-slate-400">{tx.reference || '-'}</td>
                                    <td className={`p-6 text-right font-black ${tx.type === 'REFUND' ? 'text-red-600' : 'text-slate-900'}`}>
                                        {tx.type === 'REFUND' ? '-' : ''}{(tx.amount / 100).toLocaleString()} €
                                    </td>
                                </tr>
                            ))
                        )}
                    </tbody>
                </table>
            </div>
        </div>
    );
}
