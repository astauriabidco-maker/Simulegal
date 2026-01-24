'use client';

import React, { useState, useEffect } from 'react';
import { ArrowRightLeft, Download } from 'lucide-react';
import { FinanceStore } from '../../../services/FinanceStore';

export default function FinanceCreditNotes() {
    const [creditNotes, setCreditNotes] = useState<any[]>([]);

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const allCreditNotes = await FinanceStore.getCreditNotes();
            setCreditNotes(allCreditNotes);
        } catch (error) {
            console.error('[Finance] Erreur chargement avoirs:', error);
        }
    };

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10">
                <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Avoirs & Remboursements</h1>
                <p className="text-slate-500 font-medium">Liste des avoirs émis aux clients</p>
            </div>

            <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                <table className="w-full text-left">
                    <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-widest text-slate-400">
                        <tr>
                            <th className="p-6">N° Avoir</th>
                            <th className="p-6">Date</th>
                            <th className="p-6">Dossier Client</th>
                            <th className="p-6">Raison</th>
                            <th className="p-6 text-right">Montant</th>
                            <th className="p-6"></th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {creditNotes.length === 0 ? (
                            <tr><td colSpan={6} className="p-20 text-center text-slate-400 font-bold italic">Aucun avoir émis</td></tr>
                        ) : (
                            creditNotes.map(cn => (
                                <tr key={cn.id} className="hover:bg-slate-50/50 transition-colors">
                                    <td className="p-6 font-black text-slate-900">{cn.number}</td>
                                    <td className="p-6 text-sm text-slate-500">{new Date(cn.createdAt).toLocaleDateString()}</td>
                                    <td className="p-6 font-bold text-slate-700">{cn.lead?.name}</td>
                                    <td className="p-6 text-sm text-slate-500">{cn.reason}</td>
                                    <td className="p-6 text-right font-black text-red-600">{(cn.amount / 100).toLocaleString()} €</td>
                                    <td className="p-6 text-right">
                                        <button className="p-2 text-indigo-600 hover:bg-indigo-50 rounded-lg">
                                            <Download size={18} />
                                        </button>
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
