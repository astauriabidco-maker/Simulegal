'use client';

import React, { useState, useEffect } from 'react';
import { FileText, Download } from 'lucide-react';
import { FinanceStore } from '../../../services/FinanceStore';
import { Lead } from '../../../services/crmStore';

export default function FinanceInvoices() {
    const [invoices, setInvoices] = useState<Lead[]>([]);

    useEffect(() => {
        loadData();
    }, []);

    const loadData = async () => {
        try {
            const allInvoices = await FinanceStore.getInvoices();
            setInvoices(allInvoices);
        } catch (error) {
            console.error('[Finance] Erreur chargement factures:', error);
        }
    };

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-10">
                <h1 className="text-4xl font-black text-slate-900 tracking-tighter uppercase mb-2">Factures Clients</h1>
                <p className="text-slate-500 font-medium">Historique de toutes les factures émises</p>
            </div>

            <div className="bg-white rounded-[32px] border border-slate-100 shadow-sm overflow-hidden">
                <table className="w-full text-left">
                    <thead className="bg-slate-50 text-[10px] uppercase font-black tracking-widest text-slate-400">
                        <tr>
                            <th className="p-6">Facture N°</th>
                            <th className="p-6">Date</th>
                            <th className="p-6">Client</th>
                            <th className="p-6">Service</th>
                            <th className="p-6">Agence</th>
                            <th className="p-6 text-right">Montant</th>
                            <th className="p-6"></th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-slate-100">
                        {invoices.length === 0 ? (
                            <tr><td colSpan={7} className="p-20 text-center text-slate-400 font-bold italic">Aucune facture trouvée</td></tr>
                        ) : (
                            invoices.map(inv => (
                                <tr key={inv.id} className="hover:bg-slate-50/50 transition-colors">
                                    <td className="p-6 font-black text-slate-900">{inv.invoiceNumber}</td>
                                    <td className="p-6 text-sm text-slate-500">{inv.paymentDate ? new Date(inv.paymentDate).toLocaleDateString() : '-'}</td>
                                    <td className="p-6 font-bold text-slate-700">{inv.name}</td>
                                    <td className="p-6 text-xs font-bold text-indigo-600 uppercase tracking-tight">{inv.serviceName}</td>
                                    <td className="p-6 text-xs font-bold text-slate-500">{(inv as any).originAgency?.name || 'Vente Directe'}</td>
                                    <td className="p-6 text-right font-black text-slate-900">{(inv.amountPaid / 100).toLocaleString()} €</td>
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
