'use client';

import GlobalFinancePanel from '../../../components/admin/finance/GlobalFinancePanel';

export default function FinancesPage() {
    return (
        <div className="p-8">
            <h1 className="text-2xl font-bold mb-6 text-slate-800">Finance Globale</h1>
            <GlobalFinancePanel />
        </div>
    );
}
