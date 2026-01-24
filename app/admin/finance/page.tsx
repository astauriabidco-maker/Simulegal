'use client';

import GlobalFinancePanel from '../../../components/admin/finance/GlobalFinancePanel';

export default function FinancePage() {
    return (
        <div className="p-8">
            <div className="mb-6">
                <h1 className="text-2xl font-black text-slate-900">Finances</h1>
                <p className="text-slate-500 text-sm">Gestion financière du réseau</p>
            </div>
            <GlobalFinancePanel />
        </div>
    );
}
