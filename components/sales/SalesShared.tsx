import React from 'react';
import { Prospect } from '../../services/SalesStore';
import { ProspectStatus } from '../../services/SalesStore';

export const COLUMNS: { id: ProspectStatus; label: string; color: string; icon: string }[] = [
    { id: 'NEW', label: 'Nouveau', color: 'bg-amber-100 text-amber-800', icon: '🟡' },
    { id: 'CONTACTED', label: 'Contacté', color: 'bg-purple-100 text-purple-800', icon: '🟣' },
    { id: 'QUALIFIED', label: 'Qualifié', color: 'bg-cyan-100 text-cyan-800', icon: '🔵' },
    { id: 'MEETING_BOOKED', label: 'RDV Fixé', color: 'bg-indigo-100 text-indigo-800', icon: '📅' },
    { id: 'SIGNED', label: 'Signé', color: 'bg-emerald-100 text-emerald-800', icon: '✅' },
    { id: 'NO_SHOW', label: 'Non Honoré', color: 'bg-red-100 text-red-800', icon: '🚫' },
    { id: 'LOST', label: 'Perdu', color: 'bg-slate-100 text-slate-800', icon: '⚫' }
];

export function PaymentButtons({ prospect, onPay, fetchPrice }: {
    prospect: Prospect;
    onPay: (prospect: Prospect, installments: 1 | 3) => void;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; pricePer3: number; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        const serviceId = prospect.eligibilityResult?.matchedProcedures?.[0] || prospect.interestServiceId || '';
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [prospect.id, prospect.interestServiceId]);

    if (!pricing) {
        return (
            <div className="flex gap-2">
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
            </div>
        );
    }

    return (
        <div className="flex gap-2">
            <button
                onClick={() => onPay(prospect, 1)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-emerald-600 text-white rounded-xl text-sm font-bold hover:bg-emerald-700 transition-all shadow-sm shadow-emerald-200 active:scale-[0.97]"
            >
                💳 Encaisser {pricing.priceEuros}€
                {pricing.promoActive && <span className="ml-1 px-1 py-0.5 bg-red-500 text-[9px] rounded-md uppercase">Promo</span>}
            </button>
            <button
                onClick={() => onPay(prospect, 3)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-emerald-300 text-emerald-700 rounded-xl text-sm font-bold hover:bg-emerald-50 transition-all active:scale-[0.97]"
            >
                💳 3x {pricing.pricePer3}€
            </button>
        </div>
    );
}

export function ServicePriceDisplay({ serviceId, fetchPrice }: {
    serviceId: string;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; serviceName: string; source: string; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [serviceId]);

    if (!serviceId || !pricing) return null;

    const sourceLabel = pricing.source === 'PROMO' ? '🏷️ Promo' :
        pricing.source === 'ADMIN_OVERRIDE' ? '⚙️ Admin' :
            pricing.source === 'CATALOG_DEFAULT' || pricing.source === 'FRONTEND_MAP' ? '📋 Défaut' : '';

    const sourceColor = pricing.source === 'PROMO' ? 'text-red-500' :
        pricing.source === 'ADMIN_OVERRIDE' ? 'text-indigo-500' : 'text-slate-400';

    return (
        <div className="flex items-center gap-2 mt-1">
            <span className="text-sm font-black text-emerald-600">{pricing.priceEuros}€</span>
            {sourceLabel && <span className={`text-[9px] font-bold ${sourceColor}`}>{sourceLabel}</span>}
            {pricing.promoActive && <span className="px-1.5 py-0.5 bg-red-100 text-red-600 text-[9px] font-bold rounded-md animate-pulse">PROMO ACTIVE</span>}
        </div>
    );
}
