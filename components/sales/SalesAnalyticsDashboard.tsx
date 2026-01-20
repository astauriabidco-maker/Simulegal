import React, { useState, useEffect } from 'react';
import { TrendingUp, Users, Target, Euro, BarChart2, Filter, Info, ArrowUpRight, ArrowDownRight, Phone } from 'lucide-react';
import { SalesStore } from '../../services/SalesStore';

interface SalesAnalyticsDashboardProps {
    onClose?: () => void;
}

export const SalesAnalyticsDashboard: React.FC<SalesAnalyticsDashboardProps> = () => {
    const [period, setPeriod] = useState<'TODAY' | 'WEEK' | 'MONTH'>('MONTH');
    const [loading, setLoading] = useState(true);
    const [stats, setStats] = useState<any>(null);

    useEffect(() => {
        const loadStats = async () => {
            setLoading(true);
            try {
                const data = await SalesStore.getAnalytics(period);
                setStats(data);
            } finally {
                setLoading(false);
            }
        };
        loadStats();
    }, [period]);

    if (loading || !stats) return (
        <div className="flex flex-col items-center justify-center py-20 animate-pulse">
            <div className="w-12 h-12 bg-indigo-100 rounded-full mb-4 animate-bounce" />
            <p className="text-slate-400 font-black text-sm uppercase tracking-widest">Chargement des Analytics...</p>
        </div>
    );

    const { kpis, funnel, sources } = stats;

    return (
        <div className="space-y-6 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* Header: Period Filter */}
            <div className="flex justify-between items-center mb-6">
                <h2 className="text-xl font-black text-slate-900 tracking-tight flex items-center gap-2">
                    <BarChart2 className="text-indigo-600" />
                    Performance Commerciale
                </h2>
                <div className="flex bg-white p-1 rounded-2xl border border-slate-100 shadow-sm">
                    {(['TODAY', 'WEEK', 'MONTH'] as const).map(p => (
                        <button
                            key={p}
                            onClick={() => setPeriod(p)}
                            className={`px-4 py-2 text-xs font-black rounded-xl transition-all ${period === p ? 'bg-indigo-600 text-white shadow-indigo-200 shadow-lg' : 'text-slate-500 hover:text-slate-700 hover:bg-slate-50'
                                }`}
                        >
                            {p === 'TODAY' ? "Aujourd'hui" : p === 'WEEK' ? 'Semaine' : 'Mois'}
                        </button>
                    ))}
                </div>
            </div>

            {/* KPI Cards Grid */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
                {/* Leads Volume */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 group hover:border-indigo-100 transition-all">
                    <div className="flex justify-between items-start mb-4">
                        <div className="p-2 bg-indigo-50 rounded-xl text-indigo-600 group-hover:bg-indigo-600 group-hover:text-white transition-colors">
                            <Users size={20} />
                        </div>
                        <span className="flex items-center gap-1 text-[10px] font-bold text-emerald-500 bg-emerald-50 px-2 py-1 rounded-lg">
                            <ArrowUpRight size={12} /> +12%
                        </span>
                    </div>
                    <h3 className="text-slate-400 text-[10px] font-black uppercase tracking-widest mb-1">Nouveaux Leads</h3>
                    <div className="flex items-baseline gap-2">
                        <span className="text-3xl font-black text-slate-900">{kpis.newLeads}</span>
                        <span className="text-xs font-bold text-slate-400">/ {kpis.totalLeads} total</span>
                    </div>
                    <div className="w-full bg-slate-100 h-1.5 rounded-full mt-3 overflow-hidden">
                        <div className="bg-indigo-500 h-full rounded-full" style={{ width: '65%' }} />
                    </div>
                </div>

                {/* Conversion Rate */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 group hover:border-emerald-100 transition-all">
                    <div className="flex justify-between items-start mb-4">
                        <div className="p-2 bg-emerald-50 rounded-xl text-emerald-600 group-hover:bg-emerald-600 group-hover:text-white transition-colors">
                            <Target size={20} />
                        </div>
                    </div>
                    <h3 className="text-slate-400 text-[10px] font-black uppercase tracking-widest mb-1">Taux de Conversion</h3>
                    <span className="text-3xl font-black text-slate-900">{kpis.conversionRate}%</span>
                    <p className="text-[10px] text-slate-400 mt-2 font-medium">Objectif: 15%</p>
                </div>

                {/* Pipeline Value */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 group hover:border-amber-100 transition-all">
                    <div className="flex justify-between items-start mb-4">
                        <div className="p-2 bg-amber-50 rounded-xl text-amber-600 group-hover:bg-amber-600 group-hover:text-white transition-colors">
                            <Euro size={20} />
                        </div>
                    </div>
                    <h3 className="text-slate-400 text-[10px] font-black uppercase tracking-widest mb-1">Valeur Pipeline</h3>
                    <span className="text-3xl font-black text-slate-900">{kpis.pipelineValue.toLocaleString('fr-FR')} €</span>
                    <p className="text-[10px] text-slate-400 mt-2 font-medium">Valeur estimée pondérée</p>
                </div>

                {/* Avg Response Time (Mock for now) */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 group hover:border-rose-100 transition-all">
                    <div className="flex justify-between items-start mb-4">
                        <div className="p-2 bg-rose-50 rounded-xl text-rose-600 group-hover:bg-rose-600 group-hover:text-white transition-colors">
                            <Phone size={20} />
                        </div>
                        <span className="flex items-center gap-1 text-[10px] font-bold text-rose-500 bg-rose-50 px-2 py-1 rounded-lg">
                            <ArrowDownRight size={12} /> -5 min
                        </span>
                    </div>
                    <h3 className="text-slate-400 text-[10px] font-black uppercase tracking-widest mb-1">Temps Réponse</h3>
                    <span className="text-3xl font-black text-slate-900">12 min</span>
                    <p className="text-[10px] text-slate-400 mt-2 font-medium">Moyenne équipe</p>
                </div>
            </div>

            {/* Charts Section */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                {/* Acquisition Mix */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 col-span-1">
                    <h3 className="text-sm font-black text-slate-900 mb-6 flex items-center gap-2">
                        <Filter size={16} className="text-indigo-500" />
                        Sources d'Acquisition
                    </h3>
                    <div className="space-y-4">
                        {sources.map((src: any) => (
                            <div key={src.source}>
                                <div className="flex justify-between text-xs font-bold mb-1">
                                    <span className="text-slate-600 uppercase">{src.source.replace('_', ' ')}</span>
                                    <span className="text-slate-900">{src.count}</span>
                                </div>
                                <div className="h-2 bg-slate-50 rounded-full overflow-hidden">
                                    <div
                                        className="h-full bg-slate-900 rounded-full"
                                        style={{ width: `${(src.count / kpis.newLeads) * 100}%` }}
                                    />
                                </div>
                            </div>
                        ))}
                        {sources.length === 0 && <p className="text-xs text-slate-400 italic">Aucune donnée sur cette période.</p>}
                    </div>
                </div>

                {/* Funnel Visualization */}
                <div className="bg-white p-6 rounded-3xl shadow-sm border border-slate-100 col-span-2">
                    <h3 className="text-sm font-black text-slate-900 mb-6 flex items-center gap-2">
                        <TrendingUp size={16} className="text-emerald-500" />
                        Entonnoir de Conversion
                    </h3>
                    <div className="flex items-end justify-between gap-2 h-48 px-4">
                        {funnel.map((step: any, index: number) => {
                            const maxVal = Math.max(...funnel.map((f: any) => f.count));
                            const height = (step.count / maxVal) * 100;
                            const colors = ['bg-slate-200', 'bg-indigo-300', 'bg-indigo-500', 'bg-emerald-400', 'bg-emerald-600'];

                            return (
                                <div key={step.status} className="flex-1 flex flex-col items-center group">
                                    <div className="mb-2 opacity-0 group-hover:opacity-100 transition-opacity text-xs font-bold text-slate-600 bg-white shadow-md border px-2 py-1 rounded-lg">
                                        {step.count}
                                    </div>
                                    <div
                                        className={`w-full rounded-t-xl transition-all hover:opacity-80 ${colors[index % colors.length]}`}
                                        style={{ height: `${height}%` }}
                                    />
                                    <div className="mt-3 text-center">
                                        <div className="text-[10px] font-black text-slate-400 uppercase tracking-tighter truncate w-20">
                                            {step.status.replace('_', ' ')}
                                        </div>
                                        <div className="text-xs font-black text-slate-900">{step.count}</div>
                                    </div>
                                </div>
                            );
                        })}
                    </div>
                </div>
            </div>
        </div>
    );
};
