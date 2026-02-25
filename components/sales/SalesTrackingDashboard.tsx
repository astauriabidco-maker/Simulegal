'use client';

import React, { useState, useEffect, useCallback } from 'react';
import {
    BarChart2, Target, Trophy, Phone, Calendar, Users, TrendingUp,
    DollarSign, Clock, CheckCircle, AlertTriangle, ChevronDown, ChevronUp,
    Award, Flame, Star, Filter, RefreshCw, ArrowUp, ArrowDown
} from 'lucide-react';

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

function getHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('token') : null;
    return {
        'Content-Type': 'application/json',
        ...(token ? { Authorization: `Bearer ${token}` } : {}),
    };
}

interface LeaderboardEntry {
    rank: number;
    user: { id: string; name: string; email: string; agencyId: string | null };
    metrics: {
        calls: number;
        meetings: number;
        conversions: number;
        qualified: number;
        totalActivities: number;
        revenue: number;
        commissionsEarned: number;
    };
    compositeScore: number;
    objectiveCompletion: number | null;
    objectives: { metric: string; target: number; current: number; progress: number }[];
}

interface UserPerformance {
    period: string;
    userId: string;
    summary: {
        totalActivities: number;
        totalCallDuration: number;
        avgCallDuration: number;
        activityBreakdown: Record<string, number>;
    };
    objectives: { metric: string; target: number; current: number; progress: number; onTrack: boolean }[];
    commissions: { total: number; pending: number; approved: number; paid: number; details: any[] };
    dailyActivity: { date: string; count: number }[];
    prospectPortfolio: { status: string; count: number }[];
    recentActivities: any[];
}

export default function SalesTrackingDashboard() {
    const [activeTab, setActiveTab] = useState<'leaderboard' | 'performance' | 'objectives' | 'commissions'>('leaderboard');
    const [period, setPeriod] = useState(() => {
        const now = new Date();
        return `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}`;
    });
    const [sortMetric, setSortMetric] = useState('compositeScore');
    const [leaderboard, setLeaderboard] = useState<LeaderboardEntry[]>([]);
    const [myPerformance, setMyPerformance] = useState<UserPerformance | null>(null);
    const [loading, setLoading] = useState(true);
    const [selectedUserId, setSelectedUserId] = useState<string | null>(null);
    const [selectedUserPerf, setSelectedUserPerf] = useState<UserPerformance | null>(null);

    const fetchData = useCallback(async () => {
        setLoading(true);
        try {
            const [lbRes, perfRes] = await Promise.all([
                fetch(`${API_URL}/sales-tracking/leaderboard?period=${period}&metric=${sortMetric}`, { headers: getHeaders() }),
                fetch(`${API_URL}/sales-tracking/performance/me?period=${period}`, { headers: getHeaders() }),
            ]);
            if (lbRes.ok) setLeaderboard(await lbRes.json());
            if (perfRes.ok) setMyPerformance(await perfRes.json());
        } catch (err) {
            console.error('[SalesTracking] Error:', err);
        } finally {
            setLoading(false);
        }
    }, [period, sortMetric]);

    useEffect(() => { fetchData(); }, [fetchData]);

    const loadUserPerformance = async (userId: string) => {
        setSelectedUserId(userId);
        try {
            const res = await fetch(`${API_URL}/sales-tracking/performance/${userId}?period=${period}`, { headers: getHeaders() });
            if (res.ok) setSelectedUserPerf(await res.json());
        } catch { /* ignore */ }
    };

    const recalculateObjectives = async () => {
        try {
            await fetch(`${API_URL}/sales-tracking/objectives/recalculate`, {
                method: 'POST',
                headers: getHeaders(),
            });
            fetchData();
        } catch { /* ignore */ }
    };

    const metricLabels: Record<string, string> = {
        CALLS: 'Appels', MEETINGS_BOOKED: 'RDV fixés', CONVERSIONS: 'Conversions',
        QUALIFIED: 'Qualifiés', REVENUE: 'Chiffre d\'affaires',
    };

    const statusLabels: Record<string, { label: string; color: string }> = {
        NEW: { label: 'Nouveau', color: 'bg-blue-100 text-blue-700' },
        CONTACTED: { label: 'Contacté', color: 'bg-indigo-100 text-indigo-700' },
        QUALIFIED: { label: 'Qualifié', color: 'bg-emerald-100 text-emerald-700' },
        MEETING_BOOKED: { label: 'RDV fixé', color: 'bg-violet-100 text-violet-700' },
        NO_SHOW: { label: 'Absent', color: 'bg-amber-100 text-amber-700' },
        SIGNED: { label: 'Signé', color: 'bg-emerald-100 text-emerald-800' },
        LOST: { label: 'Perdu', color: 'bg-slate-100 text-slate-600' },
    };

    const activityIcons: Record<string, string> = {
        CALL: '📞', EMAIL: '📧', WHATSAPP: '💬', MEETING: '📅',
        NOTE: '📝', FOLLOW_UP: '⏰', QUALIFICATION: '✅', CONVERSION: '🎉',
    };

    const rankBadge = (rank: number) => {
        if (rank === 1) return <div className="w-8 h-8 rounded-full bg-gradient-to-br from-yellow-400 to-amber-500 flex items-center justify-center shadow-lg shadow-amber-200"><Trophy size={14} className="text-white" /></div>;
        if (rank === 2) return <div className="w-8 h-8 rounded-full bg-gradient-to-br from-slate-300 to-slate-400 flex items-center justify-center"><Trophy size={14} className="text-white" /></div>;
        if (rank === 3) return <div className="w-8 h-8 rounded-full bg-gradient-to-br from-orange-400 to-orange-500 flex items-center justify-center"><Trophy size={14} className="text-white" /></div>;
        return <div className="w-8 h-8 rounded-full bg-slate-100 flex items-center justify-center text-sm font-black text-slate-500">{rank}</div>;
    };

    const progressBar = (value: number, color = 'indigo') => (
        <div className="w-full h-2 bg-slate-100 rounded-full overflow-hidden">
            <div
                className={`h-full rounded-full transition-all duration-700 ${value >= 100 ? 'bg-gradient-to-r from-emerald-400 to-emerald-500' :
                        value >= 80 ? `bg-gradient-to-r from-${color}-400 to-${color}-500` :
                            value >= 50 ? `bg-gradient-to-r from-amber-400 to-amber-500` :
                                'bg-gradient-to-r from-red-400 to-red-500'
                    }`}
                style={{ width: `${Math.min(100, value)}%` }}
            />
        </div>
    );

    return (
        <div className="min-h-screen bg-slate-50">
            {/* Header */}
            <div className="bg-white border-b border-slate-200 px-6 py-5">
                <div className="flex items-center justify-between">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900 flex items-center gap-3">
                            <div className="w-10 h-10 rounded-2xl bg-gradient-to-br from-indigo-600 to-violet-600 flex items-center justify-center">
                                <BarChart2 size={20} className="text-white" />
                            </div>
                            Suivi Commercial
                        </h1>
                        <p className="text-sm text-slate-500 mt-1 ml-[52px]">Performance, objectifs et commissions de l'équipe</p>
                    </div>
                    <div className="flex items-center gap-3">
                        <select
                            value={period}
                            onChange={e => setPeriod(e.target.value)}
                            className="px-4 py-2 border border-slate-200 rounded-xl text-sm font-bold text-slate-700 bg-white focus:outline-none focus:ring-2 focus:ring-indigo-200"
                        >
                            {Array.from({ length: 6 }, (_, i) => {
                                const d = new Date();
                                d.setMonth(d.getMonth() - i);
                                const val = `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}`;
                                const label = d.toLocaleDateString('fr-FR', { month: 'long', year: 'numeric' });
                                return <option key={val} value={val}>{label}</option>;
                            })}
                        </select>
                        <button
                            onClick={recalculateObjectives}
                            className="p-2 rounded-xl border border-slate-200 text-slate-500 hover:bg-slate-50 transition-all"
                            title="Recalculer les objectifs"
                        >
                            <RefreshCw size={16} />
                        </button>
                    </div>
                </div>

                {/* Tabs */}
                <div className="flex gap-1 mt-5 ml-[52px]">
                    {[
                        { id: 'leaderboard' as const, label: '🏆 Classement', icon: Trophy },
                        { id: 'performance' as const, label: '📊 Ma performance', icon: TrendingUp },
                        { id: 'objectives' as const, label: '🎯 Objectifs', icon: Target },
                        { id: 'commissions' as const, label: '💰 Commissions', icon: DollarSign },
                    ].map(tab => (
                        <button
                            key={tab.id}
                            onClick={() => setActiveTab(tab.id)}
                            className={`px-4 py-2 rounded-xl text-sm font-bold transition-all ${activeTab === tab.id
                                    ? 'bg-indigo-600 text-white shadow-lg shadow-indigo-200'
                                    : 'text-slate-500 hover:bg-slate-100'
                                }`}
                        >
                            {tab.label}
                        </button>
                    ))}
                </div>
            </div>

            <div className="max-w-7xl mx-auto px-6 py-6">
                {loading ? (
                    <div className="flex items-center justify-center py-32">
                        <div className="animate-spin w-8 h-8 border-3 border-indigo-600 border-t-transparent rounded-full" />
                    </div>
                ) : (
                    <>
                        {/* ═══════════════════════════ LEADERBOARD ═══════════════════════════ */}
                        {activeTab === 'leaderboard' && (
                            <div>
                                {/* Sort controls */}
                                <div className="flex items-center gap-2 mb-6">
                                    <span className="text-sm font-bold text-slate-500">Trier par :</span>
                                    {[
                                        { id: 'compositeScore', label: 'Score global' },
                                        { id: 'CONVERSIONS', label: 'Conversions' },
                                        { id: 'CALLS', label: 'Appels' },
                                        { id: 'REVENUE', label: 'CA généré' },
                                    ].map(s => (
                                        <button
                                            key={s.id}
                                            onClick={() => setSortMetric(s.id)}
                                            className={`px-3 py-1.5 rounded-lg text-xs font-bold transition-all ${sortMetric === s.id ? 'bg-indigo-100 text-indigo-700' : 'bg-white text-slate-500 hover:bg-slate-50'
                                                }`}
                                        >
                                            {s.label}
                                        </button>
                                    ))}
                                </div>

                                {/* Leaderboard Table */}
                                <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
                                    <div className="grid grid-cols-[60px_1fr_100px_100px_100px_100px_120px_100px] px-4 py-3 bg-slate-50 border-b border-slate-200 text-xs font-bold text-slate-500 uppercase tracking-wider">
                                        <div>#</div>
                                        <div>Commercial</div>
                                        <div className="text-center">📞 Appels</div>
                                        <div className="text-center">✅ Qualifiés</div>
                                        <div className="text-center">📅 RDV</div>
                                        <div className="text-center">🎉 Convert.</div>
                                        <div className="text-right">💰 CA</div>
                                        <div className="text-center">🎯 Obj.</div>
                                    </div>
                                    {leaderboard.length === 0 ? (
                                        <div className="px-4 py-12 text-center text-slate-400 text-sm">
                                            Aucune donnée pour cette période. Commencez à enregistrer des activités !
                                        </div>
                                    ) : leaderboard.map((entry) => (
                                        <div
                                            key={entry.user.id}
                                            onClick={() => loadUserPerformance(entry.user.id)}
                                            className={`grid grid-cols-[60px_1fr_100px_100px_100px_100px_120px_100px] px-4 py-3.5 items-center border-b border-slate-50 cursor-pointer transition-colors hover:bg-indigo-50/50 ${entry.rank <= 3 ? 'bg-amber-50/30' : ''
                                                } ${selectedUserId === entry.user.id ? 'bg-indigo-50 border-l-4 border-l-indigo-500' : ''}`}
                                        >
                                            <div>{rankBadge(entry.rank)}</div>
                                            <div>
                                                <div className="font-bold text-sm text-slate-900">{entry.user.name}</div>
                                                <div className="text-xs text-slate-400">{entry.user.agencyId || 'Siège'}</div>
                                            </div>
                                            <div className="text-center font-bold text-sm text-slate-700">{entry.metrics.calls}</div>
                                            <div className="text-center font-bold text-sm text-slate-700">{entry.metrics.qualified}</div>
                                            <div className="text-center font-bold text-sm text-slate-700">{entry.metrics.meetings}</div>
                                            <div className="text-center">
                                                <span className={`font-black text-sm ${entry.metrics.conversions > 0 ? 'text-emerald-600' : 'text-slate-400'}`}>
                                                    {entry.metrics.conversions}
                                                </span>
                                            </div>
                                            <div className="text-right font-bold text-sm text-slate-700">
                                                {entry.metrics.revenue > 0 ? `${entry.metrics.revenue.toLocaleString('fr-FR')} €` : '—'}
                                            </div>
                                            <div className="text-center">
                                                {entry.objectiveCompletion !== null ? (
                                                    <span className={`text-xs font-bold px-2 py-1 rounded-full ${entry.objectiveCompletion >= 100 ? 'bg-emerald-100 text-emerald-700' :
                                                            entry.objectiveCompletion >= 80 ? 'bg-blue-100 text-blue-700' :
                                                                entry.objectiveCompletion >= 50 ? 'bg-amber-100 text-amber-700' :
                                                                    'bg-red-100 text-red-700'
                                                        }`}>
                                                        {entry.objectiveCompletion}%
                                                    </span>
                                                ) : (
                                                    <span className="text-xs text-slate-300">—</span>
                                                )}
                                            </div>
                                        </div>
                                    ))}
                                </div>

                                {/* Selected user detail */}
                                {selectedUserPerf && (
                                    <div className="mt-6 bg-white rounded-2xl border border-slate-200 p-6 shadow-sm animate-in fade-in duration-300">
                                        <h3 className="text-lg font-black text-slate-900 mb-4">
                                            📊 Détail — {leaderboard.find(e => e.user.id === selectedUserId)?.user.name}
                                        </h3>

                                        {/* KPIs */}
                                        <div className="grid grid-cols-5 gap-4 mb-6">
                                            {[
                                                { label: 'Activités', value: selectedUserPerf.summary.totalActivities, icon: '📊', color: 'indigo' },
                                                { label: 'Durée appels', value: `${Math.floor(selectedUserPerf.summary.totalCallDuration / 60)}min`, icon: '⏱️', color: 'blue' },
                                                { label: 'Moy. appel', value: `${Math.floor(selectedUserPerf.summary.avgCallDuration / 60)}:${(selectedUserPerf.summary.avgCallDuration % 60).toString().padStart(2, '0')}`, icon: '📞', color: 'violet' },
                                                { label: 'Commissions', value: `${selectedUserPerf.commissions.total.toFixed(0)}€`, icon: '💰', color: 'emerald' },
                                                { label: 'Prospects', value: selectedUserPerf.prospectPortfolio.reduce((s, p) => s + p.count, 0), icon: '👥', color: 'amber' },
                                            ].map((kpi, i) => (
                                                <div key={i} className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                                                    <div className="text-xs font-bold text-slate-400 mb-1">{kpi.icon} {kpi.label}</div>
                                                    <div className="text-xl font-black text-slate-900">{kpi.value}</div>
                                                </div>
                                            ))}
                                        </div>

                                        {/* Activités récentes */}
                                        <h4 className="text-sm font-bold text-slate-700 mb-3">Activités récentes</h4>
                                        <div className="space-y-1.5 max-h-60 overflow-y-auto">
                                            {selectedUserPerf.recentActivities.slice(0, 15).map((a: any, i: number) => (
                                                <div key={i} className="flex items-center gap-3 px-3 py-2 bg-slate-50 rounded-lg text-sm">
                                                    <span className="text-lg">{activityIcons[a.activityType] || '📌'}</span>
                                                    <span className="font-bold text-slate-700 flex-1">{a.prospectName || a.activityType}</span>
                                                    {a.outcome && <span className="text-xs px-2 py-0.5 bg-white rounded-full text-slate-500 border">{a.outcome}</span>}
                                                    <span className="text-xs text-slate-400">{new Date(a.date).toLocaleDateString('fr-FR')}</span>
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                )}
                            </div>
                        )}

                        {/* ═══════════════════════════ MA PERFORMANCE ═══════════════════════════ */}
                        {activeTab === 'performance' && myPerformance && (
                            <div className="space-y-6">
                                {/* KPI Cards */}
                                <div className="grid grid-cols-4 gap-4">
                                    {[
                                        { label: 'Total activités', value: myPerformance.summary.totalActivities, delta: '+12%', positive: true, icon: TrendingUp, gradient: 'from-indigo-500 to-violet-500' },
                                        { label: 'Appels passés', value: myPerformance.summary.activityBreakdown.CALL || 0, delta: '', positive: true, icon: Phone, gradient: 'from-blue-500 to-indigo-500' },
                                        { label: 'RDV fixés', value: myPerformance.summary.activityBreakdown.MEETING || 0, delta: '', positive: true, icon: Calendar, gradient: 'from-violet-500 to-purple-500' },
                                        { label: 'Commissions', value: `${myPerformance.commissions.total.toFixed(0)} €`, delta: '', positive: true, icon: DollarSign, gradient: 'from-emerald-500 to-teal-500' },
                                    ].map((kpi, i) => (
                                        <div key={i} className="bg-white rounded-2xl border border-slate-200 p-5 shadow-sm">
                                            <div className="flex items-center justify-between mb-3">
                                                <span className="text-xs font-bold text-slate-400 uppercase tracking-wider">{kpi.label}</span>
                                                <div className={`w-9 h-9 rounded-xl bg-gradient-to-br ${kpi.gradient} flex items-center justify-center`}>
                                                    <kpi.icon size={16} className="text-white" />
                                                </div>
                                            </div>
                                            <div className="text-2xl font-black text-slate-900">{kpi.value}</div>
                                        </div>
                                    ))}
                                </div>

                                {/* Activity Chart (simple bars) */}
                                <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                                    <h3 className="text-sm font-bold text-slate-700 mb-4">📈 Activité quotidienne</h3>
                                    <div className="flex items-end gap-1 h-32">
                                        {myPerformance.dailyActivity.map((d, i) => {
                                            const maxCount = Math.max(...myPerformance.dailyActivity.map(x => x.count), 1);
                                            const height = (d.count / maxCount) * 100;
                                            return (
                                                <div key={i} className="flex-1 flex flex-col items-center gap-1">
                                                    <span className="text-[10px] font-bold text-slate-400">{d.count > 0 ? d.count : ''}</span>
                                                    <div
                                                        className="w-full bg-gradient-to-t from-indigo-500 to-indigo-400 rounded-t-lg transition-all duration-500 min-h-[2px]"
                                                        style={{ height: `${Math.max(2, height)}%` }}
                                                    />
                                                    <span className="text-[9px] text-slate-400 rotate-[-45deg] origin-top-left mt-1">
                                                        {d.date.slice(8)}
                                                    </span>
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>

                                {/* Portfolio & Activity grid */}
                                <div className="grid grid-cols-2 gap-6">
                                    {/* Portfolio */}
                                    <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                                        <h3 className="text-sm font-bold text-slate-700 mb-4">👥 Mon portefeuille</h3>
                                        <div className="space-y-2">
                                            {myPerformance.prospectPortfolio.map((p, i) => {
                                                const info = statusLabels[p.status] || { label: p.status, color: 'bg-slate-100 text-slate-600' };
                                                return (
                                                    <div key={i} className="flex items-center justify-between py-2 px-3 bg-slate-50 rounded-xl">
                                                        <span className={`text-xs font-bold px-2.5 py-1 rounded-lg ${info.color}`}>{info.label}</span>
                                                        <span className="text-sm font-black text-slate-900">{p.count}</span>
                                                    </div>
                                                );
                                            })}
                                        </div>
                                    </div>

                                    {/* Activity breakdown */}
                                    <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                                        <h3 className="text-sm font-bold text-slate-700 mb-4">📊 Répartition activités</h3>
                                        <div className="space-y-2">
                                            {Object.entries(myPerformance.summary.activityBreakdown).sort(([, a], [, b]) => (b as number) - (a as number)).map(([type, count]) => (
                                                <div key={type} className="flex items-center gap-3 py-2 px-3 bg-slate-50 rounded-xl">
                                                    <span className="text-lg">{activityIcons[type] || '📌'}</span>
                                                    <span className="text-sm font-bold text-slate-700 flex-1">{type}</span>
                                                    <span className="text-sm font-black text-slate-900">{count as number}</span>
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                </div>
                            </div>
                        )}

                        {/* ═══════════════════════════ OBJECTIFS ═══════════════════════════ */}
                        {activeTab === 'objectives' && myPerformance && (
                            <div className="space-y-6">
                                <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                                    <div className="flex items-center justify-between mb-6">
                                        <h3 className="text-lg font-black text-slate-900">🎯 Mes objectifs — {period}</h3>
                                    </div>

                                    {myPerformance.objectives.length === 0 ? (
                                        <div className="text-center py-12">
                                            <Target size={40} className="text-slate-200 mx-auto mb-3" />
                                            <p className="text-sm text-slate-400">Aucun objectif défini pour cette période.</p>
                                            <p className="text-xs text-slate-300 mt-1">Contactez votre responsable pour définir vos objectifs.</p>
                                        </div>
                                    ) : (
                                        <div className="grid grid-cols-2 gap-4">
                                            {myPerformance.objectives.map((obj, i) => (
                                                <div key={i} className="bg-slate-50 rounded-2xl p-5 border border-slate-100">
                                                    <div className="flex items-center justify-between mb-3">
                                                        <span className="text-sm font-bold text-slate-700">{metricLabels[obj.metric] || obj.metric}</span>
                                                        <span className={`text-xs font-bold px-2.5 py-1 rounded-full flex items-center gap-1.5 ${obj.onTrack ? 'bg-emerald-100 text-emerald-700' : 'bg-red-100 text-red-700'
                                                            }`}>
                                                            {obj.onTrack ? <CheckCircle size={12} /> : <AlertTriangle size={12} />}
                                                            {obj.onTrack ? 'En bonne voie' : 'En retard'}
                                                        </span>
                                                    </div>
                                                    <div className="flex items-end gap-2 mb-3">
                                                        <span className="text-3xl font-black text-slate-900">{obj.current}</span>
                                                        <span className="text-sm text-slate-400 mb-1">/ {obj.target}</span>
                                                    </div>
                                                    {progressBar(obj.progress)}
                                                    <div className="text-right mt-1.5">
                                                        <span className="text-xs font-bold text-slate-400">{obj.progress}%</span>
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    )}
                                </div>
                            </div>
                        )}

                        {/* ═══════════════════════════ COMMISSIONS ═══════════════════════════ */}
                        {activeTab === 'commissions' && myPerformance && (
                            <div className="space-y-6">
                                {/* Commission summary cards */}
                                <div className="grid grid-cols-4 gap-4">
                                    {[
                                        { label: 'Total', value: myPerformance.commissions.total, color: 'from-indigo-500 to-violet-500', icon: '💰' },
                                        { label: 'En attente', value: myPerformance.commissions.pending, color: 'from-amber-400 to-orange-400', icon: '⏳' },
                                        { label: 'Approuvées', value: myPerformance.commissions.approved, color: 'from-blue-400 to-indigo-400', icon: '✅' },
                                        { label: 'Versées', value: myPerformance.commissions.paid, color: 'from-emerald-400 to-teal-400', icon: '🏦' },
                                    ].map((card, i) => (
                                        <div key={i} className="bg-white rounded-2xl border border-slate-200 p-5 shadow-sm">
                                            <div className="text-xs font-bold text-slate-400 uppercase tracking-wider mb-2">{card.icon} {card.label}</div>
                                            <div className="text-2xl font-black text-slate-900">{card.value.toFixed(2)} €</div>
                                        </div>
                                    ))}
                                </div>

                                {/* Commission details */}
                                <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
                                    <div className="px-6 py-4 border-b border-slate-100">
                                        <h3 className="text-sm font-bold text-slate-700">📋 Détail des commissions</h3>
                                    </div>
                                    {myPerformance.commissions.details.length === 0 ? (
                                        <div className="px-6 py-12 text-center text-slate-400 text-sm">
                                            Aucune commission pour cette période.
                                        </div>
                                    ) : (
                                        <div className="divide-y divide-slate-50">
                                            {myPerformance.commissions.details.map((c: any, i: number) => (
                                                <div key={i} className="px-6 py-4 flex items-center gap-4">
                                                    <div className={`w-3 h-3 rounded-full ${c.status === 'PAID' ? 'bg-emerald-500' :
                                                            c.status === 'APPROVED' ? 'bg-blue-500' :
                                                                c.status === 'CANCELLED' ? 'bg-red-500' :
                                                                    'bg-amber-400'
                                                        }`} />
                                                    <div className="flex-1">
                                                        <div className="text-sm font-bold text-slate-900">{c.prospectName || c.serviceName || 'Commission'}</div>
                                                        <div className="text-xs text-slate-400">{c.serviceName} • {c.rate}% de {c.baseAmount}€</div>
                                                    </div>
                                                    <span className={`text-xs font-bold px-2.5 py-1 rounded-full ${c.status === 'PAID' ? 'bg-emerald-100 text-emerald-700' :
                                                            c.status === 'APPROVED' ? 'bg-blue-100 text-blue-700' :
                                                                c.status === 'CANCELLED' ? 'bg-red-100 text-red-700' :
                                                                    'bg-amber-100 text-amber-700'
                                                        }`}>
                                                        {c.status === 'PAID' ? 'Versée' : c.status === 'APPROVED' ? 'Approuvée' : c.status === 'CANCELLED' ? 'Annulée' : 'En attente'}
                                                    </span>
                                                    <div className="text-right">
                                                        <div className="text-sm font-black text-slate-900">{c.amountEuros.toFixed(2)} €</div>
                                                        <div className="text-xs text-slate-400">{new Date(c.createdAt).toLocaleDateString('fr-FR')}</div>
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    )}
                                </div>
                            </div>
                        )}
                    </>
                )}
            </div>
        </div>
    );
}
