'use client';

import React, { useState } from 'react';
import {
    Sparkles,
    Send,
    BarChart3,
    PieChart,
    LineChart,
    Table as TableIcon,
    Loader2
} from 'lucide-react';
import {
    BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer,
    PieChart as RechartsPieChart, Pie, Cell, LineChart as RechartsLineChart, Line
} from 'recharts';

interface WidgetData {
    type: 'BAR_CHART' | 'PIE_CHART' | 'LINE_CHART' | 'KPI_CARD' | 'TABLE';
    title: string;
    description: string;
    data: any;
}

const COLORS = ['#4f46e5', '#10b981', '#f59e0b', '#ef4444', '#3b82f6', '#8b5cf6', '#ec4899'];

export default function AiReportingChat() {
    const [prompt, setPrompt] = useState('');
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [widget, setWidget] = useState<WidgetData | null>(null);
    const [debugSql, setDebugSql] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!prompt.trim()) return;

        setIsLoading(true);
        setError(null);
        setWidget(null);
        setDebugSql(null);

        try {
            const token = localStorage.getItem('admin_token'); // Or import AuthStore
            const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

            const res = await fetch(`${API}/ai-reporting/query`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${token}`
                },
                body: JSON.stringify({ prompt })
            });

            if (!res.ok) {
                const data = await res.json();
                throw new Error(data.message || 'Erreur lors de la génération du widget');
            }

            const data = await res.json();
            setWidget(data.widget);
            if (data.debug?.sql) setDebugSql(data.debug.sql);

        } catch (err: any) {
            setError(err.message || 'Une erreur inattendue est survenue.');
        } finally {
            setIsLoading(false);
        }
    };

    const renderWidgetContent = () => {
        if (!widget || !widget.data) return null;

        if (widget.type === 'KPI_CARD') {
            return (
                <div className="flex items-center justify-center p-12">
                    <span className="text-6xl font-black text-indigo-600">
                        {typeof widget.data === 'number' ? widget.data.toLocaleString() : widget.data}
                    </span>
                </div>
            );
        }

        if (widget.type === 'BAR_CHART') {
            return (
                <div className="h-72 w-full mt-6">
                    <ResponsiveContainer width="100%" height="100%">
                        <BarChart data={widget.data}>
                            <CartesianGrid strokeDasharray="3 3" vertical={false} stroke="#e2e8f0" />
                            <XAxis dataKey="label" axisLine={false} tickLine={false} tick={{ fill: '#64748b', fontSize: 12 }} />
                            <YAxis axisLine={false} tickLine={false} tick={{ fill: '#64748b', fontSize: 12 }} />
                            <Tooltip
                                cursor={{ fill: '#f1f5f9' }}
                                contentStyle={{ borderRadius: '12px', border: 'none', boxShadow: '0 10px 15px -3px rgb(0 0 0 / 0.1)' }}
                            />
                            <Bar dataKey="value" fill="#4f46e5" radius={[4, 4, 0, 0]} />
                        </BarChart>
                    </ResponsiveContainer>
                </div>
            );
        }

        if (widget.type === 'PIE_CHART') {
            return (
                <div className="h-72 w-full mt-6 flex justify-center">
                    <ResponsiveContainer width="100%" height="100%">
                        <RechartsPieChart>
                            <Pie
                                data={widget.data}
                                cx="50%"
                                cy="50%"
                                innerRadius={60}
                                outerRadius={100}
                                paddingAngle={5}
                                dataKey="value"
                                nameKey="label"
                            >
                                {widget.data.map((entry: any, index: number) => (
                                    <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                                ))}
                            </Pie>
                            <Tooltip contentStyle={{ borderRadius: '12px', border: 'none', boxShadow: '0 10px 15px -3px rgb(0 0 0 / 0.1)' }} />
                        </RechartsPieChart>
                    </ResponsiveContainer>
                    <div className="flex flex-col justify-center ml-8 gap-3">
                        {widget.data.map((entry: any, index: number) => (
                            <div key={`legend-${index}`} className="flex items-center gap-2 text-sm font-medium text-slate-600">
                                <div className="w-3 h-3 rounded-full" style={{ backgroundColor: COLORS[index % COLORS.length] }} />
                                {entry.label} ({entry.value})
                            </div>
                        ))}
                    </div>
                </div>
            );
        }

        if (widget.type === 'TABLE' && Array.isArray(widget.data) && widget.data.length > 0) {
            const columns = Object.keys(widget.data[0]);
            return (
                <div className="mt-6 overflow-x-auto rounded-xl border border-slate-200">
                    <table className="w-full text-sm text-left text-slate-600">
                        <thead className="text-xs uppercase bg-slate-50 text-slate-500 font-bold">
                            <tr>
                                {columns.map(col => <th key={col} className="px-6 py-3">{col}</th>)}
                            </tr>
                        </thead>
                        <tbody>
                            {widget.data.map((row, i) => (
                                <tr key={i} className="bg-white border-b border-slate-100 hover:bg-slate-50">
                                    {columns.map(col => <td key={`${i}-${col}`} className="px-6 py-4">{row[col]?.toString() || '-'}</td>)}
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            );
        }

        return <div className="p-8 text-center text-slate-500">Visualisation non supportée</div>;
    };

    return (
        <div className="bg-white rounded-[2rem] border border-slate-200 p-8 shadow-sm mb-8 transform transition-all">
            <div className="flex items-center gap-3 mb-6 relative">
                <div className="w-12 h-12 rounded-2xl bg-indigo-50 flex items-center justify-center text-indigo-600 absolute -top-4 -left-4 shadow-lg shadow-indigo-100 border border-indigo-100/50">
                    <Sparkles className="w-6 h-6" />
                </div>
                <div className="pl-12">
                    <h2 className="text-2xl font-black text-slate-900 tracking-tight">Agent BI Analytics</h2>
                    <p className="text-slate-500 text-sm font-medium">Demandez à votre tableau de bord en langage naturel</p>
                </div>
            </div>

            <form onSubmit={handleSubmit} className="relative mb-6">
                <input
                    type="text"
                    value={prompt}
                    onChange={(e) => setPrompt(e.target.value)}
                    placeholder="Ex: Quel est le chiffre d'affaires généré par l'agence de Paris ce mois-ci ?"
                    className="w-full pl-6 pr-14 py-4 rounded-2xl bg-slate-50 border border-slate-200 text-slate-900 font-medium placeholder:text-slate-400 focus:outline-none focus:ring-4 focus:ring-indigo-500/10 focus:border-indigo-500 transition-all"
                    disabled={isLoading}
                />
                <button
                    type="submit"
                    disabled={isLoading || !prompt.trim()}
                    className="absolute right-2 top-2 bottom-2 aspect-square bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl flex items-center justify-center transition-colors disabled:opacity-50 disabled:hover:bg-indigo-600"
                >
                    {isLoading ? <Loader2 className="w-5 h-5 animate-spin" /> : <Send className="w-5 h-5" />}
                </button>
            </form>

            {error && (
                <div className="mb-6 p-4 rounded-xl bg-red-50 text-red-700 font-medium text-sm flex items-center gap-2 border border-red-100">
                    <div className="w-2 h-2 rounded-full bg-red-500" />
                    {error}
                </div>
            )}

            {widget && (
                <div className="p-6 rounded-2xl border border-slate-100 bg-slate-50/50 mt-8 relative group">
                    <div className="absolute top-4 right-4 flex gap-2 opacity-0 group-hover:opacity-100 transition-opacity">
                        <button className="px-3 py-1.5 text-xs font-bold bg-white text-indigo-600 hover:bg-indigo-50 rounded-lg shadow-sm border border-slate-200 transition-colors">
                            Épingler
                        </button>
                    </div>

                    <div className="flex items-start gap-4">
                        <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center text-slate-600 shrink-0 border border-slate-100">
                            {widget.type === 'BAR_CHART' && <BarChart3 className="w-5 h-5" />}
                            {widget.type === 'PIE_CHART' && <PieChart className="w-5 h-5" />}
                            {widget.type === 'LINE_CHART' && <LineChart className="w-5 h-5" />}
                            {widget.type === 'TABLE' && <TableIcon className="w-5 h-5" />}
                            {widget.type === 'KPI_CARD' && <Sparkles className="w-5 h-5" />}
                        </div>
                        <div>
                            <h3 className="text-lg font-bold text-slate-900">{widget.title}</h3>
                            <p className="text-sm text-slate-500 font-medium">{widget.description}</p>
                        </div>
                    </div>

                    {renderWidgetContent()}

                    {/* Hack debug SQL - Only local */}
                    {debugSql && process.env.NODE_ENV === 'development' && (
                        <div className="mt-8 pt-4 border-t border-slate-200">
                            <details className="text-xs text-slate-400">
                                <summary className="cursor-pointer hover:text-slate-600">Voir la requête SQL générée (Debug)</summary>
                                <pre className="mt-2 p-3 rounded-lg bg-slate-800 text-slate-300 font-mono text-[10px] overflow-x-auto">
                                    {debugSql}
                                </pre>
                            </details>
                        </div>
                    )}
                </div>
            )}
        </div>
    );
}
