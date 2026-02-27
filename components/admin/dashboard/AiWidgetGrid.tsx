'use client';

import React, { useEffect, useState } from 'react';
import { Sparkles, BarChart3, PieChart, LineChart, Table as TableIcon, Trash2 } from 'lucide-react';
import {
    BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer,
    PieChart as RechartsPieChart, Pie, Cell, LineChart as RechartsLineChart, Line
} from 'recharts';

const COLORS = ['#4f46e5', '#10b981', '#f59e0b', '#ef4444', '#3b82f6', '#8b5cf6', '#ec4899'];

export default function AiWidgetGrid() {
    const [widgets, setWidgets] = useState<any[]>([]);
    const [isLoading, setIsLoading] = useState(true);

    const loadWidgets = async () => {
        try {
            const token = localStorage.getItem('admin_token');
            const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
            const res = await fetch(`${API}/ai-reporting/widgets`, {
                headers: { 'Authorization': `Bearer ${token}` }
            });
            if (res.ok) {
                const data = await res.json();
                setWidgets(data);
            }
        } catch (err) {
            console.error(err);
        } finally {
            setIsLoading(false);
        }
    };

    useEffect(() => {
        loadWidgets();
    }, []);

    const handleDelete = async (id: string) => {
        if (!confirm('Voulez-vous vraiment supprimer ce widget ?')) return;
        try {
            const token = localStorage.getItem('admin_token');
            const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
            await fetch(`${API}/ai-reporting/widgets/${id}`, {
                method: 'DELETE',
                headers: { 'Authorization': `Bearer ${token}` }
            });
            loadWidgets();
        } catch (err) {
            console.error(err);
        }
    };

    if (isLoading) return <div className="text-center p-8 text-slate-400">Chargement des widgets...</div>;
    if (widgets.length === 0) return null;

    return (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-2 gap-6 mb-8">
            {widgets.map(widget => (
                <div key={widget.id} className="bg-white rounded-[2rem] border border-slate-200 p-8 shadow-sm relative group overflow-hidden">
                    <button
                        onClick={() => handleDelete(widget.id)}
                        className="absolute top-4 right-4 p-2 bg-red-50 text-red-500 rounded-xl opacity-0 group-hover:opacity-100 transition-opacity hover:bg-red-100"
                    >
                        <Trash2 size={16} />
                    </button>

                    <div className="flex items-start gap-4 mb-6">
                        <div className="w-10 h-10 rounded-xl bg-slate-50 flex items-center justify-center text-slate-600 shrink-0 border border-slate-100">
                            {widget.type === 'BAR_CHART' && <BarChart3 className="w-5 h-5 text-indigo-500" />}
                            {widget.type === 'PIE_CHART' && <PieChart className="w-5 h-5 text-emerald-500" />}
                            {widget.type === 'LINE_CHART' && <LineChart className="w-5 h-5 text-amber-500" />}
                            {widget.type === 'TABLE' && <TableIcon className="w-5 h-5 text-slate-500" />}
                            {widget.type === 'KPI_CARD' && <Sparkles className="w-5 h-5 text-indigo-600" />}
                        </div>
                        <div>
                            <h3 className="text-lg font-bold text-slate-900">{widget.title}</h3>
                            <p className="text-sm text-slate-500 font-medium">{widget.description}</p>
                        </div>
                    </div>

                    {widget.error ? (
                        <div className="p-4 bg-red-50 rounded-xl text-red-500 text-sm">{widget.description}</div>
                    ) : (
                        <div className="mt-4">
                            {/* RENDU DES WIDGETS */}
                            {widget.type === 'KPI_CARD' && (
                                <div className="flex items-center justify-center p-8">
                                    <span className="text-6xl font-black text-indigo-600">
                                        {typeof widget.data === 'number' ? widget.data.toLocaleString() : widget.data}
                                    </span>
                                </div>
                            )}

                            {widget.type === 'BAR_CHART' && (
                                <div className="h-64 w-full">
                                    <ResponsiveContainer width="100%" height="100%">
                                        <BarChart data={widget.data}>
                                            <CartesianGrid strokeDasharray="3 3" vertical={false} stroke="#e2e8f0" />
                                            <XAxis dataKey="label" axisLine={false} tickLine={false} tick={{ fill: '#64748b', fontSize: 12 }} />
                                            <YAxis axisLine={false} tickLine={false} tick={{ fill: '#64748b', fontSize: 12 }} />
                                            <Tooltip cursor={{ fill: '#f1f5f9' }} contentStyle={{ borderRadius: '12px', border: 'none', boxShadow: '0 10px 15px -3px rgb(0 0 0 / 0.1)' }} />
                                            <Bar dataKey="value" fill="#4f46e5" radius={[4, 4, 0, 0]} />
                                        </BarChart>
                                    </ResponsiveContainer>
                                </div>
                            )}

                            {widget.type === 'PIE_CHART' && (
                                <div className="h-64 w-full flex">
                                    <ResponsiveContainer width="100%" height="100%">
                                        <RechartsPieChart>
                                            <Pie data={widget.data} cx="50%" cy="50%" innerRadius={60} outerRadius={90} paddingAngle={5} dataKey="value" nameKey="label">
                                                {widget.data.map((entry: any, index: number) => (
                                                    <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                                                ))}
                                            </Pie>
                                            <Tooltip contentStyle={{ borderRadius: '12px', border: 'none', boxShadow: '0 10px 15px -3px rgb(0 0 0 / 0.1)' }} />
                                        </RechartsPieChart>
                                    </ResponsiveContainer>
                                </div>
                            )}

                            {widget.type === 'TABLE' && Array.isArray(widget.data) && widget.data.length > 0 && (
                                <div className="overflow-x-auto rounded-xl border border-slate-200 h-64 overflow-y-auto">
                                    <table className="w-full text-sm text-left text-slate-600">
                                        <thead className="text-xs uppercase bg-slate-50 text-slate-500 font-bold sticky top-0">
                                            <tr>{Object.keys(widget.data[0]).map(col => <th key={col} className="px-6 py-3">{col}</th>)}</tr>
                                        </thead>
                                        <tbody>
                                            {widget.data.map((row: any, i: number) => (
                                                <tr key={i} className="bg-white border-b border-slate-100 hover:bg-slate-50">
                                                    {Object.keys(widget.data[0]).map(col => <td key={`${i}-${col}`} className="px-6 py-4">{row[col]?.toString() || '-'}</td>)}
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </div>
                            )}
                        </div>
                    )}
                </div>
            ))}
        </div>
    );
}
