import React from 'react';

const COLUMNS = [
    { id: 'NEW', label: 'Nouveau', color: 'bg-amber-100 text-amber-800', icon: '🟡' },
    { id: 'CONTACTED', label: 'Contacté', color: 'bg-purple-100 text-purple-800', icon: '🟣' },
    { id: 'QUALIFIED', label: 'Qualifié', color: 'bg-cyan-100 text-cyan-800', icon: '🔵' },
    { id: 'MEETING_BOOKED', label: 'RDV Fixé', color: 'bg-indigo-100 text-indigo-800', icon: '📅' },
    { id: 'SIGNED', label: 'Signé', color: 'bg-emerald-100 text-emerald-800', icon: '✅' },
    { id: 'NO_SHOW', label: 'Non Honoré', color: 'bg-red-100 text-red-800', icon: '🚫' },
    { id: 'LOST', label: 'Perdu', color: 'bg-slate-100 text-slate-800', icon: '⚫' }
];

export default function PipelineVelocity({ velocityMetrics }: { velocityMetrics: any }) {
    if (!velocityMetrics) return null;

    return (
        <div className="space-y-6">
            <h2 className="text-lg font-black text-slate-800">📊 Vélocité du Pipeline</h2>

            {/* Alerts */}
            {(velocityMetrics.alerts?.staleLeads > 0 || velocityMetrics.alerts?.overdueCallbacks > 0) && (
                <div className="flex gap-3">
                    {velocityMetrics.alerts.staleLeads > 0 && (
                        <div className="flex-1 flex items-center gap-3 p-4 bg-red-50 border-2 border-red-200 rounded-xl">
                            <span className="text-2xl">🚨</span>
                            <div>
                                <p className="text-sm font-black text-red-800">{velocityMetrics.alerts.staleLeads} leads stagnants</p>
                                <p className="text-xs text-red-600">En NEW depuis &gt;48h sans appel</p>
                            </div>
                        </div>
                    )}
                    {velocityMetrics.alerts.overdueCallbacks > 0 && (
                        <div className="flex-1 flex items-center gap-3 p-4 bg-amber-50 border-2 border-amber-200 rounded-xl">
                            <span className="text-2xl">⏰</span>
                            <div>
                                <p className="text-sm font-black text-amber-800">{velocityMetrics.alerts.overdueCallbacks} rappels en retard</p>
                                <p className="text-xs text-amber-600">Rappels programmés non effectués</p>
                            </div>
                        </div>
                    )}
                </div>
            )}

            {/* Conversion Funnel */}
            <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                <h3 className="text-sm font-black text-slate-700 mb-4">Taux de Conversion</h3>
                <div className="flex items-center gap-2">
                    {[
                        { label: 'Nouveau → Contacté', value: velocityMetrics.conversionRates?.newToContacted, color: 'bg-purple-500' },
                        { label: 'Contacté → Qualifié', value: velocityMetrics.conversionRates?.contactedToQualified, color: 'bg-cyan-500' },
                        { label: 'Qualifié → RDV', value: velocityMetrics.conversionRates?.qualifiedToMeeting, color: 'bg-indigo-500' },
                        { label: 'RDV → Signé', value: velocityMetrics.conversionRates?.meetingToSigned, color: 'bg-emerald-500' },
                    ].map((step, i) => (
                        <div key={i} className="flex-1 flex flex-col items-center">
                            <div className="text-center mb-2">
                                <div className="text-2xl font-black text-slate-800">{step.value || 0}%</div>
                                <div className="text-[10px] text-slate-500 font-medium">{step.label}</div>
                            </div>
                            <div className="w-full bg-slate-100 rounded-full h-2">
                                <div className={`${step.color} h-2 rounded-full transition-all`} style={{ width: `${Math.min(step.value || 0, 100)}%` }} />
                            </div>
                            {i < 3 && <div className="text-slate-300 text-lg mt-1">→</div>}
                        </div>
                    ))}
                </div>
                <div className="mt-4 pt-4 border-t border-slate-100 flex items-center justify-between">
                    <span className="text-xs font-bold text-slate-500">Conversion globale</span>
                    <span className="text-lg font-black text-emerald-600">{velocityMetrics.conversionRates?.overall || 0}%</span>
                </div>
            </div>

            {/* Time in Stage + Status Distribution */}
            <div className="grid grid-cols-2 gap-4">
                <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                    <h3 className="text-sm font-black text-slate-700 mb-4">⏱ Temps moyen par étape</h3>
                    <div className="space-y-3">
                        {(['NEW', 'CONTACTED', 'QUALIFIED', 'MEETING_BOOKED'] as const).map(status => {
                            const hours = velocityMetrics.avgHoursInStage?.[status] || 0;
                            const labelMap = { NEW: 'Nouveau', CONTACTED: 'Contacté', QUALIFIED: 'Qualifié', MEETING_BOOKED: 'RDV Fixé' };
                            const label = labelMap[status];
                            const displayTime = hours >= 24 ? `${Math.round(hours / 24)}j` : `${hours}h`;
                            const isLong = (status === 'NEW' && hours > 48) || (status === 'CONTACTED' && hours > 72);
                            return (
                                <div key={status} className="flex items-center justify-between">
                                    <span className="text-xs font-medium text-slate-600">{label}</span>
                                    <span className={`text-sm font-black ${isLong ? 'text-red-600' : 'text-slate-800'}`}>
                                        {displayTime} {isLong && '⚠️'}
                                    </span>
                                </div>
                            );
                        })}
                    </div>
                </div>

                <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                    <h3 className="text-sm font-black text-slate-700 mb-4">📈 Répartition par statut</h3>
                    <div className="space-y-2">
                        {COLUMNS.map(col => {
                            const count = velocityMetrics.statusCounts?.[col.id] || 0;
                            const percent = velocityMetrics.totalProspects ? Math.round((count / velocityMetrics.totalProspects) * 100) : 0;
                            return (
                                <div key={col.id} className="flex items-center gap-2">
                                    <span className="text-xs w-24 font-medium text-slate-600">{col.icon} {col.label}</span>
                                    <div className="flex-1 bg-slate-100 rounded-full h-3">
                                        <div className={`${col.color.split(' ')[0]} h-3 rounded-full transition-all`} style={{ width: `${percent}%` }} />
                                    </div>
                                    <span className="text-xs font-black text-slate-700 w-10 text-right">{count}</span>
                                </div>
                            );
                        })}
                    </div>
                </div>
            </div>
        </div>
    );
}
