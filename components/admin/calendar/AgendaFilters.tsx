import React, { useState, useEffect } from 'react';
import { Calendar, Users, MapPin, Tag, Search, Filter, X, ChevronDown } from 'lucide-react';
import { ServiceConfigStore } from '../../../services/ServiceConfigStore';
import { AgencyStore, AgencyExt } from '../../../services/AgencyStore';

export interface AgendaFilterState {
    period: 'TODAY' | 'WEEK' | 'MONTH';
    viewMode: 'JURIST' | 'AGENCY';
    services: string[];
    statuses: string[];
    types: ('VISIO_JURISTE' | 'PHYSICAL_AGENCY')[];
    search: string;
    specialFilters?: string[]; // 'MISSING_DOCS' | 'UNCONFIRMED' | 'CONFLICTS'
}

interface AgendaFiltersProps {
    filters: AgendaFilterState;
    onFilterChange: (filters: AgendaFilterState) => void;
}

export const AgendaFilters: React.FC<AgendaFiltersProps> = ({ filters, onFilterChange }) => {
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [showAdvanced, setShowAdvanced] = useState(false);

    const servicesList = ServiceConfigStore.getAllServices();
    const statusOptions = [
        { id: 'SCHEDULED', label: 'Planifié', color: 'bg-blue-500' },
        { id: 'COMPLETED', label: 'Terminé', color: 'bg-green-500' },
        { id: 'CANCELLED', label: 'Annulé', color: 'bg-red-500' },
        { id: 'NO_SHOW', label: 'No-Show', color: 'bg-orange-500' },
    ];

    useEffect(() => {
        AgencyStore.getAllAgencies().then(setAgencies);
    }, []);

    const toggleService = (serviceId: string) => {
        const newServices = filters.services.includes(serviceId)
            ? filters.services.filter(id => id !== serviceId)
            : [...filters.services, serviceId];
        onFilterChange({ ...filters, services: newServices });
    };

    const toggleStatus = (statusId: string) => {
        const newStatuses = filters.statuses.includes(statusId)
            ? filters.statuses.filter(id => id !== statusId)
            : [...filters.statuses, statusId];
        onFilterChange({ ...filters, statuses: newStatuses });
    };

    return (
        <div className="bg-white rounded-3xl shadow-sm border border-slate-100 p-4 mb-6 space-y-4">
            {/* Primary Bar */}
            <div className="flex flex-wrap items-center justify-between gap-4">
                <div className="flex items-center gap-2 bg-slate-50 p-1 rounded-2xl border border-slate-100">
                    {(['TODAY', 'WEEK', 'MONTH'] as const).map(p => (
                        <button
                            key={p}
                            onClick={() => onFilterChange({ ...filters, period: p })}
                            className={`px-4 py-2 text-xs font-black rounded-xl transition-all ${filters.period === p ? 'bg-white shadow-sm text-indigo-600' : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            {p === 'TODAY' ? "Aujourd'hui" : p === 'WEEK' ? 'Semaine' : 'Mois'}
                        </button>
                    ))}
                </div>

                <div className="flex items-center gap-2 bg-slate-50 p-1 rounded-2xl border border-slate-100">
                    {(['JURIST', 'AGENCY'] as const).map(m => (
                        <button
                            key={m}
                            onClick={() => onFilterChange({ ...filters, viewMode: m })}
                            className={`flex items-center gap-2 px-4 py-2 text-xs font-black rounded-xl transition-all ${filters.viewMode === m ? 'bg-white shadow-sm text-indigo-600' : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            {m === 'JURIST' ? <Users size={14} /> : <MapPin size={14} />}
                            {m === 'JURIST' ? 'Par Juriste' : 'Par Agence'}
                        </button>
                    ))}
                </div>

                <div className="flex-1 min-w-[200px] relative">
                    <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                    <input
                        type="text"
                        placeholder="Rechercher un client ou un RDV..."
                        className="w-full pl-12 pr-4 py-3 bg-slate-50 border border-slate-100 rounded-2xl text-sm font-bold text-slate-900 focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                        value={filters.search}
                        onChange={(e) => onFilterChange({ ...filters, search: e.target.value })}
                    />
                </div>

                <button
                    onClick={() => setShowAdvanced(!showAdvanced)}
                    className={`flex items-center gap-2 px-6 py-3 rounded-2xl text-sm font-black transition-all ${showAdvanced ? 'bg-indigo-600 text-white shadow-indigo-200' : 'bg-slate-900 text-white hover:bg-slate-800 shadow-slate-200'
                        } shadow-lg`}
                >
                    <Filter size={18} />
                    {showAdvanced ? 'Masquer Filtres' : 'Filtres Avancés'}
                </button>
            </div>

            {/* Advanced Filters Panel */}
            {showAdvanced && (
                <div className="grid grid-cols-1 md:grid-cols-3 gap-6 pt-4 border-t border-dashed border-slate-100 animate-in fade-in slide-in-from-top-2">
                    {/* Services */}
                    <div className="space-y-3">
                        <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                            <Tag size={12} /> Services
                        </h4>
                        <div className="flex flex-wrap gap-2 max-h-32 overflow-y-auto custom-scrollbar p-1">
                            {servicesList.map(s => (
                                <button
                                    key={s.id}
                                    onClick={() => toggleService(s.id)}
                                    className={`px-3 py-1.5 rounded-lg text-[10px] font-bold border transition-all ${filters.services.includes(s.id)
                                        ? 'bg-indigo-50 border-indigo-200 text-indigo-600'
                                        : 'bg-white border-slate-100 text-slate-500 hover:border-slate-300'
                                        }`}
                                >
                                    {s.name}
                                </button>
                            ))}
                        </div>
                    </div>

                    {/* Statuses */}
                    <div className="space-y-3">
                        <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                            <Filter size={12} /> Statuts
                        </h4>
                        <div className="flex flex-wrap gap-2">
                            {statusOptions.map(opt => (
                                <button
                                    key={opt.id}
                                    onClick={() => toggleStatus(opt.id)}
                                    className={`flex items-center gap-2 px-3 py-1.5 rounded-lg text-[10px] font-bold border transition-all ${filters.statuses.includes(opt.id)
                                        ? 'bg-white border-slate-900 text-slate-900'
                                        : 'bg-white border-slate-100 text-slate-500 hover:border-slate-300'
                                        }`}
                                >
                                    <span className={`w-1.5 h-1.5 rounded-full ${opt.color}`} />
                                    {opt.label}
                                </button>
                            ))}
                        </div>
                    </div>

                    {/* Types */}
                    <div className="space-y-3">
                        <h4 className="text-[10px] font-black uppercase tracking-widest text-slate-400 flex items-center gap-2">
                            <Calendar size={12} /> Format
                        </h4>
                        <div className="flex gap-2">
                            <button
                                onClick={() => {
                                    const newTypes = filters.types.includes('VISIO_JURISTE')
                                        ? filters.types.filter(t => t !== 'VISIO_JURISTE')
                                        : [...filters.types, 'VISIO_JURISTE' as const];
                                    onFilterChange({ ...filters, types: newTypes });
                                }}
                                className={`flex-1 flex flex-col items-center justify-center p-3 rounded-2xl border-2 transition-all ${filters.types.includes('VISIO_JURISTE') ? 'bg-indigo-50 border-indigo-500 text-indigo-700' : 'bg-white border-slate-100 text-slate-400'
                                    }`}
                            >
                                <Tag size={16} className="mb-1" />
                                <span className="text-[10px] font-black uppercase">Visio</span>
                            </button>
                            <button
                                onClick={() => {
                                    const newTypes = filters.types.includes('PHYSICAL_AGENCY')
                                        ? filters.types.filter(t => t !== 'PHYSICAL_AGENCY')
                                        : [...filters.types, 'PHYSICAL_AGENCY' as const];
                                    onFilterChange({ ...filters, types: newTypes });
                                }}
                                className={`flex-1 flex flex-col items-center justify-center p-3 rounded-2xl border-2 transition-all ${filters.types.includes('PHYSICAL_AGENCY') ? 'bg-emerald-50 border-emerald-500 text-emerald-700' : 'bg-white border-slate-100 text-slate-400'
                                    }`}
                            >
                                <MapPin size={16} className="mb-1" />
                                <span className="text-[10px] font-black uppercase">Agence</span>
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

export default AgendaFilters;
