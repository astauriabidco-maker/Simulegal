'use client';

import React, { useState } from 'react';
import {
    Plus, Pencil, Trash2, GripVertical, Eye, EyeOff,
    Save, X, Search, ArrowUpDown, Zap, ChevronDown
} from 'lucide-react';
import {
    SERVICES_CATALOG, Service, ServicePole, ServiceType,
    SERVICE_POLES, SERVICE_TYPES, getActiveServices
} from '@/data/services';
import { resolveIcon, getAvailableIcons } from '@/lib/icon-resolver';

interface ServiceManagerTabProps {
    onNotify?: (msg: string, type?: 'success' | 'error') => void;
}

export default function ServiceManagerTab({ onNotify }: ServiceManagerTabProps) {
    const [services, setServices] = useState<Service[]>([...SERVICES_CATALOG]);
    const [editing, setEditing] = useState<Service | null>(null);
    const [isNew, setIsNew] = useState(false);
    const [searchQuery, setSearchQuery] = useState('');
    const [filterPole, setFilterPole] = useState<ServicePole | 'ALL'>('ALL');
    const [showIconPicker, setShowIconPicker] = useState(false);

    const filtered = services
        .filter(s => filterPole === 'ALL' || s.pole === filterPole)
        .filter(s =>
            s.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
            s.id.toLowerCase().includes(searchQuery.toLowerCase()) ||
            (s.keywords || []).some(k => k.toLowerCase().includes(searchQuery.toLowerCase()))
        )
        .sort((a, b) => {
            if (a.pole !== b.pole) {
                const order = ['PROCEDURES', 'EXPERTISE', 'INTEGRATION'];
                return order.indexOf(a.pole) - order.indexOf(b.pole);
            }
            return (a.sortOrder ?? 50) - (b.sortOrder ?? 50);
        });

    const handleNew = () => {
        const newService: Service = {
            id: '',
            title: '',
            description: '',
            pole: 'PROCEDURES',
            type: 'SIMULATION',
            isSimulatable: true,
            iconName: 'FileText',
            badge: 'SIMUL',
            isActive: true,
            sortOrder: 50,
            keywords: [],
        };
        setEditing(newService);
        setIsNew(true);
    };

    const handleEdit = (service: Service) => {
        setEditing({ ...service });
        setIsNew(false);
    };

    const handleSave = () => {
        if (!editing) return;
        if (!editing.id || !editing.title) {
            onNotify?.('L\'ID et le titre sont obligatoires.', 'error');
            return;
        }

        if (isNew) {
            if (services.some(s => s.id === editing.id)) {
                onNotify?.(`Un service avec l'ID "${editing.id}" existe déjà.`, 'error');
                return;
            }
            setServices(prev => [...prev, editing]);
            onNotify?.(`Service "${editing.title}" ajouté avec succès !`, 'success');
        } else {
            setServices(prev => prev.map(s => s.id === editing.id ? editing : s));
            onNotify?.(`Service "${editing.title}" mis à jour.`, 'success');
        }
        setEditing(null);
        setIsNew(false);
    };

    const handleDelete = (id: string) => {
        const service = services.find(s => s.id === id);
        if (!service) return;
        if (!confirm(`Supprimer le service "${service.title}" ? Cette action est irréversible.`)) return;
        setServices(prev => prev.filter(s => s.id !== id));
        onNotify?.(`Service "${service.title}" supprimé.`, 'success');
    };

    const toggleActive = (id: string) => {
        setServices(prev => prev.map(s =>
            s.id === id ? { ...s, isActive: !(s.isActive ?? true) } : s
        ));
        const service = services.find(s => s.id === id);
        const newState = !(service?.isActive ?? true);
        onNotify?.(`Service "${service?.title}" ${newState ? 'activé' : 'désactivé'}.`, 'success');
    };

    const poleColors: Record<ServicePole, { bg: string; text: string; border: string }> = {
        PROCEDURES: { bg: 'bg-blue-50', text: 'text-blue-700', border: 'border-blue-200' },
        INTEGRATION: { bg: 'bg-indigo-50', text: 'text-indigo-700', border: 'border-indigo-200' },
        EXPERTISE: { bg: 'bg-purple-50', text: 'text-purple-700', border: 'border-purple-200' },
    };

    const activeCount = services.filter(s => s.isActive !== false).length;
    const simulCount = services.filter(s => s.isSimulatable && s.isActive !== false).length;

    return (
        <div className="space-y-6">
            {/* Stats Header */}
            <div className="grid grid-cols-3 gap-4">
                <div className="bg-white rounded-2xl p-5 border border-slate-200 shadow-sm">
                    <div className="text-3xl font-black text-slate-900">{services.length}</div>
                    <div className="text-xs font-bold text-slate-400 uppercase tracking-wider mt-1">Services Total</div>
                </div>
                <div className="bg-white rounded-2xl p-5 border border-emerald-200 shadow-sm">
                    <div className="text-3xl font-black text-emerald-600">{activeCount}</div>
                    <div className="text-xs font-bold text-slate-400 uppercase tracking-wider mt-1">Services Actifs</div>
                </div>
                <div className="bg-white rounded-2xl p-5 border border-orange-200 shadow-sm">
                    <div className="text-3xl font-black text-orange-600">{simulCount}</div>
                    <div className="text-xs font-bold text-slate-400 uppercase tracking-wider mt-1">Avec Simulateur</div>
                </div>
            </div>

            {/* Toolbar */}
            <div className="flex items-center gap-4">
                <div className="relative flex-1">
                    <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-slate-400" />
                    <input
                        type="text"
                        placeholder="Rechercher un service..."
                        value={searchQuery}
                        onChange={e => setSearchQuery(e.target.value)}
                        className="w-full pl-10 pr-4 py-3 bg-white border border-slate-200 rounded-xl text-sm font-medium focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                    />
                </div>
                <select
                    value={filterPole}
                    onChange={e => setFilterPole(e.target.value as ServicePole | 'ALL')}
                    className="px-4 py-3 bg-white border border-slate-200 rounded-xl text-sm font-bold text-slate-700 cursor-pointer outline-none"
                >
                    <option value="ALL">Tous les pôles</option>
                    {SERVICE_POLES.map(p => (
                        <option key={p.id} value={p.id}>{p.emoji} {p.label}</option>
                    ))}
                </select>
                <button
                    onClick={handleNew}
                    className="flex items-center gap-2 px-5 py-3 bg-indigo-600 text-white rounded-xl font-bold text-sm hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-100 whitespace-nowrap"
                >
                    <Plus size={16} /> Nouveau Service
                </button>
            </div>

            {/* Services List */}
            <div className="space-y-3">
                {filtered.map(service => {
                    const Icon = resolveIcon(service.iconName);
                    const pole = poleColors[service.pole];
                    const isActive = service.isActive !== false;
                    return (
                        <div
                            key={service.id}
                            className={`bg-white rounded-2xl border p-5 flex items-center gap-5 transition-all hover:shadow-md ${!isActive ? 'opacity-50 border-slate-200' : 'border-slate-200'
                                }`}
                        >
                            {/* Grip + Icon */}
                            <GripVertical className="w-4 h-4 text-slate-300 cursor-grab flex-shrink-0" />
                            <div className={`w-12 h-12 rounded-xl flex items-center justify-center flex-shrink-0 ${pole.bg} ${pole.text}`}>
                                <Icon className="w-6 h-6" />
                            </div>

                            {/* Info */}
                            <div className="flex-1 min-w-0">
                                <div className="flex items-center gap-3">
                                    <h3 className="font-bold text-slate-900 text-base">{service.title}</h3>
                                    <span className={`px-2 py-0.5 rounded-md text-[10px] font-black uppercase ${pole.bg} ${pole.text} ${pole.border} border`}>
                                        {service.pole}
                                    </span>
                                    {service.badge && (
                                        <span className="px-2 py-0.5 rounded-md text-[10px] font-black uppercase bg-orange-50 text-orange-600 border border-orange-200">
                                            {service.badge}
                                        </span>
                                    )}
                                    {!isActive && (
                                        <span className="px-2 py-0.5 rounded-md text-[10px] font-black uppercase bg-red-50 text-red-500 border border-red-200">
                                            INACTIF
                                        </span>
                                    )}
                                </div>
                                <p className="text-sm text-slate-400 mt-1 truncate">{service.description}</p>
                                <div className="flex items-center gap-3 mt-2">
                                    <span className="text-[10px] text-slate-400 font-mono bg-slate-50 px-2 py-0.5 rounded">{service.id}</span>
                                    <span className="text-[10px] text-slate-400">Ordre: {service.sortOrder ?? 50}</span>
                                    {service.isSimulatable && <span className="text-[10px] text-emerald-500 font-bold">✓ Simulable</span>}
                                </div>
                            </div>

                            {/* Actions */}
                            <div className="flex items-center gap-2 flex-shrink-0">
                                <button
                                    onClick={() => toggleActive(service.id)}
                                    className={`p-2 rounded-lg transition-all ${isActive ? 'hover:bg-amber-50 text-emerald-500' : 'hover:bg-emerald-50 text-slate-400'}`}
                                    title={isActive ? 'Désactiver' : 'Activer'}
                                >
                                    {isActive ? <Eye size={16} /> : <EyeOff size={16} />}
                                </button>
                                <button
                                    onClick={() => handleEdit(service)}
                                    className="p-2 rounded-lg hover:bg-indigo-50 text-slate-400 hover:text-indigo-600 transition-all"
                                    title="Modifier"
                                >
                                    <Pencil size={16} />
                                </button>
                                <button
                                    onClick={() => handleDelete(service.id)}
                                    className="p-2 rounded-lg hover:bg-red-50 text-slate-400 hover:text-red-600 transition-all"
                                    title="Supprimer"
                                >
                                    <Trash2 size={16} />
                                </button>
                            </div>
                        </div>
                    );
                })}

                {filtered.length === 0 && (
                    <div className="text-center py-16 text-slate-400">
                        <Search className="w-12 h-12 mx-auto mb-3 opacity-50" />
                        <p className="font-bold">Aucun service trouvé</p>
                        <p className="text-sm mt-1">Essayez un autre terme de recherche ou changez de filtre.</p>
                    </div>
                )}
            </div>

            {/* Propagation Info */}
            <div className="bg-gradient-to-r from-indigo-50 via-blue-50 to-emerald-50 rounded-2xl p-6 border border-indigo-100">
                <div className="flex items-start gap-4">
                    <div className="w-10 h-10 rounded-xl bg-indigo-100 text-indigo-600 flex items-center justify-center flex-shrink-0">
                        <Zap className="w-5 h-5" />
                    </div>
                    <div>
                        <h4 className="font-bold text-slate-900 mb-1">Propagation Automatique</h4>
                        <p className="text-sm text-slate-500 leading-relaxed">
                            Les modifications sont automatiquement propagées vers : <strong>Landing Page</strong>, <strong>Simulateur</strong>,
                            <strong> Pipeline Commercial</strong>, <strong>Kiosque Agence</strong>, <strong>Calendrier RDV</strong> et le
                            <strong> Catalogue Documents</strong>.
                        </p>
                    </div>
                </div>
            </div>

            {/* Edit Modal */}
            {editing && (
                <div className="fixed inset-0 z-[100] flex items-center justify-center p-6 backdrop-blur-xl bg-slate-900/40">
                    <div className="bg-white w-full max-w-2xl rounded-3xl shadow-2xl overflow-hidden max-h-[90vh] flex flex-col">
                        {/* Modal Header */}
                        <div className="flex items-center justify-between px-8 py-6 border-b border-slate-100">
                            <h2 className="text-xl font-black text-slate-900">
                                {isNew ? '➕ Nouveau Service' : `✏️ Modifier : ${editing.title}`}
                            </h2>
                            <button
                                onClick={() => { setEditing(null); setIsNew(false); setShowIconPicker(false); }}
                                className="p-2 hover:bg-slate-100 rounded-lg transition-colors"
                            >
                                <X className="w-5 h-5 text-slate-400" />
                            </button>
                        </div>

                        {/* Modal Body */}
                        <div className="p-8 overflow-y-auto space-y-6 flex-1">
                            {/* ID + Titre */}
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        ID unique *
                                    </label>
                                    <input
                                        type="text"
                                        value={editing.id}
                                        onChange={e => setEditing({ ...editing, id: e.target.value.toLowerCase().replace(/[^a-z0-9_]/g, '_') })}
                                        disabled={!isNew}
                                        placeholder="ex: visa_long_sejour"
                                        className={`w-full px-4 py-3 border rounded-xl text-sm font-mono ${isNew ? 'border-slate-200 focus:ring-2 focus:ring-indigo-500' : 'bg-slate-50 border-slate-100 text-slate-400 cursor-not-allowed'} outline-none transition-all`}
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Titre *
                                    </label>
                                    <input
                                        type="text"
                                        value={editing.title}
                                        onChange={e => setEditing({ ...editing, title: e.target.value })}
                                        placeholder="ex: Visa Long Séjour"
                                        className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-medium focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                                    />
                                </div>
                            </div>

                            {/* Description */}
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                    Description
                                </label>
                                <textarea
                                    value={editing.description}
                                    onChange={e => setEditing({ ...editing, description: e.target.value })}
                                    rows={2}
                                    placeholder="Description affichée sur la landing page et dans le kiosque..."
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-medium focus:ring-2 focus:ring-indigo-500 outline-none transition-all resize-none"
                                />
                            </div>

                            {/* Pôle + Type */}
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Pôle
                                    </label>
                                    <select
                                        value={editing.pole}
                                        onChange={e => setEditing({ ...editing, pole: e.target.value as ServicePole })}
                                        className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-bold cursor-pointer outline-none"
                                    >
                                        {SERVICE_POLES.map(p => (
                                            <option key={p.id} value={p.id}>{p.emoji} {p.label}</option>
                                        ))}
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Type
                                    </label>
                                    <select
                                        value={editing.type}
                                        onChange={e => setEditing({ ...editing, type: e.target.value as ServiceType })}
                                        className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-bold cursor-pointer outline-none"
                                    >
                                        {SERVICE_TYPES.map(t => (
                                            <option key={t.id} value={t.id}>{t.emoji} {t.label}</option>
                                        ))}
                                    </select>
                                </div>
                            </div>

                            {/* Icône + Badge */}
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Icône
                                    </label>
                                    <div className="relative">
                                        <button
                                            onClick={() => setShowIconPicker(!showIconPicker)}
                                            className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-medium flex items-center gap-3 hover:border-indigo-300 transition-all"
                                        >
                                            {React.createElement(resolveIcon(editing.iconName), { className: 'w-5 h-5 text-indigo-600' })}
                                            <span>{editing.iconName}</span>
                                            <ChevronDown className="w-4 h-4 ml-auto text-slate-400" />
                                        </button>
                                        {showIconPicker && (
                                            <div className="absolute z-50 top-full left-0 right-0 mt-2 bg-white border border-slate-200 rounded-xl shadow-2xl p-4 max-h-64 overflow-y-auto">
                                                <div className="grid grid-cols-6 gap-2">
                                                    {getAvailableIcons().map(({ name, icon: Ic }) => (
                                                        <button
                                                            key={name}
                                                            onClick={() => {
                                                                setEditing({ ...editing, iconName: name });
                                                                setShowIconPicker(false);
                                                            }}
                                                            className={`flex flex-col items-center gap-1 p-2 rounded-lg transition-all ${editing.iconName === name ? 'bg-indigo-100 text-indigo-600 ring-2 ring-indigo-500' : 'hover:bg-slate-50 text-slate-600'}`}
                                                            title={name}
                                                        >
                                                            <Ic className="w-5 h-5" />
                                                            <span className="text-[8px] font-bold truncate w-full text-center">{name}</span>
                                                        </button>
                                                    ))}
                                                </div>
                                            </div>
                                        )}
                                    </div>
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Badge
                                    </label>
                                    <input
                                        type="text"
                                        value={editing.badge || ''}
                                        onChange={e => setEditing({ ...editing, badge: e.target.value || undefined })}
                                        placeholder="ex: SIMUL, NOUVEAU, GRATUIT"
                                        className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-medium focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                                    />
                                </div>
                            </div>

                            {/* Options */}
                            <div className="grid grid-cols-3 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                        Ordre
                                    </label>
                                    <input
                                        type="number"
                                        value={editing.sortOrder ?? 50}
                                        onChange={e => setEditing({ ...editing, sortOrder: parseInt(e.target.value) || 50 })}
                                        className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-mono focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                                    />
                                </div>
                                <div className="flex flex-col justify-end">
                                    <label className="flex items-center gap-3 cursor-pointer py-3">
                                        <button
                                            onClick={() => setEditing({ ...editing, isSimulatable: !editing.isSimulatable })}
                                            className={`w-10 h-6 rounded-full transition-all flex items-center ${editing.isSimulatable ? 'bg-emerald-500 justify-end pr-0.5' : 'bg-slate-300 justify-start pl-0.5'}`}
                                        >
                                            <div className="w-5 h-5 bg-white rounded-full shadow" />
                                        </button>
                                        <span className="text-sm font-bold text-slate-700">Simulable</span>
                                    </label>
                                </div>
                                <div className="flex flex-col justify-end">
                                    <label className="flex items-center gap-3 cursor-pointer py-3">
                                        <button
                                            onClick={() => setEditing({ ...editing, isActive: !(editing.isActive ?? true) })}
                                            className={`w-10 h-6 rounded-full transition-all flex items-center ${(editing.isActive ?? true) ? 'bg-emerald-500 justify-end pr-0.5' : 'bg-slate-300 justify-start pl-0.5'}`}
                                        >
                                            <div className="w-5 h-5 bg-white rounded-full shadow" />
                                        </button>
                                        <span className="text-sm font-bold text-slate-700">Actif</span>
                                    </label>
                                </div>
                            </div>

                            {/* Keywords */}
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                    Mots-clés (séparés par virgule)
                                </label>
                                <input
                                    type="text"
                                    value={(editing.keywords || []).join(', ')}
                                    onChange={e => setEditing({
                                        ...editing,
                                        keywords: e.target.value.split(',').map(k => k.trim()).filter(Boolean)
                                    })}
                                    placeholder="ex: visa, séjour, carte, résidence"
                                    className="w-full px-4 py-3 border border-slate-200 rounded-xl text-sm font-medium focus:ring-2 focus:ring-indigo-500 outline-none transition-all"
                                />
                            </div>
                        </div>

                        {/* Modal Footer */}
                        <div className="flex items-center justify-between px-8 py-5 border-t border-slate-100 bg-slate-50">
                            <button
                                onClick={() => { setEditing(null); setIsNew(false); setShowIconPicker(false); }}
                                className="px-6 py-3 text-slate-600 font-bold text-sm rounded-xl hover:bg-slate-200 transition-all"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleSave}
                                className="flex items-center gap-2 px-8 py-3 bg-indigo-600 text-white font-bold text-sm rounded-xl hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-100"
                            >
                                <Save size={16} /> {isNew ? 'Créer le Service' : 'Sauvegarder'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
