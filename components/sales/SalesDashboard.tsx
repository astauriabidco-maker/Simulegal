'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { SalesStore, Prospect, ProspectStatus } from '../../services/SalesStore';
import CallCockpit from './CallCockpit';
import {
    Phone,
    Mail,
    Calendar,
    CheckCircle,
    XCircle,
    MoreHorizontal,
    Plus,
    Upload,
    Flame,
    Search,
    Filter,
    ArrowRight,
    Layout,
    BarChart2,
    Download
} from 'lucide-react';
import { CRM } from '../../services/crmStore';
import { getRequirementsForService } from '../../config/DocumentTemplates';
import { SalesAnalyticsDashboard } from './SalesAnalyticsDashboard';

const COLUMNS: { id: ProspectStatus; label: string; color: string }[] = [
    { id: 'TO_CALL', label: 'À Appeler', color: 'bg-amber-100 text-amber-800' },
    { id: 'IN_DISCUSSION', label: 'En Discussion', color: 'bg-purple-100 text-purple-800' },
    { id: 'MEETING_BOOKED', label: 'RDV Fixé', color: 'bg-indigo-100 text-indigo-800' },
    { id: 'LINK_SENT', label: 'Lien Envoyé', color: 'bg-orange-100 text-orange-800' },
    { id: 'CONVERTED', label: 'Converti', color: 'bg-emerald-100 text-emerald-800' },
    { id: 'LOST', label: 'Perdu', color: 'bg-slate-100 text-slate-800' }
];

export default function SalesDashboard() {
    const [prospects, setProspects] = useState<Prospect[]>([]);
    const [viewMode, setViewMode] = useState<'KANBAN' | 'ANALYTICS'>('KANBAN');
    const [isLoading, setIsLoading] = useState(true);
    const [selectedProspect, setSelectedProspect] = useState<Prospect | null>(null);
    const [showImportModal, setShowImportModal] = useState(false);
    const [showCallCockpit, setShowCallCockpit] = useState(false);
    const [callHistory, setCallHistory] = useState<any[]>([]);

    // Advanced Filters
    const [filters, setFilters] = useState<{
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
    }>({});
    const [showFilters, setShowFilters] = useState(false);
    const [hoveredColumnId, setHoveredColumnId] = useState<string | null>(null);

    const router = useRouter();

    // Initial load
    useEffect(() => {
        loadProspects();
    }, []);

    // Load call history when prospect is selected
    useEffect(() => {
        if (selectedProspect) {
            SalesStore.getCallHistory(selectedProspect.id).then(setCallHistory);
        } else {
            setCallHistory([]);
        }
    }, [selectedProspect]);


    const loadProspects = async () => {
        setIsLoading(true);
        const result = await SalesStore.getProspects(1, 100, filters);
        // Handle paginated response
        setProspects(Array.isArray(result) ? result : result.data || []);
        setIsLoading(false);
    };

    // Reload when filters change
    useEffect(() => {
        loadProspects();
    }, [filters]);

    const handleStatusChange = async (prospectId: string, newStatus: ProspectStatus) => {
        // Optimistic update
        setProspects((prev: Prospect[]) => prev.map((p: Prospect) =>
            p.id === prospectId ? { ...p, status: newStatus } : p
        ));

        await SalesStore.updateProspect(prospectId, { status: newStatus });

        // If prospect was open in drawer, update it too
        if (selectedProspect?.id === prospectId) {
            setSelectedProspect((prev: Prospect | null) => prev ? { ...prev, status: newStatus } : null);
        }
    };

    const handleConvert = async (prospect: Prospect) => {
        if (!confirm(`Confirmer la conversion de ${prospect.firstName} en dossier client ?`)) return;

        // Initialiser les documents requis pour ce service
        const requirements = getRequirementsForService(prospect.interestServiceId || 'default');
        const initialDocs = requirements.map(req => ({
            id: req.id,
            docType: req.label,
            status: 'EMPTY' as const,
            required: req.required
        }));

        // Create actual Lead in CRM
        await CRM.saveLead({
            id: `LEAD-${Date.now()}`,
            name: `${prospect.firstName} ${prospect.lastName}`,
            email: prospect.email,
            phone: prospect.phone,
            serviceId: prospect.interestServiceId || 'unknown',
            serviceName: prospect.interestServiceId || 'Projet Client',
            status: 'NEW',
            currentStage: 'NEW',
            documents: initialDocs,
            originAgencyId: prospect.agencyId
        });

        // Update prospect status
        await handleStatusChange(prospect.id, 'CONVERTED');
        alert('Dossier créé avec succès !');
    };

    const handleImport = async () => {
        // Mock import
        const count = await SalesStore.importProspectsFromCSV(new File([], 'dummy.csv'));
        await loadProspects();
        setShowImportModal(false);
        alert(`${count} prospects importés !`);
    };

    const handleSaveNote = async (text: string) => {
        if (!selectedProspect) return;
        await SalesStore.addNote(selectedProspect.id, text);
        // Refresh to see notes (optional, but good for UX)
        loadProspects();
    };

    return (
        <div className="h-full flex flex-col bg-slate-50">
            {/* Header / Actions Bar */}
            <div className="bg-white border-b border-slate-200 px-6 py-4 flex items-center justify-between">
                <div>
                    <h1 className="text-2xl font-bold text-slate-900">Sales Hub (Pilotage)</h1>
                    <p className="text-slate-500 text-sm">Gestion de la prospection commerciale</p>
                </div>
                <div className="flex items-center gap-3">
                    <div className="bg-white p-1 rounded-xl border border-slate-200 flex">
                        <button
                            onClick={() => setViewMode('KANBAN')}
                            className={`px-4 py-2 rounded-lg text-xs font-black transition-all flex items-center gap-2 ${viewMode === 'KANBAN' ? 'bg-slate-900 text-white shadow-lg' : 'text-slate-500 hover:bg-slate-50'}`}
                        >
                            <Layout size={14} /> Kanban
                        </button>
                        <button
                            onClick={() => setViewMode('ANALYTICS')}
                            className={`px-4 py-2 rounded-lg text-xs font-black transition-all flex items-center gap-2 ${viewMode === 'ANALYTICS' ? 'bg-indigo-600 text-white shadow-lg shadow-indigo-200' : 'text-slate-500 hover:bg-slate-50'}`}
                        >
                            <BarChart2 size={14} /> Pilotage
                        </button>
                    </div>

                    <button
                        onClick={() => {
                            SalesStore.addProspect({
                                firstName: 'Nouveau',
                                lastName: 'Prospect',
                                phone: '0600000000',
                                agencyId: 'HQ-001',
                                score: 0
                            }).then(loadProspects);
                        }}
                        className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-lg hover:bg-indigo-700 transition-colors font-medium shadow-sm"
                    >
                        <Plus size={18} />
                        Ajouter
                    </button>

                    <button
                        onClick={() => SalesStore.exportProspects(filters)}
                        className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-lg hover:bg-slate-200 transition-colors font-medium"
                    >
                        <Download size={18} />
                        Export CSV
                    </button>
                </div>
            </div>

            {viewMode === 'ANALYTICS' ? (
                <div className="p-8 overflow-y-auto">
                    <SalesAnalyticsDashboard />
                </div>
            ) : (
                <>
                    {/* Filters Bar */}
                    <div className="bg-white border-b border-slate-100 px-6 py-3 flex items-center gap-4">
                        <button
                            onClick={() => setShowFilters(!showFilters)}
                            className={`flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors ${showFilters ? 'bg-indigo-100 text-indigo-700' : 'bg-slate-100 text-slate-600 hover:bg-slate-200'}`}
                        >
                            <Filter size={16} />
                            Filtres
                            {Object.values(filters).filter(Boolean).length > 0 && (
                                <span className="bg-indigo-600 text-white text-xs px-1.5 rounded-full">
                                    {Object.values(filters).filter(Boolean).length}
                                </span>
                            )}
                        </button>

                        {showFilters && (
                            <>
                                <select
                                    value={filters.source || ''}
                                    onChange={(e) => setFilters(prev => ({ ...prev, source: e.target.value || undefined }))}
                                    className="px-3 py-2 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-indigo-500"
                                >
                                    <option value="">Toutes sources</option>
                                    <option value="GOOGLE_ADS">Google Ads</option>
                                    <option value="META_ADS">Meta Ads</option>
                                    <option value="TIKTOK_ADS">TikTok Ads</option>
                                    <option value="CSV_IMPORT">Import CSV</option>
                                    <option value="WEBSITE">Site Web</option>
                                    <option value="REFERRAL">Parrainage</option>
                                </select>

                                <input
                                    type="text"
                                    placeholder="ID Agence"
                                    value={filters.agencyId || ''}
                                    onChange={(e) => setFilters(prev => ({ ...prev, agencyId: e.target.value || undefined }))}
                                    className="px-3 py-2 border border-slate-200 rounded-lg text-sm w-32 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                                />

                                <div className="flex items-center gap-2 text-sm text-slate-500">
                                    <span>Du</span>
                                    <input
                                        type="date"
                                        value={filters.dateFrom || ''}
                                        onChange={(e) => setFilters(prev => ({ ...prev, dateFrom: e.target.value || undefined }))}
                                        className="px-2 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-indigo-500"
                                    />
                                    <span>au</span>
                                    <input
                                        type="date"
                                        value={filters.dateTo || ''}
                                        onChange={(e) => setFilters(prev => ({ ...prev, dateTo: e.target.value || undefined }))}
                                        className="px-2 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-indigo-500"
                                    />
                                </div>

                                {Object.values(filters).filter(Boolean).length > 0 && (
                                    <button
                                        onClick={() => setFilters({})}
                                        className="text-sm text-red-600 hover:text-red-700 font-medium"
                                    >
                                        Réinitialiser
                                    </button>
                                )}
                            </>
                        )}

                        <span className="ml-auto text-sm text-slate-400">
                            {prospects.length} prospect{prospects.length !== 1 ? 's' : ''}
                        </span>
                    </div>

                    {/* Kanban Board */}
                    <div className="flex-1 overflow-x-auto overflow-y-hidden p-6">
                        <div className="flex h-full gap-6 min-w-[1200px]">
                            {COLUMNS.map(column => (
                                <div key={column.id} className="flex-1 flex flex-col min-w-[280px]">
                                    {/* Column Header */}
                                    <div className="flex items-center justify-between mb-4 px-2">
                                        <div className="flex items-center gap-2">
                                            <span className={`w-3 h-3 rounded-full ${column.color.split(' ')[0].replace('bg-', 'bg-')}`} />
                                            <h3 className="font-semibold text-slate-700">{column.label}</h3>
                                        </div>
                                        <span className="text-slate-400 text-sm font-medium">
                                            {prospects.filter(p => p.status === column.id).length}
                                        </span>
                                    </div>

                                    {/* Column Content */}
                                    <div
                                        className={`flex-1 rounded-xl p-2 overflow-y-auto transition-colors ${hoveredColumnId === column.id ? 'bg-slate-200/70 border-2 border-dashed border-slate-300' : 'bg-slate-100/50 border-2 border-transparent'
                                            }`}
                                        onDragOver={(e) => {
                                            e.preventDefault();
                                            setHoveredColumnId(column.id);
                                        }}
                                        onDragLeave={() => setHoveredColumnId(null)}
                                        onDrop={(e) => {
                                            e.preventDefault();
                                            setHoveredColumnId(null);
                                            const prospectId = e.dataTransfer.getData('prospectId');
                                            if (prospectId) {
                                                handleStatusChange(prospectId, column.id as ProspectStatus);
                                            }
                                        }}
                                    >
                                        <div className="space-y-3">
                                            {prospects
                                                .filter(p => p.status === column.id)
                                                .sort((a, b) => {
                                                    // Special sort for TO_CALL: High score first
                                                    if (column.id === 'TO_CALL') {
                                                        return b.score - a.score;
                                                    }
                                                    // Default: Newest first
                                                    return new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
                                                })
                                                .map(prospect => (
                                                    <div
                                                        key={prospect.id}
                                                        draggable={true}
                                                        onDragStart={(e) => {
                                                            e.dataTransfer.setData('prospectId', prospect.id);
                                                            e.dataTransfer.effectAllowed = 'move';
                                                        }}
                                                        onClick={() => setSelectedProspect(prospect)}
                                                        className="bg-white p-4 rounded-lg shadow-sm border border-slate-200 cursor-pointer hover:shadow-md transition-all group relative active:scale-[0.98] active:rotate-1"
                                                    >
                                                        {/* Hot Lead Badge */}
                                                        {prospect.score >= 50 && (
                                                            <div className="absolute top-2 right-2 text-orange-500 animate-pulse" title="Prospect chaud !">
                                                                <Flame size={16} fill="currentColor" />
                                                            </div>
                                                        )}

                                                        <h4 className="font-semibold text-slate-900 mb-1">
                                                            {prospect.firstName} {prospect.lastName}
                                                        </h4>

                                                        {/* Tags */}
                                                        <div className="flex flex-wrap gap-2 mb-3">
                                                            <span className="text-xs px-2 py-0.5 bg-slate-100 text-slate-600 rounded">
                                                                {prospect.source.replace('_', ' ')}
                                                            </span>
                                                            {prospect.campaignName && (
                                                                <span className="text-xs px-2 py-0.5 bg-indigo-50 text-indigo-600 rounded">
                                                                    {prospect.campaignName}
                                                                </span>
                                                            )}
                                                        </div>

                                                        {/* Quick Actions (visible on hover) */}
                                                        <div className="flex items-center justify-between pt-2 border-t border-slate-50 text-slate-400 text-sm">
                                                            <span>{new Date(prospect.createdAt).toLocaleDateString()}</span>
                                                            <div className="flex gap-2 opacity-0 group-hover:opacity-100 transition-opacity">
                                                                {column.id !== 'CONVERTED' && (
                                                                    <button
                                                                        onClick={(e: React.MouseEvent) => {
                                                                            e.stopPropagation();
                                                                            handleStatusChange(prospect.id, 'CONVERTED');
                                                                        }}
                                                                        className="hover:text-emerald-600 p-1"
                                                                        title="Convertir rapide"
                                                                    >
                                                                        <CheckCircle size={16} />
                                                                    </button>
                                                                )}
                                                                <button className="hover:text-indigo-600 p-1">
                                                                    <Phone size={16} />
                                                                </button>
                                                            </div>
                                                        </div>
                                                    </div>
                                                ))}
                                        </div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Drawer Detail */}
                    {selectedProspect && (
                        <div className="fixed inset-0 z-50 flex justify-end">
                            {/* Backdrop */}
                            <div
                                className="absolute inset-0 bg-black/20 backdrop-blur-sm"
                                onClick={() => setSelectedProspect(null)}
                            />

                            {/* Panel */}
                            <div className="relative w-[500px] h-full bg-white shadow-2xl flex flex-col animate-in slide-in-from-right duration-300">
                                {/* Drawer Header */}
                                <div className="p-6 border-b border-slate-100 flex justify-between items-start bg-slate-50/50">
                                    <div>
                                        <h2 className="text-2xl font-bold text-slate-900 mb-1">
                                            {selectedProspect.firstName} {selectedProspect.lastName}
                                        </h2>
                                        <p className="text-slate-500 text-sm flex items-center gap-2">
                                            {selectedProspect.email || 'Pas d\'email'} • {selectedProspect.phone}
                                        </p>
                                    </div>
                                    <button
                                        onClick={() => setSelectedProspect(null)}
                                        className="p-2 hover:bg-slate-200 rounded-full transition-colors"
                                    >
                                        <XCircle className="text-slate-400" size={24} />
                                    </button>
                                </div>

                                {/* Drawer Content */}
                                <div className="flex-1 overflow-y-auto p-6 space-y-8">

                                    {/* Actions Critiques */}
                                    <div className="grid grid-cols-2 gap-4">
                                        <button
                                            onClick={() => setShowCallCockpit(true)}
                                            className="flex items-center justify-center gap-2 py-3 px-4 bg-indigo-600 text-white rounded-xl font-medium hover:bg-indigo-700 transition-colors shadow-sm shadow-indigo-200"
                                        >
                                            <Phone size={18} />
                                            Appeler maintenant
                                        </button>
                                        <button
                                            onClick={() => router.push('/admin/calendar')}
                                            className="flex items-center justify-center gap-2 py-3 px-4 bg-white border border-slate-200 text-slate-700 rounded-xl font-medium hover:bg-slate-50 transition-colors"
                                        >
                                            <Calendar size={18} />
                                            Fixer RDV
                                        </button>
                                        <button
                                            className="col-span-2 flex items-center justify-center gap-2 py-3 px-4 bg-orange-50 border border-orange-200 text-orange-700 rounded-xl font-medium hover:bg-orange-100 transition-colors"
                                            onClick={async () => {
                                                const result = await SalesStore.sendSimulationLink(
                                                    selectedProspect.id,
                                                    selectedProspect.phone,
                                                    selectedProspect.firstName,
                                                    'SMS'
                                                );
                                                if (result.success) {
                                                    await handleStatusChange(selectedProspect.id, 'LINK_SENT');
                                                    alert('✅ SMS envoyé avec succès !');
                                                } else {
                                                    alert('❌ Erreur: ' + (result.error || 'Envoi échoué'));
                                                }
                                            }}
                                        >
                                            <Mail size={18} />
                                            Envoyer le lien de simulation (SMS)
                                        </button>
                                    </div>

                                    {/* Statut Pipeline */}
                                    <div>
                                        <h3 className="text-sm font-semibold text-slate-900 uppercase tracking-wider mb-3">Pipeline</h3>
                                        <div className="flex flex-wrap gap-2">
                                            {COLUMNS.map(status => (
                                                <button
                                                    key={status.id}
                                                    onClick={() => handleStatusChange(selectedProspect.id, status.id)}
                                                    className={`px-3 py-1.5 rounded-full text-xs font-medium border transition-colors ${selectedProspect.status === status.id
                                                        ? `${status.color} border-transparent ring-2 ring-offset-2 ring-indigo-500`
                                                        : 'bg-white border-slate-200 text-slate-600 hover:border-indigo-300'
                                                        }`}
                                                >
                                                    {status.label}
                                                </button>
                                            ))}
                                        </div>
                                    </div>

                                    {/* Conversion Zone */}
                                    {selectedProspect.status !== 'CONVERTED' ? (
                                        <div className="bg-gradient-to-br from-indigo-50 to-purple-50 p-6 rounded-2xl border border-indigo-100">
                                            <div className="flex items-start gap-4">
                                                <div className="p-3 bg-white rounded-xl shadow-sm">
                                                    <Flame className="text-indigo-600" size={24} />
                                                </div>
                                                <div>
                                                    <h3 className="font-bold text-slate-900 mb-1">Prêt à signer ?</h3>
                                                    <p className="text-sm text-slate-600 mb-4">
                                                        Convertir ce prospect créera automatiquement un dossier client et enverra le lien de paiement.
                                                    </p>
                                                    <button
                                                        onClick={() => handleConvert(selectedProspect)}
                                                        className="flex items-center gap-2 font-semibold text-indigo-700 hover:text-indigo-800 transition-colors"
                                                    >
                                                        Lancer la conversion <ArrowRight size={16} />
                                                    </button>
                                                </div>
                                            </div>
                                        </div>
                                    ) : (
                                        <div className="bg-emerald-50 p-4 rounded-xl border border-emerald-100 flex items-center gap-3 text-emerald-800">
                                            <CheckCircle size={20} />
                                            <span className="font-medium">Ce prospect a été converti en dossier client.</span>
                                        </div>
                                    )}

                                    {/* Info Marketing */}
                                    <div>
                                        <h3 className="text-sm font-semibold text-slate-900 uppercase tracking-wider mb-3">Contexte Marketing</h3>
                                        <dl className="grid grid-cols-2 gap-y-4 text-sm">
                                            <div>
                                                <dt className="text-slate-500 mb-1">Source</dt>
                                                <dd className="font-medium">{selectedProspect.source}</dd>
                                            </div>
                                            <div>
                                                <dt className="text-slate-500 mb-1">Campagne</dt>
                                                <dd className="font-medium">{selectedProspect.campaignName || '—'}</dd>
                                            </div>
                                            <div>
                                                <dt className="text-slate-500 mb-1">Intérêt</dt>
                                                <dd className="font-medium">{selectedProspect.interestServiceId || 'Non spécifié'}</dd>
                                            </div>
                                            <div>
                                                <dt className="text-slate-500 mb-1">Score</dt>
                                                <dd className={`font-bold ${selectedProspect.score > 50 ? 'text-emerald-600' : 'text-slate-600'}`}>
                                                    {selectedProspect.score}/100
                                                </dd>
                                            </div>
                                        </dl>
                                    </div>

                                    {/* Historique des Appels */}
                                    <div>
                                        <h3 className="text-sm font-semibold text-slate-900 uppercase tracking-wider mb-3 flex items-center gap-2">
                                            <Phone size={16} className="text-indigo-600" />
                                            Historique Appels ({callHistory.length})
                                        </h3>
                                        {callHistory.length === 0 ? (
                                            <p className="text-sm text-slate-400 italic">Aucun appel enregistré.</p>
                                        ) : (
                                            <div className="space-y-2 max-h-48 overflow-y-auto">
                                                {callHistory.map((call: any) => (
                                                    <div key={call.id} className="p-3 bg-slate-50 rounded-xl border border-slate-100 text-sm">
                                                        <div className="flex justify-between items-center mb-1">
                                                            <span className={`font-medium ${call.status === 'COMPLETED' ? 'text-emerald-600' : call.status === 'FAILED' ? 'text-red-500' : 'text-slate-600'}`}>
                                                                {call.status === 'COMPLETED' ? '✅ Terminé' : call.status === 'FAILED' ? '❌ Échoué' : call.status === 'NO_ANSWER' ? '📵 Sans réponse' : '📞 ' + call.status}
                                                            </span>
                                                            <span className="text-slate-400 text-xs">
                                                                {new Date(call.startedAt).toLocaleDateString('fr-FR')} {new Date(call.startedAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                            </span>
                                                        </div>
                                                        <div className="flex items-center gap-4 text-slate-500 text-xs">
                                                            <span>⏱️ {Math.floor(call.duration / 60)}:{(call.duration % 60).toString().padStart(2, '0')}</span>
                                                            {call.notes && <span className="truncate">📝 {call.notes}</span>}
                                                        </div>
                                                    </div>
                                                ))}
                                            </div>
                                        )}
                                    </div>

                                    {/* Historique des Notes */}
                                    <div>
                                        <h3 className="text-sm font-semibold text-slate-900 uppercase tracking-wider mb-3 flex items-center gap-2">
                                            📝 Notes ({selectedProspect.notes?.length || 0})
                                        </h3>
                                        {(!selectedProspect.notes || selectedProspect.notes.length === 0) ? (
                                            <p className="text-sm text-slate-400 italic">Aucune note enregistrée.</p>
                                        ) : (
                                            <div className="space-y-2 max-h-48 overflow-y-auto">
                                                {selectedProspect.notes.map((note: any) => (
                                                    <div key={note.id} className="p-3 bg-amber-50 rounded-xl border border-amber-100 text-sm">
                                                        <p className="text-slate-700 whitespace-pre-wrap">{note.text}</p>
                                                        <div className="flex justify-between items-center mt-2 text-xs text-slate-500">
                                                            <span>👤 {note.authorId}</span>
                                                            <span>{new Date(note.createdAt).toLocaleDateString('fr-FR')} à {new Date(note.createdAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}</span>
                                                        </div>
                                                    </div>
                                                ))}
                                            </div>
                                        )}
                                    </div>
                                </div>
                            </div>
                        </div>
                    )}

                    {/* Modale d'import */}
                    {showImportModal && (
                        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 backdrop-blur-sm">
                            <div className="bg-white p-8 rounded-2xl shadow-xl max-w-md w-full">
                                <h2 className="text-xl font-bold mb-4">Importer des prospects</h2>
                                <div className="border-2 border-dashed border-slate-200 rounded-xl p-8 text-center bg-slate-50 hover:bg-slate-100 transition-colors cursor-pointer" onClick={handleImport}>
                                    <Upload className="mx-auto text-slate-400 mb-2" size={32} />
                                    <p className="font-medium text-slate-700">Glisser un fichier CSV ici</p>
                                    <p className="text-xs text-slate-500 mt-1">ou cliquer pour parcourir</p>
                                </div>
                                <div className="mt-6 flex justify-end gap-3">
                                    <button
                                        onClick={() => setShowImportModal(false)}
                                        className="px-4 py-2 text-slate-600 hover:bg-slate-100 rounded-lg transition-colors"
                                    >
                                        Annuler
                                    </button>
                                </div>
                            </div>
                        </div>
                    )}
                    {/* Cockpit d'appel */}
                    {showCallCockpit && selectedProspect && (
                        <CallCockpit
                            prospect={selectedProspect}
                            onClose={() => setShowCallCockpit(false)}
                            onSaveNote={handleSaveNote}
                        />
                    )}
                </>
            )}
        </div>
    );
}
