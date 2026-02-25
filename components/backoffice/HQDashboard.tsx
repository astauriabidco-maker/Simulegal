'use client';

import React, { useState, useEffect, useMemo, useCallback } from 'react';
import { Lead, LeadDocument, LeadNote } from '../../services/crmStore';
import { DossierStore } from '../../services/DossierStore';
import { UserStore, StaffUser } from '../../services/UserStore';
import { WorkflowService, WorkflowStage, CATEGORY_LABELS } from '../../services/WorkflowService';
import { NotificationService } from '../../services/NotificationService';
import { WhatsAppWidget } from './WhatsAppWidget';
import {
    Sparkles,
    FolderOpen,
    Building2,
    CheckCircle2,
    Eye,
    ChevronDown,
    X,
    Calendar,
    CreditCard,
    MapPin,
    FileText,
    TrendingUp,
    Users,
    Bot,
    CheckCircle,
    Filter,
    ArrowRight,
    Search,
    Clock,
    Home,
    SearchCheck,
    Plus
} from 'lucide-react';

interface HQDashboardProps {
    onViewDossier?: (lead: Lead) => void;
}

export default function HQDashboard({ onViewDossier }: HQDashboardProps) {
    const [leads, setLeads] = useState<Lead[]>([]);
    const [staff, setStaff] = useState<StaffUser[]>([]);
    const [selectedLead, setSelectedLead] = useState<Lead | null>(null);
    const [isUpdating, setIsUpdating] = useState(false);
    const [serviceFilter, setServiceFilter] = useState('all');
    const [searchQuery, setSearchQuery] = useState('');
    const [activeTab, setActiveTab] = useState<'PRODUCTION' | 'QUALIFICATION'>('PRODUCTION');
    const [servicesByCategory, setServicesByCategory] = useState<Record<string, { id: string; name: string; shortName: string }[]>>({});
    const [catalogLoaded, setCatalogLoaded] = useState(false);

    useEffect(() => {
        loadLeads();
    }, []);

    const loadLeads = async () => {
        try {
            const allLeads = await DossierStore.getAll();
            setLeads(allLeads);
        } catch (error) {
            console.error('[HQ] Erreur chargement leads:', error);
        }
    };

    const loadStaff = async () => {
        const users = await UserStore.getAllUsers();
        setStaff(users);
    };

    // Charger le catalogue de services depuis le backend
    const loadServiceCatalog = useCallback(async () => {
        const catalog = await WorkflowService.loadServiceCatalog();
        if (catalog) {
            setServicesByCategory(WorkflowService.getServicesByCategory());
            setCatalogLoaded(true);
        }
    }, []);

    useEffect(() => {
        loadLeads();
        loadStaff();
        loadServiceCatalog();
    }, []);

    // Pré-charger les pipelines quand les leads changent
    useEffect(() => {
        if (leads.length > 0) {
            const serviceIds = leads.map(l => l.serviceId).filter(Boolean);
            WorkflowService.preloadPipelines(serviceIds);
        }
    }, [leads]);

    // Déterminer les colonnes à afficher
    const columns = useMemo(() => {
        return WorkflowService.getKanbanColumns(serviceFilter, activeTab);
    }, [serviceFilter, activeTab]);

    // Filtrer les leads selon le service, la recherche et l'onglet actif
    const filteredLeads = useMemo(() => {
        return leads.filter(lead => {
            // 1. Filtrage par onglet (Qualification vs Production)
            const isQualificationService = ['rappel_echeances', 'contact_simple', 'rdv_juriste'].includes(lead.serviceId);
            const matchesTab = activeTab === 'QUALIFICATION' ? isQualificationService : !isQualificationService;

            // 2. Filtrage par service sélectionné
            const matchesService = serviceFilter === 'all' || lead.serviceId === serviceFilter;

            // 3. Filtrage par recherche
            const matchesSearch = searchQuery === '' ||
                lead.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
                lead.id.toLowerCase().includes(searchQuery.toLowerCase());

            return matchesTab && matchesService && matchesSearch;
        });
    }, [leads, serviceFilter, searchQuery, activeTab]);

    const getLeadsByStage = (stage: WorkflowStage): Lead[] => {
        return filteredLeads.filter(lead => lead.currentStage === stage);
    };

    const handleUpdateStage = async (leadId: string, newStage: WorkflowStage) => {
        setIsUpdating(true);
        try {
            const updated = await DossierStore.updateStatus(leadId, newStage);
            if (updated) {
                setLeads(prev => prev.map(l => l.id === leadId ? updated : l));
                if (selectedLead?.id === leadId) setSelectedLead(updated);
            }
        } finally {
            setIsUpdating(false);
        }
    };

    const handleAssign = async (leadId: string, userId: string) => {
        setIsUpdating(true);
        try {
            const updated = await DossierStore.assignJurist(leadId, userId);
            if (updated) {
                setLeads(prev => prev.map(l => l.id === leadId ? updated : l));
                if (selectedLead?.id === leadId) setSelectedLead(updated);
            }
        } finally {
            setIsUpdating(false);
        }
    };

    const handleDocAction = async (leadId: string, docId: string, newStatus: 'VALID' | 'REJECTED') => {
        if (!selectedLead) return;
        setIsUpdating(true);
        try {
            const updatedDocs = (selectedLead.documents || []).map(doc =>
                doc.id === docId ? { ...doc, status: newStatus, validatedAt: new Date().toISOString() } : doc
            );
            const updated = await DossierStore.updateDocuments(leadId, updatedDocs);
            if (updated) {
                setLeads(prev => prev.map(l => l.id === leadId ? updated : l));
                setSelectedLead(updated);

                // Auto-advance logic: If all docs are VALID and we are in COLLECTING stage, move to REVIEW
                const allValid = updated.documents && updated.documents.every(d => d.status === 'VALID');
                if (allValid && updated.currentStage === 'COLLECTING') {
                    console.log('[AutoAdvance] All documents valid, moving to REVIEW');
                    const nextStepLead = await DossierStore.updateStatus(leadId, 'REVIEW' as any);
                    if (nextStepLead) {
                        setLeads(prev => prev.map(l => l.id === leadId ? nextStepLead : l));
                        setSelectedLead(nextStepLead);
                        alert('Dossier complet ! Passage automatique en Vérification.');
                    }
                }
            }
        } finally {
            setIsUpdating(false);
        }
    };

    const handleAddNote = async (leadId: string, text: string) => {
        setIsUpdating(true);
        try {
            const updated = await DossierStore.addNote(leadId, text);
            if (updated) {
                setLeads(prev => prev.map(l => l.id === leadId ? updated : l));
                setSelectedLead(updated);
            }
        } finally {
            setIsUpdating(false);
        }
    };

    return (
        <div className="h-full flex flex-col bg-slate-50">
            {/* Header / Toolbar */}
            <div className="bg-white border-b border-slate-200 p-6">
                <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900">Dashboard Siège</h1>
                        <p className="text-slate-500 text-sm">Gestion des flux entrants et qualification</p>
                    </div>

                    <div className="flex bg-slate-100 p-1 rounded-xl">
                        <button
                            onClick={() => { setActiveTab('PRODUCTION'); setServiceFilter('all'); }}
                            className={`px-4 py-2 rounded-lg text-sm font-bold transition-all ${activeTab === 'PRODUCTION' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-500 hover:text-slate-700'}`}
                        >
                            Production
                        </button>
                        <button
                            onClick={() => { setActiveTab('QUALIFICATION'); setServiceFilter('all'); }}
                            className={`px-4 py-2 rounded-lg text-sm font-bold transition-all ${activeTab === 'QUALIFICATION' ? 'bg-white text-indigo-600 shadow-sm' : 'text-slate-500 hover:text-slate-700'}`}
                        >
                            Qualification
                        </button>
                    </div>

                    <div className="flex flex-wrap items-center gap-3">
                        {/* Filtre Service */}
                        <div className="relative">
                            <select
                                value={serviceFilter}
                                onChange={(e) => setServiceFilter(e.target.value)}
                                className="pl-10 pr-8 py-2.5 bg-slate-100 border-none rounded-xl text-sm font-bold text-slate-700 appearance-none focus:ring-2 focus:ring-indigo-500 outline-none cursor-pointer min-w-[220px]"
                            >
                                {activeTab === 'PRODUCTION' ? (
                                    <>
                                        <option value="all">📁 Tous les dossiers</option>
                                        {Object.entries(servicesByCategory)
                                            .filter(([cat]) => cat !== 'CONSULTATION' || false)
                                            .map(([category, services]) => (
                                                <optgroup key={category} label={CATEGORY_LABELS[category] || category}>
                                                    {services.map(s => (
                                                        <option key={s.id} value={s.id}>{s.shortName}</option>
                                                    ))}
                                                </optgroup>
                                            ))
                                        }
                                    </>
                                ) : (
                                    <>
                                        <option value="all">📞 Tous les flux</option>
                                        <option value="rappel_echeances">Rappels</option>
                                        <option value="contact_simple">Contact Simple</option>
                                        <option value="rdv_juriste">RDV Juriste</option>
                                    </>
                                )}
                            </select>
                            <Filter className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                            <ChevronDown className="absolute right-3 top-1/2 -translate-y-1/2 text-slate-400 pointer-events-none" size={16} />
                        </div>

                        {/* Recherche */}
                        <div className="relative">
                            <input
                                type="text"
                                placeholder="Rechercher un dossier..."
                                value={searchQuery}
                                onChange={(e) => setSearchQuery(e.target.value)}
                                className="pl-10 pr-4 py-2.5 bg-slate-100 border-none rounded-xl text-sm focus:ring-2 focus:ring-indigo-500 outline-none w-64"
                            />
                            <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                        </div>

                        <button
                            onClick={loadLeads}
                            className="p-2.5 bg-indigo-50 text-indigo-600 rounded-xl hover:bg-indigo-100 transition-colors"
                        >
                            <Users size={20} />
                        </button>
                    </div>
                </div>
            </div>

            {/* Kanban Board */}
            <div className="flex-1 overflow-x-auto p-6 scrollbar-thin scrollbar-thumb-slate-300">
                <div className="flex gap-6 h-full min-w-max">
                    {columns.map((stage) => {
                        const stageLeads = getLeadsByStage(stage);
                        const label = WorkflowService.getStageLabel(stage);
                        const color = WorkflowService.getStageColor(stage);

                        return (
                            <div
                                key={stage}
                                className="w-80 flex-shrink-0 flex flex-col group"
                            >
                                {/* Column Header */}
                                <div className="flex items-center justify-between mb-4 px-1">
                                    <div className="flex items-center gap-2">
                                        <div className={`w-2 h-2 rounded-full bg-${color}-500 shadow-[0_0_8px_rgba(var(--color-${color}-500),0.5)]`} />
                                        <h3 className="font-black text-slate-800 text-sm uppercase tracking-wider">{label}</h3>
                                        <span className="bg-white px-2 py-0.5 rounded-full text-xs font-bold text-slate-400 border border-slate-200">
                                            {stageLeads.length}
                                        </span>
                                    </div>
                                </div>

                                {/* Column Content */}
                                <div className="flex-1 space-y-4 overflow-y-auto pr-1">
                                    {stageLeads.length === 0 ? (
                                        <div className="h-24 border-2 border-dashed border-slate-200 rounded-2xl flex items-center justify-center text-slate-400 text-xs font-medium">
                                            File vide
                                        </div>
                                    ) : (
                                        stageLeads.map((lead) => (
                                            <LeadCard
                                                key={lead.id}
                                                lead={lead}
                                                onView={() => setSelectedLead(lead)}
                                                onNext={() => {
                                                    const next = WorkflowService.getNextStage(lead.serviceId, lead.currentStage!);
                                                    if (next) handleUpdateStage(lead.id, next);
                                                }}
                                            />
                                        ))
                                    )}
                                </div>
                            </div>
                        );
                    })}
                </div>
            </div>

            {/* Modal Détail */}
            {selectedLead && (
                <LeadDetailModal
                    lead={selectedLead}
                    staff={staff}
                    isUpdating={isUpdating}
                    onClose={() => setSelectedLead(null)}
                    onAssign={(userId) => handleAssign(selectedLead.id, userId)}
                    onDocAction={(docId, status) => handleDocAction(selectedLead.id, docId, status)}
                    onNext={() => {
                        const next = WorkflowService.getNextStage(selectedLead.serviceId, selectedLead.currentStage!);
                        if (next) handleUpdateStage(selectedLead.id, next);
                    }}
                    onAddNote={(text) => handleAddNote(selectedLead.id, text)}
                />
            )}
        </div>
    );
}

// Composant Carte Lead
function LeadCard({
    lead,
    onView,
    onNext
}: {
    lead: Lead;
    onView: () => void;
    onNext: () => void;
}) {
    // Calculer les actions rapides
    const renderQuickActions = () => {
        if (lead.currentStage === 'REVIEW') {
            return (
                <button
                    onClick={(e) => { e.stopPropagation(); onNext(); }}
                    className="w-full mt-3 bg-emerald-500 hover:bg-emerald-600 text-white py-2 px-3 rounded-lg text-xs font-bold flex items-center justify-center gap-2 transition-all shadow-lg shadow-emerald-500/20"
                >
                    <SearchCheck size={14} />
                    Valider le dossier
                </button>
            );
        }

        if (lead.currentStage === 'SUBMITTED' && lead.serviceId === 'regroupement_familial') {
            return (
                <button
                    onClick={(e) => { e.stopPropagation(); onNext(); }}
                    className="w-full mt-3 bg-pink-500 hover:bg-pink-600 text-white py-2 px-3 rounded-lg text-xs font-bold flex items-center justify-center gap-2 transition-all shadow-lg shadow-pink-500/20"
                >
                    <Home size={14} />
                    Déclencher Suivi OFII
                </button>
            );
        }

        // Action par défaut : passer à l'étape suivante
        const nextStage = WorkflowService.getNextStage(lead.serviceId, lead.currentStage!);
        if (nextStage && lead.currentStage !== 'CLOSED') {
            return (
                <button
                    onClick={(e) => { e.stopPropagation(); onNext(); }}
                    className="w-full mt-3 bg-slate-900 hover:bg-slate-800 text-white py-2 px-3 rounded-lg text-xs font-bold flex items-center justify-center gap-2 transition-all"
                >
                    Étape suivante
                    <ArrowRight size={14} />
                </button>
            );
        }

        return null;
    };

    return (
        <div
            onClick={onView}
            className="bg-white rounded-2xl p-4 shadow-sm border border-slate-200 hover:border-indigo-300 hover:shadow-md transition-all cursor-pointer group/card"
        >
            <div className="flex items-center justify-between mb-3">
                <span className={`text-[10px] font-black px-2 py-1 rounded-lg uppercase tracking-wider ${lead.serviceId === 'regroupement_familial' ? 'bg-pink-100 text-pink-700' :
                    lead.serviceId === 'rdv_prefecture' ? 'bg-purple-100 text-purple-700' :
                        'bg-blue-100 text-blue-700'
                    }`}>
                    {lead.serviceName?.split(' ')[0]}
                </span>
                <span className="text-[10px] text-slate-400 font-mono">#{lead.id.slice(0, 6)}</span>
            </div>

            <h4 className="font-black text-slate-900 mb-1 group-hover/card:text-indigo-600 transition-colors">{lead.name}</h4>

            <div className="space-y-2 mt-3">
                <div className="flex items-center gap-2 text-[11px] text-slate-500">
                    <Calendar size={12} />
                    <span>{new Date(lead.createdAt).toLocaleDateString('fr-FR')}</span>
                </div>
                <div className="flex items-center gap-2 text-[11px] text-slate-500">
                    <Clock size={12} />
                    <span>Mis à jour il y a 2h</span>
                </div>

                {/* Indicateur de documents */}
                <div className="flex items-center gap-2 mt-2">
                    <div className="flex-1 h-1.5 bg-slate-100 rounded-full overflow-hidden">
                        <div
                            className="h-full bg-indigo-500 rounded-full"
                            style={{
                                width: `${(lead.documents?.filter(d => d.status === 'VALID').length || 0) / (lead.documents?.length || 1) * 100}%`
                            }}
                        />
                    </div>
                    <span className="text-[10px] font-bold text-slate-400">
                        {lead.documents?.filter(d => d.status === 'VALID').length || 0}/{lead.documents?.length || 0} docs
                    </span>
                </div>
            </div>

            {/* Actions rapides contextuelles */}
            {renderQuickActions()}
        </div>
    );
}

// Modal Détail Lead (Complété pour Juriste)
function LeadDetailModal({
    lead,
    staff,
    isUpdating,
    onClose,
    onAssign,
    onDocAction,
    onNext,
    onAddNote
}: {
    lead: Lead;
    staff: StaffUser[];
    isUpdating: boolean;
    onClose: () => void;
    onAssign: (userId: string) => void;
    onDocAction: (docId: string, status: 'VALID' | 'REJECTED') => void;
    onNext: () => void;
    onAddNote: (text: string) => void;
}) {
    const nextStage = WorkflowService.getNextStage(lead.serviceId, lead.currentStage!);
    const [noteText, setNoteText] = React.useState('');

    return (
        <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-sm flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-3xl shadow-2xl w-full max-w-4xl max-h-[90vh] overflow-hidden animate-in fade-in zoom-in duration-200 flex flex-col">
                {/* Header */}
                <div className="p-8 border-b border-slate-100 flex items-center justify-between">
                    <div>
                        <div className="flex items-center gap-3 mb-2">
                            <span className={`text-[10px] font-black px-2 py-1 rounded-lg uppercase tracking-wider bg-indigo-100 text-indigo-700`}>
                                {lead.serviceName}
                            </span>
                            <span className="text-[10px] text-slate-400 font-mono italic">#{lead.id}</span>
                        </div>
                        <h2 className="text-3xl font-black text-slate-900 leading-none">{lead.name}</h2>
                    </div>
                    <button onClick={onClose} className="p-3 bg-slate-100 hover:bg-slate-200 rounded-2xl transition-all">
                        <X size={20} />
                    </button>
                </div>

                <div className="flex-1 overflow-y-auto p-8">
                    <div className="grid grid-cols-3 gap-8 mb-8">
                        {/* Infos Contact */}
                        <div className="col-span-1 space-y-6">
                            <div>
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-3">Client</label>
                                <div className="bg-slate-50 rounded-2xl p-4 space-y-3">
                                    <p className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                        <FileText size={16} className="text-slate-400" /> {lead.email}
                                    </p>
                                    <p className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                        <TrendingUp size={16} className="text-slate-400" /> {lead.phone}
                                    </p>
                                    <p className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                        <Building2 size={16} className="text-slate-400" /> {(lead as any).originAgency?.name || 'Vente Directe'}
                                    </p>
                                </div>
                            </div>

                            {/* Assignation Juriste */}
                            <div className="pt-4 border-t border-slate-100">
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-3">Juriste Assigné</label>
                                <div className="space-y-3">
                                    <select
                                        value={lead.assignedUser || ''}
                                        onChange={(e) => onAssign(e.target.value)}
                                        disabled={isUpdating}
                                        className="w-full pl-4 pr-10 py-3 bg-slate-50 border-none rounded-2xl text-sm font-bold text-slate-700 appearance-none focus:ring-2 focus:ring-indigo-500 outline-none cursor-pointer"
                                    >
                                        <option value="">Non assigné</option>
                                        {staff.filter(u => u.role === 'HQ_ADMIN' || u.role === 'SUPER_ADMIN').map(u => (
                                            <option key={u.id} value={u.id}>{u.name}</option>
                                        ))}
                                    </select>
                                    {lead.assignedUser && (
                                        <div className="flex items-center gap-2 px-4 py-2 bg-emerald-50 text-emerald-700 rounded-xl text-[11px] font-bold">
                                            <CheckCircle2 size={14} />
                                            Dossier pris en charge
                                        </div>
                                    )}
                                </div>
                            </div>

                            {/* Notes Internes */}
                            <div className="pt-4 border-t border-slate-100">
                                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-3">Notes Internes</label>
                                <div className="space-y-4 max-h-[250px] overflow-y-auto mb-4 pr-2">
                                    {lead.notes && lead.notes.length > 0 ? (
                                        lead.notes.map((note, idx) => {
                                            const authorLabel = note.authorName || note.author;
                                            const isAgentNote = authorLabel?.includes('🤖');

                                            return (
                                                <div
                                                    key={idx}
                                                    className={`p-4 rounded-2xl relative group border-2 ${isAgentNote
                                                            ? 'bg-amber-50/80 border-amber-200'
                                                            : 'bg-slate-50 border-transparent'
                                                        }`}
                                                >
                                                    {isAgentNote && (
                                                        <div className="absolute -top-2.5 right-4 bg-amber-400 text-amber-900 px-2 py-0.5 rounded-full text-[9px] font-black tracking-widest uppercase shadow-sm flex items-center gap-1">
                                                            <span>Agent QA</span>
                                                        </div>
                                                    )}
                                                    <p className={`text-xs font-medium leading-relaxed whitespace-pre-wrap ${isAgentNote ? 'text-amber-900 font-semibold' : 'text-slate-700'
                                                        }`}>
                                                        {note.content}
                                                    </p>
                                                    <div className="mt-3 flex items-center justify-between">
                                                        <span className={`text-[9px] font-black uppercase tracking-wider ${isAgentNote ? 'text-amber-600' : 'text-indigo-400'
                                                            }`}>
                                                            {authorLabel}
                                                        </span>
                                                        <span className={`text-[9px] font-mono flex items-center gap-1 ${isAgentNote ? 'text-amber-500 font-bold' : 'text-slate-400 italic'
                                                            }`}>
                                                            {new Date(note.createdAt).toLocaleDateString('fr-FR')} à {new Date(note.createdAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                        </span>
                                                    </div>
                                                </div>
                                            );
                                        })
                                    ) : (
                                        <p className="text-xs text-slate-400 italic text-center py-4">Aucune note pour le moment</p>
                                    )}
                                </div>
                                <div className="relative">
                                    <textarea
                                        value={noteText}
                                        onChange={(e) => setNoteText(e.target.value)}
                                        placeholder="Ajouter une instruction..."
                                        className="w-full p-4 bg-slate-50 border-none rounded-2xl text-sm font-bold text-slate-700 focus:ring-2 focus:ring-indigo-500 outline-none resize-none h-24 placeholder:text-slate-300"
                                    />
                                    <button
                                        onClick={() => {
                                            if (noteText.trim()) {
                                                onAddNote(noteText);
                                                setNoteText('');
                                            }
                                        }}
                                        disabled={!noteText.trim() || isUpdating}
                                        className="absolute bottom-3 right-3 p-2 bg-indigo-500 text-white rounded-xl shadow-lg shadow-indigo-200 hover:bg-indigo-600 transition-all disabled:opacity-50"
                                    >
                                        <Plus size={16} />
                                    </button>
                                </div>
                            </div>
                        </div>

                        {/* Documents */}
                        <div className="col-span-1">
                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-3">Dossier Digital ({lead.documents?.length || 0} pièces)</label>
                            <div className="space-y-3 shadow-inner bg-slate-50/50 p-4 rounded-3xl min-h-[300px]">
                                {lead.documents && lead.documents.length > 0 ? (
                                    lead.documents.map(doc => (
                                        <div key={doc.id} className="group bg-white p-4 rounded-2xl border border-slate-100 flex items-center justify-between hover:shadow-md transition-all">
                                            <div className="flex items-center gap-4">
                                                <div className={`p-3 rounded-xl ${doc.status === 'VALID' ? 'bg-emerald-100 text-emerald-600' : doc.status === 'REJECTED' ? 'bg-rose-100 text-rose-600' : 'bg-slate-100 text-slate-400'}`}>
                                                    <FileText size={20} />
                                                </div>
                                                <div>
                                                    <p className="text-sm font-black text-slate-800 break-all">{doc.docType}</p>
                                                    <p className="text-[11px] text-slate-400 font-medium">Reçu le 12/01 • PDF (1.2MB)</p>
                                                </div>
                                            </div>

                                            <div className="flex items-center gap-2 opacity-0 group-hover:opacity-100 transition-opacity">
                                                <button
                                                    onClick={() => onDocAction(doc.id, 'VALID')}
                                                    disabled={isUpdating || doc.status === 'VALID'}
                                                    className={`p-2 rounded-xl transition-all ${doc.status === 'VALID' ? 'bg-emerald-500 text-white' : 'bg-slate-50 text-slate-400 hover:bg-emerald-50 hover:text-emerald-600'}`}
                                                >
                                                    <CheckCircle2 size={18} />
                                                </button>
                                                <button
                                                    onClick={() => onDocAction(doc.id, 'REJECTED')}
                                                    disabled={isUpdating || doc.status === 'REJECTED'}
                                                    className={`p-2 rounded-xl transition-all ${doc.status === 'REJECTED' ? 'bg-rose-500 text-white' : 'bg-slate-50 text-slate-400 hover:bg-rose-50 hover:text-rose-600'}`}
                                                >
                                                    <X size={18} />
                                                </button>
                                            </div>
                                        </div>
                                    ))
                                ) : (
                                    <div className="flex flex-col items-center justify-center h-full text-slate-400 italic text-sm">
                                        Aucun document téléversé
                                    </div>
                                )}
                            </div>
                        </div>

                        {/* WhatsApp Widget */}
                        <div className="col-span-1">
                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-3">Communication Client</label>
                            <WhatsAppWidget
                                contactId={lead.id}
                                contactType="LEAD"
                                contactName={lead.name}
                                contactPhone={lead.phone || '0600000000'}
                            />
                        </div>
                    </div>
                </div>

                {/* Footer Actions */}
                <div className="p-8 bg-slate-50 border-t border-slate-100 flex items-center justify-between">
                    <div className="flex items-center gap-4">
                        <div className="text-center">
                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1">Paiement</p>
                            <p className="text-xl font-black text-emerald-600">{(lead.amountPaid / 100).toFixed(0)} €</p>
                        </div>
                        <div className="h-10 w-[1px] bg-slate-200" />
                        <div>
                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-1">Statut Actuel</p>
                            <p className="text-sm font-bold text-slate-700">{WorkflowService.getStageLabel(lead.currentStage!)}</p>
                        </div>
                    </div>

                    <div className="flex gap-4">
                        <button
                            onClick={onClose}
                            className="px-8 py-4 bg-white hover:bg-slate-100 text-slate-600 rounded-2xl font-black text-sm transition-all border border-slate-200"
                        >
                            Fermer
                        </button>
                        {nextStage && (
                            <button
                                onClick={onNext}
                                disabled={isUpdating}
                                className="px-8 py-4 bg-slate-900 hover:bg-slate-800 text-white rounded-2xl font-black text-sm transition-all flex items-center gap-2 shadow-xl shadow-slate-900/20"
                            >
                                Passer à : {WorkflowService.getStageLabel(nextStage)}
                                <ArrowRight size={18} />
                            </button>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
}
