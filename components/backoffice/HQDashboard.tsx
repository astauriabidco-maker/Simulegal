'use client';

import React, { useState, useEffect, useMemo } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import { WorkflowService, WorkflowStage } from '../../services/WorkflowService';
import { NotificationService } from '../../services/NotificationService';
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
    SearchCheck
} from 'lucide-react';

// Services disponibles pour le filtre
const SERVICES = [
    { id: 'all', label: 'Tous les services' },
    { id: 'naturalisation', label: 'Naturalisation' },
    { id: 'rdv_prefecture', label: 'RDV Préfecture' },
    { id: 'regroupement_familial', label: 'Regroupement Familial' },
    { id: 'permis_conduire', label: 'Permis de conduire' },
];

interface HQDashboardProps {
    onViewDossier?: (lead: Lead) => void;
}

export default function HQDashboard({ onViewDossier }: HQDashboardProps) {
    const [leads, setLeads] = useState<Lead[]>([]);
    const [selectedLead, setSelectedLead] = useState<Lead | null>(null);
    const [serviceFilter, setServiceFilter] = useState('all');
    const [searchQuery, setSearchQuery] = useState('');
    const [activeTab, setActiveTab] = useState<'PRODUCTION' | 'QUALIFICATION'>('PRODUCTION');

    useEffect(() => {
        loadLeads();
    }, []);

    const loadLeads = async () => {
        try {
            const allLeads = await CRM.getAllLeads();
            setLeads(allLeads);
        } catch (error) {
            console.error('[HQ] Erreur chargement leads:', error);
        }
    };

    // Déterminer les colonnes à afficher
    const columns = useMemo(() => {
        if (activeTab === 'QUALIFICATION') {
            return ['NEW', 'TO_CONTACT', 'QUALIFIED', 'ARCHIVED'] as WorkflowStage[];
        }

        if (serviceFilter === 'all') {
            // Vue par défaut pour "Tous"
            return ['NEW', 'COLLECTING', 'REVIEW', 'DRAFTING', 'SUBMITTED', 'DECISION_WAIT', 'CLOSED'] as WorkflowStage[];
        }
        return WorkflowService.getStepsForService(serviceFilter);
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
        // En prod, appel API
        console.log(`[HQ] Passage du dossier ${leadId} à l'étape ${newStage}`);

        const leadToUpdate = leads.find(l => l.id === leadId);
        if (leadToUpdate) {
            NotificationService.onStageChange(leadToUpdate, leadToUpdate.currentStage, newStage);
        }

        const updatedLeads = leads.map(l =>
            l.id === leadId ? { ...l, currentStage: newStage, updatedAt: new Date().toISOString() } : l
        );
        setLeads(updatedLeads);
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
                                className="pl-10 pr-8 py-2.5 bg-slate-100 border-none rounded-xl text-sm font-bold text-slate-700 appearance-none focus:ring-2 focus:ring-indigo-500 outline-none cursor-pointer"
                            >
                                {activeTab === 'PRODUCTION' ? (
                                    <>
                                        <option value="all">Tous les dossiers</option>
                                        <option value="naturalisation">Naturalisation</option>
                                        <option value="rdv_prefecture">RDV Préfecture</option>
                                        <option value="regroupement_familial">Regroupement Familial</option>
                                        <option value="permis_conduire">Permis de conduire</option>
                                    </>
                                ) : (
                                    <>
                                        <option value="all">Tous les flux</option>
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
                                                    const next = WorkflowService.getNextStage(lead.serviceId, lead.currentStage);
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
                    onClose={() => setSelectedLead(null)}
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
        const nextStage = WorkflowService.getNextStage(lead.serviceId, lead.currentStage);
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

// Modal Détail Lead (Simplifié)
function LeadDetailModal({ lead, onClose }: { lead: Lead; onClose: () => void }) {
    return (
        <div className="fixed inset-0 bg-slate-900/60 backdrop-blur-sm flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-3xl shadow-2xl w-full max-w-2xl overflow-hidden animate-in fade-in zoom-in duration-200">
                <div className="p-8">
                    <div className="flex items-center justify-between mb-8">
                        <div>
                            <h2 className="text-3xl font-black text-slate-900">{lead.name}</h2>
                            <p className="text-slate-500 font-medium">#{lead.id} • {lead.serviceName}</p>
                        </div>
                        <button onClick={onClose} className="p-2 bg-slate-100 hover:bg-slate-200 rounded-2xl transition-colors">
                            <X size={24} />
                        </button>
                    </div>

                    <div className="grid grid-cols-2 gap-8 mb-8">
                        <div>
                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-2">Informations</label>
                            <div className="bg-slate-50 rounded-2xl p-4 space-y-3">
                                <p className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                    <FileText size={16} /> {lead.email}
                                </p>
                                <p className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                    <MapPin size={16} /> Web Direct
                                </p>
                            </div>
                        </div>
                        <div>
                            <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-2">Paiement</label>
                            <div className="bg-emerald-50 rounded-2xl p-4">
                                <p className="text-lg font-black text-emerald-700">{(lead.amountPaid / 100).toFixed(0)} € Payé</p>
                                <p className="text-xs font-bold text-emerald-600/60 uppercase">Détails de la transaction</p>
                            </div>
                        </div>
                    </div>

                    <div>
                        <label className="text-[10px] font-black uppercase tracking-widest text-slate-400 block mb-2">Documents</label>
                        <div className="grid grid-cols-2 gap-4">
                            {lead.documents?.map(doc => (
                                <div key={doc.id} className="flex items-center justify-between p-3 bg-slate-50 hover:bg-slate-100 rounded-xl transition-colors">
                                    <span className="text-xs font-bold text-slate-700">{doc.docType}</span>
                                    <div className={`px-2 py-0.5 rounded-full text-[10px] font-black uppercase ${doc.status === 'VALID' ? 'bg-emerald-100 text-emerald-700' : 'bg-amber-100 text-amber-700'
                                        }`}>
                                        {doc.status}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>

                <div className="p-6 bg-slate-50 border-t border-slate-100 flex gap-4">
                    <button onClick={onClose} className="flex-1 py-4 bg-white hover:bg-slate-100 text-slate-600 rounded-2xl font-bold transition-all border border-slate-200">
                        Fermer le dossier
                    </button>
                    <button className="flex-1 py-4 bg-indigo-600 hover:bg-indigo-700 text-white rounded-2xl font-bold transition-all shadow-lg shadow-indigo-600/20">
                        Ouvrir dans le CRM
                    </button>
                </div>
            </div>
        </div>
    );
}
