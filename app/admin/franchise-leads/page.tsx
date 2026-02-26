'use client';

import React, { useEffect, useState, useMemo, useCallback } from 'react';
import { FranchiseLead, FranchiseLeadStore, FranchiseLeadStatus } from '../../../services/FranchiseLeadStore';
import {
    Plus, Search, MoreHorizontal, MapPin, Mail, Phone, CheckCircle, FileText,
    Download, BarChart3, Kanban, X, Filter, TrendingUp, Users, Percent, Calendar,
    ScrollText, FileSignature, Clock, AlertCircle, XCircle, Eye, Loader2, Ban
} from 'lucide-react';
import { AuthStore } from '../../../services/authStore';

const STATUS_COLUMNS: { id: FranchiseLeadStatus; label: string; color: string; bgColor: string }[] = [
    { id: 'NEW', label: 'Nouveaux', color: 'text-blue-700', bgColor: 'bg-blue-100' },
    { id: 'CONTACTED', label: 'Contactés', color: 'text-yellow-700', bgColor: 'bg-yellow-100' },
    { id: 'MEETING', label: 'RDV Planifié', color: 'text-purple-700', bgColor: 'bg-purple-100' },
    { id: 'VALIDATED', label: 'Projet Validé', color: 'text-indigo-700', bgColor: 'bg-indigo-100' },
    { id: 'DIP_SENT', label: 'DIP Envoyé', color: 'text-cyan-700', bgColor: 'bg-cyan-100' },
    { id: 'CONTRACT_SENT', label: 'Contrat Envoyé', color: 'text-orange-700', bgColor: 'bg-orange-100' },
    { id: 'SIGNED', label: 'Signés', color: 'text-emerald-700', bgColor: 'bg-emerald-100' },
    { id: 'REJECTED', label: 'Rejetés', color: 'text-red-700', bgColor: 'bg-red-100' }
];

/** Allowed drag-and-drop transitions (respects pipeline order + Loi Doubin) */
const ALLOWED_TRANSITIONS: Record<string, string[]> = {
    NEW: ['CONTACTED', 'REJECTED'],
    CONTACTED: ['MEETING', 'REJECTED'],
    MEETING: ['VALIDATED', 'REJECTED'],
    VALIDATED: ['REJECTED'], // DIP_SENT only via "Envoyer DIP" button (triggers backend logic)
    DIP_SENT: ['REJECTED'],  // CONTRACT_SENT only via "Générer Contrat" (respects 20-day cooling)
    CONTRACT_SENT: ['REJECTED'], // SIGNED only via "Signer" button (creates agency)
    SIGNED: [],               // Terminal state
    REJECTED: ['NEW'],        // Can be re-opened
};

const REGIONS = ['IDF', 'AURA', 'PACA', 'HDF', 'NAQ', 'OCC', 'BRE', 'NOR', 'GES', 'PDL', 'BFC', 'CVL'];

type ViewMode = 'KANBAN' | 'ANALYTICS';

interface Analytics {
    total: number;
    statusCounts: Record<string, number>;
    regionCounts: Record<string, number>;
    conversionRate: number;
    monthlyTrend: { month: string; count: number; signed: number }[];
}

export default function FranchiseLeadsPage() {
    const [leads, setLeads] = useState<FranchiseLead[]>([]);
    const [loading, setLoading] = useState(true);
    const [viewMode, setViewMode] = useState<ViewMode>('KANBAN');
    const [analytics, setAnalytics] = useState<Analytics | null>(null);

    // Filters
    const [searchQuery, setSearchQuery] = useState('');
    const [regionFilter, setRegionFilter] = useState<string>('');
    const [dateFilter, setDateFilter] = useState<string>('');

    // Modal
    const [isCreateModalOpen, setIsCreateModalOpen] = useState(false);
    const [newLeadData, setNewLeadData] = useState({
        name: '', email: '', phone: '', targetCity: '', region: 'IDF', type: 'FRANCHISE'
    });

    // Drag state
    const [draggedLead, setDraggedLead] = useState<FranchiseLead | null>(null);
    const [dropTarget, setDropTarget] = useState<string | null>(null);

    // Toast
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'warning' } | null>(null);
    const [actionLoading, setActionLoading] = useState<string | null>(null);

    const showToast = useCallback((message: string, type: 'success' | 'error' | 'warning' = 'success') => {
        setToast({ message, type });
        setTimeout(() => setToast(null), 4000);
    }, []);

    useEffect(() => {
        loadLeads();
    }, []);

    useEffect(() => {
        if (viewMode === 'ANALYTICS') loadAnalytics();
    }, [viewMode]);

    const loadLeads = async () => {
        setLoading(true);
        const data = await FranchiseLeadStore.getAll();
        setLeads(data);
        setLoading(false);
    };

    const loadAnalytics = async () => {
        const data = await FranchiseLeadStore.getAnalytics();
        if (data) setAnalytics(data);
    };

    // Filtered leads
    const filteredLeads = useMemo(() => {
        return leads.filter(lead => {
            if (searchQuery) {
                const q = searchQuery.toLowerCase();
                const match = lead.name.toLowerCase().includes(q) ||
                    lead.email.toLowerCase().includes(q) ||
                    lead.targetCity.toLowerCase().includes(q);
                if (!match) return false;
            }
            if (regionFilter && lead.region !== regionFilter) return false;
            if (dateFilter) {
                const days = parseInt(dateFilter);
                const leadDate = new Date(lead.createdAt);
                const cutoff = new Date();
                cutoff.setDate(cutoff.getDate() - days);
                if (leadDate < cutoff) return false;
            }
            return true;
        });
    }, [leads, searchQuery, regionFilter, dateFilter]);

    const handleCreateLead = async () => {
        if (!newLeadData.name || !newLeadData.email) {
            showToast('Veuillez remplir au moins le nom et l\'email.', 'error');
            return;
        }
        const { type, ...leadData } = newLeadData;
        await FranchiseLeadStore.create({
            ...leadData,
            status: 'NEW' as any,
            contractDetails: JSON.stringify({ type, commissionRate: type === 'CORNER' ? 5 : 15 })
        });
        setIsCreateModalOpen(false);
        setNewLeadData({ name: '', email: '', phone: '', targetCity: '', region: 'IDF', type: 'FRANCHISE' });
        loadLeads();
        showToast('Candidat créé avec succès');
    };

    // ── Quick Actions ──

    const handleQuickTransition = async (lead: FranchiseLead, newStatus: FranchiseLeadStatus, label: string) => {
        setActionLoading(lead.id);
        await FranchiseLeadStore.updateStatus(lead.id, newStatus);
        await loadLeads();
        setActionLoading(null);
        showToast(`${lead.name} → ${label}`);
    };

    const handleSendDIP = async (lead: FranchiseLead) => {
        if (!confirm(`Envoyer le DIP à ${lead.name} ?\nCela démarrera le délai légal de réflexion de 20 jours.`)) return;
        setActionLoading(lead.id);
        const result = await FranchiseLeadStore.sendDIP(lead.id);
        setActionLoading(null);
        if (result) {
            await loadLeads();
            showToast(`DIP envoyé à ${lead.name}. Délai de 20 jours lancé.`);
        } else {
            showToast('Erreur lors de l\'envoi du DIP', 'error');
        }
    };

    const handleGenerateContract = async (lead: FranchiseLead) => {
        setActionLoading(lead.id);
        window.open(`http://localhost:4000/franchise-leads/${lead.id}/contract`, '_blank');
        await new Promise(r => setTimeout(r, 2000));
        await loadLeads();
        setActionLoading(null);
        showToast(`Contrat généré pour ${lead.name}`);
    };

    const handleSignContract = async (lead: FranchiseLead) => {
        if (!confirm(`Signer le contrat de ${lead.name} ?\nCela créera automatiquement l'agence et le compte gérant.`)) return;
        setActionLoading(lead.id);
        const result = await FranchiseLeadStore.signContract(lead.id);
        setActionLoading(null);
        if (result) {
            await loadLeads();
            showToast(`🎉 Contrat signé ! Agence créée pour ${lead.name}`);
        } else {
            showToast('Erreur lors de la signature', 'error');
        }
    };

    const handleReject = async (lead: FranchiseLead) => {
        const reason = prompt(`Motif du rejet pour ${lead.name} :`);
        if (!reason) return;
        setActionLoading(lead.id);
        await FranchiseLeadStore.update(lead.id, { status: 'REJECTED' as any, rejectionReason: reason });
        await loadLeads();
        setActionLoading(null);
        showToast(`${lead.name} rejeté`, 'warning');
    };

    // ── Drag & Drop (secured) ──

    const handleDragStart = (e: React.DragEvent, lead: FranchiseLead) => {
        setDraggedLead(lead);
        e.dataTransfer.effectAllowed = 'move';
    };

    const handleDragOver = (e: React.DragEvent, columnId: string) => {
        e.preventDefault();
        if (draggedLead) {
            const allowed = ALLOWED_TRANSITIONS[draggedLead.status] || [];
            if (allowed.includes(columnId)) {
                e.dataTransfer.dropEffect = 'move';
                setDropTarget(columnId);
            } else {
                e.dataTransfer.dropEffect = 'none';
                setDropTarget(null);
            }
        }
    };

    const handleDragLeave = () => {
        setDropTarget(null);
    };

    const handleDrop = async (e: React.DragEvent, newStatus: FranchiseLeadStatus) => {
        e.preventDefault();
        setDropTarget(null);
        if (!draggedLead || draggedLead.status === newStatus) {
            setDraggedLead(null);
            return;
        }

        // Check if transition is allowed
        const allowed = ALLOWED_TRANSITIONS[draggedLead.status] || [];
        if (!allowed.includes(newStatus)) {
            setDraggedLead(null);
            const statusLabel = STATUS_COLUMNS.find(c => c.id === newStatus)?.label || newStatus;
            showToast(`Transition interdite vers "${statusLabel}". Utilisez les boutons d'action.`, 'error');
            return;
        }

        // If rejecting via drag, ask for reason
        if (newStatus === 'REJECTED') {
            const reason = prompt(`Motif du rejet pour ${draggedLead.name} :`);
            if (!reason) {
                setDraggedLead(null);
                return;
            }
            setLeads(prev => prev.map(l => l.id === draggedLead.id ? { ...l, status: newStatus } : l));
            setDraggedLead(null);
            await FranchiseLeadStore.update(draggedLead.id, { status: 'REJECTED' as any, rejectionReason: reason });
            showToast(`${draggedLead.name} rejeté`, 'warning');
            return;
        }

        // Optimistic update + API call
        setLeads(prev => prev.map(l => l.id === draggedLead.id ? { ...l, status: newStatus } : l));
        const leadName = draggedLead.name;
        setDraggedLead(null);
        await FranchiseLeadStore.updateStatus(draggedLead.id, newStatus);
        const label = STATUS_COLUMNS.find(c => c.id === newStatus)?.label || newStatus;
        showToast(`${leadName} → ${label}`);
    };

    /** Compute cooling period days remaining */
    const getCoolingDays = (lead: FranchiseLead): number | null => {
        if (lead.status !== 'DIP_SENT' || !lead.dipSentAt) return null;
        const days = Math.floor((Date.now() - new Date(lead.dipSentAt).getTime()) / (1000 * 60 * 60 * 24));
        return Math.max(0, 20 - days);
    };

    return (
        <div className="p-8 h-full flex flex-col">
            {/* Toast */}
            {toast && (
                <div className={`fixed top-6 right-6 z-50 px-5 py-3 rounded-xl shadow-xl font-bold text-sm flex items-center gap-2 transition-all animate-in slide-in-from-right ${toast.type === 'success' ? 'bg-emerald-500 text-white' :
                        toast.type === 'error' ? 'bg-rose-500 text-white' :
                            'bg-amber-500 text-white'
                    }`}>
                    {toast.type === 'success' && <CheckCircle size={16} />}
                    {toast.type === 'error' && <XCircle size={16} />}
                    {toast.type === 'warning' && <AlertCircle size={16} />}
                    {toast.message}
                    <button onClick={() => setToast(null)} className="ml-2 opacity-70 hover:opacity-100"><X size={14} /></button>
                </div>
            )}

            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-2xl font-black text-slate-900">Recrutement Franchisés</h1>
                    <p className="text-slate-500 text-sm">Pipeline commercial du réseau</p>
                </div>
                <div className="flex items-center gap-3">
                    {/* View Toggle */}
                    <div className="flex bg-slate-100 rounded-xl p-1">
                        <button
                            onClick={() => setViewMode('KANBAN')}
                            className={`flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-bold transition-all ${viewMode === 'KANBAN' ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                        >
                            <Kanban size={16} /> Pipeline
                        </button>
                        <button
                            onClick={() => setViewMode('ANALYTICS')}
                            className={`flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-bold transition-all ${viewMode === 'ANALYTICS' ? 'bg-white shadow text-indigo-600' : 'text-slate-500'}`}
                        >
                            <BarChart3 size={16} /> Analytics
                        </button>
                    </div>

                    {/* Export */}
                    <button
                        onClick={() => FranchiseLeadStore.downloadCSV()}
                        className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-200 transition-colors"
                    >
                        <Download size={16} /> Export CSV
                    </button>

                    {/* Create */}
                    <button
                        onClick={() => setIsCreateModalOpen(true)}
                        className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors shadow-lg shadow-indigo-200"
                    >
                        <Plus size={16} /> Nouveau
                    </button>
                </div>
            </div>

            {/* Filters Bar */}
            {viewMode === 'KANBAN' && (
                <div className="flex items-center gap-4 mb-6 p-4 bg-white rounded-2xl border border-slate-200 shadow-sm">
                    <div className="flex items-center gap-2 flex-1">
                        <Search size={18} className="text-slate-400" />
                        <input
                            type="text"
                            placeholder="Rechercher (nom, email, ville)..."
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="flex-1 outline-none text-sm font-medium"
                        />
                        {searchQuery && (
                            <button onClick={() => setSearchQuery('')} className="text-slate-400 hover:text-slate-600">
                                <X size={16} />
                            </button>
                        )}
                    </div>

                    <div className="h-6 w-px bg-slate-200" />

                    <div className="flex items-center gap-2">
                        <MapPin size={16} className="text-slate-400" />
                        <select
                            value={regionFilter}
                            onChange={(e) => setRegionFilter(e.target.value)}
                            className="outline-none text-sm font-medium bg-transparent cursor-pointer"
                        >
                            <option value="">Toutes régions</option>
                            {REGIONS.map(r => <option key={r} value={r}>{r}</option>)}
                        </select>
                    </div>

                    <div className="h-6 w-px bg-slate-200" />

                    <div className="flex items-center gap-2">
                        <Calendar size={16} className="text-slate-400" />
                        <select
                            value={dateFilter}
                            onChange={(e) => setDateFilter(e.target.value)}
                            className="outline-none text-sm font-medium bg-transparent cursor-pointer"
                        >
                            <option value="">Toutes dates</option>
                            <option value="7">7 derniers jours</option>
                            <option value="30">30 derniers jours</option>
                            <option value="90">3 derniers mois</option>
                        </select>
                    </div>

                    {(searchQuery || regionFilter || dateFilter) && (
                        <>
                            <div className="h-6 w-px bg-slate-200" />
                            <button
                                onClick={() => { setSearchQuery(''); setRegionFilter(''); setDateFilter(''); }}
                                className="text-xs text-red-500 font-bold hover:underline"
                            >
                                Réinitialiser
                            </button>
                        </>
                    )}

                    <div className="text-xs text-slate-400 font-medium">
                        {filteredLeads.length} résultat{filteredLeads.length > 1 ? 's' : ''}
                    </div>
                </div>
            )}

            {/* Kanban View */}
            {viewMode === 'KANBAN' && (
                <div className="flex gap-4 overflow-x-auto pb-4 flex-1">
                    {STATUS_COLUMNS.map(column => {
                        const isValidDrop = draggedLead ? (ALLOWED_TRANSITIONS[draggedLead.status] || []).includes(column.id) : false;
                        const isActiveTarget = dropTarget === column.id;

                        return (
                            <div
                                key={column.id}
                                className={`min-w-[280px] w-[280px] rounded-2xl flex flex-col max-h-full transition-all ${isActiveTarget ? 'bg-indigo-50 ring-2 ring-indigo-300' :
                                        draggedLead && !isValidDrop && draggedLead.status !== column.id ? 'bg-slate-50 opacity-50' :
                                            'bg-slate-50'
                                    }`}
                                onDragOver={(e) => handleDragOver(e, column.id)}
                                onDragLeave={handleDragLeave}
                                onDrop={(e) => handleDrop(e, column.id)}
                            >
                                <div className="p-4 border-b border-slate-200 flex justify-between items-center sticky top-0 bg-inherit rounded-t-2xl z-10">
                                    <div className="flex items-center gap-2">
                                        <span className={`px-2.5 py-1 rounded-lg text-xs font-black ${column.bgColor} ${column.color}`}>
                                            {filteredLeads.filter(l => l.status === column.id).length}
                                        </span>
                                        <span className="font-bold text-slate-700 text-sm">{column.label}</span>
                                    </div>
                                    {draggedLead && isValidDrop && (
                                        <span className="text-[10px] text-indigo-500 font-bold animate-pulse">Déposer ici</span>
                                    )}
                                    {draggedLead && !isValidDrop && draggedLead.status !== column.id && (
                                        <Ban size={14} className="text-slate-300" />
                                    )}
                                </div>

                                <div className="p-3 space-y-3 overflow-y-auto flex-1">
                                    {filteredLeads.filter(l => l.status === column.id).map(lead => {
                                        const coolingDays = getCoolingDays(lead);
                                        const isLoading = actionLoading === lead.id;

                                        return (
                                            <div
                                                key={lead.id}
                                                draggable={!isLoading}
                                                onDragStart={(e) => handleDragStart(e, lead)}
                                                className={`bg-white p-4 rounded-xl shadow-sm border-2 border-transparent hover:border-indigo-200 hover:shadow-md transition-all cursor-grab active:cursor-grabbing relative ${draggedLead?.id === lead.id ? 'opacity-50 scale-95' : ''
                                                    } ${isLoading ? 'opacity-60 pointer-events-none' : ''}`}
                                            >
                                                {isLoading && (
                                                    <div className="absolute inset-0 bg-white/50 rounded-xl flex items-center justify-center z-10">
                                                        <Loader2 size={20} className="animate-spin text-indigo-500" />
                                                    </div>
                                                )}

                                                {/* Card Header */}
                                                <div className="flex justify-between items-start mb-2">
                                                    <h3 className="font-bold text-slate-800 text-sm">{lead.name}</h3>
                                                    <button
                                                        onClick={(e) => { e.stopPropagation(); window.location.href = `/admin/franchise-leads/${lead.id}`; }}
                                                        className="text-slate-400 hover:text-indigo-600 p-1 rounded-lg hover:bg-indigo-50 transition-colors"
                                                        title="Voir le détail"
                                                    >
                                                        <Eye size={14} />
                                                    </button>
                                                </div>

                                                {/* Card Info */}
                                                <div className="space-y-1 mb-3">
                                                    <div className="flex items-center gap-2 text-xs text-slate-500">
                                                        <MapPin size={12} />
                                                        <span>{lead.targetCity} ({lead.region})</span>
                                                    </div>
                                                    <div className="flex items-center gap-2 text-xs text-slate-500">
                                                        <Mail size={12} />
                                                        <span className="truncate">{lead.email}</span>
                                                    </div>
                                                    {lead.companyName && (
                                                        <div className="text-[10px] text-slate-400 font-medium truncate">
                                                            🏢 {lead.companyName}
                                                        </div>
                                                    )}
                                                </div>

                                                {/* Cooling Period Badge */}
                                                {coolingDays !== null && (
                                                    <div className={`mb-3 px-3 py-2 rounded-lg text-xs font-bold flex items-center gap-2 ${coolingDays > 0
                                                            ? 'bg-amber-50 text-amber-700 border border-amber-200'
                                                            : 'bg-emerald-50 text-emerald-700 border border-emerald-200'
                                                        }`}>
                                                        <Clock size={12} />
                                                        {coolingDays > 0
                                                            ? `Délai : ${coolingDays}j restants`
                                                            : '✅ Délai écoulé'
                                                        }
                                                    </div>
                                                )}

                                                {/* Rejection reason */}
                                                {lead.status === 'REJECTED' && lead.rejectionReason && (
                                                    <div className="mb-3 px-3 py-2 rounded-lg text-[10px] bg-rose-50 text-rose-600 border border-rose-200 line-clamp-2">
                                                        ❌ {lead.rejectionReason}
                                                    </div>
                                                )}

                                                {/* Action Buttons — Contextual per status */}
                                                <div className="flex flex-wrap gap-1.5 pt-2 border-t border-slate-100">
                                                    {/* NEW → Contacté */}
                                                    {lead.status === 'NEW' && (
                                                        <button
                                                            onClick={(e) => { e.stopPropagation(); handleQuickTransition(lead, 'CONTACTED', 'Contacté'); }}
                                                            className="text-[11px] bg-blue-50 text-blue-600 px-2 py-1 rounded-lg hover:bg-blue-100 flex items-center gap-1 font-bold transition-colors"
                                                        >
                                                            <Phone size={11} /> Contacté
                                                        </button>
                                                    )}

                                                    {/* CONTACTED → Meeting / Validated */}
                                                    {lead.status === 'CONTACTED' && (
                                                        <>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); handleQuickTransition(lead, 'MEETING', 'RDV planifié'); }}
                                                                className="text-[11px] bg-purple-50 text-purple-600 px-2 py-1 rounded-lg hover:bg-purple-100 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <Calendar size={11} /> RDV
                                                            </button>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); handleQuickTransition(lead, 'VALIDATED', 'Projet validé'); }}
                                                                className="text-[11px] bg-indigo-50 text-indigo-600 px-2 py-1 rounded-lg hover:bg-indigo-100 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <CheckCircle size={11} /> Valider
                                                            </button>
                                                        </>
                                                    )}

                                                    {/* MEETING → Validated */}
                                                    {lead.status === 'MEETING' && (
                                                        <button
                                                            onClick={(e) => { e.stopPropagation(); handleQuickTransition(lead, 'VALIDATED', 'Projet validé'); }}
                                                            className="text-[11px] bg-indigo-50 text-indigo-600 px-2 py-1 rounded-lg hover:bg-indigo-100 flex items-center gap-1 font-bold transition-colors"
                                                        >
                                                            <CheckCircle size={11} /> Valider le projet
                                                        </button>
                                                    )}

                                                    {/* VALIDATED → Send DIP */}
                                                    {lead.status === 'VALIDATED' && (
                                                        <button
                                                            onClick={(e) => { e.stopPropagation(); handleSendDIP(lead); }}
                                                            className="text-[11px] bg-cyan-50 text-cyan-700 px-2 py-1 rounded-lg hover:bg-cyan-100 flex items-center gap-1 font-bold transition-colors"
                                                        >
                                                            <ScrollText size={11} /> Envoyer DIP
                                                        </button>
                                                    )}

                                                    {/* DIP_SENT → View DIP + Generate Contract (if cooling ok) */}
                                                    {lead.status === 'DIP_SENT' && (
                                                        <>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); window.open(`http://localhost:4000/franchise-leads/${lead.id}/dip`, '_blank'); }}
                                                                className="text-[11px] bg-slate-100 text-slate-600 px-2 py-1 rounded-lg hover:bg-slate-200 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <Eye size={11} /> DIP
                                                            </button>
                                                            {coolingDays !== null && coolingDays <= 0 && (
                                                                <button
                                                                    onClick={(e) => { e.stopPropagation(); handleGenerateContract(lead); }}
                                                                    className="text-[11px] bg-orange-50 text-orange-600 px-2 py-1 rounded-lg hover:bg-orange-100 flex items-center gap-1 font-bold transition-colors"
                                                                >
                                                                    <FileSignature size={11} /> Contrat
                                                                </button>
                                                            )}
                                                        </>
                                                    )}

                                                    {/* CONTRACT_SENT → Sign + View docs */}
                                                    {lead.status === 'CONTRACT_SENT' && (
                                                        <>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); handleSignContract(lead); }}
                                                                className="text-[11px] bg-emerald-50 text-emerald-600 px-2 py-1 rounded-lg hover:bg-emerald-100 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <FileSignature size={11} /> Signer
                                                            </button>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); window.open(`http://localhost:4000/franchise-leads/${lead.id}/contract`, '_blank'); }}
                                                                className="text-[11px] bg-slate-100 text-slate-600 px-2 py-1 rounded-lg hover:bg-slate-200 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <Eye size={11} /> Contrat
                                                            </button>
                                                        </>
                                                    )}

                                                    {/* SIGNED → View docs + Kit */}
                                                    {lead.status === 'SIGNED' && (
                                                        <>
                                                            <span className="text-[11px] text-emerald-600 font-bold flex items-center gap-1">
                                                                <CheckCircle size={11} /> Agence créée
                                                            </span>
                                                            <button
                                                                onClick={(e) => { e.stopPropagation(); window.open(`http://localhost:4000/franchise-leads/${lead.id}/opening-kit`, '_blank'); }}
                                                                className="text-[11px] bg-slate-100 text-slate-600 px-2 py-1 rounded-lg hover:bg-slate-200 flex items-center gap-1 font-bold transition-colors"
                                                            >
                                                                <Download size={11} /> Kit
                                                            </button>
                                                        </>
                                                    )}

                                                    {/* REJECTED → Re-open */}
                                                    {lead.status === 'REJECTED' && (
                                                        <button
                                                            onClick={(e) => { e.stopPropagation(); handleQuickTransition(lead, 'NEW', 'Réouvert'); }}
                                                            className="text-[11px] bg-blue-50 text-blue-600 px-2 py-1 rounded-lg hover:bg-blue-100 flex items-center gap-1 font-bold transition-colors"
                                                        >
                                                            <Plus size={11} /> Réouvrir
                                                        </button>
                                                    )}

                                                    {/* Reject button (for non-terminal states) */}
                                                    {!['SIGNED', 'REJECTED'].includes(lead.status) && (
                                                        <button
                                                            onClick={(e) => { e.stopPropagation(); handleReject(lead); }}
                                                            className="text-[11px] text-slate-400 hover:text-rose-500 px-1.5 py-1 rounded-lg hover:bg-rose-50 transition-colors ml-auto"
                                                            title="Rejeter"
                                                        >
                                                            <XCircle size={13} />
                                                        </button>
                                                    )}
                                                </div>
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>
                        );
                    })}
                </div>
            )}

            {/* Analytics View */}
            {viewMode === 'ANALYTICS' && analytics && (
                <div className="grid grid-cols-4 gap-6">
                    {/* Stats Cards */}
                    <div className="bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 mb-2">
                            <div className="w-10 h-10 bg-indigo-100 rounded-xl flex items-center justify-center">
                                <Users size={20} className="text-indigo-600" />
                            </div>
                            <div>
                                <p className="text-xs text-slate-500 font-medium">Total Leads</p>
                                <p className="text-2xl font-black text-slate-900">{analytics.total}</p>
                            </div>
                        </div>
                    </div>

                    <div className="bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 mb-2">
                            <div className="w-10 h-10 bg-emerald-100 rounded-xl flex items-center justify-center">
                                <CheckCircle size={20} className="text-emerald-600" />
                            </div>
                            <div>
                                <p className="text-xs text-slate-500 font-medium">Signés</p>
                                <p className="text-2xl font-black text-slate-900">{analytics.statusCounts['SIGNED'] || 0}</p>
                            </div>
                        </div>
                    </div>

                    <div className="bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 mb-2">
                            <div className="w-10 h-10 bg-amber-100 rounded-xl flex items-center justify-center">
                                <Percent size={20} className="text-amber-600" />
                            </div>
                            <div>
                                <p className="text-xs text-slate-500 font-medium">Taux de conversion</p>
                                <p className="text-2xl font-black text-slate-900">{analytics.conversionRate}%</p>
                            </div>
                        </div>
                    </div>

                    <div className="bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <div className="flex items-center gap-3 mb-2">
                            <div className="w-10 h-10 bg-purple-100 rounded-xl flex items-center justify-center">
                                <TrendingUp size={20} className="text-purple-600" />
                            </div>
                            <div>
                                <p className="text-xs text-slate-500 font-medium">En cours</p>
                                <p className="text-2xl font-black text-slate-900">
                                    {(analytics.statusCounts['CONTACTED'] || 0) + (analytics.statusCounts['MEETING'] || 0) + (analytics.statusCounts['VALIDATED'] || 0)}
                                </p>
                            </div>
                        </div>
                    </div>

                    {/* Funnel */}
                    <div className="col-span-2 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">Funnel de conversion</h3>
                        <div className="space-y-3">
                            {STATUS_COLUMNS.filter(c => c.id !== 'REJECTED').map((col, i) => {
                                const count = analytics.statusCounts[col.id] || 0;
                                const maxCount = Math.max(...Object.values(analytics.statusCounts));
                                const width = maxCount > 0 ? (count / maxCount) * 100 : 0;
                                return (
                                    <div key={col.id} className="flex items-center gap-3">
                                        <span className="text-xs font-bold text-slate-500 w-28">{col.label}</span>
                                        <div className="flex-1 h-8 bg-slate-100 rounded-lg overflow-hidden">
                                            <div className={`h-full ${col.bgColor} transition-all`} style={{ width: `${width}%` }} />
                                        </div>
                                        <span className="text-sm font-black text-slate-700 w-8 text-right">{count}</span>
                                    </div>
                                );
                            })}
                        </div>
                    </div>

                    {/* By Region */}
                    <div className="col-span-2 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">Par région</h3>
                        <div className="grid grid-cols-4 gap-3">
                            {Object.entries(analytics.regionCounts).sort((a, b) => b[1] - a[1]).map(([region, count]) => (
                                <div key={region} className="bg-slate-50 rounded-xl p-3 text-center">
                                    <p className="text-lg font-black text-indigo-600">{count}</p>
                                    <p className="text-xs font-bold text-slate-500">{region}</p>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Trend */}
                    <div className="col-span-4 bg-white rounded-2xl p-6 border border-slate-200 shadow-sm">
                        <h3 className="font-black text-slate-900 mb-4">Tendance mensuelle</h3>
                        <div className="flex items-end justify-between h-40 gap-4">
                            {analytics.monthlyTrend.map(m => (
                                <div key={m.month} className="flex-1 flex flex-col items-center gap-2">
                                    <div className="w-full flex flex-col items-center gap-1" style={{ height: '120px' }}>
                                        <div className="w-full bg-indigo-500 rounded-t-lg transition-all" style={{ height: `${(m.count / Math.max(...analytics.monthlyTrend.map(x => x.count), 1)) * 100}%` }} />
                                    </div>
                                    <span className="text-xs font-bold text-slate-500">{m.month}</span>
                                    <span className="text-xs font-black text-slate-700">{m.count}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {/* Create Modal */}
            {isCreateModalOpen && (
                <div className="fixed inset-0 bg-slate-900/50 backdrop-blur-sm z-50 flex items-center justify-center p-4">
                    <div className="bg-white rounded-2xl shadow-xl w-full max-w-md overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex justify-between items-center">
                            <h2 className="text-xl font-black text-slate-900">Nouveau Candidat</h2>
                            <button onClick={() => setIsCreateModalOpen(false)} className="text-slate-400 hover:text-slate-600">
                                <X size={24} />
                            </button>
                        </div>
                        <div className="p-6 space-y-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Nom Complet</label>
                                <input
                                    type="text"
                                    className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                    placeholder="Jean Dupont"
                                    value={newLeadData.name}
                                    onChange={(e) => setNewLeadData({ ...newLeadData, name: e.target.value })}
                                />
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Email</label>
                                    <input
                                        type="email"
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                        placeholder="jean@exemple.com"
                                        value={newLeadData.email}
                                        onChange={(e) => setNewLeadData({ ...newLeadData, email: e.target.value })}
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Téléphone</label>
                                    <input
                                        type="tel"
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                        placeholder="0612345678"
                                        value={newLeadData.phone}
                                        onChange={(e) => setNewLeadData({ ...newLeadData, phone: e.target.value })}
                                    />
                                </div>
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Ville Cible</label>
                                    <input
                                        type="text"
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                        placeholder="Lyon"
                                        value={newLeadData.targetCity}
                                        onChange={(e) => setNewLeadData({ ...newLeadData, targetCity: e.target.value })}
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Région</label>
                                    <select
                                        className="w-full p-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 outline-none"
                                        value={newLeadData.region}
                                        onChange={(e) => setNewLeadData({ ...newLeadData, region: e.target.value })}
                                    >
                                        {REGIONS.map(r => <option key={r} value={r}>{r}</option>)}
                                    </select>
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase mb-1">Type</label>
                                <div className="flex gap-2">
                                    <button
                                        onClick={() => setNewLeadData({ ...newLeadData, type: 'FRANCHISE' })}
                                        className={`flex-1 py-3 text-sm font-bold rounded-xl border-2 transition-all ${newLeadData.type === 'FRANCHISE' ? 'bg-indigo-600 border-indigo-600 text-white' : 'bg-white border-slate-200 text-slate-600'}`}
                                    >
                                        Franchise
                                    </button>
                                    <button
                                        onClick={() => setNewLeadData({ ...newLeadData, type: 'CORNER' })}
                                        className={`flex-1 py-3 text-sm font-bold rounded-xl border-2 transition-all ${newLeadData.type === 'CORNER' ? 'bg-purple-600 border-purple-600 text-white' : 'bg-white border-slate-200 text-slate-600'}`}
                                    >
                                        Corner
                                    </button>
                                </div>
                            </div>
                        </div>
                        <div className="p-6 bg-slate-50 flex gap-3">
                            <button
                                onClick={() => setIsCreateModalOpen(false)}
                                className="flex-1 py-3 bg-white border border-slate-200 text-slate-600 font-bold rounded-xl hover:bg-slate-100 transition-colors"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleCreateLead}
                                className="flex-1 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 transition-colors shadow-lg"
                            >
                                Créer
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
