'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { SalesStore, Prospect, ProspectNote, ProspectStatus, AppointmentInfo } from '../../services/SalesStore';
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
    Download,
    MapPin,
    Clock,
    Microscope
} from 'lucide-react';


import { SalesAnalyticsDashboard } from './SalesAnalyticsDashboard';
import { WhatsAppWidget } from '../backoffice/WhatsAppWidget';
import BookAppointmentModal from './BookAppointmentModal';

const COLUMNS: { id: ProspectStatus; label: string; color: string; icon: string }[] = [
    { id: 'NEW', label: 'Nouveau', color: 'bg-amber-100 text-amber-800', icon: '🟡' },
    { id: 'CONTACTED', label: 'Contacté', color: 'bg-purple-100 text-purple-800', icon: '🟣' },
    { id: 'QUALIFIED', label: 'Qualifié', color: 'bg-blue-100 text-blue-800', icon: '🔵' },
    { id: 'MEETING_BOOKED', label: 'RDV Fixé', color: 'bg-indigo-100 text-indigo-800', icon: '�' },
    { id: 'SIGNED', label: 'Signé', color: 'bg-emerald-100 text-emerald-800', icon: '✅' },
    { id: 'NO_SHOW', label: 'Non Honoré', color: 'bg-red-100 text-red-800', icon: '🚫' },
    { id: 'LOST', label: 'Perdu', color: 'bg-slate-100 text-slate-800', icon: '⚫' }
];

export default function SalesDashboard() {
    const [prospects, setProspects] = useState<Prospect[]>([]);
    const [viewMode, setViewMode] = useState<'KANBAN' | 'ANALYTICS'>('KANBAN');
    const [isLoading, setIsLoading] = useState(true);
    const [selectedProspect, setSelectedProspect] = useState<Prospect | null>(null);
    const [showImportModal, setShowImportModal] = useState(false);
    const [showCallCockpit, setShowCallCockpit] = useState(false);
    const [showAddModal, setShowAddModal] = useState(false);
    const [showBookingModal, setShowBookingModal] = useState(false);
    const [callHistory, setCallHistory] = useState<any[]>([]);

    const [isEditingInfo, setIsEditingInfo] = useState(false);
    const [editForm, setEditForm] = useState({ firstName: '', lastName: '', phone: '', email: '', address: '', city: '', zipCode: '', country: 'France' });
    const [addForm, setAddForm] = useState({ firstName: '', lastName: '', phone: '', email: '', source: 'WEBSITE', address: '', city: '', zipCode: '', country: 'France' });

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
            setEditForm({
                firstName: selectedProspect.firstName || '',
                lastName: selectedProspect.lastName || '',
                phone: selectedProspect.phone || '',
                email: selectedProspect.email || '',
                address: selectedProspect.address || '',
                city: selectedProspect.city || '',
                zipCode: selectedProspect.zipCode || '',
                country: selectedProspect.country || 'France'
            });
            setIsEditingInfo(false);
        } else {
            setCallHistory([]);
            setIsEditingInfo(false);
        }
    }, [selectedProspect?.id]);

    const handleSaveInfo = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, editForm);
        setSelectedProspect(prev => prev ? { ...prev, ...editForm } : null);
        setProspects(prev => prev.map(p => p.id === selectedProspect.id ? { ...p, ...editForm } : p));
        setIsEditingInfo(false);
        setIsLoading(false);
    };
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

    // Fixer un RDV en agence
    const handleBookAppointment = async (prospect: Prospect, appointment: AppointmentInfo) => {
        const updated = await SalesStore.bookAppointment(prospect.id, appointment, true);
        if (updated) {
            // Optimistic update
            setProspects((prev: Prospect[]) => prev.map((p: Prospect) =>
                p.id === prospect.id ? { ...p, status: 'MEETING_BOOKED', appointment } : p
            ));
            if (selectedProspect?.id === prospect.id) {
                setSelectedProspect((prev: Prospect | null) => prev ? { ...prev, status: 'MEETING_BOOKED', appointment } : null);
            }
            setShowBookingModal(false);
            alert('📅 RDV fixé avec succès ! Confirmation envoyée.');
        }
    };

    // Marquer comme non honoré (no-show)
    const handleNoShow = async (prospect: Prospect) => {
        if (!confirm(`Confirmer que ${prospect.firstName} n'est pas venu au RDV ?`)) return;
        await handleStatusChange(prospect.id, 'NO_SHOW');
    };

    // Encaissement : crée une session Stripe pour le prospect
    // Le webhook Stripe déclenchera auto : SIGNED + Lead CRM
    const handleStartPayment = async (prospect: Prospect, installments: 1 | 3 = 1) => {
        const serviceId = prospect.eligibilityResult?.matchedProcedures?.[0] || prospect.interestServiceId || 'consultation_juridique';
        // Prix selon le service (à terme, vient de la config backend)
        const SERVICE_PRICES: Record<string, { price: number; label: string }> = {
            'naturalisation': { price: 1500, label: 'Naturalisation' },
            'regroupement_familial': { price: 1200, label: 'Regroupement familial' },
            'titre_sejour': { price: 800, label: 'Titre de séjour' },
            'changement_statut': { price: 900, label: 'Changement de statut' },
            'consultation_juridique': { price: 150, label: 'Consultation juridique' },
        };
        const service = SERVICE_PRICES[serviceId] || { price: 1000, label: serviceId };
        const amount = installments === 3 ? Math.ceil(service.price / 3) : service.price;

        try {
            const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000';
            const response = await fetch(`${API_URL}/payments/prospect-checkout`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({
                    prospectId: prospect.id,
                    amount,
                    serviceId,
                    serviceName: service.label,
                    installments,
                    successUrl: `${window.location.origin}/admin/sales?payment=success&prospect=${prospect.id}`,
                    cancelUrl: `${window.location.origin}/admin/sales?payment=cancelled&prospect=${prospect.id}`,
                }),
            });

            if (!response.ok) throw new Error('Checkout creation failed');
            const data = await response.json();

            if (data.url) {
                // Redirection vers Stripe Checkout
                window.location.href = data.url;
            } else {
                alert('❌ Impossible de créer la session de paiement. Vérifiez la configuration Stripe.');
            }
        } catch (error) {
            console.error('[Payment] Error:', error);
            alert('❌ Erreur de paiement. Vérifiez votre connexion ou la configuration Stripe.');
        }
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
                        onClick={() => setShowAddModal(true)}
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
                                                // SIGNED ne peut être atteint que par paiement
                                                if (column.id === 'SIGNED') {
                                                    alert('⚠️ Le statut "Signé" est automatique.\n\nIl se déclenche uniquement après encaissement (paiement Stripe validé).\n\n→ Ouvrez le prospect → Cliquez sur "Encaisser"');
                                                    return;
                                                }
                                                handleStatusChange(prospectId, column.id as ProspectStatus);
                                            }
                                        }}
                                    >
                                        <div className="space-y-3">
                                            {prospects
                                                .filter(p => p.status === column.id)
                                                .sort((a, b) => {
                                                    // Special sort for NEW: High score first
                                                    if (column.id === 'NEW') {
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
                                                                <button
                                                                    onClick={(e: React.MouseEvent) => {
                                                                        e.stopPropagation();
                                                                        router.push(`/admin/calendar?leadId=${prospect.id}&name=${encodeURIComponent(prospect.firstName + ' ' + prospect.lastName)}&email=${encodeURIComponent(prospect.email || '')}&service=${encodeURIComponent(prospect.interestServiceId || '')}`);
                                                                    }}
                                                                    className="hover:text-amber-600 p-1"
                                                                    title="Prendre RDV"
                                                                >
                                                                    <Calendar size={16} />
                                                                </button>
                                                                <button className="hover:text-indigo-600 p-1" onClick={(e) => { e.stopPropagation(); setSelectedProspect(prospect); setShowCallCockpit(true); }} title="Appeler">
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

                    {/* Drawer Detail — Fiche Prospect Enrichie */}
                    {selectedProspect && (
                        <div className="fixed inset-0 z-50 flex justify-end">
                            {/* Backdrop */}
                            <div
                                className="absolute inset-0 bg-black/30 backdrop-blur-sm"
                                onClick={() => setSelectedProspect(null)}
                            />

                            {/* Panel */}
                            <div className="relative w-[520px] h-full bg-white shadow-2xl flex flex-col animate-in slide-in-from-right duration-300">

                                {/* ─── HEADER ENRICHI ─── */}
                                <div className="relative bg-gradient-to-br from-slate-900 via-slate-800 to-indigo-900 p-6 pb-5">
                                    {/* Close */}
                                    <button
                                        onClick={() => setSelectedProspect(null)}
                                        className="absolute top-4 right-4 p-2 hover:bg-white/10 rounded-full transition-colors text-white/60 hover:text-white"
                                    >
                                        <XCircle size={22} />
                                    </button>

                                    <div className="flex items-start gap-4">
                                        {/* Avatar avec initiales */}
                                        <div className="w-16 h-16 rounded-2xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center text-white font-black text-xl shadow-lg shadow-indigo-500/30 flex-shrink-0">
                                            {selectedProspect.firstName[0]}{selectedProspect.lastName[0]}
                                        </div>

                                        <div className="flex-1 min-w-0">
                                            {isEditingInfo ? (
                                                <div className="space-y-2 mb-3">
                                                    <div className="flex gap-2">
                                                        <input type="text" value={editForm.firstName} onChange={e => setEditForm(prev => ({ ...prev, firstName: e.target.value }))} className="flex-1 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Prénom" autoFocus />
                                                        <input type="text" value={editForm.lastName} onChange={e => setEditForm(prev => ({ ...prev, lastName: e.target.value }))} className="flex-1 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Nom" />
                                                    </div>
                                                    <div className="flex gap-2">
                                                        <input type="text" value={editForm.phone} onChange={e => setEditForm(prev => ({ ...prev, phone: e.target.value }))} className="flex-1 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Téléphone" />
                                                        <input type="email" value={editForm.email} onChange={e => setEditForm(prev => ({ ...prev, email: e.target.value }))} className="flex-1 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Email" />
                                                    </div>
                                                    <input type="text" value={editForm.address} onChange={e => setEditForm(prev => ({ ...prev, address: e.target.value }))} className="w-full px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Adresse (rue)" />
                                                    <div className="flex gap-2">
                                                        <input type="text" value={editForm.zipCode} onChange={e => setEditForm(prev => ({ ...prev, zipCode: e.target.value }))} className="w-24 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="CP" />
                                                        <input type="text" value={editForm.city} onChange={e => setEditForm(prev => ({ ...prev, city: e.target.value }))} className="flex-1 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Ville" />
                                                        <input type="text" value={editForm.country} onChange={e => setEditForm(prev => ({ ...prev, country: e.target.value }))} className="w-28 px-3 py-1.5 text-sm rounded-lg bg-slate-800/80 text-white border border-slate-700 focus:border-indigo-500 focus:outline-none placeholder:text-slate-500" placeholder="Pays" />
                                                    </div>
                                                    <div className="flex gap-2 pt-1">
                                                        <button onClick={handleSaveInfo} className="px-4 py-1.5 bg-indigo-500 hover:bg-indigo-600 text-white text-xs font-bold rounded-lg transition-colors flex items-center gap-1 shadow-sm">
                                                            <CheckCircle size={14} /> Enregistrer
                                                        </button>
                                                        <button onClick={() => setIsEditingInfo(false)} className="px-4 py-1.5 bg-slate-700 hover:bg-slate-600 text-white text-xs font-bold rounded-lg transition-colors">Annuler</button>
                                                    </div>
                                                </div>
                                            ) : (
                                                <div className="group/edit relative">
                                                    <h2 className="text-xl font-bold text-white mb-1 truncate pr-8">
                                                        {selectedProspect.firstName} {selectedProspect.lastName}
                                                    </h2>
                                                    <button onClick={() => setIsEditingInfo(true)} className="absolute right-0 top-0 opacity-0 group-hover/edit:opacity-100 text-slate-400 hover:text-white p-1.5 rounded-lg hover:bg-white/10 transition-all cursor-pointer" title="Modifier le prospect">
                                                        <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"></path><path d="M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"></path></svg>
                                                    </button>
                                                    <div className="flex items-center gap-2 text-slate-300 text-sm mb-1">
                                                        <Phone size={13} />
                                                        <span>{selectedProspect.phone}</span>
                                                        {selectedProspect.email && (
                                                            <>
                                                                <span className="text-slate-500">•</span>
                                                                <Mail size={13} />
                                                                <span className="truncate">{selectedProspect.email}</span>
                                                            </>
                                                        )}
                                                    </div>
                                                    {(selectedProspect.city || selectedProspect.zipCode) && (
                                                        <div className="flex items-center gap-1.5 text-slate-400 text-xs mb-3">
                                                            <MapPin size={11} />
                                                            <span>{[selectedProspect.address, selectedProspect.zipCode, selectedProspect.city, selectedProspect.country].filter(Boolean).join(', ')}</span>
                                                        </div>
                                                    )}
                                                    {!selectedProspect.city && !selectedProspect.zipCode && (
                                                        <div className="flex items-center gap-1.5 text-amber-400/70 text-xs mb-3">
                                                            <MapPin size={11} />
                                                            <span className="italic">Adresse non renseignée — à qualifier</span>
                                                        </div>
                                                    )}
                                                </div>
                                            )}

                                            {/* Status Badge + Score */}
                                            <div className="flex items-center gap-3">
                                                <span className={`px-3 py-1 rounded-full text-xs font-bold ${COLUMNS.find(c => c.id === selectedProspect.status)?.color || 'bg-slate-100 text-slate-600'}`}>
                                                    {COLUMNS.find(c => c.id === selectedProspect.status)?.label || selectedProspect.status}
                                                </span>

                                                {/* Score Jauge visuelle */}
                                                <div className="flex items-center gap-2">
                                                    <div className="relative w-20 h-2 bg-white/10 rounded-full overflow-hidden">
                                                        <div
                                                            className={`absolute inset-y-0 left-0 rounded-full transition-all duration-500 ${selectedProspect.score >= 70 ? 'bg-emerald-400' : selectedProspect.score >= 40 ? 'bg-amber-400' : 'bg-red-400'}`}
                                                            style={{ width: `${selectedProspect.score}%` }}
                                                        />
                                                    </div>
                                                    <span className={`text-xs font-black ${selectedProspect.score >= 70 ? 'text-emerald-400' : selectedProspect.score >= 40 ? 'text-amber-400' : 'text-red-400'}`}>
                                                        {selectedProspect.score}
                                                    </span>
                                                </div>

                                                {selectedProspect.score >= 50 && (
                                                    <Flame size={16} className="text-orange-400 animate-pulse" fill="currentColor" />
                                                )}
                                            </div>
                                        </div>
                                    </div>
                                </div>

                                {/* ─── STICKY ACTION BAR (Dynamique selon statut) ─── */}
                                <div className="px-5 py-3 bg-white border-b border-slate-100 flex items-center gap-2 shadow-sm">
                                    {/* NEW / CONTACTED : Appeler est l'action principale */}
                                    {(selectedProspect.status === 'NEW' || selectedProspect.status === 'CONTACTED') && (
                                        <>
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Appeler
                                            </button>
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-slate-200 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                            >
                                                <Calendar size={15} />
                                                Fixer RDV
                                            </button>
                                        </>
                                    )}

                                    {/* QUALIFIED : Fixer RDV est l'action principale */}
                                    {selectedProspect.status === 'QUALIFIED' && (
                                        <>
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Calendar size={15} />
                                                Fixer RDV
                                            </button>
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-slate-200 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Rappeler
                                            </button>
                                        </>
                                    )}

                                    {/* MEETING_BOOKED : Simulateur → Encaisser → Auto SIGNED */}
                                    {selectedProspect.status === 'MEETING_BOOKED' && (
                                        <div className="flex flex-col gap-2 w-full">
                                            {/* Ligne 1 : Simulateur + Non honoré */}
                                            <div className="flex gap-2">
                                                <button
                                                    onClick={() => router.push(`/simulateur?prospectId=${selectedProspect.id}`)}
                                                    className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                                >
                                                    <Microscope size={15} />
                                                    Simulateur
                                                </button>
                                                <button
                                                    onClick={() => handleNoShow(selectedProspect)}
                                                    className="flex items-center justify-center gap-2 py-2.5 px-4 bg-white border border-red-200 text-red-600 rounded-xl text-sm font-bold hover:bg-red-50 transition-all active:scale-[0.97]"
                                                >
                                                    🚫 Non honoré
                                                </button>
                                            </div>
                                            {/* Ligne 2 : Encaissement (déclenche auto-SIGNED via Stripe) */}
                                            <div className="flex gap-2">
                                                <button
                                                    onClick={() => handleStartPayment(selectedProspect, 1)}
                                                    className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-emerald-600 text-white rounded-xl text-sm font-bold hover:bg-emerald-700 transition-all shadow-sm shadow-emerald-200 active:scale-[0.97]"
                                                >
                                                    💳 Encaisser (1x)
                                                </button>
                                                <button
                                                    onClick={() => handleStartPayment(selectedProspect, 3)}
                                                    className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-emerald-300 text-emerald-700 rounded-xl text-sm font-bold hover:bg-emerald-50 transition-all active:scale-[0.97]"
                                                >
                                                    💳 Encaisser (3x)
                                                </button>
                                            </div>
                                        </div>
                                    )}

                                    {/* SIGNED : Voir le dossier */}
                                    {selectedProspect.status === 'SIGNED' && (
                                        <button
                                            onClick={() => router.push(selectedProspect.convertedLeadId ? `/admin/leads?id=${selectedProspect.convertedLeadId}` : '/admin/leads')}
                                            className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-emerald-50 border border-emerald-200 text-emerald-700 rounded-xl text-sm font-bold hover:bg-emerald-100 transition-all active:scale-[0.97]"
                                        >
                                            <CheckCircle size={15} />
                                            Voir dossier CRM
                                        </button>
                                    )}

                                    {/* NO_SHOW : Reprogrammer ou Abandonner */}
                                    {selectedProspect.status === 'NO_SHOW' && (
                                        <>
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Calendar size={15} />
                                                Reprogrammer
                                            </button>
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-slate-200 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Rappeler
                                            </button>
                                        </>
                                    )}

                                    {/* LOST : Réactiver */}
                                    {selectedProspect.status === 'LOST' && (
                                        <button
                                            onClick={() => handleStatusChange(selectedProspect.id, 'NEW')}
                                            className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-amber-50 border border-amber-200 text-amber-700 rounded-xl text-sm font-bold hover:bg-amber-100 transition-all active:scale-[0.97]"
                                        >
                                            <ArrowRight size={15} />
                                            Réactiver
                                        </button>
                                    )}
                                </div>

                                {/* ─── SCROLLABLE CONTENT ─── */}
                                <div className="flex-1 overflow-y-auto">

                                    {/* Pipeline Étapes */}
                                    <div className="px-5 py-4 border-b border-slate-100">
                                        <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-2.5">Pipeline</h3>
                                        <div className="flex flex-wrap gap-1.5">
                                            {COLUMNS.map((status) => (
                                                <button
                                                    key={status.id}
                                                    onClick={() => handleStatusChange(selectedProspect.id, status.id)}
                                                    className={`px-3 py-1.5 rounded-lg text-xs font-bold border transition-all active:scale-95 ${selectedProspect.status === status.id
                                                        ? `${status.color} border-transparent shadow-sm`
                                                        : 'bg-white border-slate-200 text-slate-400 hover:border-slate-300 hover:text-slate-600'
                                                        }`}
                                                >
                                                    {status.icon} {status.label}
                                                </button>
                                            ))}
                                        </div>
                                    </div>

                                    {/* RDV Info (si RDV fixé) */}
                                    {selectedProspect.appointment && ['MEETING_BOOKED', 'NO_SHOW', 'SIGNED'].includes(selectedProspect.status) && (
                                        <div className="px-5 py-4 border-b border-slate-100">
                                            <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-3">Rendez-vous</h3>
                                            <div className={`p-4 rounded-2xl border ${selectedProspect.status === 'MEETING_BOOKED' ? 'bg-indigo-50 border-indigo-100' : 'bg-emerald-50 border-emerald-100'}`}>
                                                <div className="flex items-start gap-3">
                                                    <div className={`w-10 h-10 rounded-xl flex items-center justify-center ${selectedProspect.status === 'MEETING_BOOKED' ? 'bg-indigo-100' : 'bg-emerald-100'}`}>
                                                        <Calendar size={18} className={selectedProspect.status === 'MEETING_BOOKED' ? 'text-indigo-600' : 'text-emerald-600'} />
                                                    </div>
                                                    <div className="flex-1">
                                                        <div className="flex items-center gap-2 mb-1">
                                                            <MapPin size={13} className="text-slate-400" />
                                                            <span className="text-sm font-bold text-slate-900">{selectedProspect.appointment.agencyName}</span>
                                                        </div>
                                                        <div className="flex items-center gap-2 mb-1">
                                                            <Clock size={13} className="text-slate-400" />
                                                            <span className="text-sm text-slate-700">
                                                                {new Date(selectedProspect.appointment.date).toLocaleDateString('fr-FR', { weekday: 'long', day: 'numeric', month: 'long' })}
                                                                {' à '}
                                                                {new Date(selectedProspect.appointment.date).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                            </span>
                                                        </div>
                                                        {selectedProspect.appointment.serviceId && (
                                                            <p className="text-xs text-slate-500 mt-1">🎯 Service : {selectedProspect.appointment.serviceId}</p>
                                                        )}
                                                        {selectedProspect.appointment.confirmed && (
                                                            <p className="text-xs text-emerald-600 font-bold mt-1">✅ Confirmé par {selectedProspect.appointment.confirmationSentVia || 'SMS'}</p>
                                                        )}
                                                    </div>
                                                </div>
                                                {selectedProspect.status === 'MEETING_BOOKED' && (
                                                    <div className="flex gap-2 mt-3 pt-3 border-t border-indigo-100">
                                                        <button
                                                            onClick={() => setShowBookingModal(true)}
                                                            className="text-xs font-bold text-indigo-600 hover:text-indigo-800 transition-colors"
                                                        >
                                                            Modifier
                                                        </button>
                                                        <span className="text-slate-300">|</span>
                                                        <button
                                                            onClick={() => handleStatusChange(selectedProspect.id, 'LOST')}
                                                            className="text-xs font-bold text-red-500 hover:text-red-700 transition-colors"
                                                        >
                                                            Annuler RDV
                                                        </button>
                                                    </div>
                                                )}
                                            </div>
                                        </div>
                                    )}

                                    {/* Action Zone contextuelle */}
                                    <div className="px-5 py-4 border-b border-slate-100">
                                        {selectedProspect.status === 'NEW' && (
                                            <button
                                                onClick={() => { setShowCallCockpit(true); }}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-amber-50 via-orange-50 to-amber-50 border border-amber-100 hover:border-amber-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <Phone className="text-amber-600" size={20} />
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-slate-900 text-sm">Appeler pour qualifier</p>
                                                    <p className="text-xs text-slate-500">Confirmer le besoin et proposer un RDV en agence</p>
                                                </div>
                                                <ArrowRight size={18} className="text-amber-400 group-hover:translate-x-1 transition-transform" />
                                            </button>
                                        )}
                                        {selectedProspect.status === 'CONTACTED' && (
                                            <button
                                                onClick={() => { setShowCallCockpit(true); }}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-purple-50 via-violet-50 to-purple-50 border border-purple-100 hover:border-purple-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <Phone className="text-purple-600" size={20} />
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-slate-900 text-sm">Rappeler pour compléter la qualification</p>
                                                    <p className="text-xs text-slate-500">Collecter les infos manquantes et fixer un RDV</p>
                                                </div>
                                                <ArrowRight size={18} className="text-purple-400 group-hover:translate-x-1 transition-transform" />
                                            </button>
                                        )}
                                        {selectedProspect.status === 'QUALIFIED' && (
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-indigo-50 via-blue-50 to-indigo-50 border border-indigo-100 hover:border-indigo-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <Calendar className="text-indigo-600" size={20} />
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-slate-900 text-sm">Fixer un rendez-vous en agence</p>
                                                    <p className="text-xs text-slate-500">Lead qualifié — proposer un créneau pour signer</p>
                                                </div>
                                                <ArrowRight size={18} className="text-indigo-400 group-hover:translate-x-1 transition-transform" />
                                            </button>
                                        )}
                                        {selectedProspect.status === 'MEETING_BOOKED' && (
                                            <button
                                                onClick={() => router.push(`/?prospectId=${selectedProspect.id}&serviceId=${encodeURIComponent(selectedProspect.interestServiceId || '')}`)}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-indigo-50 via-purple-50 to-indigo-50 border border-indigo-100 hover:border-indigo-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <Microscope className="text-indigo-600" size={20} />
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-slate-900 text-sm">Dérouler le simulateur d'éligibilité</p>
                                                    <p className="text-xs text-slate-500">Vérifier l'éligibilité du lead lors du RDV en agence</p>
                                                </div>
                                                <ArrowRight size={18} className="text-indigo-400 group-hover:translate-x-1 transition-transform" />
                                            </button>
                                        )}
                                        {selectedProspect.status === 'NO_SHOW' && (
                                            <div className="flex items-center gap-3 p-4 bg-red-50 rounded-2xl border border-red-100 text-red-700">
                                                🚫
                                                <div>
                                                    <p className="font-bold text-sm">RDV non honoré</p>
                                                    <p className="text-xs text-red-500">Relancer le prospect ou reprogrammer un nouveau RDV</p>
                                                </div>
                                            </div>
                                        )}
                                        {selectedProspect.status === 'SIGNED' && (
                                            <div className="flex items-center gap-3 p-4 bg-emerald-50 rounded-2xl border border-emerald-100 text-emerald-700">
                                                <CheckCircle size={20} />
                                                <span className="font-bold text-sm">✅ Contrat signé — Dossier ouvert dans le CRM</span>
                                            </div>
                                        )}
                                    </div>

                                    {/* Adresse & Localisation */}
                                    <div className="px-5 py-4 border-b border-slate-100">
                                        <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-3">📍 Adresse & Localisation</h3>
                                        {(selectedProspect.address || selectedProspect.city || selectedProspect.zipCode) ? (
                                            <div className="bg-slate-50 rounded-xl p-4 space-y-2">
                                                {selectedProspect.address && (
                                                    <p className="text-sm text-slate-700 font-medium">{selectedProspect.address}</p>
                                                )}
                                                <p className="text-sm text-slate-900 font-bold">
                                                    {[selectedProspect.zipCode, selectedProspect.city].filter(Boolean).join(' ')}
                                                </p>
                                                <p className="text-xs text-slate-500">{selectedProspect.country || 'France'}</p>
                                            </div>
                                        ) : (
                                            <div className="bg-amber-50 border border-amber-200 rounded-xl p-4 flex items-center gap-3">
                                                <MapPin size={18} className="text-amber-500 flex-shrink-0" />
                                                <div>
                                                    <p className="text-sm font-bold text-amber-700">Adresse non renseignée</p>
                                                    <p className="text-xs text-amber-600">À compléter lors de la qualification téléphonique pour le routage agence</p>
                                                </div>
                                                <button
                                                    onClick={() => setIsEditingInfo(true)}
                                                    className="ml-auto px-3 py-1.5 bg-amber-500 text-white text-xs font-bold rounded-lg hover:bg-amber-600 transition-colors flex-shrink-0"
                                                >
                                                    Compléter
                                                </button>
                                            </div>
                                        )}
                                    </div>

                                    {/* Contexte Marketing */}
                                    <div className="px-5 py-4 border-b border-slate-100">
                                        <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-3">Contexte Marketing</h3>
                                        <div className="grid grid-cols-2 gap-3">
                                            <div className="bg-slate-50 rounded-xl p-3">
                                                <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Source</p>
                                                <p className="text-sm font-bold text-slate-900">{selectedProspect.source.replace(/_/g, ' ')}</p>
                                            </div>
                                            <div className="bg-slate-50 rounded-xl p-3">
                                                <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Campagne</p>
                                                <p className="text-sm font-bold text-slate-900">{selectedProspect.campaignName || '—'}</p>
                                            </div>
                                            <div className="bg-slate-50 rounded-xl p-3">
                                                <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Service d'intérêt</p>
                                                <p className="text-sm font-bold text-slate-900">{selectedProspect.interestServiceId || 'Non spécifié'}</p>
                                            </div>
                                            <div className="bg-slate-50 rounded-xl p-3">
                                                <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Créé le</p>
                                                <p className="text-sm font-bold text-slate-900">{new Date(selectedProspect.createdAt).toLocaleDateString('fr-FR')}</p>
                                            </div>
                                        </div>
                                    </div>

                                    {/* ─── SECTIONS ACCORDÉON ─── */}

                                    {/* Historique Appels — Accordéon */}
                                    <details className="border-b border-slate-100 group" open>
                                        <summary className="px-5 py-3.5 flex items-center gap-3 cursor-pointer hover:bg-slate-50 transition-colors select-none">
                                            <div className="w-8 h-8 rounded-lg bg-indigo-100 flex items-center justify-center">
                                                <Phone size={15} className="text-indigo-600" />
                                            </div>
                                            <span className="flex-1 text-sm font-bold text-slate-900">Appels</span>
                                            <span className="text-xs font-bold text-slate-400 bg-slate-100 px-2 py-0.5 rounded-full">{callHistory.length}</span>
                                            <ArrowRight size={14} className="text-slate-400 group-open:rotate-90 transition-transform" />
                                        </summary>
                                        <div className="px-5 pb-4">
                                            {callHistory.length === 0 ? (
                                                <p className="text-sm text-slate-400 italic py-2">Aucun appel enregistré.</p>
                                            ) : (
                                                <div className="space-y-2 max-h-48 overflow-y-auto pr-1">
                                                    {callHistory.map((call: any) => (
                                                        <div key={call.id} className="p-3 bg-slate-50 rounded-xl border border-slate-100 text-sm">
                                                            <div className="flex justify-between items-center mb-1">
                                                                <span className={`font-bold ${call.status === 'COMPLETED' ? 'text-emerald-600' : call.status === 'FAILED' ? 'text-red-500' : 'text-slate-600'}`}>
                                                                    {call.status === 'COMPLETED' ? '✅ Terminé' : call.status === 'FAILED' ? '❌ Échoué' : call.status === 'NO_ANSWER' ? '📵 Sans rép.' : '📞 ' + call.status}
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
                                    </details>

                                    {/* Notes — Accordéon */}
                                    <details className="border-b border-slate-100 group" open>
                                        <summary className="px-5 py-3.5 flex items-center gap-3 cursor-pointer hover:bg-slate-50 transition-colors select-none">
                                            <div className="w-8 h-8 rounded-lg bg-amber-100 flex items-center justify-center">
                                                <span className="text-amber-600 text-sm">📝</span>
                                            </div>
                                            <span className="flex-1 text-sm font-bold text-slate-900">Notes</span>
                                            <span className="text-xs font-bold text-slate-400 bg-slate-100 px-2 py-0.5 rounded-full">{selectedProspect.notes?.length || 0}</span>
                                            <ArrowRight size={14} className="text-slate-400 group-open:rotate-90 transition-transform" />
                                        </summary>
                                        <div className="px-5 pb-4">
                                            {(!selectedProspect.notes || selectedProspect.notes.length === 0) ? (
                                                <p className="text-sm text-slate-400 italic py-2">Aucune note enregistrée.</p>
                                            ) : (
                                                <div className="space-y-2 max-h-48 overflow-y-auto pr-1">
                                                    {selectedProspect.notes.map((note: ProspectNote) => (
                                                        <div key={note.id} className="p-3 bg-amber-50 rounded-xl border border-amber-100 text-sm">
                                                            <p className="text-slate-700 whitespace-pre-wrap">{note.text}</p>
                                                            <div className="flex justify-between items-center mt-2 text-xs text-slate-500">
                                                                <span>👤 {note.authorId}</span>
                                                                <span>{new Date(note.createdAt || '').toLocaleDateString('fr-FR')} à {new Date(note.createdAt || '').toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}</span>
                                                            </div>
                                                        </div>
                                                    ))}
                                                </div>
                                            )}
                                        </div>
                                    </details>

                                    {/* WhatsApp — Accordéon Collapsible */}
                                    <details className="border-b border-slate-100 group">
                                        <summary className="px-5 py-3.5 flex items-center gap-3 cursor-pointer hover:bg-slate-50 transition-colors select-none">
                                            <div className="w-8 h-8 rounded-lg bg-emerald-100 flex items-center justify-center">
                                                <span className="text-emerald-600 text-sm">💬</span>
                                            </div>
                                            <span className="flex-1 text-sm font-bold text-slate-900">WhatsApp</span>
                                            <span className="text-[10px] font-bold text-emerald-600 bg-emerald-50 px-2 py-0.5 rounded-full">Cliquer pour ouvrir</span>
                                            <ArrowRight size={14} className="text-slate-400 group-open:rotate-90 transition-transform" />
                                        </summary>
                                        <div className="px-5 pb-4">
                                            <WhatsAppWidget
                                                contactId={selectedProspect.id}
                                                contactType="PROSPECT"
                                                contactName={`${selectedProspect.firstName} ${selectedProspect.lastName}`}
                                                contactPhone={selectedProspect.phone || ''}
                                            />
                                        </div>
                                    </details>

                                    {/* Timeline d'activité */}
                                    <div className="px-5 py-4">
                                        <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-3">Activité récente</h3>
                                        <div className="relative pl-6 space-y-4 before:absolute before:left-[9px] before:top-1 before:bottom-1 before:w-px before:bg-slate-200">
                                            {/* Création */}
                                            <div className="relative">
                                                <div className="absolute -left-6 top-0.5 w-[18px] h-[18px] rounded-full bg-indigo-100 border-2 border-indigo-400 flex items-center justify-center">
                                                    <div className="w-1.5 h-1.5 rounded-full bg-indigo-500" />
                                                </div>
                                                <div>
                                                    <p className="text-sm font-medium text-slate-700">Prospect créé</p>
                                                    <p className="text-xs text-slate-400">
                                                        {new Date(selectedProspect.createdAt).toLocaleDateString('fr-FR')} à {new Date(selectedProspect.createdAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                        {' • '}Source: {selectedProspect.source.replace(/_/g, ' ')}
                                                    </p>
                                                </div>
                                            </div>

                                            {/* Dernier contact */}
                                            {selectedProspect.lastContactAt && (
                                                <div className="relative">
                                                    <div className="absolute -left-6 top-0.5 w-[18px] h-[18px] rounded-full bg-emerald-100 border-2 border-emerald-400 flex items-center justify-center">
                                                        <div className="w-1.5 h-1.5 rounded-full bg-emerald-500" />
                                                    </div>
                                                    <div>
                                                        <p className="text-sm font-medium text-slate-700">Dernier contact</p>
                                                        <p className="text-xs text-slate-400">
                                                            {new Date(selectedProspect.lastContactAt).toLocaleDateString('fr-FR')} à {new Date(selectedProspect.lastContactAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                        </p>
                                                    </div>
                                                </div>
                                            )}

                                            {/* Appels dans la timeline */}
                                            {callHistory.slice(0, 3).map((call: any) => (
                                                <div key={call.id} className="relative">
                                                    <div className={`absolute -left-6 top-0.5 w-[18px] h-[18px] rounded-full border-2 flex items-center justify-center ${call.status === 'COMPLETED' ? 'bg-emerald-100 border-emerald-400' : call.status === 'FAILED' ? 'bg-red-100 border-red-400' : 'bg-slate-100 border-slate-400'}`}>
                                                        <div className={`w-1.5 h-1.5 rounded-full ${call.status === 'COMPLETED' ? 'bg-emerald-500' : call.status === 'FAILED' ? 'bg-red-500' : 'bg-slate-500'}`} />
                                                    </div>
                                                    <div>
                                                        <p className="text-sm font-medium text-slate-700">
                                                            Appel {call.direction === 'OUTBOUND' ? 'sortant' : 'entrant'} — {call.status === 'COMPLETED' ? 'Terminé' : call.status === 'FAILED' ? 'Échoué' : call.status}
                                                        </p>
                                                        <p className="text-xs text-slate-400">
                                                            {new Date(call.startedAt).toLocaleDateString('fr-FR')} à {new Date(call.startedAt).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                            {call.duration > 0 && ` • ${Math.floor(call.duration / 60)}:${(call.duration % 60).toString().padStart(2, '0')}`}
                                                        </p>
                                                    </div>
                                                </div>
                                            ))}

                                            {/* Notes dans la timeline */}
                                            {(selectedProspect.notes || []).slice(0, 3).map((note: ProspectNote) => (
                                                <div key={note.id} className="relative">
                                                    <div className="absolute -left-6 top-0.5 w-[18px] h-[18px] rounded-full bg-amber-100 border-2 border-amber-400 flex items-center justify-center">
                                                        <div className="w-1.5 h-1.5 rounded-full bg-amber-500" />
                                                    </div>
                                                    <div>
                                                        <p className="text-sm font-medium text-slate-700">Note ajoutée</p>
                                                        <p className="text-xs text-slate-500 line-clamp-2">{note.text}</p>
                                                        <p className="text-xs text-slate-400 mt-0.5">
                                                            {new Date(note.createdAt || '').toLocaleDateString('fr-FR')} à {new Date(note.createdAt || '').toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                        </p>
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
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

                    {/* Modale d'ajout manuel de prospect */}
                    {showAddModal && (
                        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 backdrop-blur-sm">
                            <div className="bg-white p-6 rounded-2xl shadow-xl max-w-md w-full animate-in slide-in-from-bottom-4 duration-200">
                                <div className="flex justify-between items-center mb-6">
                                    <h2 className="text-xl font-bold flex items-center gap-2">
                                        <Plus className="text-indigo-600" /> Ajouter un prospect
                                    </h2>
                                    <button onClick={() => setShowAddModal(false)} className="text-slate-400 hover:text-slate-600"><XCircle size={20} /></button>
                                </div>
                                <div className="space-y-4">
                                    <div className="grid grid-cols-2 gap-4">
                                        <div>
                                            <label className="block text-xs font-bold text-slate-500 mb-1">Prénom</label>
                                            <input type="text" value={addForm.firstName} onChange={e => setAddForm({ ...addForm, firstName: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="Prénom" autoFocus />
                                        </div>
                                        <div>
                                            <label className="block text-xs font-bold text-slate-500 mb-1">Nom</label>
                                            <input type="text" value={addForm.lastName} onChange={e => setAddForm({ ...addForm, lastName: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="Nom" />
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Téléphone</label>
                                        <input type="tel" value={addForm.phone} onChange={e => setAddForm({ ...addForm, phone: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="06 XX XX XX XX" />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Email (Optionnel)</label>
                                        <input type="email" value={addForm.email} onChange={e => setAddForm({ ...addForm, email: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="contact@email.com" />
                                    </div>

                                    {/* Section Adresse (optionnelle à la création) */}
                                    <div className="pt-2 border-t border-slate-100">
                                        <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-2 flex items-center gap-1"><MapPin size={10} /> Adresse (optionnel — pour le routage agence)</p>
                                        <div className="space-y-3">
                                            <input type="text" value={addForm.address} onChange={e => setAddForm({ ...addForm, address: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="Adresse (rue)" />
                                            <div className="grid grid-cols-3 gap-3">
                                                <input type="text" value={addForm.zipCode} onChange={e => setAddForm({ ...addForm, zipCode: e.target.value })} className="px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="Code postal" />
                                                <input type="text" value={addForm.city} onChange={e => setAddForm({ ...addForm, city: e.target.value })} className="col-span-2 px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500" placeholder="Ville" />
                                            </div>
                                        </div>
                                    </div>

                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Source</label>
                                        <select value={addForm.source} onChange={e => setAddForm({ ...addForm, source: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500">
                                            <option value="WEBSITE">Site Web</option>
                                            <option value="PHONE">Appel Entrant</option>
                                            <option value="REFERRAL">Parrainage</option>
                                            <option value="NETWORKING">Réseau / Événement</option>
                                        </select>
                                    </div>
                                </div>
                                <div className="mt-8 flex justify-end gap-3">
                                    <button onClick={() => setShowAddModal(false)} className="px-4 py-2 text-slate-600 hover:bg-slate-100 rounded-lg font-medium transition-colors">Annuler</button>
                                    <button
                                        onClick={() => {
                                            SalesStore.addProspect({
                                                firstName: addForm.firstName || 'Nouveau',
                                                lastName: addForm.lastName || 'Prospect',
                                                phone: addForm.phone || '0600000000',
                                                email: addForm.email || undefined,
                                                address: addForm.address || undefined,
                                                city: addForm.city || undefined,
                                                zipCode: addForm.zipCode || undefined,
                                                country: addForm.country || 'France',
                                                source: addForm.source as any,
                                                agencyId: 'HQ-001',
                                                score: 0
                                            }).then((newProspect) => {
                                                loadProspects();
                                                setShowAddModal(false);
                                                setAddForm({ firstName: '', lastName: '', phone: '', email: '', source: 'WEBSITE', address: '', city: '', zipCode: '', country: 'France' });
                                                // Optionnel : ouvrir directement le prospect créé
                                                if (newProspect) setSelectedProspect(newProspect);
                                            });
                                        }}
                                        disabled={!addForm.firstName && !addForm.lastName && !addForm.phone}
                                        className="px-6 py-2 bg-indigo-600 text-white font-bold rounded-lg hover:bg-indigo-700 transition-colors shadow-sm disabled:opacity-50 disabled:cursor-not-allowed"
                                    >
                                        Créer le prospect
                                    </button>
                                </div>
                            </div>
                        </div>
                    )}
                    {/* Modal RDV */}
                    {showBookingModal && selectedProspect && (
                        <BookAppointmentModal
                            prospectName={`${selectedProspect.firstName} ${selectedProspect.lastName}`}
                            prospectPhone={selectedProspect.phone}
                            defaultServiceId={selectedProspect.interestServiceId}
                            existingAppointment={selectedProspect.appointment}
                            onBook={(appointment) => handleBookAppointment(selectedProspect, appointment)}
                            onClose={() => setShowBookingModal(false)}
                        />
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
