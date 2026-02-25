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
    Microscope,
    AlertTriangle,
    Info,
    X
} from 'lucide-react';


import { SalesAnalyticsDashboard } from './SalesAnalyticsDashboard';
import { WhatsAppWidget } from '../backoffice/WhatsAppWidget';
import BookAppointmentModal from './BookAppointmentModal';
import { SERVICES_CATALOG } from '../../data/services';
import { AuthStore } from '../../services/authStore';
import { AgencyStore, AgencyExt } from '../../services/AgencyStore';
import { UserStore, StaffUser } from '../../services/UserStore';
import SimulatorWrapper from '../SimulatorWrapper';

const COLUMNS: { id: ProspectStatus; label: string; color: string; icon: string }[] = [
    { id: 'NEW', label: 'Nouveau', color: 'bg-amber-100 text-amber-800', icon: '🟡' },
    { id: 'CONTACTED', label: 'Contacté', color: 'bg-purple-100 text-purple-800', icon: '🟣' },
    { id: 'QUALIFIED', label: 'Qualifié', color: 'bg-cyan-100 text-cyan-800', icon: '🔵' },
    { id: 'MEETING_BOOKED', label: 'RDV Fixé', color: 'bg-indigo-100 text-indigo-800', icon: '📅' },
    { id: 'SIGNED', label: 'Signé', color: 'bg-emerald-100 text-emerald-800', icon: '✅' },
    { id: 'NO_SHOW', label: 'Non Honoré', color: 'bg-red-100 text-red-800', icon: '🚫' },
    { id: 'LOST', label: 'Perdu', color: 'bg-slate-100 text-slate-800', icon: '⚫' }
];

// ─── Boutons d'encaissement avec prix backend ───
function PaymentButtons({ prospect, onPay, fetchPrice }: {
    prospect: Prospect;
    onPay: (prospect: Prospect, installments: 1 | 3) => void;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; pricePer3: number; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        const serviceId = prospect.eligibilityResult?.matchedProcedures?.[0] || prospect.interestServiceId || '';
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [prospect.id, prospect.interestServiceId]);

    if (!pricing) {
        return (
            <div className="flex gap-2">
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
            </div>
        );
    }

    return (
        <div className="flex gap-2">
            <button
                onClick={() => onPay(prospect, 1)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-emerald-600 text-white rounded-xl text-sm font-bold hover:bg-emerald-700 transition-all shadow-sm shadow-emerald-200 active:scale-[0.97]"
            >
                💳 Encaisser {pricing.priceEuros}€
                {pricing.promoActive && <span className="ml-1 px-1 py-0.5 bg-red-500 text-[9px] rounded-md uppercase">Promo</span>}
            </button>
            <button
                onClick={() => onPay(prospect, 3)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-emerald-300 text-emerald-700 rounded-xl text-sm font-bold hover:bg-emerald-50 transition-all active:scale-[0.97]"
            >
                💳 3x {pricing.pricePer3}€
            </button>
        </div>
    );
}

// ─── Affichage informatif du prix backend dans le drawer ───
function ServicePriceDisplay({ serviceId, fetchPrice }: {
    serviceId: string;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; serviceName: string; source: string; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [serviceId]);

    if (!serviceId || !pricing) return null;

    const sourceLabel = pricing.source === 'PROMO' ? '🏷️ Promo' :
        pricing.source === 'ADMIN_OVERRIDE' ? '⚙️ Admin' :
            pricing.source === 'CATALOG_DEFAULT' || pricing.source === 'FRONTEND_MAP' ? '📋 Défaut' : '';

    const sourceColor = pricing.source === 'PROMO' ? 'text-red-500' :
        pricing.source === 'ADMIN_OVERRIDE' ? 'text-indigo-500' : 'text-slate-400';

    return (
        <div className="flex items-center gap-2 mt-1">
            <span className="text-sm font-black text-emerald-600">{pricing.priceEuros}€</span>
            {sourceLabel && <span className={`text-[9px] font-bold ${sourceColor}`}>{sourceLabel}</span>}
            {pricing.promoActive && <span className="px-1.5 py-0.5 bg-red-100 text-red-600 text-[9px] font-bold rounded-md animate-pulse">PROMO ACTIVE</span>}
        </div>
    );
}

export default function SalesDashboard() {
    const [prospects, setProspects] = useState<Prospect[]>([]);
    const [viewMode, setViewMode] = useState<'KANBAN' | 'ANALYTICS'>('KANBAN');
    const [isLoading, setIsLoading] = useState(true);
    const [selectedProspect, setSelectedProspect] = useState<Prospect | null>(null);
    const [showImportModal, setShowImportModal] = useState(false);
    const [showCallCockpit, setShowCallCockpit] = useState(false);
    const [showAddModal, setShowAddModal] = useState(false);
    const [showBookingModal, setShowBookingModal] = useState(false);
    const [showSimulatorModal, setShowSimulatorModal] = useState(false);
    const [callHistory, setCallHistory] = useState<any[]>([]);

    // ─── Toast & Confirmation system ───
    const [toast, setToast] = useState<{ message: string; type: 'success' | 'error' | 'warning' | 'info'; icon?: string } | null>(null);
    const [confirmDialog, setConfirmDialog] = useState<{
        title: string;
        message: string;
        confirmLabel: string;
        cancelLabel?: string;
        type: 'danger' | 'warning' | 'info';
        onConfirm: () => void;
    } | null>(null);

    const showToast = (message: string, type: 'success' | 'error' | 'warning' | 'info' = 'success', icon?: string) => {
        setToast({ message, type, icon });
        setTimeout(() => setToast(null), 3500);
    };

    const [isEditingInfo, setIsEditingInfo] = useState(false);
    const [editForm, setEditForm] = useState({ firstName: '', lastName: '', phone: '', email: '', address: '', city: '', zipCode: '', country: 'France' });
    const [isEditingMarketing, setIsEditingMarketing] = useState(false);
    const [marketingForm, setMarketingForm] = useState({ source: '', campaignName: '', interestServiceId: '' });
    const [newNote, setNewNote] = useState('');
    const currentUserInfo = AuthStore.getCurrentUser();
    const [addForm, setAddForm] = useState({ firstName: '', lastName: '', phone: '', email: '', source: 'WEBSITE', address: '', city: '', zipCode: '', country: 'France', interestServiceId: '', agencyId: currentUserInfo?.agencyId || 'HQ-001', assignedToSalesId: '' });
    const [agencies, setAgencies] = useState<AgencyExt[]>([]);
    const [users, setUsers] = useState<StaffUser[]>([]);

    // Advanced Filters
    const [filters, setFilters] = useState<{
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
        search?: string;
        tags?: string;
    }>({});
    const [showFilters, setShowFilters] = useState(false);
    const [hoveredColumnId, setHoveredColumnId] = useState<string | null>(null);
    const [velocityMetrics, setVelocityMetrics] = useState<any>(null);
    const [callbackSchedule, setCallbackSchedule] = useState('');

    // ─── Recherche temps réel ───
    const [searchQuery, setSearchQuery] = useState('');
    const searchTimeout = React.useRef<NodeJS.Timeout | null>(null);

    // ─── Sélection en masse ───
    const [selectedIds, setSelectedIds] = useState<Set<string>>(new Set());
    const [showBulkBar, setShowBulkBar] = useState(false);

    // ─── Timeline / Historique ───
    const [timeline, setTimeline] = useState<any[]>([]);
    const [showTimeline, setShowTimeline] = useState(false);

    // ─── Relance programmée ───
    const [showFollowUpModal, setShowFollowUpModal] = useState(false);
    const [followUpDate, setFollowUpDate] = useState('');
    const [followUpReason, setFollowUpReason] = useState('');

    // ─── Raison de perte ───
    const [showLostReasonModal, setShowLostReasonModal] = useState(false);
    const [lostReason, setLostReason] = useState('');
    const [pendingLostProspectId, setPendingLostProspectId] = useState<string | null>(null);

    // ─── Tags ───
    const AVAILABLE_TAGS = ['VIP', 'Urgent', 'Diaspora', 'À relancer', 'Référé', 'Froid', 'Chaud', 'Concurrent'];
    const [showTagsModal, setShowTagsModal] = useState(false);
    // ─── Tarification backend (source unique de vérité) ───
    const [backendPricing, setBackendPricing] = useState<Record<string, { priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>>({});
    const pricingCache = React.useRef<Record<string, Promise<any> | undefined>>({});

    const router = useRouter();

    // Fetch le prix résolu d'un service depuis le backend
    const fetchServicePrice = async (serviceId: string): Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }> => {
        // Cache statique
        if (backendPricing[serviceId]) return backendPricing[serviceId];

        // Cache des requêtes en cours
        if (pricingCache.current[serviceId]) {
            return pricingCache.current[serviceId];
        }

        const fetchPromise = (async () => {
            try {
                const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
                const res = await fetch(`${API_URL}/payments/resolve-price/${serviceId}`);
                if (!res.ok) throw new Error('Failed to resolve price');
                const data = await res.json();
                const entry = { priceEuros: data.priceEuros, pricePer3: data.pricePer3, serviceName: data.serviceName, source: data.source, promoActive: data.promoActive };
                setBackendPricing(prev => ({ ...prev, [serviceId]: entry }));
                return entry;
            } catch (e) {
                console.error('[Pricing] Failed to fetch price for', serviceId, e);
                // Fallback si le backend est indisponible
                const fallback = SERVICES_CATALOG.find(s => s.id === serviceId);
                return { priceEuros: 100, pricePer3: 34, serviceName: fallback?.title || serviceId, source: 'OFFLINE_FALLBACK', promoActive: false };
            }
        })();

        pricingCache.current[serviceId] = fetchPromise;
        return fetchPromise;
    };

    // Initial load
    useEffect(() => {
        loadProspects();
        AgencyStore.getAllAgencies().then(setAgencies);
        UserStore.getAllUsers().then(setUsers);
        SalesStore.getPipelineVelocity().then(setVelocityMetrics);
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
            setMarketingForm({
                source: selectedProspect.source || 'WEBSITE',
                campaignName: selectedProspect.campaignName || '',
                interestServiceId: selectedProspect.interestServiceId || ''
            });
            setIsEditingInfo(false);
            setIsEditingMarketing(false);
            setNewNote('');
        } else {
            setCallHistory([]);
            setIsEditingInfo(false);
            setIsEditingMarketing(false);
            setNewNote('');
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

    const handleSaveMarketing = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, marketingForm as any);
        setSelectedProspect(prev => prev ? { ...prev, ...marketingForm } as any : null);
        setProspects(prev => prev.map(p => p.id === selectedProspect.id ? { ...p, ...marketingForm } as any : p));
        setIsEditingMarketing(false);
        setIsLoading(false);
    };

    const loadProspects = async () => {
        setIsLoading(true);
        const result = await SalesStore.getProspects(1, 100, filters);

        // Handle paginated response
        const newData = Array.isArray(result) ? result : result.data || [];
        setProspects(newData);

        // Maj du prospect sélectionné si ouvert (pour que la drawer affiche les nouvelles infos)
        setSelectedProspect(prev => {
            if (prev) {
                const updated = newData.find((p: Prospect) => p.id === prev.id);
                return updated || prev;
            }
            return prev;
        });

        setIsLoading(false);
    };

    // Reload when filters change
    useEffect(() => {
        loadProspects();
    }, [filters]);

    const handleStatusChange = async (prospectId: string, newStatus: ProspectStatus) => {
        if (newStatus === 'SIGNED') {
            showToast('Le statut "Signé" est automatique. Il se déclenche après encaissement Stripe validé.', 'warning', '⚠️');
            return;
        }

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
            showToast('RDV confirmé et juriste assigné automatiquement', 'success', '📅');
        } else {
            showToast('Impossible de réserver ce créneau. Il a peut-être été pris entre-temps.', 'error', '⚠️');
        }
    };

    // Marquer comme non honoré (no-show)
    const handleNoShow = (prospect: Prospect) => {
        setConfirmDialog({
            title: 'Marquer comme Non Honoré',
            message: `Confirmer que ${prospect.firstName} ${prospect.lastName} n'est pas venu au rendez-vous ?\n\nLe prospect sera déplacé dans la colonne "Non Honoré" et pourra être reprogrammé.`,
            confirmLabel: 'Confirmer le No-Show',
            type: 'warning',
            onConfirm: async () => {
                await handleStatusChange(prospect.id, 'NO_SHOW');
                showToast(`${prospect.firstName} marqué Non Honoré — à reprogrammer ou relancer`, 'warning', '🚫');
                setConfirmDialog(null);
            }
        });
    };

    // Encaissement : crée une session Stripe pour le prospect
    // Le webhook Stripe déclenchera auto : SIGNED + Lead CRM
    const handleStartPayment = async (prospect: Prospect, installments: 1 | 3 = 1) => {
        const serviceId = prospect.eligibilityResult?.matchedProcedures?.[0] || prospect.interestServiceId || 'consultation_juridique';

        // ── Récupérer le prix officiel depuis le backend ──
        const resolved = await fetchServicePrice(serviceId);
        const amount = installments === 3 ? resolved.pricePer3 : resolved.priceEuros;

        try {
            const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
            const response = await fetch(`${API_URL}/payments/prospect-checkout`, {
                method: 'POST',
                headers: SalesStore.getHeaders(),
                body: JSON.stringify({
                    prospectId: prospect.id,
                    amount, // informatif — le backend résout le vrai prix
                    serviceId,
                    serviceName: resolved.serviceName,
                    installments,
                    successUrl: `${window.location.origin}/admin/sales?payment=success&prospect=${prospect.id}`,
                    cancelUrl: `${window.location.origin}/admin/sales?payment=cancelled&prospect=${prospect.id}`,
                }),
            });

            if (!response.ok) {
                const errorData = await response.json().catch(() => ({}));
                throw new Error(errorData.message || 'Checkout creation failed');
            }
            const data = await response.json();

            if (data.url) {
                // Redirection vers Stripe Checkout
                window.location.href = data.url;
            } else {
                showToast('Impossible de créer la session de paiement. Vérifiez la configuration Stripe.', 'error', '❌');
            }
        } catch (error) {
            console.error('[Payment] Error:', error);
            showToast('Erreur de paiement. Vérifiez votre connexion ou la configuration Stripe.', 'error', '❌');
        }
    };

    const handleImport = async () => {
        // Mock import
        const count = await SalesStore.importProspectsFromCSV(new File([], 'dummy.csv'));
        await loadProspects();
        setShowImportModal(false);
        showToast(`${count} prospect${(count || 0) > 1 ? 's' : ''} importé${(count || 0) > 1 ? 's' : ''} avec succès !`, 'success', '📥');
    };

    // ─── Recherche avec debounce ───
    const handleSearchChange = (value: string) => {
        setSearchQuery(value);
        if (searchTimeout.current) clearTimeout(searchTimeout.current);
        searchTimeout.current = setTimeout(() => {
            setFilters(prev => ({ ...prev, search: value || undefined }));
        }, 300);
    };

    // ─── Suppression de prospect ───
    const handleDeleteProspect = (prospect: Prospect) => {
        setConfirmDialog({
            title: '🗑️ Supprimer le prospect',
            message: `Êtes-vous sûr de vouloir supprimer ${prospect.firstName} ${prospect.lastName} ? Cette action est irréversible. Toutes les notes, appels et RDV liés seront également supprimés.`,
            confirmLabel: 'Supprimer définitivement',
            type: 'danger',
            onConfirm: async () => {
                const success = await SalesStore.deleteProspect(prospect.id);
                if (success) {
                    setProspects(prev => prev.filter(p => p.id !== prospect.id));
                    if (selectedProspect?.id === prospect.id) setSelectedProspect(null);
                    showToast(`${prospect.firstName} ${prospect.lastName} supprimé`, 'success', '🗑️');
                } else {
                    showToast('Échec de la suppression', 'error', '❌');
                }
                setConfirmDialog(null);
            }
        });
    };

    // ─── Annulation de RDV ───
    const handleCancelAppointment = async (prospectId: string, reason?: string) => {
        const result = await SalesStore.cancelAppointment(prospectId, reason);
        if (result) {
            await loadProspects();
            setSelectedProspect(result);
            showToast('RDV annulé avec succès', 'success', '❌');
        } else {
            showToast('Échec annulation RDV', 'error');
        }
    };

    // ─── Relance programmée ───
    const handleScheduleFollowUp = async () => {
        if (!selectedProspect || !followUpDate) return;
        const result = await SalesStore.scheduleFollowUp(selectedProspect.id, followUpDate, followUpReason);
        if (result) {
            await loadProspects();
            setShowFollowUpModal(false);
            setFollowUpDate('');
            setFollowUpReason('');
            showToast(`Relance programmée le ${new Date(followUpDate).toLocaleDateString('fr-FR')}`, 'success', '⏰');
        }
    };

    // ─── Passage en LOST avec raison ───
    const handleMarkAsLost = (prospectId: string) => {
        setPendingLostProspectId(prospectId);
        setLostReason('');
        setShowLostReasonModal(true);
    };

    const confirmLostReason = async () => {
        if (!pendingLostProspectId) return;
        await SalesStore.updateProspect(pendingLostProspectId, { status: 'LOST', lostReason: lostReason || 'Non spécifié' } as any);
        await loadProspects();
        setShowLostReasonModal(false);
        setPendingLostProspectId(null);
        showToast('Prospect marqué comme perdu', 'warning', '⚫');
    };

    // ─── Tags ───
    const handleToggleTag = async (prospect: Prospect, tag: string) => {
        const currentTags: string[] = prospect.tags ? JSON.parse(prospect.tags) : [];
        const newTags = currentTags.includes(tag)
            ? currentTags.filter(t => t !== tag)
            : [...currentTags, tag];

        await SalesStore.updateProspect(prospect.id, { tags: JSON.stringify(newTags) } as any);
        setProspects(prev => prev.map(p => p.id === prospect.id ? { ...p, tags: JSON.stringify(newTags) } : p));
        setSelectedProspect(prev => prev?.id === prospect.id ? { ...prev, tags: JSON.stringify(newTags) } : prev);
    };

    // ─── Sélection en masse ───
    const toggleSelection = (id: string) => {
        setSelectedIds(prev => {
            const next = new Set(prev);
            if (next.has(id)) next.delete(id);
            else next.add(id);
            return next;
        });
    };

    const handleBulkStatusChange = async (status: string) => {
        const ids = Array.from(selectedIds);
        const result = await SalesStore.bulkUpdate(ids, { status });
        showToast(`${result.updated} prospect(s) mis à jour`, 'success', '📦');
        setSelectedIds(new Set());
        await loadProspects();
    };

    const handleBulkDelete = () => {
        const count = selectedIds.size;
        setConfirmDialog({
            title: '🗑️ Suppression en masse',
            message: `Supprimer ${count} prospect(s) sélectionné(s) ? Cette action est irréversible.`,
            confirmLabel: `Supprimer ${count} prospect(s)`,
            type: 'danger',
            onConfirm: async () => {
                const ids = Array.from(selectedIds);
                const result = await SalesStore.bulkDelete(ids);
                showToast(`${result.deleted} prospect(s) supprimé(s)`, 'success', '🗑️');
                setSelectedIds(new Set());
                await loadProspects();
                setConfirmDialog(null);
            }
        });
    };

    // ─── Timeline ───
    const loadTimeline = async (prospectId: string) => {
        const data = await SalesStore.getTimeline(prospectId);
        setTimeline(data);
        setShowTimeline(true);
    };

    // ─── Action rapide : Issue d'appel sans cockpit ─────────────
    const handleQuickCallOutcome = async (outcome: 'NO_ANSWER' | 'CALLBACK' | 'NOT_INTERESTED' | 'WRONG_NUMBER' | 'INTERESTED') => {
        if (!selectedProspect) return;

        const MAX_NO_ANSWER = 5;
        const MAX_CALLBACKS = 3;

        const updates: any = {};
        const newCallAttempts = (selectedProspect.callAttempts || 0) + 1;
        let newNoAnswerCount = selectedProspect.noAnswerCount || 0;
        let newCallbackCount = selectedProspect.callbackCount || 0;

        updates.callAttempts = newCallAttempts;
        updates.lastCallOutcome = outcome;
        updates.lastContactAt = new Date().toISOString();

        if (outcome === 'NO_ANSWER') {
            newNoAnswerCount++;
            updates.noAnswerCount = newNoAnswerCount;
        } else {
            updates.noAnswerCount = 0;
            newNoAnswerCount = 0;
        }

        if (outcome === 'CALLBACK') {
            newCallbackCount++;
            updates.callbackCount = newCallbackCount;
            updates.callbackRequestedAt = new Date().toISOString();
            if (callbackSchedule) {
                updates.callbackScheduledAt = new Date(callbackSchedule).toISOString();
            }
        }

        let autoLostReason = '';

        if (outcome === 'NOT_INTERESTED' || outcome === 'WRONG_NUMBER') {
            updates.status = 'LOST';
            autoLostReason = outcome === 'NOT_INTERESTED' ? 'Pas intéressé' : 'Mauvais numéro';
            updates.lostReason = autoLostReason;
        } else if (outcome === 'INTERESTED') {
            updates.status = 'QUALIFIED';
            updates.lastCallOutcome = 'INTERESTED';
        } else if (newNoAnswerCount >= MAX_NO_ANSWER) {
            updates.status = 'LOST';
            autoLostReason = `${newNoAnswerCount} appels sans réponse consécutifs`;
            updates.lostReason = autoLostReason;
        } else if (newCallbackCount >= MAX_CALLBACKS) {
            updates.status = 'LOST';
            autoLostReason = `${newCallbackCount} demandes de rappel sans suite`;
            updates.lostReason = autoLostReason;
        } else {
            updates.status = 'CONTACTED';
        }

        await SalesStore.updateProspect(selectedProspect.id, updates);

        // Log note
        const outcomeLabels: Record<string, string> = {
            'NO_ANSWER': 'Pas de réponse',
            'CALLBACK': 'À rappeler',
            'NOT_INTERESTED': 'Pas intéressé',
            'WRONG_NUMBER': 'Mauvais numéro',
            'INTERESTED': 'Intéressé — Qualifié ✅'
        };
        let noteText = `📞 Tentative #${newCallAttempts} — ${outcomeLabels[outcome]}`;
        if (outcome === 'NO_ANSWER') noteText += ` (${newNoAnswerCount}/${MAX_NO_ANSWER})`;
        if (outcome === 'CALLBACK') noteText += ` (${newCallbackCount}/${MAX_CALLBACKS})`;
        if (autoLostReason) noteText += `\n⚠️ Auto-classé PERDU : ${autoLostReason}`;

        await SalesStore.addNote(selectedProspect.id, noteText);

        if (autoLostReason) {
            showToast(`Prospect automatiquement classé PERDU — ${autoLostReason}`, 'warning', '⚠️');
        } else {
            const outcomeEmoji: Record<string, string> = { 'NO_ANSWER': '📵', 'CALLBACK': '🔄', 'NOT_INTERESTED': '❌', 'WRONG_NUMBER': '⚠️', 'INTERESTED': '✅' };
            showToast(`${outcomeLabels[outcome]} enregistré (tentative #${newCallAttempts})`, 'info', outcomeEmoji[outcome] || '📞');
        }

        loadProspects();
    };

    const handleSaveNote = async (text: string) => {
        if (!selectedProspect) return;
        if (text) {
            await SalesStore.addNote(selectedProspect.id, text);
        }
        // Refresh to see notes (optional, but good for UX)
        loadProspects();
    };

    return (
        <div className="h-full flex flex-col bg-slate-50">
            {/* Header / Actions Bar */}
            <div className="bg-white border-b border-slate-200 px-6 py-4">
                <div className="flex items-center justify-between mb-3">
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

                {/* ── Barre de recherche ── */}
                <div className="flex items-center gap-3">
                    <div className="relative flex-1 max-w-md">
                        <Search size={16} className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" />
                        <input
                            type="text"
                            value={searchQuery}
                            onChange={(e) => handleSearchChange(e.target.value)}
                            placeholder="Rechercher par nom, téléphone, email, ville..."
                            className="w-full pl-10 pr-4 py-2 bg-slate-50 border border-slate-200 rounded-xl text-sm focus:outline-none focus:ring-2 focus:ring-indigo-200 focus:border-indigo-400 transition-all"
                        />
                        {searchQuery && (
                            <button
                                onClick={() => { setSearchQuery(''); setFilters(prev => ({ ...prev, search: undefined })); }}
                                className="absolute right-3 top-1/2 -translate-y-1/2 text-slate-400 hover:text-slate-600"
                            >
                                <X size={14} />
                            </button>
                        )}
                    </div>

                    <button
                        onClick={() => setShowFilters(!showFilters)}
                        className={`flex items-center gap-2 px-3 py-2 rounded-xl text-sm font-medium border transition-all ${showFilters ? 'bg-indigo-50 border-indigo-200 text-indigo-700' : 'bg-white border-slate-200 text-slate-600 hover:bg-slate-50'}`}
                    >
                        <Filter size={14} /> Filtres
                        {Object.values(filters).filter(v => v && v !== searchQuery).length > 0 && (
                            <span className="w-5 h-5 flex items-center justify-center bg-indigo-600 text-white text-[10px] font-bold rounded-full">
                                {Object.values(filters).filter(v => v && v !== searchQuery).length}
                            </span>
                        )}
                    </button>

                    <span className="text-xs text-slate-400 font-medium">
                        {prospects.length} prospect{prospects.length > 1 ? 's' : ''}
                    </span>
                </div>
            </div>

            {/* ── Barre d'actions en masse ── */}
            {selectedIds.size > 0 && (
                <div className="bg-indigo-600 text-white px-6 py-3 flex items-center justify-between animate-in slide-in-from-top">
                    <span className="text-sm font-bold">
                        📦 {selectedIds.size} prospect(s) sélectionné(s)
                    </span>
                    <div className="flex items-center gap-2">
                        <select
                            onChange={(e) => { if (e.target.value) handleBulkStatusChange(e.target.value); e.target.value = ''; }}
                            className="bg-white/20 text-white border border-white/30 rounded-lg px-3 py-1.5 text-sm font-medium focus:outline-none [&>option]:text-slate-900"
                            defaultValue=""
                        >
                            <option value="" disabled>Changer statut...</option>
                            {COLUMNS.filter(c => c.id !== 'SIGNED').map(col => (
                                <option key={col.id} value={col.id}>{col.label}</option>
                            ))}
                        </select>
                        <button
                            onClick={handleBulkDelete}
                            className="px-3 py-1.5 bg-red-500 text-white rounded-lg text-sm font-bold hover:bg-red-600 transition-all"
                        >
                            🗑️ Supprimer
                        </button>
                        <button
                            onClick={() => setSelectedIds(new Set())}
                            className="px-3 py-1.5 bg-white/20 text-white rounded-lg text-sm font-medium hover:bg-white/30 transition-all"
                        >
                            Annuler
                        </button>
                    </div>
                </div>
            )}

            {viewMode === 'ANALYTICS' ? (
                <div className="p-8 overflow-y-auto space-y-8">
                    {/* ─── Pipeline Velocity Metrics ─── */}
                    {velocityMetrics && (
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
                                        {['NEW', 'CONTACTED', 'QUALIFIED', 'MEETING_BOOKED'].map(status => {
                                            const hours = velocityMetrics.avgHoursInStage?.[status] || 0;
                                            const label = { NEW: 'Nouveau', CONTACTED: 'Contacté', QUALIFIED: 'Qualifié', MEETING_BOOKED: 'RDV Fixé' }[status];
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
                    )}

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
                                                    showToast('Le statut "Signé" est automatique — encaissement Stripe requis.', 'warning', '⚠️');
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
                                                    // CONTACTED: Overdue callbacks first, then by score
                                                    if (column.id === 'CONTACTED' || column.id === 'QUALIFIED') {
                                                        const aOverdue = a.callbackScheduledAt && new Date(a.callbackScheduledAt) < new Date() ? 1 : 0;
                                                        const bOverdue = b.callbackScheduledAt && new Date(b.callbackScheduledAt) < new Date() ? 1 : 0;
                                                        if (aOverdue !== bOverdue) return bOverdue - aOverdue;
                                                        return b.score - a.score;
                                                    }
                                                    // Default: Newest first
                                                    return new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
                                                })
                                                .map(prospect => {
                                                    // ─── Visual differentiation by call outcome ───
                                                    const outcome = prospect.lastCallOutcome;
                                                    const noAnswerCount = prospect.noAnswerCount || 0;
                                                    const callbackCount = prospect.callbackCount || 0;

                                                    // Default (no call yet)
                                                    let cardClasses = 'bg-white border border-slate-200';
                                                    let stripColor = '';
                                                    let badgeContent: React.ReactNode = null;
                                                    let nameClass = 'text-slate-900';
                                                    let cardOpacity = '';

                                                    if (outcome === 'CALLBACK') {
                                                        cardClasses = 'bg-amber-50 border-2 border-amber-400';
                                                        stripColor = 'bg-amber-500';
                                                        nameClass = 'text-amber-900';
                                                        badgeContent = (
                                                            <div className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-amber-500 text-white text-xs font-black tracking-wide mb-2">
                                                                <span className="text-sm">🔄</span>
                                                                <span>À RAPPELER</span>
                                                                {callbackCount > 1 && <span className="ml-auto bg-white/30 px-1.5 py-0.5 rounded text-[10px]">×{callbackCount}</span>}
                                                            </div>
                                                        );
                                                    } else if (outcome === 'NO_ANSWER') {
                                                        const critical = noAnswerCount >= 3;
                                                        cardClasses = critical
                                                            ? 'bg-red-50 border-2 border-red-500 shadow-red-100'
                                                            : 'bg-orange-50 border-2 border-orange-400';
                                                        stripColor = critical ? 'bg-red-600' : 'bg-orange-500';
                                                        nameClass = critical ? 'text-red-900' : 'text-orange-900';
                                                        badgeContent = (
                                                            <div className={`flex items-center gap-2 px-3 py-1.5 rounded-lg text-white text-xs font-black tracking-wide mb-2 ${critical ? 'bg-red-600 animate-pulse' : 'bg-orange-500'}`}>
                                                                <span className="text-sm">📵</span>
                                                                <span>SANS RÉPONSE</span>
                                                                <span className="ml-auto bg-white/30 px-1.5 py-0.5 rounded text-[10px] font-mono">{noAnswerCount}/5</span>
                                                            </div>
                                                        );
                                                    } else if (outcome === 'INTERESTED') {
                                                        cardClasses = 'bg-emerald-50 border-2 border-emerald-500 shadow-emerald-100';
                                                        stripColor = 'bg-emerald-600';
                                                        nameClass = 'text-emerald-900';
                                                        badgeContent = (
                                                            <div className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-emerald-600 text-white text-xs font-black tracking-wide mb-2">
                                                                <span className="text-sm">✅</span>
                                                                <span>INTÉRESSÉ — Prêt pour RDV</span>
                                                            </div>
                                                        );
                                                    } else if (outcome === 'NOT_INTERESTED') {
                                                        cardClasses = 'bg-slate-100 border border-slate-300';
                                                        stripColor = 'bg-slate-400';
                                                        nameClass = 'text-slate-400 line-through';
                                                        cardOpacity = 'opacity-60';
                                                        badgeContent = (
                                                            <div className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-slate-400 text-white text-xs font-black tracking-wide mb-2">
                                                                <span className="text-sm">❌</span>
                                                                <span>PAS INTÉRESSÉ</span>
                                                            </div>
                                                        );
                                                    } else if (outcome === 'WRONG_NUMBER') {
                                                        cardClasses = 'bg-slate-100 border border-slate-300';
                                                        stripColor = 'bg-slate-500';
                                                        nameClass = 'text-slate-400 line-through';
                                                        cardOpacity = 'opacity-50';
                                                        badgeContent = (
                                                            <div className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-slate-500 text-white text-xs font-black tracking-wide mb-2">
                                                                <span className="text-sm">⚠️</span>
                                                                <span>MAUVAIS NUMÉRO</span>
                                                            </div>
                                                        );
                                                    }

                                                    return (
                                                        <div
                                                            key={prospect.id}
                                                            draggable={true}
                                                            onDragStart={(e) => {
                                                                e.dataTransfer.setData('prospectId', prospect.id);
                                                                e.dataTransfer.effectAllowed = 'move';
                                                            }}
                                                            onClick={() => setSelectedProspect(prospect)}
                                                            className={`relative overflow-hidden rounded-xl shadow-sm cursor-pointer hover:shadow-lg transition-all group active:scale-[0.98] ${cardClasses} ${cardOpacity}`}
                                                        >
                                                            {/* Colored left strip */}
                                                            {stripColor && (
                                                                <div className={`absolute left-0 top-0 bottom-0 w-1.5 ${stripColor}`} />
                                                            )}

                                                            <div className={`p-4 ${stripColor ? 'pl-5' : ''}`}>
                                                                {/* Selection checkbox + Hot Lead Badge */}
                                                                <div className="flex items-center justify-between mb-1">
                                                                    <input
                                                                        type="checkbox"
                                                                        checked={selectedIds.has(prospect.id)}
                                                                        onChange={(e) => { e.stopPropagation(); toggleSelection(prospect.id); }}
                                                                        onClick={(e) => e.stopPropagation()}
                                                                        className="w-4 h-4 rounded border-slate-300 text-indigo-600 focus:ring-indigo-200 cursor-pointer opacity-0 group-hover:opacity-100 transition-opacity"
                                                                        style={selectedIds.has(prospect.id) ? { opacity: 1 } : {}}
                                                                    />
                                                                    {prospect.score >= 50 && (
                                                                        <div className="text-orange-500 animate-pulse" title="Prospect chaud !">
                                                                            <Flame size={16} fill="currentColor" />
                                                                        </div>
                                                                    )}
                                                                </div>

                                                                {/* Call Outcome Banner */}
                                                                {badgeContent}

                                                                <h4 className={`font-bold mb-1.5 ${nameClass}`}>
                                                                    {prospect.firstName} {prospect.lastName}
                                                                </h4>

                                                                {/* Tags */}
                                                                <div className="flex flex-wrap gap-1.5 mb-3">
                                                                    <span className="text-[11px] px-2 py-0.5 bg-white/80 text-slate-600 rounded-md font-medium border border-slate-100">
                                                                        {prospect.source.replace('_', ' ')}
                                                                    </span>
                                                                    {prospect.campaignName && (
                                                                        <span className="text-[11px] px-2 py-0.5 bg-indigo-50 text-indigo-600 rounded-md font-medium border border-indigo-100">
                                                                            {prospect.campaignName}
                                                                        </span>
                                                                    )}
                                                                    {(prospect.callAttempts || 0) > 0 && (
                                                                        <span className="text-[11px] px-2 py-0.5 bg-violet-100 text-violet-700 rounded-md font-bold border border-violet-200">
                                                                            📞 {prospect.callAttempts} appel{(prospect.callAttempts || 0) > 1 ? 's' : ''}
                                                                        </span>
                                                                    )}
                                                                    {/* Custom Tags */}
                                                                    {prospect.tags && JSON.parse(prospect.tags).map((tag: string) => (
                                                                        <span key={tag} className="text-[11px] px-2 py-0.5 bg-amber-50 text-amber-700 rounded-md font-bold border border-amber-200">
                                                                            🏷️ {tag}
                                                                        </span>
                                                                    ))}
                                                                </div>

                                                                {/* Contextual status indicators */}
                                                                {prospect.status === 'MEETING_BOOKED' && prospect.eligibilityResult && (
                                                                    <div className="text-[10px] px-2 py-1 bg-emerald-50 text-emerald-700 rounded-lg border border-emerald-200 mb-2 font-bold">
                                                                        ✅ Simulation faite
                                                                    </div>
                                                                )}
                                                                {prospect.callbackScheduledAt && prospect.lastCallOutcome === 'CALLBACK' && (
                                                                    <div className={`text-[10px] px-2 py-1 rounded-lg border mb-2 font-bold ${new Date(prospect.callbackScheduledAt) < new Date() ? 'bg-red-50 text-red-700 border-red-200' : 'bg-blue-50 text-blue-700 border-blue-200'}`}>
                                                                        📅 Rappel : {new Date(prospect.callbackScheduledAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })}
                                                                        {new Date(prospect.callbackScheduledAt) < new Date() && ' ⚠️ EN RETARD'}
                                                                    </div>
                                                                )}
                                                                {prospect.status === 'LOST' && prospect.lostReason && (
                                                                    <div className="text-[10px] px-2 py-1 bg-slate-100 text-slate-500 rounded-lg mb-2 font-medium">
                                                                        📋 {prospect.lostReason}
                                                                    </div>
                                                                )}
                                                                {prospect.status === 'NO_SHOW' && (
                                                                    <div className="text-[10px] px-2 py-1 bg-red-50 text-red-600 rounded-lg border border-red-200 mb-2 font-bold">
                                                                        🚫 No-show #{prospect.noShowCount || 1}
                                                                    </div>
                                                                )}

                                                                {/* Footer */}
                                                                <div className="flex items-center justify-between pt-2 border-t border-black/5 text-slate-400 text-xs">
                                                                    <span>{new Date(prospect.createdAt).toLocaleDateString('fr-FR')}</span>
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
                                                        </div>
                                                    );
                                                })}
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
                                    {/* NEW : Appeler uniquement (doit d'abord qualifier) */}
                                    {selectedProspect.status === 'NEW' && (
                                        <div className="flex gap-2 w-full">
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Appeler pour qualifier
                                            </button>
                                            <button
                                                onClick={async () => {
                                                    await handleStatusChange(selectedProspect.id, 'QUALIFIED');
                                                    showToast(`${selectedProspect.firstName} qualifié manuellement`, 'success', '🟢');
                                                }}
                                                className="flex items-center justify-center gap-2 py-2.5 px-4 bg-cyan-50 border border-cyan-200 text-cyan-700 rounded-xl text-xs font-bold hover:bg-cyan-100 transition-all active:scale-[0.97]"
                                                title="Qualifier directement (si déjà contacté par un autre canal)"
                                            >
                                                🟢 Qualifier
                                            </button>
                                        </div>
                                    )}

                                    {/* CONTACTED : Appeler + Fixer RDV */}
                                    {selectedProspect.status === 'CONTACTED' && (
                                        <div className="flex gap-2 w-full">
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Rappeler
                                            </button>
                                            <button
                                                onClick={async () => {
                                                    await handleStatusChange(selectedProspect.id, 'QUALIFIED');
                                                    showToast(`${selectedProspect.firstName} qualifié — prêt pour RDV`, 'success', '🟢');
                                                }}
                                                className="flex items-center justify-center gap-2 py-2.5 px-4 bg-cyan-500 text-white rounded-xl text-sm font-bold hover:bg-cyan-600 transition-all shadow-sm shadow-cyan-200 active:scale-[0.97]"
                                            >
                                                🟢 Qualifier
                                            </button>
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="flex items-center justify-center gap-2 py-2.5 px-4 bg-white border border-slate-200 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                            >
                                                <Calendar size={15} />
                                                RDV
                                            </button>
                                        </div>
                                    )}

                                    {/* QUALIFIED : Fixer RDV + Appeler */}
                                    {selectedProspect.status === 'QUALIFIED' && (
                                        <div className="flex gap-2 w-full">
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                            >
                                                <Calendar size={15} />
                                                Fixer un RDV en agence
                                            </button>
                                            <button
                                                onClick={() => setShowCallCockpit(true)}
                                                className="flex items-center justify-center gap-2 py-2.5 px-4 bg-white border border-slate-200 text-slate-700 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                            >
                                                <Phone size={15} />
                                                Appeler
                                            </button>
                                        </div>
                                    )}

                                    {/* MEETING_BOOKED : Simulateur + Appeler + Non honoré → Encaisser → Auto SIGNED */}
                                    {selectedProspect.status === 'MEETING_BOOKED' && (
                                        <div className="flex flex-col gap-2 w-full">
                                            {/* Ligne 1 : Simulateur + Appeler + Non honoré */}
                                            <div className="flex gap-2">
                                                <button
                                                    onClick={() => setShowSimulatorModal(true)}
                                                    className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                                >
                                                    <Microscope size={15} />
                                                    Simulateur
                                                </button>
                                                <button
                                                    onClick={() => setShowCallCockpit(true)}
                                                    className="flex items-center justify-center gap-2 py-2.5 px-3 bg-white border border-slate-200 text-slate-600 rounded-xl text-sm font-bold hover:bg-slate-50 transition-all active:scale-[0.97]"
                                                >
                                                    <Phone size={14} />
                                                </button>
                                                <button
                                                    onClick={() => handleNoShow(selectedProspect)}
                                                    className="flex items-center justify-center gap-2 py-2.5 px-4 bg-white border border-red-200 text-red-600 rounded-xl text-sm font-bold hover:bg-red-50 transition-all active:scale-[0.97]"
                                                    title="Marquer Non Honoré"
                                                >
                                                    🚫 Non Honoré
                                                </button>
                                            </div>
                                            {/* Indicateur simulation réalisée */}
                                            {selectedProspect.eligibilityResult ? (
                                                <>
                                                    <div className="flex items-center gap-2 px-3 py-2 bg-emerald-50 border border-emerald-200 rounded-xl">
                                                        <span className="text-emerald-600 text-sm">✅</span>
                                                        <span className="text-xs font-bold text-emerald-800">Simulation réalisée — {selectedProspect.eligibilityResult.isEligible ? 'Éligible' : 'Non éligible'}</span>
                                                    </div>
                                                    <PaymentButtons prospect={selectedProspect} onPay={handleStartPayment} fetchPrice={fetchServicePrice} />
                                                </>
                                            ) : (
                                                <div className="text-center p-3 border-2 border-dashed border-amber-200 bg-amber-50/50 rounded-xl">
                                                    <p className="text-xs text-amber-700 font-medium">⏳ En attente de la simulation lors du RDV — encaissement bloqué</p>
                                                </div>
                                            )}
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

                                    {/* NO_SHOW : Reprogrammer, Rappeler ou Marquer PERDU */}
                                    {selectedProspect.status === 'NO_SHOW' && (
                                        <div className="flex flex-col gap-2 w-full">
                                            <div className="flex items-center gap-2 px-3 py-2 bg-red-50 border border-red-200 rounded-xl">
                                                <span className="text-red-600 text-sm">🚫</span>
                                                <span className="text-xs font-bold text-red-800">No-show #{selectedProspect.noShowCount || 1} — {(selectedProspect.noShowCount || 0) >= 2 ? 'Sera auto-perdu au prochain' : 'Possibilité de reprogrammer'}</span>
                                            </div>
                                            <div className="flex gap-2">
                                                <button
                                                    onClick={() => setShowBookingModal(true)}
                                                    className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 active:scale-[0.97]"
                                                >
                                                    <Calendar size={15} />
                                                    Reprogrammer RDV
                                                </button>
                                                <button
                                                    onClick={async () => {
                                                        await handleStatusChange(selectedProspect.id, 'CONTACTED');
                                                        showToast(`${selectedProspect.firstName} remis en file de prospection`, 'success', '🔄');
                                                    }}
                                                    className="flex items-center justify-center gap-2 py-2.5 px-4 bg-white border border-purple-200 text-purple-700 rounded-xl text-sm font-bold hover:bg-purple-50 transition-all active:scale-[0.97]"
                                                >
                                                    <Phone size={14} />
                                                    Recontacter
                                                </button>
                                            </div>
                                            <button
                                                onClick={() => setConfirmDialog({
                                                    title: 'Marquer comme Perdu',
                                                    message: `Confirmer que ${selectedProspect.firstName} ${selectedProspect.lastName} est définitivement perdu ?`,
                                                    confirmLabel: 'Confirmer Perdu',
                                                    type: 'danger',
                                                    onConfirm: async () => {
                                                        await handleStatusChange(selectedProspect.id, 'LOST');
                                                        showToast('Prospect marqué comme Perdu', 'info', '⚫');
                                                        setConfirmDialog(null);
                                                    }
                                                })}
                                                className="w-full flex items-center justify-center gap-2 py-2 bg-slate-100 text-slate-500 rounded-xl text-xs font-bold hover:bg-slate-200 transition-all"
                                            >
                                                ⚫ Marquer Perdu
                                            </button>
                                        </div>
                                    )}

                                    {/* LOST : Réactiver */}
                                    {selectedProspect.status === 'LOST' && (
                                        <div className="flex flex-col gap-2 w-full">
                                            {selectedProspect.lostReason && (
                                                <div className="flex items-center gap-2 px-3 py-2 bg-slate-100 border border-slate-200 rounded-xl">
                                                    <span className="text-slate-500 text-sm">📋</span>
                                                    <span className="text-xs font-bold text-slate-600">Raison : {selectedProspect.lostReason}</span>
                                                </div>
                                            )}
                                            <button
                                                onClick={async () => {
                                                    setIsLoading(true);
                                                    const reactivated = await SalesStore.reactivateProspect(selectedProspect.id);
                                                    if (reactivated) {
                                                        setSelectedProspect(reactivated);
                                                        await loadProspects();
                                                        showToast(`${selectedProspect.firstName} réactivé — retour en prospection`, 'success', '♻️');
                                                    } else {
                                                        showToast('Erreur lors de la réactivation', 'error', '❌');
                                                    }
                                                    setIsLoading(false);
                                                }}
                                                className="w-full flex items-center justify-center gap-2 py-2.5 bg-amber-500 text-white rounded-xl text-sm font-bold hover:bg-amber-600 transition-all shadow-sm shadow-amber-200 active:scale-[0.97]"
                                            >
                                                ♻️ Réactiver ce prospect
                                            </button>
                                        </div>
                                    )}
                                </div>

                                {/* ─── SCROLLABLE CONTENT ─── */}
                                <div className="flex-1 overflow-y-auto">

                                    {/* Pipeline Étapes — indicateur uniquement, pas de transition libre */}
                                    <div className="px-5 py-4 border-b border-slate-100">
                                        <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-2.5">Pipeline</h3>
                                        <div className="flex flex-wrap gap-1.5">
                                            {COLUMNS.map((status) => {
                                                const isActive = selectedProspect.status === status.id;
                                                // Determine passed steps based on logical flow
                                                const ORDER = ['NEW', 'CONTACTED', 'QUALIFIED', 'MEETING_BOOKED', 'SIGNED'];
                                                const currentIdx = ORDER.indexOf(selectedProspect.status);
                                                const statusIdx = ORDER.indexOf(status.id);
                                                const isPassed = currentIdx >= 0 && statusIdx >= 0 && statusIdx < currentIdx;
                                                return (
                                                    <div
                                                        key={status.id}
                                                        className={`px-3 py-1.5 rounded-lg text-xs font-bold border transition-all ${isActive
                                                            ? `${status.color} border-transparent shadow-sm`
                                                            : isPassed
                                                                ? 'bg-emerald-50 border-emerald-100 text-emerald-400'
                                                                : 'bg-white border-slate-100 text-slate-300'
                                                            }`}
                                                    >
                                                        {isPassed ? '✓' : status.icon} {status.label}
                                                    </div>
                                                );
                                            })}
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
                                                            onClick={() => setConfirmDialog({
                                                                title: 'Annuler le rendez-vous',
                                                                message: `Êtes-vous sûr de vouloir annuler le RDV de ${selectedProspect.firstName} ${selectedProspect.lastName} ?\n\nLe prospect sera déplacé dans la colonne "Perdu".`,
                                                                confirmLabel: 'Annuler le RDV',
                                                                type: 'danger',
                                                                onConfirm: async () => {
                                                                    await handleStatusChange(selectedProspect.id, 'LOST');
                                                                    showToast('RDV annulé — prospect marqué comme Perdu', 'warning', '🚫');
                                                                    setConfirmDialog(null);
                                                                }
                                                            })}
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
                                        {selectedProspect.status === 'CONTACTED' && (
                                            <button
                                                onClick={() => setShowBookingModal(true)}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-indigo-50 via-blue-50 to-indigo-50 border border-indigo-100 hover:border-indigo-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <Calendar className="text-indigo-600" size={20} />
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-slate-900 text-sm">Fixer un rendez-vous en agence</p>
                                                    <p className="text-xs text-slate-500">Prospect contacté — proposer un créneau</p>
                                                </div>
                                                <ArrowRight size={18} className="text-indigo-400 group-hover:translate-x-1 transition-transform" />
                                            </button>
                                        )}
                                        {selectedProspect.status === 'MEETING_BOOKED' && !selectedProspect.eligibilityResult && (
                                            <button
                                                onClick={() => setShowSimulatorModal(true)}
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
                                        {selectedProspect.status === 'MEETING_BOOKED' && selectedProspect.eligibilityResult && (
                                            <button
                                                onClick={() => handleStartPayment(selectedProspect, 1)}
                                                className="w-full flex items-center gap-4 p-4 rounded-2xl bg-gradient-to-r from-emerald-50 via-teal-50 to-emerald-50 border border-emerald-100 hover:border-emerald-200 transition-all group active:scale-[0.98]"
                                            >
                                                <div className="w-10 h-10 rounded-xl bg-white shadow-sm flex items-center justify-center group-hover:scale-110 transition-transform">
                                                    <span className="text-xl">💳</span>
                                                </div>
                                                <div className="flex-1 text-left">
                                                    <p className="font-bold text-emerald-900 text-sm">Procéder à l'encaissement</p>
                                                    <p className="text-xs text-emerald-600">Le simulateur a été validé. Encaisser pour signer le contrat.</p>
                                                </div>
                                                <ArrowRight size={18} className="text-emerald-500 group-hover:translate-x-1 transition-transform" />
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
                                        {selectedProspect.status === 'LOST' && (
                                            <div className="flex items-center gap-3 p-4 bg-slate-50 rounded-2xl border border-slate-200 text-slate-600">
                                                <span className="text-xl">⚫</span>
                                                <div>
                                                    <p className="font-bold text-sm">Prospect perdu</p>
                                                    <p className="text-xs text-slate-400">Ce prospect n'a pas donné suite. Vous pouvez le réactiver si nécessaire.</p>
                                                </div>
                                            </div>
                                        )}
                                    </div>

                                    {/* ─── ACTIONS RAPIDES D'ISSUE D'APPEL ─── */}
                                    {(selectedProspect.status === 'NEW' || selectedProspect.status === 'CONTACTED') && (
                                        <div className="px-5 py-4 border-b border-slate-100">
                                            <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em] mb-3">📞 Issue d'appel rapide</h3>

                                            {/* Call tracking badges */}
                                            {(selectedProspect.callAttempts || 0) > 0 && (
                                                <div className="flex flex-wrap gap-1.5 mb-3">
                                                    <span className="px-2.5 py-1 bg-slate-100 rounded-lg text-[11px] font-semibold text-slate-600">
                                                        📞 {selectedProspect.callAttempts} tentative{(selectedProspect.callAttempts || 0) > 1 ? 's' : ''}
                                                    </span>
                                                    {(selectedProspect.noAnswerCount || 0) > 0 && (
                                                        <span className={`px-2.5 py-1 rounded-lg text-[11px] font-semibold ${(selectedProspect.noAnswerCount || 0) >= 3 ? 'bg-red-100 text-red-700' : 'bg-amber-100 text-amber-700'}`}>
                                                            📵 {selectedProspect.noAnswerCount}/5 sans rép.
                                                        </span>
                                                    )}
                                                    {(selectedProspect.callbackCount || 0) > 0 && (
                                                        <span className={`px-2.5 py-1 rounded-lg text-[11px] font-semibold ${(selectedProspect.callbackCount || 0) >= 2 ? 'bg-red-100 text-red-700' : 'bg-blue-100 text-blue-700'}`}>
                                                            🔄 {selectedProspect.callbackCount}/3 rappels
                                                        </span>
                                                    )}
                                                    {selectedProspect.lastCallOutcome && (
                                                        <span className="px-2.5 py-1 bg-slate-50 rounded-lg text-[11px] text-slate-500">
                                                            Dernier : {
                                                                selectedProspect.lastCallOutcome === 'NO_ANSWER' ? 'Pas de réponse' :
                                                                    selectedProspect.lastCallOutcome === 'CALLBACK' ? 'À rappeler' :
                                                                        selectedProspect.lastCallOutcome === 'INTERESTED' ? 'Intéressé' :
                                                                            selectedProspect.lastCallOutcome
                                                            }
                                                        </span>
                                                    )}
                                                </div>
                                            )}

                                            {/* Quick outcome buttons */}
                                            <div className="mb-3 p-3 bg-blue-50/50 border border-blue-100 rounded-xl">
                                                <label className="text-[10px] font-bold text-blue-700 uppercase tracking-wider block mb-1.5">📅 Programmer le rappel</label>
                                                <input
                                                    type="datetime-local"
                                                    value={callbackSchedule}
                                                    onChange={(e) => setCallbackSchedule(e.target.value)}
                                                    className="w-full text-xs px-3 py-2 border border-blue-200 rounded-lg bg-white text-slate-700 focus:ring-2 focus:ring-blue-400 focus:border-blue-400"
                                                    min={new Date().toISOString().slice(0, 16)}
                                                />
                                            </div>
                                            <div className="grid grid-cols-2 gap-2">
                                                <button
                                                    onClick={() => handleQuickCallOutcome('NO_ANSWER')}
                                                    className="flex items-center gap-2 p-3 rounded-xl border border-amber-200 bg-amber-50 text-amber-800 text-xs font-bold hover:bg-amber-100 transition-all active:scale-[0.97]"
                                                >
                                                    📵 Pas de réponse
                                                </button>
                                                <button
                                                    onClick={() => handleQuickCallOutcome('CALLBACK')}
                                                    className="flex items-center gap-2 p-3 rounded-xl border border-blue-200 bg-blue-50 text-blue-800 text-xs font-bold hover:bg-blue-100 transition-all active:scale-[0.97]"
                                                >
                                                    🔄 À rappeler {callbackSchedule && <span className="text-[9px] opacity-70">({new Date(callbackSchedule).toLocaleDateString('fr-FR')})</span>}
                                                </button>
                                                <button
                                                    onClick={() => handleQuickCallOutcome('NOT_INTERESTED')}
                                                    className="flex items-center gap-2 p-3 rounded-xl border border-red-200 bg-red-50 text-red-800 text-xs font-bold hover:bg-red-100 transition-all active:scale-[0.97]"
                                                >
                                                    ❌ Pas intéressé
                                                </button>
                                                <button
                                                    onClick={() => handleQuickCallOutcome('WRONG_NUMBER')}
                                                    className="flex items-center gap-2 p-3 rounded-xl border border-slate-200 bg-slate-50 text-slate-700 text-xs font-bold hover:bg-slate-100 transition-all active:scale-[0.97]"
                                                >
                                                    ⚠️ Mauvais numéro
                                                </button>
                                            </div>
                                            {/* Full-width INTERESTED button */}
                                            <button
                                                onClick={() => handleQuickCallOutcome('INTERESTED')}
                                                className="w-full flex items-center justify-center gap-2 p-3 rounded-xl border-2 border-emerald-300 bg-emerald-50 text-emerald-800 text-sm font-black hover:bg-emerald-100 transition-all active:scale-[0.97]"
                                            >
                                                ✅ Intéressé — Qualifier le prospect
                                            </button>
                                            <p className="text-[10px] text-slate-400 mt-2 text-center">
                                                Marquer le résultat sans ouvrir le cockpit d'appel
                                            </p>
                                        </div>
                                    )}

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
                                        <div className="flex justify-between items-center mb-3">
                                            <h3 className="text-[10px] font-black text-slate-400 uppercase tracking-[0.2em]">Contexte Marketing</h3>
                                            {!isEditingMarketing ? (
                                                <button onClick={() => setIsEditingMarketing(true)} className="text-xs text-indigo-600 font-bold hover:text-indigo-800 transition-colors flex items-center gap-1">
                                                    Modifier
                                                </button>
                                            ) : (
                                                <div className="flex items-center gap-2">
                                                    <button onClick={handleSaveMarketing} className="text-xs px-2 py-1 bg-indigo-500 text-white rounded font-bold hover:bg-indigo-600 transition-colors">Enregistrer</button>
                                                    <button onClick={() => setIsEditingMarketing(false)} className="text-xs px-2 py-1 bg-slate-200 text-slate-700 rounded font-bold hover:bg-slate-300 transition-colors">Annuler</button>
                                                </div>
                                            )}
                                        </div>
                                        {isEditingMarketing ? (
                                            <div className="grid grid-cols-2 gap-3">
                                                <div className="bg-slate-50 rounded-xl p-3">
                                                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Source</p>
                                                    <select value={marketingForm.source} onChange={e => setMarketingForm(prev => ({ ...prev, source: e.target.value }))} className="w-full text-sm font-bold bg-white border border-slate-200 rounded px-2 py-1 focus:outline-none focus:ring-1 focus:ring-indigo-500">
                                                        <option value="WEBSITE">Site Web</option>
                                                        <option value="PHONE">Appel Entrant</option>
                                                        <option value="REFERRAL">Parrainage</option>
                                                        <option value="NETWORKING">Réseau / Événement</option>
                                                    </select>
                                                </div>
                                                <div className="bg-slate-50 rounded-xl p-3">
                                                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Campagne</p>
                                                    <input type="text" value={marketingForm.campaignName} onChange={e => setMarketingForm(prev => ({ ...prev, campaignName: e.target.value }))} className="w-full text-sm font-bold bg-white border border-slate-200 rounded px-2 py-1 focus:outline-none focus:ring-1 focus:ring-indigo-500" placeholder="Nom campagne" />
                                                </div>
                                                <div className="bg-slate-50 rounded-xl p-3 col-span-2">
                                                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Service d'intérêt</p>
                                                    <select value={marketingForm.interestServiceId} onChange={e => setMarketingForm(prev => ({ ...prev, interestServiceId: e.target.value }))} className="w-full text-sm font-bold bg-white border border-slate-200 rounded px-2 py-1 focus:outline-none focus:ring-1 focus:ring-indigo-500">
                                                        <option value="">Non spécifié</option>
                                                        {SERVICES_CATALOG.filter(s => s.id !== 'rappel_echeances').map(s => (
                                                            <option key={s.id} value={s.id}>{s.title}</option>
                                                        ))}
                                                    </select>
                                                </div>
                                            </div>
                                        ) : (
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
                                                    <p className="text-sm font-bold text-slate-900">
                                                        {selectedProspect.interestServiceId
                                                            ? (SERVICES_CATALOG.find(s => s.id === selectedProspect.interestServiceId)?.title || selectedProspect.interestServiceId)
                                                            : 'Non spécifié'}
                                                    </p>
                                                    {selectedProspect.interestServiceId && (
                                                        <ServicePriceDisplay serviceId={selectedProspect.interestServiceId} fetchPrice={fetchServicePrice} />
                                                    )}
                                                </div>
                                                <div className="bg-slate-50 rounded-xl p-3">
                                                    <p className="text-[10px] font-bold text-slate-400 uppercase tracking-wider mb-1">Créé le</p>
                                                    <p className="text-sm font-bold text-slate-900">{new Date(selectedProspect.createdAt).toLocaleDateString('fr-FR')}</p>
                                                </div>
                                            </div>
                                        )}
                                    </div>

                                    {/* ─── ACTIONS SECONDAIRES ─── */}
                                    <div className="px-5 py-3 border-b border-slate-100 flex flex-wrap gap-2">
                                        <button
                                            onClick={() => setShowFollowUpModal(true)}
                                            className="flex items-center gap-1.5 px-3 py-1.5 bg-indigo-50 text-indigo-700 rounded-lg text-xs font-bold border border-indigo-200 hover:bg-indigo-100 transition-all"
                                        >
                                            ⏰ Relance
                                        </button>
                                        <button
                                            onClick={() => setShowTagsModal(true)}
                                            className="flex items-center gap-1.5 px-3 py-1.5 bg-amber-50 text-amber-700 rounded-lg text-xs font-bold border border-amber-200 hover:bg-amber-100 transition-all"
                                        >
                                            🏷️ Tags
                                        </button>
                                        <button
                                            onClick={() => loadTimeline(selectedProspect.id)}
                                            className="flex items-center gap-1.5 px-3 py-1.5 bg-slate-50 text-slate-600 rounded-lg text-xs font-bold border border-slate-200 hover:bg-slate-100 transition-all"
                                        >
                                            📜 Historique
                                        </button>
                                        <button
                                            onClick={async () => {
                                                try {
                                                    await SalesStore.generateQuote(selectedProspect.id, {
                                                        serviceId: selectedProspect.interestServiceId,
                                                    });
                                                    showToast('Devis PDF téléchargé', 'success', '📄');
                                                } catch { showToast('Erreur lors de la génération du devis', 'error', '❌'); }
                                            }}
                                            className="flex items-center gap-1.5 px-3 py-1.5 bg-emerald-50 text-emerald-700 rounded-lg text-xs font-bold border border-emerald-200 hover:bg-emerald-100 transition-all"
                                        >
                                            📄 Devis PDF
                                        </button>
                                        {selectedProspect.status === 'MEETING_BOOKED' && (
                                            <button
                                                onClick={() => {
                                                    setConfirmDialog({
                                                        title: '❌ Annuler le RDV',
                                                        message: `Annuler le rendez-vous de ${selectedProspect.firstName} ${selectedProspect.lastName} ? Le prospect sera remis en statut Contacté/Qualifié.`,
                                                        confirmLabel: 'Annuler le RDV',
                                                        type: 'warning',
                                                        onConfirm: async () => {
                                                            await handleCancelAppointment(selectedProspect.id, 'Annulé par le commercial');
                                                            setConfirmDialog(null);
                                                        }
                                                    });
                                                }}
                                                className="flex items-center gap-1.5 px-3 py-1.5 bg-orange-50 text-orange-700 rounded-lg text-xs font-bold border border-orange-200 hover:bg-orange-100 transition-all"
                                            >
                                                ❌ Annuler RDV
                                            </button>
                                        )}
                                        {selectedProspect.status !== 'LOST' && selectedProspect.status !== 'SIGNED' && (
                                            <button
                                                onClick={() => handleMarkAsLost(selectedProspect.id)}
                                                className="flex items-center gap-1.5 px-3 py-1.5 bg-slate-100 text-slate-500 rounded-lg text-xs font-bold border border-slate-200 hover:bg-slate-200 transition-all"
                                            >
                                                ⚫ Perdu
                                            </button>
                                        )}
                                        <button
                                            onClick={() => handleDeleteProspect(selectedProspect)}
                                            className="flex items-center gap-1.5 px-3 py-1.5 bg-red-50 text-red-600 rounded-lg text-xs font-bold border border-red-200 hover:bg-red-100 transition-all ml-auto"
                                        >
                                            🗑️ Supprimer
                                        </button>
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
                                            <div className="mb-4">
                                                <textarea
                                                    value={newNote}
                                                    onChange={(e) => setNewNote(e.target.value)}
                                                    placeholder="Saisissez une nouvelle note ici..."
                                                    className="w-full text-sm bg-slate-50 border border-slate-200 rounded-xl px-3 py-2 min-h-[60px] resize-y focus:outline-none focus:ring-2 focus:ring-amber-500"
                                                />
                                                <div className="flex justify-end mt-2">
                                                    <button
                                                        onClick={async () => {
                                                            if (!newNote.trim()) return;
                                                            setIsLoading(true);
                                                            const addedNote = await SalesStore.addNote(selectedProspect.id, newNote.trim());
                                                            if (addedNote) {
                                                                const updatedNotes = [addedNote, ...(selectedProspect.notes || [])];
                                                                setSelectedProspect(prev => prev ? { ...prev, notes: updatedNotes } : null);
                                                                setProspects(prev => prev.map(p => p.id === selectedProspect.id ? { ...p, notes: updatedNotes } : p));
                                                            }
                                                            setNewNote('');
                                                            setIsLoading(false);
                                                        }}
                                                        disabled={!newNote.trim() || isLoading}
                                                        className="px-3 py-1.5 bg-amber-500 text-white font-bold text-xs rounded-lg hover:bg-amber-600 transition-colors disabled:opacity-50"
                                                    >
                                                        Ajouter la note
                                                    </button>
                                                </div>
                                            </div>
                                            {(!selectedProspect.notes || selectedProspect.notes.length === 0) ? (
                                                <p className="text-sm text-slate-400 italic py-2">Aucune note enregistrée.</p>
                                            ) : (
                                                <div className="space-y-3 max-h-56 overflow-y-auto pr-1">
                                                    {selectedProspect.notes.map((note: ProspectNote) => {
                                                        const isAgentNote = note.authorId?.includes('🤖');

                                                        return (
                                                            <div
                                                                key={note.id}
                                                                className={`p-4 rounded-xl relative group border-2 ${isAgentNote
                                                                    ? 'bg-amber-50/80 border-amber-200'
                                                                    : 'bg-slate-50 border-slate-100'
                                                                    }`}
                                                            >
                                                                {isAgentNote && (
                                                                    <div className="absolute -top-2.5 right-4 bg-amber-400 text-amber-900 px-2 py-0.5 rounded-full text-[9px] font-black tracking-widest uppercase shadow-sm flex items-center gap-1">
                                                                        <span>Agent QA</span>
                                                                    </div>
                                                                )}
                                                                <p className={`text-sm whitespace-pre-wrap ${isAgentNote ? 'text-amber-900 font-semibold text-xs' : 'text-slate-700'
                                                                    }`}>
                                                                    {note.text}
                                                                </p>
                                                                <div className="flex justify-between items-center mt-3 text-xs">
                                                                    <span className={`font-black uppercase tracking-wider ${isAgentNote ? 'text-amber-600 text-[10px]' : 'text-indigo-400 text-[10px]'
                                                                        }`}>
                                                                        {isAgentNote ? 'IA Supervision' : `👤 ${note.authorId}`}
                                                                    </span>
                                                                    <span className={`font-mono flex items-center gap-1 ${isAgentNote ? 'text-amber-500 font-bold text-[9px]' : 'text-slate-400 text-[10px]'
                                                                        }`}>
                                                                        {new Date(note.createdAt || '').toLocaleDateString('fr-FR')} à {new Date(note.createdAt || '').toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}
                                                                    </span>
                                                                </div>
                                                            </div>
                                                        );
                                                    })}
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
                            <div className="bg-white p-6 rounded-2xl shadow-xl max-w-md w-full max-h-[90vh] flex flex-col animate-in slide-in-from-bottom-4 duration-200">
                                <div className="flex justify-between items-center mb-6 flex-shrink-0">
                                    <h2 className="text-xl font-bold flex items-center gap-2">
                                        <Plus className="text-indigo-600" /> Ajouter un prospect
                                    </h2>
                                    <button onClick={() => setShowAddModal(false)} className="text-slate-400 hover:text-slate-600"><XCircle size={20} /></button>
                                </div>
                                <div className="space-y-4 overflow-y-auto flex-1 pr-2">
                                    <div className="grid grid-cols-2 gap-4">
                                        <div>
                                            <label className="block text-xs font-bold text-slate-500 mb-1">Prénom <span className="text-red-500">*</span></label>
                                            <input type="text" required value={addForm.firstName} onChange={e => setAddForm({ ...addForm, firstName: e.target.value })} className={`w-full px-4 py-2 bg-slate-50 border rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 ${!addForm.firstName ? 'border-red-300' : 'border-slate-200'}`} placeholder="Prénom" autoFocus />
                                        </div>
                                        <div>
                                            <label className="block text-xs font-bold text-slate-500 mb-1">Nom <span className="text-red-500">*</span></label>
                                            <input type="text" required value={addForm.lastName} onChange={e => setAddForm({ ...addForm, lastName: e.target.value })} className={`w-full px-4 py-2 bg-slate-50 border rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 ${!addForm.lastName ? 'border-red-300' : 'border-slate-200'}`} placeholder="Nom" />
                                        </div>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Téléphone <span className="text-red-500">*</span></label>
                                        <input type="tel" required value={addForm.phone} onChange={e => setAddForm({ ...addForm, phone: e.target.value })} className={`w-full px-4 py-2 bg-slate-50 border rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 ${!addForm.phone ? 'border-red-300' : 'border-slate-200'}`} placeholder="06 XX XX XX XX" />
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

                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Service d'intérêt <span className="text-red-500">*</span></label>
                                        <select value={addForm.interestServiceId} onChange={e => setAddForm({ ...addForm, interestServiceId: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500">
                                            <option value="" disabled>-- Sélectionnez un service (Obligatoire) --</option>
                                            {SERVICES_CATALOG.filter(s => s.id !== 'rappel_echeances').map(s => (
                                                <option key={s.id} value={s.id}>{s.title}</option>
                                            ))}
                                        </select>
                                    </div>

                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Agence de rattachement</label>
                                        <select value={addForm.agencyId} onChange={e => setAddForm({ ...addForm, agencyId: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500">
                                            {agencies.map(a => (
                                                <option key={a.id} value={a.id}>{a.name} ({a.id})</option>
                                            ))}
                                            {agencies.length === 0 && <option value="HQ-001">HQ-001 (Par défaut)</option>}
                                        </select>
                                    </div>

                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Assigné à (Agent / Juriste) (Optionnel)</label>
                                        <select value={addForm.assignedToSalesId} onChange={e => setAddForm({ ...addForm, assignedToSalesId: e.target.value })} className="w-full px-4 py-2 bg-slate-50 border border-slate-200 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500">
                                            <option value="">-- Non assigné --</option>
                                            {users.map(u => (
                                                <option key={u.id} value={u.id}>{u.name} ({u.role})</option>
                                            ))}
                                        </select>
                                    </div>
                                </div>
                                <div className="mt-8 flex justify-end gap-3 flex-shrink-0">
                                    <button onClick={() => setShowAddModal(false)} className="px-4 py-2 text-slate-600 hover:bg-slate-100 rounded-lg font-medium transition-colors">Annuler</button>
                                    <button
                                        onClick={() => {
                                            if (!addForm.firstName.trim() || !addForm.lastName.trim() || !addForm.phone.trim() || !addForm.interestServiceId) {
                                                return; // Guard against invalid data
                                            }
                                            SalesStore.addProspect({
                                                firstName: addForm.firstName.trim(),
                                                lastName: addForm.lastName.trim(),
                                                phone: addForm.phone.trim(),
                                                email: addForm.email.trim() || undefined,
                                                address: addForm.address.trim() || undefined,
                                                city: addForm.city.trim() || undefined,
                                                zipCode: addForm.zipCode.trim() || undefined,
                                                country: addForm.country || 'France',
                                                interestServiceId: addForm.interestServiceId,
                                                source: addForm.source as any,
                                                agencyId: addForm.agencyId || 'HQ-001',
                                                assignedToSalesId: addForm.assignedToSalesId || undefined,
                                                score: 0
                                            }).then((newProspect) => {
                                                loadProspects();
                                                setShowAddModal(false);
                                                setAddForm({ firstName: '', lastName: '', phone: '', email: '', source: 'WEBSITE', address: '', city: '', zipCode: '', country: 'France', interestServiceId: '', agencyId: currentUserInfo?.agencyId || 'HQ-001', assignedToSalesId: '' });
                                                if (newProspect) setSelectedProspect(newProspect);
                                                showToast('Prospect créé avec succès !', 'success', '✅');
                                            });
                                        }}
                                        disabled={!addForm.firstName.trim() || !addForm.lastName.trim() || !addForm.phone.trim() || !addForm.interestServiceId}
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
                            onBookAppointment={(appointment) => handleBookAppointment(selectedProspect, appointment)}
                        />
                    )}

                    {/* Modal Simulateur */}
                    {showSimulatorModal && selectedProspect && (
                        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
                            <div
                                className="absolute inset-0 bg-slate-900/40 backdrop-blur-sm"
                                onClick={() => setShowSimulatorModal(false)}
                            />
                            <div className="relative bg-transparent w-full max-w-5xl max-h-[90vh] overflow-y-auto rounded-[3rem] shadow-2xl animate-in zoom-in-95 duration-200">
                                <button
                                    onClick={() => setShowSimulatorModal(false)}
                                    className="absolute top-6 right-6 z-10 w-10 h-10 bg-white/50 hover:bg-white text-slate-800 rounded-full flex items-center justify-center transition-all shadow-sm"
                                >
                                    <X size={20} />
                                </button>
                                <SimulatorWrapper
                                    serviceId={selectedProspect.interestServiceId || ''}
                                    prospectId={selectedProspect.id}
                                    onComplete={() => {
                                        setShowSimulatorModal(false);
                                        loadProspects();
                                        showToast('Simulation terminée et dossier mis à jour.', 'success');
                                    }}
                                />
                            </div>
                        </div>
                    )}
                </>
            )}

            {/* ─── TOAST NOTIFICATION ─── */}
            {toast && (
                <div className={`fixed bottom-6 right-6 z-[100] max-w-sm animate-in slide-in-from-bottom-4 duration-300`}>
                    <div className={`flex items-start gap-3 px-5 py-4 rounded-2xl shadow-2xl border backdrop-blur-sm ${toast.type === 'success' ? 'bg-emerald-50 border-emerald-200 text-emerald-800' :
                        toast.type === 'error' ? 'bg-red-50 border-red-200 text-red-800' :
                            toast.type === 'warning' ? 'bg-amber-50 border-amber-200 text-amber-800' :
                                'bg-indigo-50 border-indigo-200 text-indigo-800'
                        }`}>
                        {toast.icon && <span className="text-lg flex-shrink-0 mt-0.5">{toast.icon}</span>}
                        <p className="text-sm font-semibold leading-snug flex-1">{toast.message}</p>
                        <button
                            onClick={() => setToast(null)}
                            className="flex-shrink-0 p-0.5 hover:bg-black/5 rounded-full transition-colors"
                        >
                            <X size={14} />
                        </button>
                    </div>
                    {/* Auto-dismiss progress bar */}
                    <div className="mt-1 mx-4 h-0.5 rounded-full overflow-hidden bg-black/5">
                        <div
                            className={`h-full rounded-full ${toast.type === 'success' ? 'bg-emerald-400' :
                                toast.type === 'error' ? 'bg-red-400' :
                                    toast.type === 'warning' ? 'bg-amber-400' :
                                        'bg-indigo-400'
                                }`}
                            style={{ animation: 'toast-shrink 3.5s linear forwards', width: '100%' }}
                        />
                    </div>
                    <style jsx>{`
                        @keyframes toast-shrink {
                            from { width: 100%; }
                            to { width: 0%; }
                        }
                    `}</style>
                </div>
            )}

            {/* ─── MODAL : Raison de perte ─── */}
            {showLostReasonModal && (
                <div className="fixed inset-0 z-[90] flex items-center justify-center p-4">
                    <div className="absolute inset-0 bg-black/30 backdrop-blur-sm" onClick={() => setShowLostReasonModal(false)} />
                    <div className="relative w-[440px] bg-white rounded-3xl shadow-2xl p-8 animate-in zoom-in-95">
                        <h3 className="text-lg font-black text-slate-900 mb-4">⚫ Raison de la perte</h3>
                        <p className="text-sm text-slate-500 mb-4">Pourquoi ce prospect est-il perdu ?</p>

                        <div className="flex flex-wrap gap-2 mb-4">
                            {['Trop cher', 'Injoignable', 'Concurrent', 'Non éligible', 'Pas intéressé', 'Délai trop long', 'Mauvais timing', 'Doublon'].map(reason => (
                                <button
                                    key={reason}
                                    onClick={() => setLostReason(reason)}
                                    className={`px-3 py-1.5 rounded-lg text-sm font-medium border transition-all ${lostReason === reason
                                        ? 'bg-slate-900 text-white border-slate-900'
                                        : 'bg-white text-slate-600 border-slate-200 hover:bg-slate-50'
                                        }`}
                                >
                                    {reason}
                                </button>
                            ))}
                        </div>

                        <input
                            type="text"
                            value={lostReason}
                            onChange={(e) => setLostReason(e.target.value)}
                            placeholder="Ou saisir une raison personnalisée..."
                            className="w-full px-4 py-2.5 bg-slate-50 border border-slate-200 rounded-xl text-sm mb-6 focus:outline-none focus:ring-2 focus:ring-slate-200"
                        />

                        <div className="flex gap-3">
                            <button
                                onClick={() => setShowLostReasonModal(false)}
                                className="flex-1 py-3 rounded-xl border border-slate-200 text-slate-600 font-bold text-sm hover:bg-slate-50"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={confirmLostReason}
                                className="flex-1 py-3 rounded-xl bg-slate-800 text-white font-bold text-sm hover:bg-slate-900 transition-all"
                            >
                                Confirmer la perte
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ─── MODAL : Relance programmée ─── */}
            {showFollowUpModal && selectedProspect && (
                <div className="fixed inset-0 z-[90] flex items-center justify-center p-4">
                    <div className="absolute inset-0 bg-black/30 backdrop-blur-sm" onClick={() => setShowFollowUpModal(false)} />
                    <div className="relative w-[440px] bg-white rounded-3xl shadow-2xl p-8 animate-in zoom-in-95">
                        <h3 className="text-lg font-black text-slate-900 mb-2">⏰ Programmer une relance</h3>
                        <p className="text-sm text-slate-500 mb-4">
                            Relancer <strong>{selectedProspect.firstName} {selectedProspect.lastName}</strong>
                        </p>

                        {/* Quick picks */}
                        <div className="flex flex-wrap gap-2 mb-4">
                            {[
                                { label: 'Dans 2h', hours: 2 },
                                { label: 'Demain 9h', hours: 0, tomorrow9: true },
                                { label: 'Dans 3 jours', days: 3 },
                                { label: 'Dans 1 semaine', days: 7 },
                            ].map(pick => {
                                const getDate = () => {
                                    const d = new Date();
                                    if ('tomorrow9' in pick && pick.tomorrow9) {
                                        d.setDate(d.getDate() + 1);
                                        d.setHours(9, 0, 0, 0);
                                    } else if ('days' in pick && pick.days) {
                                        d.setDate(d.getDate() + pick.days);
                                        d.setHours(9, 0, 0, 0);
                                    } else if ('hours' in pick) {
                                        d.setHours(d.getHours() + (pick.hours || 0));
                                    }
                                    return d.toISOString().slice(0, 16);
                                };
                                return (
                                    <button
                                        key={pick.label}
                                        onClick={() => setFollowUpDate(getDate())}
                                        className="px-3 py-1.5 rounded-lg text-sm font-medium bg-indigo-50 text-indigo-700 border border-indigo-200 hover:bg-indigo-100 transition-all"
                                    >
                                        {pick.label}
                                    </button>
                                );
                            })}
                        </div>

                        <label className="block text-xs font-bold text-slate-600 mb-1">Date et heure</label>
                        <input
                            type="datetime-local"
                            value={followUpDate}
                            onChange={(e) => setFollowUpDate(e.target.value)}
                            className="w-full px-4 py-2.5 bg-slate-50 border border-slate-200 rounded-xl text-sm mb-4 focus:outline-none focus:ring-2 focus:ring-indigo-200"
                        />

                        <label className="block text-xs font-bold text-slate-600 mb-1">Raison (optionnel)</label>
                        <input
                            type="text"
                            value={followUpReason}
                            onChange={(e) => setFollowUpReason(e.target.value)}
                            placeholder="Ex: Rappeler pour devis, vérifier documents..."
                            className="w-full px-4 py-2.5 bg-slate-50 border border-slate-200 rounded-xl text-sm mb-6 focus:outline-none focus:ring-2 focus:ring-indigo-200"
                        />

                        <div className="flex gap-3">
                            <button
                                onClick={() => setShowFollowUpModal(false)}
                                className="flex-1 py-3 rounded-xl border border-slate-200 text-slate-600 font-bold text-sm hover:bg-slate-50"
                            >
                                Annuler
                            </button>
                            <button
                                onClick={handleScheduleFollowUp}
                                disabled={!followUpDate}
                                className="flex-1 py-3 rounded-xl bg-indigo-600 text-white font-bold text-sm hover:bg-indigo-700 transition-all disabled:opacity-50"
                            >
                                Programmer
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ─── MODAL : Tags ─── */}
            {showTagsModal && selectedProspect && (
                <div className="fixed inset-0 z-[90] flex items-center justify-center p-4">
                    <div className="absolute inset-0 bg-black/30 backdrop-blur-sm" onClick={() => setShowTagsModal(false)} />
                    <div className="relative w-[440px] bg-white rounded-3xl shadow-2xl p-8 animate-in zoom-in-95">
                        <h3 className="text-lg font-black text-slate-900 mb-2">🏷️ Tags</h3>
                        <p className="text-sm text-slate-500 mb-4">
                            Gérer les tags de <strong>{selectedProspect.firstName} {selectedProspect.lastName}</strong>
                        </p>

                        <div className="flex flex-wrap gap-2">
                            {AVAILABLE_TAGS.map(tag => {
                                const currentTags: string[] = selectedProspect.tags ? JSON.parse(selectedProspect.tags) : [];
                                const isActive = currentTags.includes(tag);
                                return (
                                    <button
                                        key={tag}
                                        onClick={() => handleToggleTag(selectedProspect, tag)}
                                        className={`px-4 py-2 rounded-xl text-sm font-bold border-2 transition-all ${isActive
                                            ? 'bg-amber-100 text-amber-800 border-amber-300 shadow-sm'
                                            : 'bg-white text-slate-500 border-slate-200 hover:bg-slate-50'
                                            }`}
                                    >
                                        {isActive ? '✓ ' : ''}{tag}
                                    </button>
                                );
                            })}
                        </div>

                        <button
                            onClick={() => setShowTagsModal(false)}
                            className="w-full mt-6 py-3 rounded-xl bg-slate-100 text-slate-700 font-bold text-sm hover:bg-slate-200 transition-all"
                        >
                            Fermer
                        </button>
                    </div>
                </div>
            )}

            {/* ─── MODAL : Timeline / Historique Communications ─── */}
            {showTimeline && selectedProspect && (
                <div className="fixed inset-0 z-[90] flex items-center justify-center p-4">
                    <div className="absolute inset-0 bg-black/30 backdrop-blur-sm" onClick={() => setShowTimeline(false)} />
                    <div className="relative w-[520px] max-h-[80vh] bg-white rounded-3xl shadow-2xl overflow-hidden animate-in zoom-in-95 flex flex-col">
                        <div className="p-6 border-b border-slate-100 flex-shrink-0">
                            <h3 className="text-lg font-black text-slate-900">📜 Historique complet</h3>
                            <p className="text-sm text-slate-500">{selectedProspect.firstName} {selectedProspect.lastName}</p>
                        </div>
                        <div className="flex-1 overflow-y-auto p-6">
                            {timeline.length === 0 ? (
                                <p className="text-sm text-slate-400 text-center py-8">Aucune communication enregistrée</p>
                            ) : (
                                <div className="space-y-3">
                                    {timeline.map((event: any, i: number) => {
                                        const typeConfig: any = {
                                            'EMAIL': { icon: '📧', color: 'bg-blue-50 border-blue-200 text-blue-700' },
                                            'WHATSAPP': { icon: '💬', color: 'bg-green-50 border-green-200 text-green-700' },
                                            'SMS': { icon: '📱', color: 'bg-purple-50 border-purple-200 text-purple-700' },
                                            'CALL': { icon: '📞', color: 'bg-orange-50 border-orange-200 text-orange-700' },
                                            'NOTE': { icon: '📝', color: 'bg-slate-50 border-slate-200 text-slate-700' },
                                        };
                                        const cfg = typeConfig[event.type] || typeConfig.NOTE;
                                        return (
                                            <div key={i} className={`p-3 rounded-xl border text-sm ${cfg.color}`}>
                                                <div className="flex items-center justify-between mb-1">
                                                    <span className="font-bold">{cfg.icon} {event.type} {event.direction === 'INBOUND' ? '→' : '←'}</span>
                                                    <span className="text-[10px] opacity-70">
                                                        {new Date(event.date).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })}
                                                    </span>
                                                </div>
                                                <p className="text-xs opacity-80 line-clamp-2">{event.content}</p>
                                                {event.duration && <span className="text-[10px] font-bold">Durée: {event.duration}s</span>}
                                            </div>
                                        );
                                    })}
                                </div>
                            )}
                        </div>
                        <div className="p-4 border-t border-slate-100 flex-shrink-0">
                            <button
                                onClick={() => setShowTimeline(false)}
                                className="w-full py-3 rounded-xl bg-slate-100 text-slate-700 font-bold text-sm hover:bg-slate-200"
                            >
                                Fermer
                            </button>
                        </div>
                    </div>
                </div>
            )}

            {/* ─── CONFIRMATION DIALOG ─── */}
            {confirmDialog && (
                <div className="fixed inset-0 z-[90] flex items-center justify-center p-4">
                    <div className="absolute inset-0 bg-black/30 backdrop-blur-sm" onClick={() => setConfirmDialog(null)} />
                    <div className="relative w-[420px] max-w-full max-h-full flex flex-col bg-white rounded-3xl shadow-2xl overflow-hidden animate-in zoom-in-95 duration-200">
                        {/* Icon */}
                        <div className="pt-8 pb-4 flex justify-center flex-shrink-0">
                            <div className={`w-16 h-16 rounded-2xl flex items-center justify-center ${confirmDialog.type === 'danger' ? 'bg-red-100' :
                                confirmDialog.type === 'warning' ? 'bg-amber-100' :
                                    'bg-indigo-100'
                                }`}>
                                {confirmDialog.type === 'danger' ? (
                                    <AlertTriangle size={28} className="text-red-600" />
                                ) : confirmDialog.type === 'warning' ? (
                                    <AlertTriangle size={28} className="text-amber-600" />
                                ) : (
                                    <Info size={28} className="text-indigo-600" />
                                )}
                            </div>
                        </div>

                        {/* Content */}
                        <div className="px-8 pb-6 text-center overflow-y-auto">
                            <h3 className="text-lg font-black text-slate-900 mb-2">{confirmDialog.title}</h3>
                            <div className="text-sm text-slate-500 leading-relaxed">
                                {confirmDialog.message.split('\n').map((line, i) => (
                                    <p key={i} className={line === '' ? 'h-2' : ''}>{line}</p>
                                ))}
                            </div>
                        </div>

                        {/* Actions */}
                        <div className="px-8 pb-8 flex gap-3 flex-shrink-0">
                            <button
                                onClick={() => setConfirmDialog(null)}
                                className="flex-1 py-3 rounded-xl border border-slate-200 text-slate-600 font-bold text-sm hover:bg-slate-50 transition-all"
                            >
                                {confirmDialog.cancelLabel || 'Annuler'}
                            </button>
                            <button
                                onClick={confirmDialog.onConfirm}
                                className={`flex-1 py-3 rounded-xl text-white font-bold text-sm transition-all shadow-sm active:scale-[0.97] ${confirmDialog.type === 'danger'
                                    ? 'bg-red-600 hover:bg-red-700 shadow-red-200'
                                    : confirmDialog.type === 'warning'
                                        ? 'bg-amber-600 hover:bg-amber-700 shadow-amber-200'
                                        : 'bg-indigo-600 hover:bg-indigo-700 shadow-indigo-200'
                                    }`}
                            >
                                {confirmDialog.confirmLabel}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
