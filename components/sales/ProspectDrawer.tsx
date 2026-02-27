import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import {
    XCircle, Phone, Mail, MapPin, CheckCircle, Calendar, Microscope, Flame, ArrowRight, Clock
} from 'lucide-react';
import { SalesStore } from '../../services/SalesStore';
import { Prospect, ProspectNote } from '../../services/SalesStore';
import { WhatsAppWidget } from '../backoffice/WhatsAppWidget';
import { SERVICES_CATALOG } from '../../data/services';
import { PaymentButtons, ServicePriceDisplay } from './SalesShared';

export default function ProspectDrawer({
    selectedProspect,
    setSelectedProspect,
    setProspects,
    loadProspects,
    COLUMNS,
    setShowCallCockpit,
    setShowBookingModal,
    setShowSimulatorModal,
    setShowFollowUpModal,
    setShowTagsModal,
    handleStatusChange,
    handleNoShow,
    handleStartPayment,
    fetchServicePrice,
    handleCancelAppointment,
    handleMarkAsLost,
    handleDeleteProspect,
    loadTimeline,
    setConfirmDialog,
    showToast,
    callHistory,
}: any) {
    const router = useRouter();
    const [isEditingInfo, setIsEditingInfo] = useState(false);
    const [editForm, setEditForm] = useState({ firstName: '', lastName: '', phone: '', email: '', address: '', city: '', zipCode: '', country: 'France' });
    const [isEditingMarketing, setIsEditingMarketing] = useState(false);
    const [marketingForm, setMarketingForm] = useState({ source: 'WEBSITE', campaignName: '', interestServiceId: '' });
    const [isLoading, setIsLoading] = useState(false);
    const [newNote, setNewNote] = useState('');
    const [callbackSchedule, setCallbackSchedule] = useState('');

    useEffect(() => {
        if (selectedProspect) {
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
        }
    }, [selectedProspect?.id]);

    const handleSaveInfo = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, editForm);
        setSelectedProspect((prev: any) => prev ? { ...prev, ...editForm } : null);
        setProspects((prev: any) => prev.map((p: any) => p.id === selectedProspect.id ? { ...p, ...editForm } : p));
        setIsEditingInfo(false);
        setIsLoading(false);
    };

    const handleSaveMarketing = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, marketingForm as any);
        setSelectedProspect((prev: any) => prev ? { ...prev, ...marketingForm } as any : null);
        setProspects((prev: any) => prev.map((p: any) => p.id === selectedProspect.id ? { ...p, ...marketingForm } as any : p));
        setIsEditingMarketing(false);
        setIsLoading(false);
    };

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
        if (autoLostReason) noteText += `
⚠️ Auto-classé PERDU : ${autoLostReason}`;

        await SalesStore.addNote(selectedProspect.id, noteText);

        if (autoLostReason) {
            showToast(`Prospect automatiquement classé PERDU — ${autoLostReason}`, 'warning', '⚠️');
        } else {
            const outcomeEmoji: Record<string, string> = { 'NO_ANSWER': '📵', 'CALLBACK': '🔄', 'NOT_INTERESTED': '❌', 'WRONG_NUMBER': '⚠️', 'INTERESTED': '✅' };
            showToast(`${outcomeLabels[outcome]} enregistré (tentative #${newCallAttempts})`, 'info', outcomeEmoji[outcome] || '📞');
        }

        loadProspects();
    };

    return (
        <>

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
                                        <span className={`px-3 py-1 rounded-full text-xs font-bold ${COLUMNS.find((c: any) => c.id === selectedProspect.status)?.color || 'bg-slate-100 text-slate-600'}`}>
                                            {COLUMNS.find((c: any) => c.id === selectedProspect.status)?.label || selectedProspect.status}
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
                                    {COLUMNS.map((status: any) => {
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
                                                        setSelectedProspect((prev: any) => prev ? { ...prev, notes: updatedNotes } : null);
                                                        setProspects((prev: any) => prev.map((p: any) => p.id === selectedProspect.id ? { ...p, notes: updatedNotes } : p));
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
        </>
    );
}
