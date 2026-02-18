'use client';

import React, { useState, useEffect } from 'react';
import { CRM, Lead, LeadNote } from '../../services/crmStore';
import {
    X,
    User,
    Calendar,
    CreditCard,
    FileText,
    MessageSquare,
    Send,
    Clock,
    CheckCircle,
    Building2,
    Lock,
    ShieldCheck,
    Download,
    DollarSign,
    Plus
} from 'lucide-react';
import { BillingStore } from '../../services/BillingStore';

interface FranchiseCaseDetailProps {
    leadId: string;
    isOpen: boolean;
    onClose: () => void;
    agencyName?: string;
}

const STATUS_CONFIG = {
    NEW: { label: 'Nouveau', color: 'bg-slate-100 text-slate-700', icon: <Clock size={16} /> },
    PAID: { label: 'Payé - En attente', color: 'bg-blue-100 text-blue-700', icon: <Clock size={16} /> },
    PROCESSING: { label: 'En constitution', color: 'bg-amber-100 text-amber-700', icon: <FileText size={16} /> },
    SUBMITTED: { label: 'En instruction Préfecture', color: 'bg-purple-100 text-purple-700', icon: <Building2 size={16} /> },
    COMPLETED: { label: 'Terminé', color: 'bg-emerald-100 text-emerald-700', icon: <CheckCircle size={16} /> },
};

export default function FranchiseCaseDetail({
    leadId,
    isOpen,
    onClose,
    agencyName = 'Mon Agence'
}: FranchiseCaseDetailProps) {
    const [lead, setLead] = useState<Lead | null>(null);
    const [newNote, setNewNote] = useState('');
    const [isSending, setIsSending] = useState(false);
    const [showPaymentForm, setShowPaymentForm] = useState(false);
    const [paymentData, setPaymentData] = useState({ amount: 0, method: 'CARD', ref: '' });

    useEffect(() => {
        if (isOpen && leadId) {
            const loadLead = async () => {
                const foundLead = await CRM.getLeadById(leadId);
                setLead(foundLead);
            };
            loadLead();
        }
    }, [isOpen, leadId]);

    if (!isOpen || !lead) return null;

    const statusConfig = STATUS_CONFIG[lead.status as keyof typeof STATUS_CONFIG] || STATUS_CONFIG.NEW;

    const handleRecordPayment = async () => {
        if (paymentData.amount <= 0) return;
        const updated = await BillingStore.recordPayment(leadId, paymentData.amount * 100, paymentData.method, paymentData.ref);
        if (updated) {
            setLead({ ...lead, ...updated });
            setShowPaymentForm(false);
            setPaymentData({ amount: 0, method: 'CARD', ref: '' });
        }
    };

    const handleDownloadInvoice = () => {
        BillingStore.downloadInvoicePdf(leadId);
    };

    const handleSendNote = async () => {
        if (!newNote.trim()) return;

        setIsSending(true);
        const updatedLead = await CRM.addNote(leadId, {
            author: 'AGENCY',
            authorName: agencyName,
            content: newNote.trim()
        });

        if (updatedLead) {
            setLead(updatedLead);
        }
        setNewNote('');
        setIsSending(false);
    };

    return (
        <div className="fixed inset-0 bg-black/50 backdrop-blur-sm flex items-center justify-center z-50 p-4 animate-in fade-in duration-200">
            <div className="bg-white rounded-2xl shadow-2xl w-full max-w-4xl max-h-[90vh] overflow-hidden flex flex-col animate-in zoom-in-95 duration-200">
                {/* Header */}
                <div className="bg-slate-900 text-white p-6 flex items-center justify-between">
                    <div className="flex items-center gap-4">
                        <div className="w-14 h-14 bg-indigo-600 rounded-xl flex items-center justify-center text-2xl font-black">
                            {lead.name.charAt(0).toUpperCase()}
                        </div>
                        <div>
                            <h2 className="text-xl font-black">{lead.name}</h2>
                            <p className="text-slate-400 text-sm font-mono">{lead.id}</p>
                        </div>
                    </div>
                    <div className="flex items-center gap-4">
                        {/* Statut Gros Badge */}
                        <div className={`flex items-center gap-2 px-4 py-2 rounded-xl font-bold ${statusConfig.color}`}>
                            {statusConfig.icon}
                            {statusConfig.label}
                            <Lock size={12} className="opacity-50 ml-1" />
                        </div>
                        <button
                            onClick={onClose}
                            className="w-10 h-10 bg-white/10 rounded-full flex items-center justify-center hover:bg-white/20 transition-colors"
                        >
                            <X size={20} />
                        </button>
                    </div>
                </div>

                {/* Avertissement Lecture Seule */}
                <div className="bg-amber-50 border-b border-amber-200 px-6 py-2 flex items-center justify-center gap-2 text-amber-700 text-xs font-bold">
                    <ShieldCheck size={14} />
                    Vue en lecture seule - Le statut est géré par le siège
                </div>

                {/* Corps - 2 Colonnes */}
                <div className="flex-1 overflow-hidden flex">
                    {/* Colonne Gauche - Infos Clés */}
                    <div className="w-1/2 p-6 border-r border-slate-100 overflow-y-auto">
                        <h3 className="font-bold text-slate-900 mb-4 flex items-center gap-2">
                            <FileText size={18} className="text-indigo-600" />
                            Informations du dossier
                        </h3>

                        <div className="space-y-4">
                            {/* Service */}
                            <div className="bg-slate-50 rounded-xl p-4">
                                <p className="text-[10px] text-slate-400 uppercase font-bold mb-1">Service commandé</p>
                                <p className="font-bold text-slate-900">{lead.serviceName || lead.serviceId}</p>
                            </div>

                            {/* Infos Client */}
                            <div className="grid grid-cols-2 gap-3">
                                <div className="bg-slate-50 rounded-xl p-4">
                                    <p className="text-[10px] text-slate-400 uppercase font-bold mb-1">Email</p>
                                    <p className="font-medium text-slate-700 text-sm truncate">{lead.email}</p>
                                </div>
                                <div className="bg-slate-50 rounded-xl p-4">
                                    <p className="text-[10px] text-slate-400 uppercase font-bold mb-1">Téléphone</p>
                                    <p className="font-medium text-slate-700 text-sm">{lead.phone}</p>
                                </div>
                            </div>

                            {/* Date et Montant */}
                            <div className="grid grid-cols-2 gap-3">
                                <div className="bg-slate-50 rounded-xl p-4 flex items-center gap-3">
                                    <Calendar className="text-slate-400" size={20} />
                                    <div>
                                        <p className="text-[10px] text-slate-400 uppercase font-bold">Date commande</p>
                                        <p className="font-bold text-slate-900">
                                            {new Date(lead.createdAt).toLocaleDateString('fr-FR')}
                                        </p>
                                    </div>
                                </div>
                                <div className="bg-emerald-50 rounded-xl p-4 flex items-center gap-3">
                                    <CreditCard className="text-emerald-600" size={20} />
                                    <div>
                                        <p className="text-[10px] text-emerald-600 uppercase font-bold">Montant payé</p>
                                        <p className="font-black text-emerald-700 text-lg">
                                            {(lead.amountPaid / 100).toFixed(0)} €
                                        </p>
                                    </div>
                                </div>
                            </div>

                            {/* Signature */}
                            {lead.contract && (
                                <div className="bg-indigo-50 border border-indigo-200 rounded-xl p-4">
                                    <div className="flex items-center gap-2 mb-2">
                                        <ShieldCheck size={16} className="text-indigo-600" />
                                        <p className="font-bold text-indigo-800 text-sm">Signature électronique</p>
                                    </div>
                                    <div className="text-xs text-indigo-700 space-y-1">
                                        <p>Signé le : {lead.contract && typeof lead.contract === 'object' && 'signedAt' in lead.contract ? new Date((lead.contract as any).signedAt).toLocaleString('fr-FR') : ''}</p>
                                    </div>
                                </div>
                            )}

                            {/* Section FACTURATION */}
                            <div className="border-t border-slate-100 pt-6 mt-6">
                                <div className="flex items-center justify-between mb-4">
                                    <h3 className="font-bold text-slate-900 flex items-center gap-2">
                                        <DollarSign size={18} className="text-emerald-600" />
                                        Facturation & Paiement
                                    </h3>
                                    {lead.invoiceNumber && (
                                        <button
                                            onClick={handleDownloadInvoice}
                                            className="text-xs font-bold text-indigo-600 hover:text-indigo-700 flex items-center gap-1"
                                        >
                                            <Download size={14} /> Facture {lead.invoiceNumber}
                                        </button>
                                    )}
                                </div>

                                {!showPaymentForm ? (
                                    <button
                                        onClick={() => setShowPaymentForm(true)}
                                        className="w-full py-3 border-2 border-dashed border-slate-200 rounded-xl text-slate-400 font-bold text-sm hover:border-emerald-300 hover:text-emerald-600 transition-all flex items-center justify-center gap-2"
                                    >
                                        <Plus size={16} /> Enregistrer un encaissement
                                    </button>
                                ) : (
                                    <div className="bg-slate-50 rounded-xl p-4 border border-emerald-100 animate-in slide-in-from-top-2">
                                        <div className="grid grid-cols-2 gap-3 mb-3">
                                            <input
                                                type="number"
                                                placeholder="Montant (€)"
                                                value={paymentData.amount || ''}
                                                onChange={e => setPaymentData({ ...paymentData, amount: parseFloat(e.target.value) })}
                                                className="p-2 rounded-lg border-none ring-1 ring-slate-200 focus:ring-emerald-500 font-bold text-sm"
                                            />
                                            <select
                                                value={paymentData.method}
                                                onChange={e => setPaymentData({ ...paymentData, method: e.target.value })}
                                                className="p-2 rounded-lg border-none ring-1 ring-slate-200 focus:ring-emerald-500 font-bold text-sm"
                                            >
                                                <option value="CARD">CB (TPE)</option>
                                                <option value="CASH">Espèces</option>
                                                <option value="TRANSFER">Virement</option>
                                                <option value="CHECK">Chèque</option>
                                            </select>
                                        </div>
                                        <input
                                            type="text"
                                            placeholder="Référence (N° transaction, etc)"
                                            value={paymentData.ref}
                                            onChange={e => setPaymentData({ ...paymentData, ref: e.target.value })}
                                            className="w-full p-2 rounded-lg border-none ring-1 ring-slate-200 focus:ring-emerald-500 font-bold text-sm mb-3"
                                        />
                                        <div className="flex gap-2">
                                            <button
                                                onClick={handleRecordPayment}
                                                className="flex-1 bg-emerald-600 text-white py-2 rounded-lg font-bold text-sm hover:bg-emerald-700 transition-colors"
                                            >
                                                Valider l'encaissement
                                            </button>
                                            <button
                                                onClick={() => setShowPaymentForm(false)}
                                                className="px-4 py-2 bg-slate-200 text-slate-600 rounded-lg font-bold text-sm hover:bg-slate-300 transition-colors"
                                            >
                                                Annuler
                                            </button>
                                        </div>
                                    </div>
                                )}
                            </div>
                        </div>
                    </div>

                    {/* Colonne Droite - Notes & Échanges */}
                    <div className="w-1/2 flex flex-col bg-slate-50">
                        <div className="p-4 border-b border-slate-200">
                            <h3 className="font-bold text-slate-900 flex items-center gap-2">
                                <MessageSquare size={18} className="text-indigo-600" />
                                Notes & Échanges Siège
                            </h3>
                        </div>

                        {/* Liste des Notes */}
                        <div className="flex-1 overflow-y-auto p-4 space-y-3">
                            {(!lead.notes || lead.notes.length === 0) ? (
                                <div className="text-center py-12 text-slate-400">
                                    <MessageSquare size={40} className="mx-auto mb-2 opacity-30" />
                                    <p className="text-sm font-medium">Aucune note pour le moment</p>
                                    <p className="text-xs">Envoyez un message au siège ci-dessous</p>
                                </div>
                            ) : (
                                lead.notes.map((note) => (
                                    <div
                                        key={note.id}
                                        className={`rounded-xl p-3 ${note.author === 'AGENCY'
                                            ? 'bg-emerald-100 ml-4'
                                            : 'bg-white border border-slate-200 mr-4'
                                            }`}
                                    >
                                        <div className="flex items-center justify-between mb-1">
                                            <span className={`text-[10px] font-bold uppercase ${note.author === 'AGENCY' ? 'text-emerald-700' : 'text-indigo-700'
                                                }`}>
                                                {note.author === 'AGENCY' ? '🏪 ' : '🏢 '}
                                                {note.authorName}
                                            </span>
                                            <span className="text-[10px] text-slate-400">
                                                {new Date(note.createdAt).toLocaleString('fr-FR')}
                                            </span>
                                        </div>
                                        <p className="text-sm text-slate-700">{note.content}</p>
                                    </div>
                                ))
                            )}
                        </div>

                        {/* Zone de saisie */}
                        <div className="p-4 border-t border-slate-200 bg-white">
                            <div className="flex gap-2">
                                <input
                                    type="text"
                                    value={newNote}
                                    onChange={(e) => setNewNote(e.target.value)}
                                    onKeyPress={(e) => e.key === 'Enter' && handleSendNote()}
                                    placeholder="Ajouter une note pour le siège..."
                                    className="flex-1 border border-slate-200 rounded-xl px-4 py-3 text-sm focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none"
                                    disabled={isSending}
                                />
                                <button
                                    onClick={handleSendNote}
                                    disabled={!newNote.trim() || isSending}
                                    className="bg-indigo-600 hover:bg-indigo-700 disabled:bg-slate-300 text-white px-4 py-3 rounded-xl font-bold flex items-center gap-2 transition-colors"
                                >
                                    {isSending ? (
                                        <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin" />
                                    ) : (
                                        <Send size={18} />
                                    )}
                                </button>
                            </div>
                        </div>
                    </div>
                </div>

                {/* Footer */}
                <div className="p-4 border-t border-slate-100 flex justify-end">
                    <button
                        onClick={onClose}
                        className="px-6 py-2 bg-slate-100 hover:bg-slate-200 text-slate-700 rounded-xl font-bold transition-colors"
                    >
                        Fermer
                    </button>
                </div>
            </div>
        </div>
    );
}
