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
    ArrowRight
} from 'lucide-react';
import { CRM } from '../../services/crmStore';

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
    const [isLoading, setIsLoading] = useState(true);
    const [selectedProspect, setSelectedProspect] = useState<Prospect | null>(null);
    const [showImportModal, setShowImportModal] = useState(false);
    const [showCallCockpit, setShowCallCockpit] = useState(false);
    const router = useRouter();

    // Initial load
    useEffect(() => {
        loadProspects();
    }, []);

    const loadProspects = async () => {
        setIsLoading(true);
        const data = await SalesStore.getProspects();
        setProspects(data);
        setIsLoading(false);
    };

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

        // Create actual Lead in CRM
        await CRM.saveLead({
            id: `LEAD-${Date.now()}`, // Simple ID gen
            name: `${prospect.firstName} ${prospect.lastName}`,
            email: prospect.email,
            phone: prospect.phone,
            serviceId: prospect.interestServiceId || 'unknown',
            serviceName: 'Projet Client',
            status: 'NEW',
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
                    <h1 className="text-2xl font-bold text-slate-900">Sales Hub</h1>
                    <p className="text-slate-500 text-sm">Gestion de la prospection commerciale</p>
                </div>
                <div className="flex items-center gap-3">
                    <button
                        onClick={() => setShowImportModal(true)}
                        className="flex items-center gap-2 px-4 py-2 bg-white border border-slate-200 text-slate-700 rounded-lg hover:bg-slate-50 transition-colors"
                    >
                        <Upload size={18} />
                        Import CSV
                    </button>
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
                </div>
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
                            <div className="flex-1 bg-slate-100/50 rounded-xl p-2 overflow-y-auto">
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
                                                onClick={() => setSelectedProspect(prospect)}
                                                className="bg-white p-4 rounded-lg shadow-sm border border-slate-200 cursor-pointer hover:shadow-md transition-all group relative"
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
                                    onClick={() => handleStatusChange(selectedProspect.id, 'LINK_SENT')}
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
        </div>
    );
}
