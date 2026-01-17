'use client';

import React, { useState, useEffect } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import { MOCK_AGENCIES } from '../../types/backoffice';
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
    CheckCircle
} from 'lucide-react';

type KanbanStatus = 'NEW' | 'PROCESSING' | 'SUBMITTED' | 'COMPLETED';

interface KanbanColumn {
    id: KanbanStatus;
    title: string;
    icon: React.ReactNode;
    color: string;
    bgColor: string;
}

const KANBAN_COLUMNS: KanbanColumn[] = [
    {
        id: 'NEW',
        title: 'Nouveaux',
        icon: <Sparkles size={16} />,
        color: 'text-blue-600',
        bgColor: 'bg-blue-50 border-blue-200'
    },
    {
        id: 'PROCESSING',
        title: 'En constitution',
        icon: <FolderOpen size={16} />,
        color: 'text-amber-600',
        bgColor: 'bg-amber-50 border-amber-200'
    },
    {
        id: 'SUBMITTED',
        title: 'Instruction',
        icon: <Building2 size={16} />,
        color: 'text-purple-600',
        bgColor: 'bg-purple-50 border-purple-200'
    },
    {
        id: 'COMPLETED',
        title: 'Terminés',
        icon: <CheckCircle2 size={16} />,
        color: 'text-emerald-600',
        bgColor: 'bg-emerald-50 border-emerald-200'
    },
];

// Services de rappel (exclus du Kanban HQ)
const RAPPEL_SERVICES = ['rappel_echeances', 'rdv_juriste'];

interface HQDashboardProps {
    onViewDossier?: (lead: Lead) => void;
}

export default function HQDashboard({ onViewDossier }: HQDashboardProps) {
    const [leads, setLeads] = useState<Lead[]>([]);
    const [selectedLead, setSelectedLead] = useState<Lead | null>(null);
    const [stats, setStats] = useState({ total: 0, revenue: 0 });

    useEffect(() => {
        loadLeads();
    }, []);

    const loadLeads = () => {
        const allLeads = CRM.getAllLeads();
        // Exclure les rappels (assignés aux agences, pas au HQ)
        const hqLeads = allLeads.filter(l => !RAPPEL_SERVICES.includes(l.serviceId));
        setLeads(hqLeads);
        setStats({
            total: hqLeads.length,
            revenue: hqLeads.reduce((sum, l) => sum + (l.amountPaid || 0), 0)
        });
    };

    const getLeadsByStatus = (status: KanbanStatus): Lead[] => {
        // Map les statuts CRM vers Kanban
        return leads.filter(lead => {
            if (status === 'NEW') return lead.status === 'PAID';
            if (status === 'PROCESSING') return lead.status === 'PROCESSING';
            if (status === 'SUBMITTED') return false; // Pas de mapping direct
            if (status === 'COMPLETED') return lead.status === 'COMPLETED';
            return false;
        });
    };

    const updateLeadStatus = (leadId: string, newStatus: KanbanStatus) => {
        // Simulation - en prod, appel API
        console.log(`[HQ] Mise à jour dossier ${leadId} → ${newStatus}`);
        // Ici on mettrait à jour le localStorage
    };

    const getSourceName = (lead: Lead): string => {
        // Simulation - cherche l'agence source
        // En prod, on utiliserait originAgencyId
        return 'Web Direct';
    };

    return (
        <div className="h-full flex flex-col">
            {/* Header avec Stats */}
            <div className="bg-white border-b border-slate-200 p-6">
                <div className="flex items-center justify-between mb-6">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900">Tableau de Bord Siège</h1>
                        <p className="text-slate-500 text-sm">Gestion des dossiers de production</p>
                    </div>
                    <div className="flex gap-4">
                        <div className="bg-slate-50 rounded-xl px-6 py-3 flex items-center gap-3">
                            <Users className="text-indigo-600" size={20} />
                            <div>
                                <p className="text-2xl font-black text-slate-900">{stats.total}</p>
                                <p className="text-[10px] text-slate-400 uppercase font-bold">Dossiers</p>
                            </div>
                        </div>
                        <div className="bg-slate-50 rounded-xl px-6 py-3 flex items-center gap-3">
                            <TrendingUp className="text-emerald-600" size={20} />
                            <div>
                                <p className="text-2xl font-black text-slate-900">{(stats.revenue / 100).toFixed(0)} €</p>
                                <p className="text-[10px] text-slate-400 uppercase font-bold">CA Total</p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Kanban Board */}
            <div className="flex-1 overflow-x-auto p-6 bg-slate-100">
                <div className="flex gap-4 min-w-max h-full">
                    {KANBAN_COLUMNS.map((column) => {
                        const columnLeads = getLeadsByStatus(column.id);
                        return (
                            <div
                                key={column.id}
                                className="w-80 flex-shrink-0 flex flex-col bg-slate-200/50 rounded-2xl"
                            >
                                {/* Column Header */}
                                <div className={`p-4 rounded-t-2xl border-b-2 ${column.bgColor}`}>
                                    <div className="flex items-center gap-2">
                                        <span className={column.color}>{column.icon}</span>
                                        <h3 className="font-bold text-slate-900">{column.title}</h3>
                                        <span className="ml-auto bg-white/80 text-slate-600 text-xs font-bold px-2 py-0.5 rounded-full">
                                            {columnLeads.length}
                                        </span>
                                    </div>
                                </div>

                                {/* Column Content */}
                                <div className="flex-1 p-3 space-y-3 overflow-y-auto">
                                    {columnLeads.length === 0 ? (
                                        <div className="text-center text-slate-400 text-sm py-8">
                                            Aucun dossier
                                        </div>
                                    ) : (
                                        columnLeads.map((lead) => (
                                            <LeadCard
                                                key={lead.id}
                                                lead={lead}
                                                sourceName={getSourceName(lead)}
                                                onView={() => setSelectedLead(lead)}
                                                onStatusChange={(status) => updateLeadStatus(lead.id, status)}
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
    sourceName,
    onView,
    onStatusChange
}: {
    lead: Lead;
    sourceName: string;
    onView: () => void;
    onStatusChange: (status: KanbanStatus) => void;
}) {
    const [showStatusMenu, setShowStatusMenu] = useState(false);

    return (
        <div className="bg-white rounded-xl shadow-sm border border-slate-100 overflow-hidden hover:shadow-md transition-shadow">
            {/* Header */}
            <div className="p-3 border-b border-slate-50">
                <div className="flex items-center justify-between mb-1">
                    <h4 className="font-bold text-slate-900 truncate flex-1">{lead.name}</h4>
                    <span className="bg-indigo-100 text-indigo-700 text-[10px] font-bold px-2 py-0.5 rounded-full ml-2">
                        {lead.serviceName?.split(' ')[0] || 'Service'}
                    </span>
                </div>
                <p className="text-[10px] text-slate-400 font-mono">{lead.id}</p>
            </div>

            {/* Body */}
            <div className="p-3 space-y-2 text-xs">
                <div className="flex items-center gap-2 text-slate-500">
                    <Calendar size={12} />
                    <span>{new Date(lead.createdAt).toLocaleDateString('fr-FR')}</span>
                </div>
                <div className="flex items-center gap-2 text-slate-500">
                    <MapPin size={12} />
                    <span>{sourceName}</span>
                </div>
                <div className="flex items-center gap-2">
                    <CreditCard size={12} className="text-emerald-500" />
                    <span className="text-emerald-600 font-bold">{(lead.amountPaid / 100).toFixed(0)} € Payé</span>
                </div>
                {/* Badge IA si documents pré-validés */}
                {lead.documents && lead.documents.some(d => d.verificationStatus === 'AUTO_VALIDATED') && (
                    <div className="flex items-center gap-1 text-purple-600 bg-purple-50 px-2 py-1 rounded-lg">
                        <Bot size={12} />
                        <span className="text-[10px] font-bold">🤖 Docs IA validés</span>
                    </div>
                )}
            </div>

            {/* Actions */}
            <div className="p-3 border-t border-slate-50 flex gap-2">
                <button
                    onClick={onView}
                    className="flex-1 bg-slate-100 hover:bg-slate-200 text-slate-700 py-2 px-3 rounded-lg text-xs font-bold flex items-center justify-center gap-1 transition-colors"
                >
                    <Eye size={14} />
                    Voir
                </button>
                <div className="relative">
                    <button
                        onClick={() => setShowStatusMenu(!showStatusMenu)}
                        className="bg-indigo-600 hover:bg-indigo-700 text-white py-2 px-3 rounded-lg text-xs font-bold flex items-center gap-1 transition-colors"
                    >
                        Statut
                        <ChevronDown size={14} />
                    </button>
                    {showStatusMenu && (
                        <div className="absolute right-0 top-full mt-1 bg-white rounded-lg shadow-xl border border-slate-200 py-1 z-10 min-w-[140px]">
                            {KANBAN_COLUMNS.map((col) => (
                                <button
                                    key={col.id}
                                    onClick={() => {
                                        onStatusChange(col.id);
                                        setShowStatusMenu(false);
                                    }}
                                    className="w-full text-left px-3 py-2 text-xs hover:bg-slate-50 flex items-center gap-2"
                                >
                                    <span className={col.color}>{col.icon}</span>
                                    {col.title}
                                </button>
                            ))}
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
}

// Modal Détail Lead
function LeadDetailModal({ lead, onClose }: { lead: Lead; onClose: () => void }) {
    return (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-2xl shadow-2xl w-full max-w-lg overflow-hidden">
                {/* Header */}
                <div className="bg-slate-900 text-white p-6 flex items-center justify-between">
                    <div>
                        <h2 className="text-xl font-bold">{lead.name}</h2>
                        <p className="text-slate-400 text-sm">{lead.id}</p>
                    </div>
                    <button
                        onClick={onClose}
                        className="w-10 h-10 bg-white/10 rounded-full flex items-center justify-center hover:bg-white/20 transition-colors"
                    >
                        <X size={20} />
                    </button>
                </div>

                {/* Content */}
                <div className="p-6 space-y-4">
                    <div className="grid grid-cols-2 gap-4">
                        <InfoBlock label="Email" value={lead.email} />
                        <InfoBlock label="Téléphone" value={lead.phone} />
                        <InfoBlock label="Service" value={lead.serviceName || lead.serviceId} />
                        <InfoBlock label="Montant" value={`${(lead.amountPaid / 100).toFixed(0)} €`} />
                        <InfoBlock label="Date" value={new Date(lead.createdAt).toLocaleString('fr-FR')} />
                        <InfoBlock label="Statut" value={lead.status} />
                    </div>

                    {/* Signature Info */}
                    {lead.contract && (
                        <div className="bg-emerald-50 border border-emerald-200 rounded-xl p-4">
                            <div className="flex items-center gap-2 mb-2">
                                <FileText size={16} className="text-emerald-600" />
                                <h4 className="font-bold text-emerald-800">Signature Électronique</h4>
                            </div>
                            <div className="text-xs text-emerald-700 space-y-1">
                                <p>Signé le : {new Date(lead.contract.signedAt).toLocaleString('fr-FR')}</p>
                                <p>IP : {lead.contract.ipAddress}</p>
                                <p>Version CGV : {lead.contract.consentVersion}</p>
                            </div>
                        </div>
                    )}

                    {/* Documents avec statut IA */}
                    {lead.documents && lead.documents.length > 0 && (
                        <div className="bg-slate-50 border border-slate-200 rounded-xl p-4">
                            <div className="flex items-center gap-2 mb-3">
                                <FileText size={16} className="text-slate-600" />
                                <h4 className="font-bold text-slate-800">Documents ({lead.documents.length})</h4>
                            </div>
                            <div className="space-y-2">
                                {lead.documents.map((doc) => (
                                    <div key={doc.id} className="flex items-center justify-between text-xs bg-white p-2 rounded-lg">
                                        <span className="font-medium text-slate-700">{doc.fileName}</span>
                                        <div className="flex items-center gap-2">
                                            {doc.verificationStatus === 'AUTO_VALIDATED' && (
                                                <span className="flex items-center gap-1 text-purple-600 bg-purple-100 px-2 py-0.5 rounded-full">
                                                    <Bot size={10} />
                                                    🤖 IA {doc.aiConfidence}%
                                                </span>
                                            )}
                                            {doc.verificationStatus === 'MANUAL_VALIDATED' && (
                                                <span className="flex items-center gap-1 text-emerald-600 bg-emerald-100 px-2 py-0.5 rounded-full">
                                                    <CheckCircle size={10} />
                                                    Validé
                                                </span>
                                            )}
                                            {doc.verificationStatus === 'PENDING' && (
                                                <span className="text-amber-600 bg-amber-100 px-2 py-0.5 rounded-full">
                                                    En attente
                                                </span>
                                            )}
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    )}
                </div>

                {/* Footer */}
                <div className="p-4 border-t border-slate-100 flex gap-3">
                    <button
                        onClick={onClose}
                        className="flex-1 bg-slate-100 hover:bg-slate-200 text-slate-700 py-3 rounded-xl font-bold transition-colors"
                    >
                        Fermer
                    </button>
                    <button className="flex-1 bg-indigo-600 hover:bg-indigo-700 text-white py-3 rounded-xl font-bold transition-colors">
                        Traiter le dossier
                    </button>
                </div>
            </div>
        </div>
    );
}

// Composant InfoBlock
function InfoBlock({ label, value }: { label: string; value: string }) {
    return (
        <div>
            <p className="text-[10px] text-slate-400 uppercase font-bold mb-1">{label}</p>
            <p className="text-sm font-bold text-slate-900">{value}</p>
        </div>
    );
}
