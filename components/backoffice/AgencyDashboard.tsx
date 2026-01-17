'use client';

import React, { useState, useEffect } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import { MOCK_AGENCIES, Agency } from '../../types/backoffice';
import FranchiseCaseDetail from './FranchiseCaseDetail';
import {
    Phone,
    Eye,
    DollarSign,
    CheckCircle,
    Clock,
    AlertCircle,
    User,
    FileText,
    Building2,
    TrendingUp,
    PhoneCall,
    XCircle,
    ExternalLink
} from 'lucide-react';

// ⚠️ SIMULATION: ID de l'agence connectée
// En production, récupéré via le système d'authentification
const CURRENT_AGENCY_ID = 'FRAN-001'; // Franchise Marseille

// Services de rappel commercial
const RAPPEL_SERVICES = ['rappel_echeances', 'rdv_juriste'];

// Services de production (titre, naturalisation...)
const PRODUCTION_SERVICES = ['titre_sejour', 'naturalisation', 'regroupement_familial', 'driving_exchange'];

type TabId = 'leads' | 'dossiers' | 'commissions';

interface TabConfig {
    id: TabId;
    label: string;
    icon: React.ReactNode;
}

const TABS: TabConfig[] = [
    { id: 'leads', label: 'Mes Leads à rappeler', icon: <PhoneCall size={18} /> },
    { id: 'dossiers', label: 'Suivi de mes dossiers', icon: <Eye size={18} /> },
    { id: 'commissions', label: 'Mes Commissions', icon: <DollarSign size={18} /> },
];

export default function AgencyDashboard() {
    const [activeTab, setActiveTab] = useState<TabId>('leads');
    const [leads, setLeads] = useState<Lead[]>([]);
    const [treatedLeads, setTreatedLeads] = useState<Set<string>>(new Set());
    const [currentAgency, setCurrentAgency] = useState<Agency | null>(null);
    const [selectedLeadId, setSelectedLeadId] = useState<string | null>(null);

    useEffect(() => {
        // Récupère l'agence courante
        const agency = MOCK_AGENCIES.find(a => a.id === CURRENT_AGENCY_ID);
        setCurrentAgency(agency || null);

        // Charge les leads du CRM
        loadLeads();
    }, []);

    const loadLeads = () => {
        const allLeads = CRM.getAllLeads();
        // ⚠️ CLOISONNEMENT: Ne garde que les leads de cette agence
        // En prod, on filtrerait sur originAgencyId === CURRENT_AGENCY_ID
        // Ici on simule en gardant tous les leads pour la démo
        setLeads(allLeads);
    };

    // Leads de rappel à traiter
    const rappelLeads = leads.filter(l =>
        RAPPEL_SERVICES.includes(l.serviceId) && !treatedLeads.has(l.id)
    );

    // Dossiers de production (vue lecture seule)
    const productionDossiers = leads.filter(l =>
        !RAPPEL_SERVICES.includes(l.serviceId)
    );

    // Stats commissions
    const commissionStats = {
        count: productionDossiers.length,
        totalCA: productionDossiers.reduce((sum, l) => sum + (l.amountPaid || 0), 0),
        commissionRate: currentAgency?.commissionRate || 15,
    };
    commissionStats.totalCA = commissionStats.totalCA / 100; // En euros
    const commissionAmount = (commissionStats.totalCA * commissionStats.commissionRate) / 100;

    const markAsTreated = (leadId: string) => {
        setTreatedLeads(prev => new Set([...prev, leadId]));
    };

    const getStatusBadge = (status: string) => {
        switch (status) {
            case 'PAID':
                return { label: 'Nouveau', color: 'bg-blue-100 text-blue-700', icon: <Clock size={12} /> };
            case 'PROCESSING':
                return { label: 'En cours', color: 'bg-amber-100 text-amber-700', icon: <FileText size={12} /> };
            case 'COMPLETED':
                return { label: 'Terminé', color: 'bg-emerald-100 text-emerald-700', icon: <CheckCircle size={12} /> };
            default:
                return { label: status, color: 'bg-slate-100 text-slate-700', icon: <AlertCircle size={12} /> };
        }
    };

    return (
        <div className="h-full flex flex-col bg-slate-50">
            {/* Header Agence */}
            <div className="bg-white border-b border-slate-200 p-6">
                <div className="flex items-center gap-4 mb-4">
                    <div className="w-12 h-12 bg-emerald-100 rounded-xl flex items-center justify-center">
                        <Building2 className="text-emerald-600" size={24} />
                    </div>
                    <div>
                        <h1 className="text-xl font-black text-slate-900">
                            {currentAgency?.name || 'Agence'}
                        </h1>
                        <p className="text-sm text-slate-500">
                            {currentAgency?.zipCodes.length || 0} zones couvertes •
                            Commission : {currentAgency?.commissionRate || 0}%
                        </p>
                    </div>
                    <span className="ml-auto bg-emerald-100 text-emerald-700 text-xs font-bold px-3 py-1.5 rounded-full uppercase">
                        {currentAgency?.type || 'Agence'}
                    </span>
                </div>

                {/* Tabs */}
                <div className="flex gap-2">
                    {TABS.map(tab => (
                        <button
                            key={tab.id}
                            onClick={() => setActiveTab(tab.id)}
                            className={`flex items-center gap-2 px-4 py-2.5 rounded-xl font-bold text-sm transition-all ${activeTab === tab.id
                                ? 'bg-slate-900 text-white shadow-lg'
                                : 'bg-slate-100 text-slate-600 hover:bg-slate-200'
                                }`}
                        >
                            {tab.icon}
                            {tab.label}
                            {tab.id === 'leads' && rappelLeads.length > 0 && (
                                <span className="bg-red-500 text-white text-[10px] font-bold px-1.5 py-0.5 rounded-full">
                                    {rappelLeads.length}
                                </span>
                            )}
                        </button>
                    ))}
                </div>
            </div>

            {/* Content */}
            <div className="flex-1 overflow-auto p-6">
                {/* ONGLET 1: LEADS À RAPPELER */}
                {activeTab === 'leads' && (
                    <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                        <div className="p-4 border-b border-slate-100 flex items-center justify-between">
                            <div className="flex items-center gap-2">
                                <Phone className="text-orange-500" size={20} />
                                <h2 className="font-bold text-slate-900">Leads à rappeler</h2>
                            </div>
                            <span className="bg-orange-100 text-orange-700 text-xs font-bold px-2 py-1 rounded-full">
                                {rappelLeads.length} en attente
                            </span>
                        </div>

                        {rappelLeads.length === 0 ? (
                            <div className="p-12 text-center text-slate-400">
                                <CheckCircle size={48} className="mx-auto mb-3 text-emerald-400" />
                                <p className="font-bold">Tous les leads ont été traités !</p>
                            </div>
                        ) : (
                            <table className="w-full">
                                <thead className="bg-slate-50 text-xs text-slate-500 uppercase">
                                    <tr>
                                        <th className="text-left p-4 font-bold">Client</th>
                                        <th className="text-left p-4 font-bold">Téléphone</th>
                                        <th className="text-left p-4 font-bold">Sujet</th>
                                        <th className="text-left p-4 font-bold">Date</th>
                                        <th className="text-right p-4 font-bold">Action</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-slate-50">
                                    {rappelLeads.map(lead => (
                                        <tr key={lead.id} className="hover:bg-slate-50 transition-colors">
                                            <td className="p-4">
                                                <div className="flex items-center gap-3">
                                                    <div className="w-8 h-8 bg-indigo-100 rounded-full flex items-center justify-center text-indigo-600 font-bold text-sm">
                                                        {lead.name.charAt(0)}
                                                    </div>
                                                    <div>
                                                        <p className="font-bold text-slate-900">{lead.name}</p>
                                                        <p className="text-xs text-slate-400">{lead.email}</p>
                                                    </div>
                                                </div>
                                            </td>
                                            <td className="p-4">
                                                <a
                                                    href={`tel:${lead.phone}`}
                                                    className="text-indigo-600 font-bold hover:underline"
                                                >
                                                    {lead.phone}
                                                </a>
                                            </td>
                                            <td className="p-4">
                                                <span className="bg-slate-100 text-slate-700 text-xs font-bold px-2 py-1 rounded">
                                                    {lead.serviceName || 'Rappel'}
                                                </span>
                                            </td>
                                            <td className="p-4 text-sm text-slate-500">
                                                {new Date(lead.createdAt).toLocaleDateString('fr-FR')}
                                            </td>
                                            <td className="p-4 text-right">
                                                <button
                                                    onClick={() => markAsTreated(lead.id)}
                                                    className="bg-emerald-600 hover:bg-emerald-700 text-white px-4 py-2 rounded-lg text-xs font-bold transition-colors"
                                                >
                                                    ✓ Traité
                                                </button>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}
                    </div>
                )}

                {/* ONGLET 2: SUIVI DOSSIERS (LECTURE SEULE) */}
                {activeTab === 'dossiers' && (
                    <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
                        <div className="p-4 border-b border-slate-100 flex items-center justify-between">
                            <div className="flex items-center gap-2">
                                <Eye className="text-indigo-500" size={20} />
                                <h2 className="font-bold text-slate-900">Suivi de mes dossiers</h2>
                            </div>
                            <span className="bg-slate-100 text-slate-600 text-xs font-bold px-2 py-1 rounded-full flex items-center gap-1">
                                <XCircle size={12} />
                                Lecture seule
                            </span>
                        </div>

                        {productionDossiers.length === 0 ? (
                            <div className="p-12 text-center text-slate-400">
                                <FileText size={48} className="mx-auto mb-3" />
                                <p className="font-bold">Aucun dossier en cours</p>
                            </div>
                        ) : (
                            <table className="w-full">
                                <thead className="bg-slate-50 text-xs text-slate-500 uppercase">
                                    <tr>
                                        <th className="text-left p-4 font-bold">Client</th>
                                        <th className="text-left p-4 font-bold">Service</th>
                                        <th className="text-left p-4 font-bold">Date</th>
                                        <th className="text-left p-4 font-bold">Statut (Siège)</th>
                                        <th className="text-right p-4 font-bold">Action</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-slate-50">
                                    {productionDossiers.map(lead => {
                                        const badge = getStatusBadge(lead.status);
                                        return (
                                            <tr key={lead.id} className="hover:bg-slate-50 transition-colors">
                                                <td className="p-4">
                                                    <div className="flex items-center gap-3">
                                                        <div className="w-8 h-8 bg-slate-100 rounded-full flex items-center justify-center text-slate-600 font-bold text-sm">
                                                            <User size={16} />
                                                        </div>
                                                        <div>
                                                            <p className="font-bold text-slate-900">{lead.name}</p>
                                                            <p className="text-[10px] text-slate-400 font-mono">{lead.id}</p>
                                                        </div>
                                                    </div>
                                                </td>
                                                <td className="p-4">
                                                    <span className="bg-indigo-100 text-indigo-700 text-xs font-bold px-2 py-1 rounded">
                                                        {lead.serviceName || lead.serviceId}
                                                    </span>
                                                </td>
                                                <td className="p-4 text-sm text-slate-500">
                                                    {new Date(lead.createdAt).toLocaleDateString('fr-FR')}
                                                </td>
                                                <td className="p-4">
                                                    <span className={`inline-flex items-center gap-1.5 text-xs font-bold px-3 py-1.5 rounded-full ${badge.color}`}>
                                                        {badge.icon}
                                                        {badge.label}
                                                    </span>
                                                </td>
                                                <td className="p-4 text-right">
                                                    <button
                                                        onClick={() => setSelectedLeadId(lead.id)}
                                                        className="bg-indigo-600 hover:bg-indigo-700 text-white px-3 py-1.5 rounded-lg text-xs font-bold transition-colors flex items-center gap-1 ml-auto"
                                                    >
                                                        <ExternalLink size={12} />
                                                        Voir
                                                    </button>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        )}
                    </div>
                )}

                {/* ONGLET 3: COMMISSIONS */}
                {activeTab === 'commissions' && (
                    <div className="space-y-6">
                        {/* Résumé */}
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                            <div className="bg-white rounded-2xl p-6 shadow-sm border border-slate-100">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="w-10 h-10 bg-indigo-100 rounded-xl flex items-center justify-center">
                                        <FileText className="text-indigo-600" size={20} />
                                    </div>
                                    <p className="text-sm text-slate-500 font-medium">Dossiers apportés</p>
                                </div>
                                <p className="text-3xl font-black text-slate-900">{commissionStats.count}</p>
                            </div>

                            <div className="bg-white rounded-2xl p-6 shadow-sm border border-slate-100">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="w-10 h-10 bg-emerald-100 rounded-xl flex items-center justify-center">
                                        <TrendingUp className="text-emerald-600" size={20} />
                                    </div>
                                    <p className="text-sm text-slate-500 font-medium">CA Total généré</p>
                                </div>
                                <p className="text-3xl font-black text-slate-900">{commissionStats.totalCA.toFixed(0)} €</p>
                            </div>

                            <div className="bg-gradient-to-br from-emerald-500 to-emerald-600 rounded-2xl p-6 shadow-lg text-white">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="w-10 h-10 bg-white/20 rounded-xl flex items-center justify-center">
                                        <DollarSign className="text-white" size={20} />
                                    </div>
                                    <p className="text-sm text-emerald-100 font-medium">
                                        Commission ({commissionStats.commissionRate}%)
                                    </p>
                                </div>
                                <p className="text-3xl font-black">{commissionAmount.toFixed(2)} €</p>
                            </div>
                        </div>

                        {/* Note explicative */}
                        <div className="bg-amber-50 border border-amber-200 rounded-xl p-4 flex items-start gap-3">
                            <AlertCircle className="text-amber-600 flex-shrink-0 mt-0.5" size={20} />
                            <div>
                                <p className="font-bold text-amber-800">Comment sont calculées les commissions ?</p>
                                <p className="text-sm text-amber-700 mt-1">
                                    Votre commission de <strong>{commissionStats.commissionRate}%</strong> est calculée
                                    sur le chiffre d'affaires des dossiers dont vous êtes l'apporteur d'affaires.
                                    Le règlement intervient après validation définitive du dossier par le siège.
                                </p>
                            </div>
                        </div>
                    </div>
                )}
            </div>

            {/* Modale Détail Dossier */}
            <FranchiseCaseDetail
                leadId={selectedLeadId || ''}
                isOpen={!!selectedLeadId}
                onClose={() => setSelectedLeadId(null)}
                agencyName={currentAgency?.name}
            />
        </div>
    );
}
