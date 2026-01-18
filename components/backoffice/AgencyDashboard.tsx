'use client';

import React, { useState, useEffect } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import { Agency } from '../../types/backoffice';
import FranchiseCaseDetail from './FranchiseCaseDetail';
import { AuthStore } from '../../services/authStore';
import { AgencyStore } from '../../services/AgencyStore';
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
    const [myLeads, setMyLeads] = useState<Lead[]>([]); // Leads dont l'agence est l'apporteur
    const [areaLeads, setAreaLeads] = useState<Lead[]>([]); // Leads dans la zone géo à rappeler
    const [currentAgency, setCurrentAgency] = useState<any>(null);
    const [selectedLeadId, setSelectedLeadId] = useState<string | null>(null);
    const [isLoading, setIsLoading] = useState(true);

    useEffect(() => {
        const user = AuthStore.getCurrentUser();
        if (user && user.agencyId) {
            loadAgencyData(user.agencyId);
        }
    }, []);

    const loadAgencyData = async (agencyId: string) => {
        setIsLoading(true);
        try {
            // 1. Charger l'agence pour récupérer ses codes postaux et taux de commission
            const agencies = await AgencyStore.getAllAgencies();
            const agency = agencies.find(a => a.id === agencyId);
            setCurrentAgency(agency || null);

            // 2. Charger TOUS les leads pour filtrer par zone géo
            const allLeads = await CRM.getAllLeads();

            // 3. Filtrer mes leads (où je suis l'apporteur d'affaires)
            const ownedLeads = allLeads.filter(l => l.originAgencyId === agencyId);
            setMyLeads(ownedLeads);

            // 4. Filtrer les leads à rappeler dans ma zone (via code postal)
            if (agency?.zipCodes) {
                const leadsInArea = allLeads.filter(l => {
                    // Si c'est un service de rappel et que le CP du lead est dans mes zones
                    // Note: On simule le CP du lead car il n'est pas encore dans l'interface Lead
                    // Pour la démo, on prend les leads sans originAgencyId qui matchent un CP simulé
                    const isRappel = RAPPEL_SERVICES.includes(l.serviceId);
                    // Match simplifié pour la démo: 50% des leads sans agence sont "dans la zone"
                    return isRappel && !l.originAgencyId;
                });
                setAreaLeads(leadsInArea);
            }
        } catch (error) {
            console.error('[AgencyDashboard] Erreur chargement:', error);
        } finally {
            setIsLoading(false);
        }
    };

    // Dossiers de production apportés (vue lecture seule)
    const productionDossiers = myLeads.filter(l =>
        !RAPPEL_SERVICES.includes(l.serviceId)
    );

    // Stats commissions
    const commissionStats = {
        count: productionDossiers.length,
        totalCA: productionDossiers.reduce((sum, l) => sum + (l.amountPaid || 0), 0) / 100,
        commissionRate: currentAgency?.commissionRate || 15,
    };
    const commissionAmount = (commissionStats.totalCA * commissionStats.commissionRate) / 100;

    const getStatusBadge = (status: string) => {
        switch (status) {
            case 'PAID':
                return { label: 'Payé', color: 'bg-emerald-100 text-emerald-700', icon: <DollarSign size={12} /> };
            case 'PROCESSING':
                return { label: 'En traitement', color: 'bg-indigo-100 text-indigo-700', icon: <FileText size={12} /> };
            case 'COMPLETED':
                return { label: 'Finalisé', color: 'bg-blue-100 text-blue-700', icon: <CheckCircle size={12} /> };
            default:
                return { label: status, color: 'bg-slate-100 text-slate-700', icon: <AlertCircle size={12} /> };
        }
    };

    if (isLoading) {
        return (
            <div className="flex-1 flex items-center justify-center bg-slate-50">
                <div className="animate-spin w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full" />
            </div>
        );
    }

    return (
        <div className="h-full flex flex-col bg-slate-50">
            {/* Header Agence */}
            <div className="bg-white border-b border-slate-200 p-6">
                <div className="flex items-center gap-4 mb-6">
                    <div className="w-14 h-14 bg-slate-900 rounded-2xl flex items-center justify-center shadow-lg shadow-slate-200">
                        <Building2 className="text-white" size={28} />
                    </div>
                    <div>
                        <h1 className="text-2xl font-black text-slate-900">
                            {currentAgency?.name || 'Ma Franchise'}
                        </h1>
                        <p className="text-sm font-bold text-slate-400 uppercase tracking-widest">
                            {currentAgency?.city} • {currentAgency?.zipCodes?.join(', ')}
                        </p>
                    </div>

                    <div className="ml-auto flex items-center gap-3">
                        <div className="text-right">
                            <p className="text-[10px] font-black text-slate-400 uppercase tracking-tighter">Mon Taux</p>
                            <p className="text-xl font-black text-emerald-600">{currentAgency?.commissionRate}%</p>
                        </div>
                        <div className="h-10 w-px bg-slate-100 mx-2" />
                        <span className="bg-indigo-50 text-indigo-700 text-[10px] font-black px-3 py-2 rounded-xl border border-indigo-100 uppercase tracking-widest">
                            {currentAgency?.type}
                        </span>
                    </div>
                </div>

                {/* Tabs */}
                <div className="flex gap-1.5 bg-slate-100 p-1.5 rounded-2xl w-fit">
                    {TABS.map(tab => (
                        <button
                            key={tab.id}
                            onClick={() => setActiveTab(tab.id)}
                            className={`flex items-center gap-2 px-6 py-2.5 rounded-xl font-black text-xs uppercase tracking-widest transition-all ${activeTab === tab.id
                                ? 'bg-white text-slate-900 shadow-sm'
                                : 'text-slate-500 hover:text-slate-700'
                                }`}
                        >
                            {tab.icon}
                            {tab.label}
                            {tab.id === 'leads' && areaLeads.length > 0 && (
                                <span className="bg-orange-500 text-white text-[10px] font-black px-1.5 py-0.5 rounded-lg ml-1">
                                    {areaLeads.length}
                                </span>
                            )}
                        </button>
                    ))}
                </div>
            </div>

            {/* Content */}
            <div className="flex-1 overflow-auto p-8">
                {/* ONGLET 1: LEADS À RAPPELER (GÉOGRAPHIQUE) */}
                {activeTab === 'leads' && (
                    <div className="bg-white rounded-3xl shadow-sm border border-slate-200 overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex items-center justify-between">
                            <div>
                                <h2 className="font-black text-slate-900 uppercase tracking-wider">Demandes de rappel zone {currentAgency?.zipCodes?.[0]}...</h2>
                                <p className="text-xs text-slate-400 font-medium">Leads identifiés dans votre secteur géographique</p>
                            </div>
                            <span className="bg-orange-100 text-orange-700 text-[10px] font-black px-3 py-1.5 rounded-full uppercase">
                                {areaLeads.length} À TRAITER
                            </span>
                        </div>

                        {areaLeads.length === 0 ? (
                            <div className="p-20 text-center">
                                <PhoneCall size={64} className="mx-auto mb-4 text-slate-100" />
                                <p className="font-bold text-slate-400">Aucune demande de rappel dans votre zone</p>
                            </div>
                        ) : (
                            <div className="divide-y divide-slate-50">
                                {areaLeads.map(lead => (
                                    <div key={lead.id} className="p-6 hover:bg-slate-50 transition-colors flex items-center gap-6">
                                        <div className="w-12 h-12 bg-indigo-50 rounded-2xl flex items-center justify-center text-indigo-600 font-black text-lg">
                                            {lead.name.charAt(0)}
                                        </div>
                                        <div className="flex-1">
                                            <p className="font-black text-slate-900">{lead.name}</p>
                                            <p className="text-xs font-bold text-indigo-600">{lead.serviceName}</p>
                                        </div>
                                        <div className="text-right">
                                            <p className="text-sm font-bold text-slate-900">{lead.phone}</p>
                                            <p className="text-[10px] text-slate-400 font-bold uppercase">{new Date(lead.createdAt).toLocaleDateString()}</p>
                                        </div>
                                        <a
                                            href={`tel:${lead.phone}`}
                                            className="bg-slate-900 hover:bg-slate-800 text-white w-12 h-12 rounded-2xl flex items-center justify-center transition-all shadow-lg shadow-slate-200"
                                        >
                                            <Phone size={20} />
                                        </a>
                                    </div>
                                ))}
                            </div>
                        )}
                    </div>
                )}

                {/* ONGLET 2: SUIVI DOSSIERS (PORTFEUILLE APPORTEUR) */}
                {activeTab === 'dossiers' && (
                    <div className="bg-white rounded-3xl shadow-sm border border-slate-200 overflow-hidden">
                        <div className="p-6 border-b border-slate-100 flex items-center justify-between">
                            <div>
                                <h2 className="font-black text-slate-900 uppercase tracking-wider text-sm">Mon Portefeuille de Production</h2>
                                <p className="text-xs text-slate-400 font-medium">Dossiers dont vous êtes l'apporteur d'affaires (Lecture seule)</p>
                            </div>
                            <span className="flex items-center gap-1.5 text-[10px] font-black text-slate-400 uppercase tracking-widest bg-slate-50 px-3 py-1.5 rounded-lg border border-slate-100">
                                <XCircle size={12} />
                                Statuts gérés par le siège
                            </span>
                        </div>

                        {productionDossiers.length === 0 ? (
                            <div className="p-20 text-center">
                                <FileText size={64} className="mx-auto mb-4 text-slate-100" />
                                <p className="font-bold text-slate-400">Aucun dossier apporté pour le moment</p>
                            </div>
                        ) : (
                            <table className="w-full">
                                <thead className="bg-slate-50/50 text-[10px] text-slate-400 uppercase font-black tracking-widest">
                                    <tr>
                                        <th className="text-left p-6">Client</th>
                                        <th className="text-left p-6">Service</th>
                                        <th className="text-left p-6">Statut de production</th>
                                        <th className="text-right p-6">Valeur Dossier</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-slate-50">
                                    {productionDossiers.map(lead => {
                                        const badge = getStatusBadge(lead.currentStage);
                                        return (
                                            <tr key={lead.id} className="hover:bg-slate-50/50 transition-colors">
                                                <td className="p-6">
                                                    <p className="font-black text-slate-900">{lead.name}</p>
                                                    <p className="text-[10px] font-mono text-slate-400">ID-{lead.id.slice(0, 8)}</p>
                                                </td>
                                                <td className="p-6">
                                                    <span className="text-xs font-black text-indigo-600 uppercase tracking-wider">
                                                        {lead.serviceName || lead.serviceId}
                                                    </span>
                                                </td>
                                                <td className="p-6">
                                                    <span className={`inline-flex items-center gap-1.5 text-[10px] font-black px-3 py-1 rounded-full uppercase tracking-tighter ${badge.color}`}>
                                                        {badge.icon}
                                                        {badge.label}
                                                    </span>
                                                </td>
                                                <td className="p-6 text-right">
                                                    <p className="font-black text-slate-900">{(lead.amountPaid / 100).toFixed(0)} €</p>
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
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                        <div className="lg:col-span-2 space-y-8">
                            {/* Stats */}
                            <div className="grid grid-cols-2 gap-6">
                                <div className="bg-white rounded-3xl p-8 border border-slate-200">
                                    <p className="text-[10px] font-black text-slate-400 uppercase tracking-widest mb-4">CA Total Apporté</p>
                                    <div className="flex items-end gap-2">
                                        <p className="text-4xl font-black text-slate-900">{commissionStats.totalCA.toFixed(0)} €</p>
                                        <TrendingUp className="text-emerald-500 mb-1" size={24} />
                                    </div>
                                </div>
                                <div className="bg-emerald-600 rounded-3xl p-8 text-white shadow-xl shadow-emerald-200">
                                    <p className="text-[10px] font-black text-emerald-100 uppercase tracking-widest mb-4">Commissions Dues</p>
                                    <div className="flex items-end gap-2">
                                        <p className="text-4xl font-black">{commissionAmount.toFixed(2)} €</p>
                                        <DollarSign className="text-emerald-200 mb-1" size={24} />
                                    </div>
                                </div>
                            </div>

                            {/* Info */}
                            <div className="bg-indigo-900 rounded-3xl p-8 text-white">
                                <div className="flex items-start gap-4">
                                    <div className="p-3 bg-white/10 rounded-2xl">
                                        <AlertCircle size={24} />
                                    </div>
                                    <div>
                                        <h3 className="text-lg font-black mb-2 uppercase tracking-wide">Règlement des commissions</h3>
                                        <p className="text-indigo-100 text-sm leading-relaxed">
                                            Vos commissions sont validées mensuellement après clôture des dossiers par le siège.
                                            Les virements sont effectués le 5 de chaque mois pour le mois précédent.
                                        </p>
                                    </div>
                                </div>
                            </div>
                        </div>

                        {/* Side List */}
                        <div className="bg-white rounded-3xl border border-slate-200 p-6">
                            <h3 className="font-black text-slate-900 uppercase tracking-wider text-xs mb-6">Derniers Apports</h3>
                            <div className="space-y-6">
                                {productionDossiers.slice(0, 5).map(l => (
                                    <div key={l.id} className="flex items-center justify-between">
                                        <div>
                                            <p className="text-xs font-black text-slate-800">{l.name}</p>
                                            <p className="text-[10px] text-slate-400 font-bold">{new Date(l.createdAt).toLocaleDateString()}</p>
                                        </div>
                                        <p className="text-xs font-black text-emerald-600">+{(l.amountPaid / 100 * (currentAgency?.commissionRate || 15) / 100).toFixed(2)} €</p>
                                    </div>
                                ))}
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
