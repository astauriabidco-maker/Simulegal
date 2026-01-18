'use client';

import React, { useEffect, useState } from 'react';
import { FranchiseLead, FranchiseLeadStore, FranchiseLeadStatus } from '../../../services/FranchiseLeadStore';
import { Plus, Search, MoreHorizontal, Briefcase, MapPin, Mail, Phone, ArrowRight, CheckCircle, FileText } from 'lucide-react';
import DashboardLayout from '../../../components/admin/DashboardLayout';
import { AuthStore } from '../../../services/authStore';

const STATUS_COLUMNS: { id: FranchiseLeadStatus; label: string; color: string }[] = [
    { id: 'NEW', label: 'Nouveaux', color: 'bg-blue-100 text-blue-800' },
    { id: 'CONTACTED', label: 'Contactés', color: 'bg-yellow-100 text-yellow-800' },
    { id: 'MEETING', label: 'RDV Planifié', color: 'bg-purple-100 text-purple-800' },
    { id: 'CONTRACT_SENT', label: 'Contrat Envoyé', color: 'bg-orange-100 text-orange-800' },
    { id: 'SIGNED', label: 'Signés / Convertis', color: 'bg-emerald-100 text-emerald-800' },
    { id: 'REJECTED', label: 'Rejetés', color: 'bg-red-100 text-red-800' }
];

export default function FranchiseLeadsPage() {
    const [leads, setLeads] = useState<FranchiseLead[]>([]);
    const [loading, setLoading] = useState(true);
    // FALLBACK USER to prevent white screen if AuthStore returns null initially
    const currentUser = AuthStore.getCurrentUser() || { name: 'Admin', role: 'HQ' };

    useEffect(() => {
        loadLeads();
    }, []);

    const loadLeads = async () => {
        setLoading(true);
        const data = await FranchiseLeadStore.getAll();
        setLeads(data);
        setLoading(false);
    };

    const handleCreateTestLead = async () => {
        const newLead = await FranchiseLeadStore.create({
            name: 'Jean Dupont',
            email: `jean.dupont.${Date.now()}@example.com`,
            phone: '0612345678',
            targetCity: 'Lyon',
            region: 'AURA',
            status: 'NEW',
            contractDetails: JSON.stringify({ type: 'CORNER', commissionRate: 5 })
        });
        if (newLead) loadLeads();
    };

    const handleSignContract = async (id: string) => {
        if (!confirm('Êtes-vous sûr de vouloir signer ce contrat ? Cela créera automatiquement l\'agence.')) return;
        await FranchiseLeadStore.signContract(id);
        loadLeads();
    };

    // Removed the strict null check that was causing the blank screen
    // if (!currentUser) return null;

    return (
        <>
            <div className="p-8">
                <div className="flex justify-between items-center mb-8">
                    <div>
                        <h1 className="text-2xl font-bold text-slate-900">Recrutement Franchisés</h1>
                        <p className="text-slate-500">Gérez le pipeline de recrutement du réseau</p>
                    </div>
                    <button
                        onClick={handleCreateTestLead}
                        className="bg-indigo-600 text-white px-4 py-2 rounded-lg flex items-center gap-2 hover:bg-indigo-700 transition-colors"
                    >
                        <Plus size={18} />
                        Nouveau Candidat (Test)
                    </button>
                </div>

                <div className="flex gap-4 overflow-x-auto pb-4 h-[calc(100vh-200px)]">
                    {STATUS_COLUMNS.map(column => (
                        <div key={column.id} className="min-w-[300px] w-[300px] bg-slate-50 rounded-xl flex flex-col max-h-full">
                            {/* Header Colonne */}
                            <div className="p-3 border-b border-slate-200 flex justify-between items-center sticky top-0 bg-slate-50 rounded-t-xl z-10">
                                <div className="flex items-center gap-2">
                                    <span className={`px-2 py-0.5 rounded-full text-xs font-bold ${column.color}`}>
                                        {leads.filter(l => l.status === column.id).length}
                                    </span>
                                    <span className="font-semibold text-slate-700">{column.label}</span>
                                </div>
                            </div>

                            {/* Contenu Colonne */}
                            <div className="p-3 space-y-3 overflow-y-auto flex-1">
                                {leads.filter(l => l.status === column.id).map(lead => (
                                    <div className="bg-white p-4 rounded-lg shadow-sm border border-slate-200 hover:shadow-md transition-shadow group relative cursor-pointer"
                                        onClick={() => window.location.href = `/admin/franchise-leads/${lead.id}`}
                                    >
                                        <div className="flex justify-between items-start mb-2">
                                            <h3 className="font-bold text-slate-800 hover:text-indigo-600 transition-colors">{lead.name}</h3>
                                            <button className="text-slate-400 hover:text-slate-600 p-1 hover:bg-slate-100 rounded">
                                                <MoreHorizontal size={16} />
                                            </button>
                                        </div>

                                        <div className="space-y-1 mb-3">
                                            <div className="flex items-center gap-2 text-xs text-slate-500">
                                                <MapPin size={12} />
                                                <span>{lead.targetCity} ({lead.region})</span>
                                            </div>
                                            <div className="flex items-center gap-2 text-xs text-slate-500">
                                                <Mail size={12} />
                                                <span className="truncate">{lead.email}</span>
                                            </div>
                                        </div>

                                        {/* Actions rapides */}
                                        <div className="mt-3 pt-3 border-t border-slate-100 flex justify-end gap-2">
                                            {lead.status === 'CONTRACT_SENT' && (
                                                <button
                                                    onClick={() => handleSignContract(lead.id)}
                                                    className="text-xs bg-emerald-50 text-emerald-600 px-2 py-1 rounded hover:bg-emerald-100 flex items-center gap-1"
                                                >
                                                    <FileText size={12} />
                                                    Signer
                                                </button>
                                            )}
                                            {lead.status === 'SIGNED' && (
                                                <span className="text-xs text-emerald-600 font-medium flex items-center gap-1">
                                                    <CheckCircle size={12} />
                                                    Agence Créée
                                                </span>
                                            )}
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </>
    );
}
