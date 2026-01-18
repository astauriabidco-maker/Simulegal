'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { FranchiseLead, FranchiseLeadStore } from '../../../../services/FranchiseLeadStore';
import DashboardLayout from '../../../../components/admin/DashboardLayout';
import LeadActivityFeed from '../../../../components/admin/LeadActivityFeed';
import { AuthStore } from '../../../../services/authStore';
import { ArrowLeft, Save, FileSignature, CheckCircle, AlertCircle, Building2, User, MapPin, FileUser } from 'lucide-react';

export default function FranchiseLeadDetailPage() {
    const params = useParams();
    const router = useRouter();
    const [lead, setLead] = useState<FranchiseLead | null>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [formData, setFormData] = useState<Partial<FranchiseLead>>({});
    const [contractConfig, setContractConfig] = useState({
        type: 'FRANCHISE',
        commissionRate: 15,
        isExclusive: true
    });

    const currentUser = AuthStore.getCurrentUser() || { name: 'Admin', role: 'HQ' };

    useEffect(() => {
        if (params.id) {
            loadLead(params.id as string);
        }
    }, [params.id]);

    const loadLead = async (id: string) => {
        setLoading(true);
        const data = await FranchiseLeadStore.getById(id);
        if (data) {
            setLead(data);
            setFormData({
                name: data.name,
                email: data.email,
                phone: data.phone,
                targetCity: data.targetCity,
                region: data.region,
                companyName: data.companyName,
                siret: data.siret,
                legalForm: data.legalForm
            });
            if (data.contractDetails) {
                try {
                    const savedConfig = JSON.parse(data.contractDetails);
                    setContractConfig(prev => ({ ...prev, ...savedConfig }));
                } catch (e) {
                    console.error("Error parsing contract details", e);
                }
            }
        }
        setLoading(false);
    };

    const handleContractTypeChange = (type: string) => {
        const newRate = type === 'CORNER' ? 5 : 15;
        setContractConfig(prev => ({
            ...prev,
            type,
            commissionRate: newRate
        }));
    };

    const handleSave = async () => {
        if (!lead) return;
        setSaving(true);
        const updatedData = {
            ...formData,
            contractDetails: JSON.stringify(contractConfig)
        };
        const updatedLead = await FranchiseLeadStore.update(lead.id, updatedData);
        if (updatedLead) {
            setLead(updatedLead);
            alert('Modifications enregistrées');
        } else {
            alert('Erreur lors de la sauvegarde');
        }
        setSaving(false);
    };

    const handleSign = async () => {
        if (!lead || !confirm("Confirmer la signature du contrat ? Une agence sera créée immédiatement.")) return;
        setSaving(true);
        // Ensure we save latest changes first
        const updatedData = {
            ...formData,
            contractDetails: JSON.stringify(contractConfig)
        };
        await FranchiseLeadStore.update(lead.id, updatedData);

        // Sign
        const result = await FranchiseLeadStore.signContract(lead.id);
        if (result) {
            alert('Contrat signé avec succès ! L\'agence a été créée.');
            router.push('/admin/franchise-leads'); // Back to board
        } else {
            alert('Erreur lors de la signature');
        }
        setSaving(false);
    };

    if (loading) return <div className="p-8">Chargement...</div>;
    if (!lead) return <div className="p-8">Candidat introuvable</div>;

    return (
        <>
            <div className="max-w-5xl mx-auto p-8">
                {/* Header */}
                {/* Stepper Pipeline */}
                <div className="mb-8">
                    <div className="flex items-center justify-between relative">
                        <div className="absolute top-1/2 left-0 w-full h-1 bg-slate-200 -z-10 rounded-full" />
                        {['NEW', 'CONTACTED', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'].map((step, index) => {
                            const statusOrder = ['NEW', 'CONTACTED', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'];
                            const currentStepIndex = statusOrder.indexOf(lead.status);
                            const stepIndex = statusOrder.indexOf(step);
                            const isCompleted = currentStepIndex >= stepIndex;
                            const isCurrent = currentStepIndex === stepIndex;

                            const labels: any = { NEW: 'Nouveau', CONTACTED: 'Contacté', VALIDATED: 'Validé', CONTRACT_SENT: 'Contrat Envoyé', SIGNED: 'Signé' };

                            return (
                                <div key={step} className="flex flex-col items-center bg-white px-2">
                                    <div className={`w-8 h-8 rounded-full flex items-center justify-center text-xs font-bold border-2 transition-colors
                                         ${isCompleted ? 'bg-indigo-600 border-indigo-600 text-white' : 'bg-white border-slate-300 text-slate-400'}
                                         ${isCurrent ? 'ring-4 ring-indigo-100' : ''}
                                     `}>
                                        {isCompleted ? <CheckCircle size={14} /> : stepIndex + 1}
                                    </div>
                                    <span className={`text-xs mt-2 font-medium ${isCompleted ? 'text-indigo-700' : 'text-slate-400'}`}>
                                        {labels[step]}
                                    </span>
                                </div>
                            );
                        })}
                    </div>
                </div>

                {/* Header */}
                <div className="flex items-center justify-between mb-8">
                    <div className="flex items-center gap-4">
                        <button
                            onClick={() => router.back()}
                            className="p-2 hover:bg-slate-200 rounded-full transition-colors"
                        >
                            <ArrowLeft size={24} className="text-slate-600" />
                        </button>
                        <div>
                            <h1 className="text-3xl font-bold text-slate-900">{formData.name}</h1>
                            <div className="text-slate-500 text-sm">Candidat ID: {lead.id}</div>
                        </div>
                    </div>
                    <div className="flex gap-3">
                        {/* Action: Save (Always visible unless signed) */}
                        {lead.status !== 'SIGNED' && (
                            <button
                                onClick={handleSave}
                                disabled={saving}
                                className="px-4 py-2 bg-white border border-slate-300 rounded-lg text-slate-700 font-medium hover:bg-slate-50 flex items-center gap-2"
                            >
                                <Save size={18} />
                                Enregistrer
                            </button>
                        )}

                        {/* Workflow Actions */}
                        {lead.status === 'NEW' && (
                            <button
                                onClick={async () => {
                                    setSaving(true);
                                    await FranchiseLeadStore.update(lead.id, { ...formData, status: 'CONTACTED' });
                                    loadLead(lead.id);
                                    setSaving(false);
                                }}
                                className="px-4 py-2 bg-blue-600 text-white rounded-lg font-medium hover:bg-blue-700 flex items-center gap-2 shadow-sm"
                            >
                                <User size={18} />
                                Marquer comme Contacté
                            </button>
                        )}

                        {lead.status === 'CONTACTED' && (
                            <button
                                onClick={async () => {
                                    if (!formData.companyName || !formData.siret) {
                                        alert('Merci de renseigner les informations entreprise avant de valider.');
                                        return;
                                    }
                                    if (confirm('Valider le projet ? Cela débloquera la génération de contrat.')) {
                                        setSaving(true);
                                        await FranchiseLeadStore.update(lead.id, { ...formData, status: 'VALIDATED' });
                                        loadLead(lead.id);
                                        setSaving(false);
                                    }
                                }}
                                className="px-4 py-2 bg-indigo-600 text-white rounded-lg font-medium hover:bg-indigo-700 flex items-center gap-2 shadow-sm"
                            >
                                <CheckCircle size={18} />
                                Valider le Projet
                            </button>
                        )}

                        {/* Generate Contract - Only if validated or later */}
                        {['VALIDATED', 'CONTRACT_SENT', 'SIGNED'].includes(lead.status) && (
                            <button
                                onClick={async () => {
                                    window.open(`http://localhost:3001/franchise-leads/${lead.id}/contract`, '_blank');
                                    // Refresh to update status to CONTRACT_SENT if needed
                                    setTimeout(() => loadLead(lead.id), 2000);
                                }}
                                className="px-4 py-2 bg-slate-100 text-slate-700 border border-slate-300 rounded-lg font-medium hover:bg-slate-200 flex items-center gap-2"
                            >
                                <FileSignature size={18} />
                                {lead.status === 'VALIDATED' ? 'Générer le Contrat' : 'Voir le Contrat'}
                            </button>
                        )}

                        {/* Sign Contract - Only if Sent */}
                        {lead.status === 'CONTRACT_SENT' && (
                            <button
                                onClick={handleSign}
                                disabled={saving}
                                className="px-4 py-2 bg-emerald-600 rounded-lg text-white font-medium hover:bg-emerald-700 flex items-center gap-2 shadow-lg shadow-emerald-200"
                            >
                                <FileSignature size={18} />
                                Signer le Contrat
                            </button>
                        )}

                        {lead.status === 'SIGNED' && (
                            <div className="px-4 py-2 bg-emerald-50 border border-emerald-200 rounded-lg text-emerald-700 flex items-center gap-2">
                                <CheckCircle size={18} />
                                Contrat Signé & Agence Active
                            </div>
                        )}
                    </div>
                </div>

                <div className="grid grid-cols-3 gap-8">
                    {/* Colonne Gauche : Info Candidat */}
                    <div className="col-span-2 space-y-6">
                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <User className="text-indigo-500" size={20} />
                                Informations Personnelles
                            </h2>
                            <div className="grid grid-cols-2 gap-4">
                                <div className="col-span-2 bg-slate-50 p-4 rounded-xl border border-slate-200 mb-2">
                                    <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                                        <Building2 size={16} className="text-indigo-500" />
                                        Identité Entreprise (Personne Morale)
                                    </h3>
                                    <div className="grid grid-cols-3 gap-4">
                                        <div className="col-span-1">
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">Raison Sociale</label>
                                            <input
                                                type="text"
                                                value={formData.companyName || ''}
                                                onChange={e => setFormData({ ...formData, companyName: e.target.value })}
                                                placeholder="Ex: SAS DUPONT"
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                            />
                                        </div>
                                        <div className="col-span-1">
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">Forme Juridique</label>
                                            <select
                                                value={formData.legalForm || ''}
                                                onChange={e => setFormData({ ...formData, legalForm: e.target.value })}
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                            >
                                                <option value="">Sélectionner...</option>
                                                <option value="SAS">SAS</option>
                                                <option value="SASU">SASU</option>
                                                <option value="SARL">SARL</option>
                                                <option value="EURL">EURL</option>
                                                <option value="EI">EI / Auto-ent</option>
                                            </select>
                                        </div>
                                        <div className="col-span-1">
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">SIRET</label>
                                            <input
                                                type="text"
                                                value={formData.siret || ''}
                                                onChange={e => setFormData({ ...formData, siret: e.target.value })}
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                            />
                                        </div>
                                    </div>
                                </div>

                                <div className="col-span-2">
                                    <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                                        <User size={16} className="text-indigo-500" />
                                        Contact Principal
                                    </h3>
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Nom Complet</label>
                                    <input
                                        type="text"
                                        value={formData.name || ''}
                                        onChange={e => setFormData({ ...formData, name: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Email</label>
                                    <input
                                        type="email"
                                        value={formData.email || ''}
                                        onChange={e => setFormData({ ...formData, email: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Téléphone</label>
                                    <input
                                        type="tel"
                                        value={formData.phone || ''}
                                        onChange={e => setFormData({ ...formData, phone: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                            </div>
                        </div>

                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <MapPin className="text-indigo-500" size={20} />
                                Zone d'Implantation
                            </h2>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Ville Cible</label>
                                    <input
                                        type="text"
                                        value={formData.targetCity || ''}
                                        onChange={e => setFormData({ ...formData, targetCity: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Région</label>
                                    <select
                                        value={formData.region || ''}
                                        onChange={e => setFormData({ ...formData, region: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none"
                                    >
                                        <option value="">Sélectionner...</option>
                                        <option value="IDF">Île-de-France (IDF)</option>
                                        <option value="AURA">Auvergne-Rhône-Alpes (AURA)</option>
                                        <option value="PACA">Provence-Alpes-Côte d'Azur (PACA)</option>
                                        <option value="HDF">Hauts-de-France (HDF)</option>
                                    </select>
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Colonne Droite : Configuration Contrat */}
                    <div className="col-span-1">
                        <div className="bg-slate-50 p-6 rounded-2xl border border-slate-200 mb-6">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <Building2 className="text-indigo-600" size={20} />
                                Configuration Contrat
                            </h2>

                            <div className="space-y-6">
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-2">Type de Partenariat</label>
                                    <div className="grid grid-cols-1 gap-2">
                                        <button
                                            onClick={() => handleContractTypeChange('FRANCHISE')}
                                            className={`p-3 rounded-lg border text-left transition-all ${contractConfig.type === 'FRANCHISE'
                                                ? 'bg-indigo-600 border-indigo-600 text-white shadow-md'
                                                : 'bg-white border-slate-300 text-slate-600 hover:border-indigo-400'
                                                }`}
                                        >
                                            <div className="font-bold text-sm">Franchise Standard</div>
                                            <div className={`text-xs ${contractConfig.type === 'FRANCHISE' ? 'text-indigo-100' : 'text-slate-400'}`}>Entrepreneur indépendant</div>
                                        </button>

                                        <button
                                            onClick={() => handleContractTypeChange('CORNER')}
                                            className={`p-3 rounded-lg border text-left transition-all ${contractConfig.type === 'CORNER'
                                                ? 'bg-purple-600 border-purple-600 text-white shadow-md'
                                                : 'bg-white border-slate-300 text-slate-600 hover:border-purple-400'
                                                }`}
                                        >
                                            <div className="font-bold text-sm">Corner / Borne</div>
                                            <div className={`text-xs ${contractConfig.type === 'CORNER' ? 'text-purple-100' : 'text-slate-400'}`}>Point relais automatisé</div>
                                        </button>
                                    </div>
                                </div>

                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Taux de Commission (%)</label>
                                    <div className="flex items-center gap-2">
                                        <input
                                            type="number"
                                            value={contractConfig.commissionRate}
                                            onChange={e => setContractConfig({ ...contractConfig, commissionRate: Number(e.target.value) })}
                                            className="w-24 p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none font-mono font-bold text-right"
                                        />
                                        <span className="text-slate-500">%</span>
                                    </div>
                                    <p className="text-xs text-slate-400 mt-1">
                                        {contractConfig.type === 'FRANCHISE' ? 'Standard: 15%' : 'Standard: 5%'}
                                    </p>
                                </div>

                                <div className="pt-4 border-t border-slate-200">
                                    <div className="flex items-center gap-2 p-3 bg-yellow-50 text-yellow-800 rounded-lg text-xs">
                                        <AlertCircle size={16} className="shrink-0" />
                                        <p>La création de l'agence et du compte gérant est automatique à la signature.</p>
                                    </div>
                                </div>
                            </div>
                        </div>

                        {/* Activity Feed */}
                        <div className="h-[500px]">
                            <LeadActivityFeed
                                lead={lead}
                                onUpdate={() => loadLead(lead.id)}
                            />
                        </div>
                    </div>
                </div>
            </div>
        </>
    );
}
