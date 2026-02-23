'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { FranchiseLead, FranchiseLeadStore } from '../../../../services/FranchiseLeadStore';
import DashboardLayout from '../../../../components/admin/DashboardLayout';
import LeadActivityFeed from '../../../../components/admin/LeadActivityFeed';
import { AuthStore } from '../../../../services/authStore';
import { ArrowLeft, Save, FileSignature, CheckCircle, AlertCircle, Building2, User, MapPin, FileUser, Mail, Clock, Shield, ScrollText } from 'lucide-react';

export default function FranchiseLeadDetailPage() {
    const params = useParams();
    const router = useRouter();
    const [lead, setLead] = useState<FranchiseLead | null>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [formData, setFormData] = useState<Partial<FranchiseLead>>({});
    const [contractConfig, setContractConfig] = useState<any>({
        isExclusive: true,
        type: 'FRANCHISE',
        commissionRate: 15
    });
    const [documents, setDocuments] = useState<any[]>([]);
    const [contractHistory, setContractHistory] = useState<any[]>([]);

    // Loi Doubin fields
    const [loiDoubinFields, setLoiDoubinFields] = useState({
        entryFee: 0,
        royaltyRate: 15,
        advertisingFee: 2,
        contractDuration: 60, // 5 ans
        terminationNotice: 3,
        nonCompeteDuration: 12,
        exclusiveTerritory: true,
        exclusiveRadius: 15,
        renewalTerms: ''
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
                    setContractConfig((prev: any) => ({ ...prev, ...savedConfig }));
                } catch (e) { console.error(e); }
            }
            if (data.documents) {
                try { setDocuments(JSON.parse(data.documents)); } catch (e) { console.error(e); }
            }
            if (data.contractHistory) {
                try { setContractHistory(JSON.parse(data.contractHistory)); } catch (e) { console.error(e); }
            }
            if (data.rejectionReason) {
                setFormData((prev: any) => ({ ...prev, rejectionReason: data.rejectionReason }));
            }
            // Restore Loi Doubin fields
            setLoiDoubinFields(prev => ({
                ...prev,
                entryFee: data.entryFee ?? prev.entryFee,
                royaltyRate: data.royaltyRate ?? prev.royaltyRate,
                advertisingFee: data.advertisingFee ?? prev.advertisingFee,
                contractDuration: data.contractDuration ?? prev.contractDuration,
                terminationNotice: data.terminationNotice ?? prev.terminationNotice,
                nonCompeteDuration: data.nonCompeteDuration ?? prev.nonCompeteDuration,
                exclusiveTerritory: data.exclusiveTerritory ?? prev.exclusiveTerritory,
                exclusiveRadius: data.exclusiveRadius ?? prev.exclusiveRadius,
                renewalTerms: data.renewalTerms ?? prev.renewalTerms,
            }));
        }
        setLoading(false);
    };

    const handleRelancer = async () => {
        if (!lead) return;
        setSaving(true);
        await FranchiseLeadStore.addNote(lead.id, {
            content: "📧 Email de relance envoyé automatiquement : 'SimuLegal - Votre projet de franchise est toujours en cours d'étude.'",
            author: currentUser.name,
            type: 'EMAIL'
        });
        alert('Email de relance envoyé au candidat.');
        setSaving(false);
    };

    const handleReject = async () => {
        const reason = prompt("Motif du rejet (obligatoire) :");
        if (!reason || !lead) return;
        setSaving(true);
        const updatedLead = await FranchiseLeadStore.update(lead.id, { status: 'REJECTED', rejectionReason: reason });
        if (updatedLead) {
            setLead(updatedLead);
            setFormData((prev: any) => ({ ...prev, rejectionReason: reason }));
            alert('Candidature rejetée.');
        }
        setSaving(false);
    };

    const handleAddDocument = async (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file || !lead) return;
        setSaving(true);
        const newDoc = {
            id: Date.now().toString(),
            label: file.name,
            status: 'RECEIVED',
            uploadedAt: new Date().toISOString(),
            size: `${(file.size / 1024).toFixed(0)} KB`
        };
        const updatedDocs = [...documents, newDoc];
        const res = await FranchiseLeadStore.updateDocuments(lead.id, updatedDocs);
        if (res) {
            setDocuments(updatedDocs);
            setLead(res);
            await FranchiseLeadStore.addNote(lead.id, {
                content: `📎 Nouveau document importé : ${file.name}`,
                author: currentUser.name,
                type: 'NOTE'
            });
        }
        setSaving(false);
    };

    const handleLogContractVersion = async (versionLabel: string) => {
        if (!lead) return;
        const res = await FranchiseLeadStore.logContractHistory(lead.id, {
            label: versionLabel,
            details: { ...contractConfig, ...loiDoubinFields }
        }, lead.contractHistory);
        if (res) {
            setLead(res);
            setContractHistory(JSON.parse(res.contractHistory || '[]'));
            alert('Version du contrat archivée.');
        }
    };

    const handleContractTypeChange = (type: string) => {
        const newRate = type === 'CORNER' ? 5 : 15;
        setContractConfig((prev: any) => ({ ...prev, type, commissionRate: newRate }));
        setLoiDoubinFields(prev => ({ ...prev, royaltyRate: newRate }));
    };

    const handleSave = async () => {
        if (!lead) return;
        setSaving(true);
        const updatedData = {
            ...formData,
            contractDetails: JSON.stringify(contractConfig),
            // Loi Doubin fields
            entryFee: loiDoubinFields.entryFee,
            royaltyRate: loiDoubinFields.royaltyRate,
            advertisingFee: loiDoubinFields.advertisingFee,
            contractDuration: loiDoubinFields.contractDuration,
            terminationNotice: loiDoubinFields.terminationNotice,
            nonCompeteDuration: loiDoubinFields.nonCompeteDuration,
            exclusiveTerritory: loiDoubinFields.exclusiveTerritory,
            exclusiveRadius: loiDoubinFields.exclusiveRadius,
            renewalTerms: loiDoubinFields.renewalTerms || undefined,
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

    const handleSendDIP = async () => {
        if (!lead) return;
        if (!confirm("Envoyer le DIP (Document d'Information Précontractuelle) au candidat ?\nCela démarrera le délai légal de réflexion de 20 jours.")) return;
        // Save first
        await handleSave();
        setSaving(true);
        const result = await FranchiseLeadStore.sendDIP(lead.id);
        if (result) {
            setLead(result);
            alert('DIP envoyé avec succès. Le délai de réflexion de 20 jours a démarré.');
        }
        setSaving(false);
    };

    const handleSign = async () => {
        if (!lead || !confirm("Confirmer la signature du contrat ? Une agence sera créée immédiatement.")) return;
        setSaving(true);
        // Save latest changes first
        const updatedData = { ...formData, contractDetails: JSON.stringify(contractConfig) };
        await FranchiseLeadStore.update(lead.id, updatedData);
        const result = await FranchiseLeadStore.signContract(lead.id);
        if (result) {
            alert('Contrat signé avec succès ! L\'agence a été créée.');
            router.push('/admin/franchise-leads');
        }
        setSaving(false);
    };

    if (loading) return <div className="p-8">Chargement...</div>;
    if (!lead) return <div className="p-8">Candidat introuvable</div>;

    const PIPELINE_STEPS = ['NEW', 'CONTACTED', 'VALIDATED', 'DIP_SENT', 'CONTRACT_SENT', 'SIGNED'];
    const STEP_LABELS: Record<string, string> = {
        NEW: 'Nouveau', CONTACTED: 'Contacté', VALIDATED: 'Validé',
        DIP_SENT: 'DIP Envoyé', CONTRACT_SENT: 'Contrat Envoyé', SIGNED: 'Signé'
    };

    const coolingDaysRemaining = lead.coolingPeriodRemaining;
    const canGenerateContract = lead.status === 'DIP_SENT' && coolingDaysRemaining !== null && coolingDaysRemaining !== undefined && coolingDaysRemaining <= 0;

    return (
        <>
            <div className="max-w-5xl mx-auto p-8">
                {/* Stepper Pipeline */}
                <div className="mb-8">
                    <div className="flex items-center justify-between relative">
                        <div className="absolute top-1/2 left-0 w-full h-1 bg-slate-200 -z-10 rounded-full" />
                        {PIPELINE_STEPS.map((step, index) => {
                            const currentStepIndex = PIPELINE_STEPS.indexOf(lead.status);
                            const stepIndex = PIPELINE_STEPS.indexOf(step);
                            const isCompleted = currentStepIndex >= stepIndex;
                            const isCurrent = currentStepIndex === stepIndex;

                            return (
                                <div key={step} className="flex flex-col items-center bg-white px-2">
                                    <div className={`w-8 h-8 rounded-full flex items-center justify-center text-xs font-bold border-2 transition-colors
                                         ${isCompleted ? 'bg-indigo-600 border-indigo-600 text-white' : 'bg-white border-slate-300 text-slate-400'}
                                         ${isCurrent ? 'ring-4 ring-indigo-100' : ''}
                                     `}>
                                        {isCompleted ? <CheckCircle size={14} /> : stepIndex + 1}
                                    </div>
                                    <span className={`text-xs mt-2 font-medium ${isCompleted ? 'text-indigo-700' : 'text-slate-400'}`}>
                                        {STEP_LABELS[step]}
                                    </span>
                                </div>
                            );
                        })}
                    </div>
                </div>

                {/* Cooling Period Banner */}
                {lead.status === 'DIP_SENT' && coolingDaysRemaining !== null && coolingDaysRemaining !== undefined && (
                    <div className={`mb-6 p-4 rounded-2xl border flex items-center gap-3 ${coolingDaysRemaining > 0
                        ? 'bg-amber-50 border-amber-200 text-amber-800'
                        : 'bg-emerald-50 border-emerald-200 text-emerald-800'
                        }`}>
                        <Clock size={20} className="shrink-0" />
                        <div>
                            <p className="font-bold text-sm">
                                {coolingDaysRemaining > 0
                                    ? `Délai de réflexion légal : ${coolingDaysRemaining} jour(s) restant(s)`
                                    : '✅ Délai de réflexion écoulé — Vous pouvez générer le contrat'}
                            </p>
                            <p className="text-xs mt-0.5 opacity-75">
                                Art. L330-3 al. 2 du Code de commerce — DIP envoyé le {lead.dipSentAt ? new Date(lead.dipSentAt).toLocaleDateString('fr-FR') : '—'}
                            </p>
                        </div>
                    </div>
                )}

                {/* Header */}
                <div className="flex items-center justify-between mb-8">
                    <div className="flex items-center gap-4">
                        <button onClick={() => router.back()} className="p-2 hover:bg-slate-200 rounded-full transition-colors">
                            <ArrowLeft size={24} className="text-slate-600" />
                        </button>
                        <div>
                            <h1 className="text-3xl font-bold text-slate-900">{formData.name}</h1>
                            <div className="text-slate-500 text-sm">Candidat ID: {lead.id}</div>
                        </div>
                    </div>
                    <div className="flex gap-3">
                        {/* Save (Always unless signed) */}
                        {lead.status !== 'SIGNED' && (
                            <button onClick={handleSave} disabled={saving}
                                className="px-4 py-2 bg-white border border-slate-300 rounded-lg text-slate-700 font-medium hover:bg-slate-50 flex items-center gap-2">
                                <Save size={18} /> Enregistrer
                            </button>
                        )}

                        {/* Workflow: Mark as Contacted */}
                        {lead.status === 'NEW' && (
                            <button
                                onClick={async () => {
                                    setSaving(true);
                                    await FranchiseLeadStore.update(lead.id, { ...formData, status: 'CONTACTED' });
                                    loadLead(lead.id);
                                    setSaving(false);
                                }}
                                className="px-4 py-2 bg-blue-600 text-white rounded-lg font-medium hover:bg-blue-700 flex items-center gap-2 shadow-sm">
                                <User size={18} /> Marquer comme Contacté
                            </button>
                        )}

                        {/* Workflow: Validate Project */}
                        {lead.status === 'CONTACTED' && (
                            <button
                                onClick={async () => {
                                    if (!formData.companyName || !formData.siret) {
                                        alert('Merci de renseigner les informations entreprise avant de valider.');
                                        return;
                                    }
                                    if (confirm('Valider le projet ? Cela débloquera l\'envoi du DIP.')) {
                                        setSaving(true);
                                        await FranchiseLeadStore.update(lead.id, { ...formData, status: 'VALIDATED' });
                                        loadLead(lead.id);
                                        setSaving(false);
                                    }
                                }}
                                className="px-4 py-2 bg-indigo-600 text-white rounded-lg font-medium hover:bg-indigo-700 flex items-center gap-2 shadow-sm">
                                <CheckCircle size={18} /> Valider le Projet
                            </button>
                        )}

                        {/* Workflow: Send DIP (Loi Doubin) */}
                        {lead.status === 'VALIDATED' && (
                            <button
                                onClick={handleSendDIP}
                                disabled={saving}
                                className="px-4 py-2 bg-cyan-600 text-white rounded-lg font-medium hover:bg-cyan-700 flex items-center gap-2 shadow-lg shadow-cyan-200">
                                <ScrollText size={18} /> Envoyer le DIP
                            </button>
                        )}

                        {/* DIP Download */}
                        {['DIP_SENT', 'CONTRACT_SENT', 'SIGNED'].includes(lead.status) && (
                            <button
                                onClick={() => window.open(`http://localhost:5000/franchise-leads/${lead.id}/dip`, '_blank')}
                                className="px-4 py-2 bg-slate-100 text-slate-700 border border-slate-300 rounded-lg font-medium hover:bg-slate-200 flex items-center gap-2">
                                <ScrollText size={18} /> Voir DIP
                            </button>
                        )}

                        {/* Generate Contract — Only after 20-day cooling */}
                        {canGenerateContract && (
                            <button
                                onClick={async () => {
                                    window.open(`http://localhost:5000/franchise-leads/${lead.id}/contract`, '_blank');
                                    setTimeout(() => loadLead(lead.id), 2000);
                                }}
                                className="px-4 py-2 bg-orange-500 text-white rounded-lg font-medium hover:bg-orange-600 flex items-center gap-2 shadow-sm">
                                <FileSignature size={18} /> Générer le Contrat
                            </button>
                        )}

                        {/* View Contract */}
                        {['CONTRACT_SENT', 'SIGNED'].includes(lead.status) && (
                            <button
                                onClick={() => window.open(`http://localhost:5000/franchise-leads/${lead.id}/contract`, '_blank')}
                                className="px-4 py-2 bg-slate-100 text-slate-700 border border-slate-300 rounded-lg font-medium hover:bg-slate-200 flex items-center gap-2">
                                <FileSignature size={18} /> Voir Contrat
                            </button>
                        )}

                        {/* Sign Contract */}
                        {lead.status === 'CONTRACT_SENT' && (
                            <button onClick={handleSign} disabled={saving}
                                className="px-4 py-2 bg-emerald-600 rounded-lg text-white font-medium hover:bg-emerald-700 flex items-center gap-2 shadow-lg shadow-emerald-200">
                                <FileSignature size={18} /> Signer le Contrat
                            </button>
                        )}

                        {lead.status === 'SIGNED' && (
                            <div className="px-4 py-2 bg-emerald-50 border border-emerald-200 rounded-lg text-emerald-700 flex items-center gap-2">
                                <CheckCircle size={18} /> Contrat Signé & Agence Active
                            </div>
                        )}

                        {/* Relancer / Rejeter */}
                        {lead.status !== 'SIGNED' && lead.status !== 'REJECTED' && (
                            <div className="flex gap-2">
                                <button onClick={handleRelancer}
                                    className="px-4 py-2 bg-amber-50 text-amber-700 border border-amber-200 rounded-lg font-medium hover:bg-amber-100 flex items-center gap-2">
                                    <Mail size={18} /> Relancer
                                </button>
                                <button onClick={handleReject}
                                    className="px-4 py-2 bg-red-50 text-red-700 border border-red-200 rounded-lg font-medium hover:bg-red-100 flex items-center gap-2">
                                    <AlertCircle size={18} /> Rejeter
                                </button>
                            </div>
                        )}

                        {lead.status === 'REJECTED' && (
                            <div className="px-4 py-2 bg-red-50 border border-red-200 rounded-lg text-red-700 flex items-center gap-2">
                                <AlertCircle size={18} /> Candidature Rejetée
                            </div>
                        )}
                    </div>
                </div>

                {lead.status === 'REJECTED' && (
                    <div className="mb-8 p-6 bg-red-50 border border-red-100 rounded-2xl">
                        <h3 className="text-red-800 font-bold mb-1 flex items-center gap-2">
                            <AlertCircle size={18} /> Motif du rejet
                        </h3>
                        <p className="text-red-700 text-sm italic">{lead.rejectionReason || 'Aucun motif renseigné'}</p>
                    </div>
                )}

                <div className="grid grid-cols-3 gap-8">
                    {/* Colonne Gauche */}
                    <div className="col-span-2 space-y-6">
                        {/* Infos Personnelles */}
                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <User className="text-indigo-500" size={20} /> Informations Personnelles
                            </h2>
                            <div className="grid grid-cols-2 gap-4">
                                <div className="col-span-2 bg-slate-50 p-4 rounded-xl border border-slate-200 mb-2">
                                    <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                                        <Building2 size={16} className="text-indigo-500" /> Identité Entreprise
                                    </h3>
                                    <div className="grid grid-cols-3 gap-4">
                                        <div>
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">Raison Sociale</label>
                                            <input type="text" value={formData.companyName || ''} onChange={e => setFormData({ ...formData, companyName: e.target.value })} placeholder="Ex: SAS DUPONT"
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                        </div>
                                        <div>
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">Forme Juridique</label>
                                            <select value={formData.legalForm || ''} onChange={e => setFormData({ ...formData, legalForm: e.target.value })}
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none">
                                                <option value="">Sélectionner...</option>
                                                <option value="SAS">SAS</option><option value="SASU">SASU</option>
                                                <option value="SARL">SARL</option><option value="EURL">EURL</option>
                                                <option value="EI">EI / Auto-ent</option>
                                            </select>
                                        </div>
                                        <div>
                                            <label className="block text-xs font-semibold text-slate-500 mb-1">SIRET</label>
                                            <input type="text" value={formData.siret || ''} onChange={e => setFormData({ ...formData, siret: e.target.value })}
                                                className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                        </div>
                                    </div>
                                </div>
                                <div className="col-span-2">
                                    <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                                        <User size={16} className="text-indigo-500" /> Contact Principal
                                    </h3>
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Nom Complet</label>
                                    <input type="text" value={formData.name || ''} onChange={e => setFormData({ ...formData, name: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Email</label>
                                    <input type="email" value={formData.email || ''} onChange={e => setFormData({ ...formData, email: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Téléphone</label>
                                    <input type="tel" value={formData.phone || ''} onChange={e => setFormData({ ...formData, phone: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                </div>
                            </div>
                        </div>

                        {/* Zone d'implantation */}
                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <MapPin className="text-indigo-500" size={20} /> Zone d'Implantation
                            </h2>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Ville Cible</label>
                                    <input type="text" value={formData.targetCity || ''} onChange={e => setFormData({ ...formData, targetCity: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none" />
                                </div>
                                <div>
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Région</label>
                                    <select value={formData.region || ''} onChange={e => setFormData({ ...formData, region: e.target.value })}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none">
                                        <option value="">Sélectionner...</option>
                                        <option value="IDF">Île-de-France (IDF)</option><option value="AURA">Auvergne-Rhône-Alpes (AURA)</option>
                                        <option value="PACA">Provence-Alpes-Côte d'Azur (PACA)</option><option value="HDF">Hauts-de-France (HDF)</option>
                                        <option value="NAQ">Nouvelle-Aquitaine (NAQ)</option><option value="OCC">Occitanie (OCC)</option>
                                        <option value="BRE">Bretagne (BRE)</option><option value="NOR">Normandie (NOR)</option>
                                        <option value="GES">Grand Est (GES)</option><option value="PDL">Pays de la Loire (PDL)</option>
                                    </select>
                                </div>
                            </div>
                        </div>

                        {/* ====== LOI DOUBIN — Conditions contractuelles ====== */}
                        <div className="bg-white p-6 rounded-2xl shadow-sm border-2 border-cyan-200">
                            <h2 className="text-lg font-bold text-slate-800 mb-1 flex items-center gap-2">
                                <Shield className="text-cyan-600" size={20} /> Conditions Contractuelles — Loi Doubin
                            </h2>
                            <p className="text-xs text-slate-400 mb-4">Art. L330-3 & R330-1 du Code de Commerce</p>

                            {/* Row 1: Financial */}
                            <div className="bg-cyan-50 p-4 rounded-xl border border-cyan-100 mb-4">
                                <h3 className="text-xs font-bold text-cyan-700 mb-3 uppercase tracking-wider">Conditions Financières (Art. R330-1, 3°)</h3>
                                <div className="grid grid-cols-3 gap-4">
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Droit d'entrée (€ HT)</label>
                                        <input type="number" value={loiDoubinFields.entryFee / 100} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, entryFee: Math.round(Number(e.target.value) * 100) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-cyan-500 outline-none font-mono" />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Redevance (%)</label>
                                        <input type="number" step="0.5" value={loiDoubinFields.royaltyRate} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, royaltyRate: Number(e.target.value) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-cyan-500 outline-none font-mono" />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Fonds Publicité (%)</label>
                                        <input type="number" step="0.5" value={loiDoubinFields.advertisingFee} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, advertisingFee: Number(e.target.value) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-cyan-500 outline-none font-mono" />
                                    </div>
                                </div>
                            </div>

                            {/* Row 2: Duration & Termination */}
                            <div className="bg-slate-50 p-4 rounded-xl border border-slate-200 mb-4">
                                <h3 className="text-xs font-bold text-slate-600 mb-3 uppercase tracking-wider">Durée & Résiliation (Art. R330-1, 4° & 6°)</h3>
                                <div className="grid grid-cols-3 gap-4">
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Durée contrat (mois)</label>
                                        <input type="number" value={loiDoubinFields.contractDuration} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, contractDuration: Number(e.target.value) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none font-mono" />
                                        <p className="text-[10px] text-slate-400 mt-1">{(loiDoubinFields.contractDuration / 12).toFixed(1)} ans</p>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Préavis résiliation (mois)</label>
                                        <input type="number" value={loiDoubinFields.terminationNotice} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, terminationNotice: Number(e.target.value) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none font-mono" />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-semibold text-slate-500 mb-1">Non-concurrence (mois)</label>
                                        <input type="number" value={loiDoubinFields.nonCompeteDuration} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, nonCompeteDuration: Number(e.target.value) })}
                                            className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none font-mono" />
                                    </div>
                                </div>
                                <div className="mt-3">
                                    <label className="block text-xs font-semibold text-slate-500 mb-1">Conditions de renouvellement</label>
                                    <textarea value={loiDoubinFields.renewalTerms} onChange={e => setLoiDoubinFields({ ...loiDoubinFields, renewalTerms: e.target.value })}
                                        placeholder="Tacite reconduction sauf dénonciation..." rows={2}
                                        className="w-full p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none text-sm" />
                                </div>
                            </div>

                            {/* Row 3: Exclusivity */}
                            <div className="bg-slate-50 p-4 rounded-xl border border-slate-200">
                                <h3 className="text-xs font-bold text-slate-600 mb-3 uppercase tracking-wider">Exclusivité Territoriale (Art. R330-1, 5°)</h3>
                                <div className="flex items-center gap-4">
                                    <label className="flex items-center gap-2 cursor-pointer">
                                        <input type="checkbox" checked={loiDoubinFields.exclusiveTerritory}
                                            onChange={e => setLoiDoubinFields({ ...loiDoubinFields, exclusiveTerritory: e.target.checked })}
                                            className="w-4 h-4 rounded border-slate-300 text-indigo-600 focus:ring-indigo-500" />
                                        <span className="text-sm font-medium text-slate-700">Exclusivité territoriale</span>
                                    </label>
                                    {loiDoubinFields.exclusiveTerritory && (
                                        <div className="flex items-center gap-2">
                                            <label className="text-xs font-semibold text-slate-500">Rayon :</label>
                                            <input type="number" value={loiDoubinFields.exclusiveRadius}
                                                onChange={e => setLoiDoubinFields({ ...loiDoubinFields, exclusiveRadius: Number(e.target.value) })}
                                                className="w-20 p-2 border border-slate-300 rounded-lg focus:ring-2 focus:ring-indigo-500 outline-none font-mono text-sm" />
                                            <span className="text-xs text-slate-500">km</span>
                                        </div>
                                    )}
                                </div>
                            </div>
                        </div>

                        {/* Documents */}
                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200">
                            <div className="flex items-center justify-between mb-4">
                                <h2 className="text-lg font-bold text-slate-800 flex items-center gap-2">
                                    <FileUser className="text-indigo-500" size={20} /> Documents & Justificatifs
                                </h2>
                                <div>
                                    <input type="file" id="doc-upload" className="hidden" onChange={handleAddDocument} disabled={saving} />
                                    <label htmlFor="doc-upload" className="text-xs font-bold text-indigo-600 hover:text-indigo-700 bg-indigo-50 px-3 py-1 rounded-full transition-colors cursor-pointer">
                                        + Ajouter
                                    </label>
                                </div>
                            </div>
                            {documents.length === 0 ? (
                                <div className="text-center py-8 bg-slate-50 rounded-xl border border-dashed border-slate-300">
                                    <p className="text-slate-400 text-sm italic">Aucun document importé</p>
                                </div>
                            ) : (
                                <div className="grid grid-cols-2 gap-3">
                                    {documents.map(doc => (
                                        <div key={doc.id} className="flex items-center justify-between p-3 bg-slate-50 rounded-xl border border-slate-200">
                                            <div className="flex items-center gap-3">
                                                <div className="p-2 bg-indigo-100 text-indigo-600 rounded-lg"><FileUser size={16} /></div>
                                                <div>
                                                    <div className="text-sm font-bold text-slate-700 truncate max-w-[150px]">{doc.label}</div>
                                                    <div className="text-[10px] text-slate-400">{new Date(doc.uploadedAt).toLocaleDateString()} {doc.size && `• ${doc.size}`}</div>
                                                </div>
                                            </div>
                                            <div className={`text-[10px] font-bold px-2 py-1 rounded-full border uppercase ${doc.status === 'RECEIVED' ? 'bg-emerald-50 text-emerald-600 border-emerald-100' : 'bg-white text-slate-500 border-slate-200'}`}>
                                                {doc.status}
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            )}
                        </div>
                    </div>

                    {/* Colonne Droite */}
                    <div className="col-span-1">
                        {/* Config Contrat */}
                        <div className="bg-slate-50 p-6 rounded-2xl border border-slate-200 mb-6">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <Building2 className="text-indigo-600" size={20} /> Type de Partenariat
                            </h2>
                            <div className="space-y-4">
                                <div className="grid grid-cols-1 gap-2">
                                    <button onClick={() => handleContractTypeChange('FRANCHISE')}
                                        className={`p-3 rounded-lg border text-left transition-all ${contractConfig.type === 'FRANCHISE' ? 'bg-indigo-600 border-indigo-600 text-white shadow-md' : 'bg-white border-slate-300 text-slate-600 hover:border-indigo-400'}`}>
                                        <div className="font-bold text-sm">Franchise Standard</div>
                                        <div className={`text-xs ${contractConfig.type === 'FRANCHISE' ? 'text-indigo-100' : 'text-slate-400'}`}>Entrepreneur indépendant</div>
                                    </button>
                                    <button onClick={() => handleContractTypeChange('CORNER')}
                                        className={`p-3 rounded-lg border text-left transition-all ${contractConfig.type === 'CORNER' ? 'bg-purple-600 border-purple-600 text-white shadow-md' : 'bg-white border-slate-300 text-slate-600 hover:border-purple-400'}`}>
                                        <div className="font-bold text-sm">Corner / Borne</div>
                                        <div className={`text-xs ${contractConfig.type === 'CORNER' ? 'text-purple-100' : 'text-slate-400'}`}>Point relais automatisé</div>
                                    </button>
                                </div>

                                <div className="p-4 bg-indigo-50 rounded-xl border border-indigo-100">
                                    <h3 className="text-xs font-bold text-indigo-700 mb-2 uppercase tracking-wider">Projections Financières (Est.)</h3>
                                    <div className="space-y-2">
                                        <div className="flex justify-between text-sm">
                                            <span className="text-indigo-600">CA Mensuel Cible</span>
                                            <span className="font-bold text-indigo-900">45 000 €</span>
                                        </div>
                                        <div className="flex justify-between text-sm">
                                            <span className="text-indigo-600">Redevance ({loiDoubinFields.royaltyRate}%)</span>
                                            <span className="font-bold text-indigo-900">{(45000 * loiDoubinFields.royaltyRate / 100).toLocaleString()} €</span>
                                        </div>
                                        {loiDoubinFields.entryFee > 0 && (
                                            <div className="flex justify-between text-sm">
                                                <span className="text-indigo-600">Droit d'entrée</span>
                                                <span className="font-bold text-indigo-900">{(loiDoubinFields.entryFee / 100).toLocaleString()} €</span>
                                            </div>
                                        )}
                                    </div>
                                    <div className="mt-3 pt-3 border-t border-indigo-100 text-[10px] text-indigo-500 italic">
                                        Basé sur la moyenne du réseau SimuLegal.
                                    </div>
                                </div>

                                <div className="pt-4 border-t border-slate-200">
                                    <div className="flex items-center gap-2 p-3 bg-cyan-50 text-cyan-800 rounded-lg text-xs">
                                        <Shield size={16} className="shrink-0" />
                                        <p>Conforme Loi Doubin : DIP + délai 20 jours + contrat complet obligatoires avant signature.</p>
                                    </div>
                                </div>
                            </div>
                        </div>

                        {/* Contract History */}
                        <div className="bg-white p-6 rounded-2xl shadow-sm border border-slate-200 mb-6">
                            <h2 className="text-lg font-bold text-slate-800 mb-4 flex items-center gap-2">
                                <FileSignature className="text-slate-600" size={20} /> Historique des Contrats
                            </h2>
                            <div className="space-y-3">
                                {contractHistory.length === 0 ? (
                                    <p className="text-xs text-slate-400 italic">Aucune archive</p>
                                ) : (
                                    contractHistory.slice().reverse().map((h, i) => (
                                        <div key={i} className="p-3 bg-slate-50 rounded-xl border border-slate-200 text-xs">
                                            <div className="flex justify-between font-bold text-slate-700 mb-1">
                                                <span>{h.label}</span>
                                                <span className="text-slate-400 font-normal">{new Date(h.timestamp).toLocaleDateString()}</span>
                                            </div>
                                            <div className="text-slate-500">
                                                Redev. : {h.details?.royaltyRate || h.details?.commissionRate}% | Type : {h.details?.type}
                                            </div>
                                        </div>
                                    ))
                                )}
                                <button onClick={() => handleLogContractVersion(`Version ${contractHistory.length + 1}`)}
                                    className="w-full mt-2 py-2 text-xs font-bold text-slate-600 border border-dashed border-slate-300 rounded-lg hover:bg-slate-50">
                                    Archiver la configuration actuelle
                                </button>
                            </div>
                        </div>

                        {/* Activity Feed */}
                        <div className="h-[400px]">
                            <LeadActivityFeed lead={lead} onUpdate={() => loadLead(lead.id)} />
                        </div>
                    </div>
                </div>
            </div>
        </>
    );
}
