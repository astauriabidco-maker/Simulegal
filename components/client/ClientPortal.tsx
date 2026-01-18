'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { CRM, Lead, LeadDocument } from '../../services/crmStore';
import { WorkflowService, WorkflowStage } from '../../services/WorkflowService';
import { DocumentAnalysisService, AnalysisResponse } from '../../services/DocumentAnalysisService';
import GuidedScanner from './GuidedScanner';
import {
    CheckCircle,
    Circle,
    Clock,
    FileText,
    Camera,
    MessageCircle,
    LogOut,
    ChevronRight,
    Loader2,
    AlertCircle,
    PartyPopper
} from 'lucide-react';

interface ClientPortalProps {
    leadId: string;
}

// Mapping des documents requis par service (simplifié)
const getRequiredDocuments = (serviceId: string): Array<{ id: string; label: string; description: string; required: boolean }> => {
    const commonDocs = [
        { id: 'IDENTITY', label: 'Pièce d\'identité', description: 'Passeport ou carte d\'identité en cours de validité', required: true },
        { id: 'PROOF_ADDRESS', label: 'Justificatif de domicile', description: 'Facture de moins de 3 mois (EDF, eau, internet...)', required: true },
    ];

    if (serviceId.includes('permis')) {
        return [
            ...commonDocs,
            { id: 'PERMIS_ETRANGER', label: 'Permis étranger', description: 'Photo recto-verso de votre permis actuel', required: true },
            { id: 'PHOTO_ID', label: 'Photo d\'identité', description: 'Photo format officiel (e-photo ou photographe)', required: true },
        ];
    }

    if (serviceId.includes('naturalisation')) {
        return [
            ...commonDocs,
            { id: 'TITRE_SEJOUR', label: 'Titre de séjour', description: 'Recto-verso de votre titre actuel', required: true },
            { id: 'ACTE_NAISSANCE', label: 'Acte de naissance', description: 'Copie intégrale traduite', required: true },
            { id: 'DIPLOME_FRANCAIS', label: 'Diplôme de français', description: 'Attestation niveau B1 minimum', required: true },
            { id: 'AVIS_IMPOSITION', label: 'Avis d\'imposition', description: '3 dernières années', required: true },
        ];
    }

    if (serviceId.includes('regroupement')) {
        return [
            ...commonDocs,
            { id: 'TITRE_SEJOUR', label: 'Titre de séjour', description: 'Votre titre actuel (recto-verso)', required: true },
            { id: 'ACTE_MARIAGE', label: 'Acte de mariage', description: 'Copie intégrale traduite', required: true },
            { id: 'PREUVES_RESSOURCES', label: 'Preuves de ressources', description: 'Fiches de paie des 12 derniers mois', required: true },
            { id: 'ATTESTATION_LOGEMENT', label: 'Attestation logement', description: 'Superficie minimum requise', required: true },
        ];
    }

    return commonDocs;
};

export default function ClientPortal({ leadId }: ClientPortalProps) {
    const [lead, setLead] = useState<Lead | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [showScanner, setShowScanner] = useState(false);
    const [error, setError] = useState<string | null>(null);

    // Chargement du lead
    useEffect(() => {
        const loadLead = async () => {
            try {
                const foundLead = await CRM.getLeadById(leadId);
                if (foundLead) {
                    setLead(foundLead);
                } else {
                    setError('Dossier introuvable');
                }
            } catch (err) {
                console.error('[PORTAL] Erreur chargement lead:', err);
                setError('Erreur de chargement');
            } finally {
                setIsLoading(false);
            }
        };

        loadLead();
    }, [leadId]);

    // Gestion de l'upload de document
    const handleDocumentUploaded = useCallback(async (docId: string, file: File, result: AnalysisResponse) => {
        if (!lead) return;

        const newDoc: LeadDocument = {
            id: docId,
            docType: docId,
            fileName: file.name,
            fileUrl: URL.createObjectURL(file),
            uploadedAt: new Date().toISOString(),
            status: result.status === 'VALID' ? 'VALID' : 'REJECTED',
            aiConfidence: result.confidence,
            rejectionReason: result.status !== 'VALID' ? result.message : undefined,
            validatedBy: 'AI',
            validatedAt: new Date().toISOString()
        };

        // Mise à jour locale
        const updatedDocs = [...(lead.documents || [])];
        const existingIndex = updatedDocs.findIndex(d => d.id === docId);
        if (existingIndex >= 0) {
            updatedDocs[existingIndex] = newDoc;
        } else {
            updatedDocs.push(newDoc);
        }

        setLead({ ...lead, documents: updatedDocs });

        // Persister (optionnel - dépend de l'API)
        // await CRM.updateLead(lead.id, { documents: updatedDocs });
    }, [lead]);

    // Vérifier si tous les documents requis sont validés
    const requiredDocs = lead ? getRequiredDocuments(lead.serviceId) : [];
    const validDocs = lead?.documents?.filter(d => d.status === 'VALID') || [];
    const missingDocs = requiredDocs.filter(req => !validDocs.find(v => v.id === req.id));
    const allDocsValid = missingDocs.length === 0;

    // Calcul de la progression
    const progressPercent = lead
        ? WorkflowService.getProgress(lead.serviceId, lead.currentStage)
        : 0;

    // Loading
    if (isLoading) {
        return (
            <div className="min-h-screen bg-slate-50 flex items-center justify-center">
                <Loader2 className="text-indigo-600 animate-spin" size={40} />
            </div>
        );
    }

    // Error
    if (error || !lead) {
        return (
            <div className="min-h-screen bg-slate-50 flex flex-col items-center justify-center p-6">
                <AlertCircle className="text-red-500 mb-4" size={48} />
                <h1 className="text-xl font-bold text-slate-900 mb-2">Oops!</h1>
                <p className="text-slate-500">{error || 'Dossier introuvable'}</p>
            </div>
        );
    }

    // Scanner mode
    if (showScanner) {
        return (
            <GuidedScanner
                documents={requiredDocs.map(doc => ({
                    ...doc,
                    icon: undefined
                }))}
                existingDocs={lead.documents || []}
                onDocumentUploaded={handleDocumentUploaded}
                onComplete={() => setShowScanner(false)}
                onClose={() => setShowScanner(false)}
            />
        );
    }

    // Étapes du workflow
    const workflowSteps = WorkflowService.getStepsForService(lead.serviceId);
    const currentStageIndex = WorkflowService.getStageIndex(lead.serviceId, lead.currentStage);

    return (
        <div className="min-h-screen bg-slate-50 pb-24">
            {/* Header */}
            <div className="bg-gradient-to-br from-indigo-600 to-purple-700 text-white p-6 pb-16">
                <div className="flex items-center justify-between mb-6">
                    <div>
                        <p className="text-white/60 text-sm">Bonjour</p>
                        <h1 className="text-xl font-bold">{lead.name}</h1>
                    </div>
                    <button className="p-2 bg-white/10 rounded-xl">
                        <LogOut size={20} />
                    </button>
                </div>

                {/* Service info */}
                <div className="bg-white/10 backdrop-blur-sm rounded-2xl p-4">
                    <p className="text-white/60 text-xs uppercase tracking-wide mb-1">Votre dossier</p>
                    <p className="font-bold">{lead.serviceName}</p>
                    <div className="flex items-center gap-2 mt-2">
                        <div className="flex-1 h-2 bg-white/20 rounded-full overflow-hidden">
                            <div
                                className="h-full bg-white rounded-full transition-all duration-500"
                                style={{ width: `${progressPercent}%` }}
                            />
                        </div>
                        <span className="text-sm font-bold">{progressPercent}%</span>
                    </div>
                </div>
            </div>

            {/* Carte flottante - Statut actuel */}
            <div className="px-4 -mt-8">
                <div className="bg-white rounded-3xl shadow-lg p-6">
                    <div className="flex items-center gap-4 mb-4">
                        <div className={`w-14 h-14 rounded-2xl flex items-center justify-center ${allDocsValid ? 'bg-emerald-100' : 'bg-amber-100'
                            }`}>
                            {allDocsValid ? (
                                <CheckCircle className="text-emerald-600" size={28} />
                            ) : (
                                <Camera className="text-amber-600" size={28} />
                            )}
                        </div>
                        <div className="flex-1">
                            <h2 className="font-bold text-slate-900">
                                {allDocsValid ? 'Documents complets' : 'Documents à fournir'}
                            </h2>
                            <p className="text-slate-500 text-sm">
                                {allDocsValid
                                    ? 'Votre dossier est en cours de traitement'
                                    : `${missingDocs.length} document${missingDocs.length > 1 ? 's' : ''} manquant${missingDocs.length > 1 ? 's' : ''}`
                                }
                            </p>
                        </div>
                    </div>

                    {!allDocsValid && (
                        <button
                            onClick={() => setShowScanner(true)}
                            className="w-full py-4 bg-gradient-to-r from-indigo-600 to-purple-600 text-white rounded-2xl font-bold flex items-center justify-center gap-2"
                        >
                            <Camera size={20} />
                            Scanner mes documents
                        </button>
                    )}
                </div>
            </div>

            {/* Timeline du dossier */}
            <div className="px-4 mt-6">
                <h3 className="font-bold text-slate-900 mb-4">Suivi de votre dossier</h3>
                <div className="bg-white rounded-2xl p-4">
                    {workflowSteps.map((stage, index) => {
                        const isCompleted = index < currentStageIndex;
                        const isCurrent = index === currentStageIndex;
                        const label = WorkflowService.getStageLabel(stage);

                        return (
                            <div key={stage} className="flex gap-4">
                                {/* Ligne verticale */}
                                <div className="flex flex-col items-center">
                                    <div className={`w-8 h-8 rounded-full flex items-center justify-center ${isCompleted ? 'bg-emerald-500' :
                                            isCurrent ? 'bg-indigo-600' : 'bg-slate-200'
                                        }`}>
                                        {isCompleted ? (
                                            <CheckCircle className="text-white" size={16} />
                                        ) : isCurrent ? (
                                            <Clock className="text-white" size={16} />
                                        ) : (
                                            <Circle className="text-slate-400" size={16} />
                                        )}
                                    </div>
                                    {index < workflowSteps.length - 1 && (
                                        <div className={`w-0.5 h-8 ${isCompleted ? 'bg-emerald-500' : 'bg-slate-200'
                                            }`} />
                                    )}
                                </div>

                                {/* Contenu */}
                                <div className="pb-6">
                                    <p className={`font-medium ${isCompleted ? 'text-emerald-600' :
                                            isCurrent ? 'text-indigo-600' : 'text-slate-400'
                                        }`}>
                                        {label}
                                    </p>
                                    {isCurrent && (
                                        <p className="text-xs text-slate-500 mt-1">
                                            {WorkflowService.getStageDescription(stage)}
                                        </p>
                                    )}
                                </div>
                            </div>
                        );
                    })}
                </div>
            </div>

            {/* Documents uploadés */}
            {lead.documents && lead.documents.length > 0 && (
                <div className="px-4 mt-6">
                    <h3 className="font-bold text-slate-900 mb-4">Vos documents</h3>
                    <div className="space-y-2">
                        {lead.documents.map((doc) => (
                            <div key={doc.id} className="bg-white rounded-xl p-4 flex items-center gap-3">
                                <div className={`w-10 h-10 rounded-lg flex items-center justify-center ${doc.status === 'VALID' ? 'bg-emerald-100' :
                                        doc.status === 'REJECTED' ? 'bg-red-100' : 'bg-slate-100'
                                    }`}>
                                    <FileText className={
                                        doc.status === 'VALID' ? 'text-emerald-600' :
                                            doc.status === 'REJECTED' ? 'text-red-600' : 'text-slate-400'
                                    } size={20} />
                                </div>
                                <div className="flex-1 min-w-0">
                                    <p className="font-medium text-slate-900 truncate">{doc.docType}</p>
                                    <p className="text-xs text-slate-500">
                                        {doc.status === 'VALID' ? '✓ Validé' :
                                            doc.status === 'REJECTED' ? '✗ Rejeté' : 'En attente'}
                                    </p>
                                </div>
                                {doc.status === 'VALID' && (
                                    <CheckCircle className="text-emerald-500" size={20} />
                                )}
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {/* Bouton contact */}
            <div className="fixed bottom-6 left-4 right-4">
                <button className="w-full py-4 bg-slate-900 text-white rounded-2xl font-bold flex items-center justify-center gap-2 shadow-lg">
                    <MessageCircle size={20} />
                    Contacter mon conseiller
                </button>
            </div>
        </div>
    );
}
