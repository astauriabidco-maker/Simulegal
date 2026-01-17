'use client';

import React, { useState, useEffect } from 'react';
import { CRM, Lead } from '../../services/crmStore';
import { getRequirementsForService, DocumentRequirement } from '../../config/DocumentTemplates';
import GuidedScanner, { DocumentStep } from './GuidedScanner';
import DocumentDropzone, { DocumentStatus } from './DocumentDropzone';
import {
    FileCheck,
    MessageCircle,
    CheckCircle,
    Clock,
    FileText,
    Building2,
    ShieldCheck,
    X,
    Smartphone,
    Monitor
} from 'lucide-react';

interface DocumentFile {
    id: string;
    status: DocumentStatus;
    fileName?: string;
    rejectionReason?: string;
    uploadedAt?: string;
}

interface ClientPortalProps {
    leadId: string;
    isOpen: boolean;
    onClose: () => void;
}

type ViewMode = 'guided' | 'classic' | 'complete';

export default function ClientPortal({ leadId, isOpen, onClose }: ClientPortalProps) {
    const [lead, setLead] = useState<Lead | null>(null);
    const [documents, setDocuments] = useState<Record<string, DocumentFile>>({});
    const [showChat, setShowChat] = useState(false);
    const [viewMode, setViewMode] = useState<ViewMode>('guided');
    const [requiredDocs, setRequiredDocs] = useState<DocumentRequirement[]>([]);

    useEffect(() => {
        if (isOpen && leadId) {
            const foundLead = CRM.getLeadById(leadId);
            setLead(foundLead);

            // Utilise la checklist figée du lead, ou fallback sur la config
            if (foundLead) {
                const docs = foundLead.requiredDocuments && foundLead.requiredDocuments.length > 0
                    ? foundLead.requiredDocuments
                    : getRequirementsForService(foundLead.serviceId);

                setRequiredDocs(docs);

                // Initialise les documents avec statut vide
                const initialDocs: Record<string, DocumentFile> = {};
                docs.forEach(doc => {
                    initialDocs[doc.id] = { id: doc.id, status: 'EMPTY' };
                });
                setDocuments(initialDocs);
            }
        }
    }, [isOpen, leadId]);

    if (!isOpen || !lead) return null;

    // Calcul de la progression
    const totalDocs = requiredDocs.length;
    const uploadedDocs = Object.values(documents).filter(d => d.status !== 'EMPTY').length;
    const isAllComplete = uploadedDocs >= totalDocs;


    // Convertir en format pour GuidedScanner
    const documentSteps: DocumentStep[] = requiredDocs.map(doc => ({
        id: doc.id,
        icon: <FileText size={80} />,
        required: doc.required
    }));

    const handleUpload = (docId: string, file: File) => {
        setDocuments(prev => ({
            ...prev,
            [docId]: {
                id: docId,
                status: 'UPLOADED',
                fileName: file.name,
                uploadedAt: new Date().toISOString()
            }
        }));

        // Notifie le CRM
        CRM.addNote(leadId, {
            author: 'AGENCY',
            authorName: 'Système',
            content: `📎 Document uploadé: ${file.name}`
        });

        console.log(`[CLIENT PORTAL] Document uploadé: ${file.name} pour dossier ${leadId}`);
    };

    const handleRemove = (docId: string) => {
        setDocuments(prev => ({
            ...prev,
            [docId]: { id: docId, status: 'EMPTY' }
        }));
    };

    const handleGuidedComplete = () => {
        setViewMode('complete');
    };

    const handleDocumentScanned = (docId: string, file: File) => {
        handleUpload(docId, file);
    };

    // ============================================
    // VUE GUIDÉE (Mobile / Scanner)
    // ============================================
    if (viewMode === 'guided' && !isAllComplete) {
        return (
            <div className="fixed inset-0 z-50">
                {/* Bouton pour passer en mode classique */}
                <button
                    onClick={() => setViewMode('classic')}
                    className="fixed top-4 left-4 z-50 bg-white/20 backdrop-blur-sm text-white px-3 py-2 rounded-xl text-xs font-bold flex items-center gap-2"
                >
                    <Monitor size={14} />
                    Mode Desktop
                </button>

                <GuidedScanner
                    documents={documentSteps}
                    onComplete={handleGuidedComplete}
                    onDocumentScanned={handleDocumentScanned}
                />
            </div>
        );
    }

    // ============================================
    // VUE COMPLÈTE (Tout envoyé)
    // ============================================
    if (viewMode === 'complete' || isAllComplete) {
        return (
            <div className="fixed inset-0 bg-gradient-to-b from-emerald-500 to-emerald-600 z-50 flex flex-col items-center justify-center p-6 text-white">
                <div className="w-32 h-32 bg-white rounded-full flex items-center justify-center mb-8 shadow-2xl">
                    <CheckCircle className="text-emerald-500" size={64} />
                </div>
                <h1 className="text-3xl font-black mb-4 text-center">Bravo ! Tout est reçu 🎉</h1>
                <p className="text-emerald-100 text-center mb-2">Votre dossier est maintenant complet.</p>
                <p className="text-emerald-200 text-sm text-center mb-8">Notre équipe va le traiter dans les plus brefs délais.</p>

                <div className="bg-white/10 rounded-2xl p-6 mb-8 max-w-sm">
                    <h3 className="font-bold mb-2">Prochaines étapes :</h3>
                    <ul className="text-sm text-emerald-100 space-y-2">
                        <li>✓ Vérification des documents (24-48h)</li>
                        <li>✓ Constitution du dossier</li>
                        <li>✓ Dépôt auprès de l'administration</li>
                        <li>✓ Suivi jusqu'à la décision finale</li>
                    </ul>
                </div>

                <button
                    onClick={onClose}
                    className="bg-white text-emerald-600 px-8 py-4 rounded-2xl font-black text-lg shadow-lg"
                >
                    Retour à l'accueil
                </button>
            </div>
        );
    }

    // ============================================
    // VUE CLASSIQUE (Desktop / Liste)
    // ============================================
    const progressPercent = Math.round((uploadedDocs / totalDocs) * 100);
    const validatedDocs = Object.values(documents).filter(d => d.status === 'VALIDATED').length;

    const getStatusConfig = () => {
        switch (lead.status) {
            case 'PAID':
                return { label: 'En attente de vos documents', color: 'bg-blue-100 text-blue-700', icon: <Clock size={16} /> };
            case 'PROCESSING':
                return { label: 'En constitution', color: 'bg-amber-100 text-amber-700', icon: <FileText size={16} /> };
            case 'SUBMITTED':
                return { label: 'En instruction', color: 'bg-purple-100 text-purple-700', icon: <Building2 size={16} /> };
            case 'COMPLETED':
                return { label: 'Dossier terminé', color: 'bg-emerald-100 text-emerald-700', icon: <CheckCircle size={16} /> };
            default:
                return { label: 'En cours', color: 'bg-slate-100 text-slate-700', icon: <Clock size={16} /> };
        }
    };

    const statusConfig = getStatusConfig();

    return (
        <div className="fixed inset-0 bg-slate-100 z-50 overflow-auto">
            {/* Header */}
            <div className="bg-white border-b border-slate-200 sticky top-0 z-10">
                <div className="max-w-4xl mx-auto p-6">
                    <div className="flex items-center justify-between mb-4">
                        <div className="flex items-center gap-4">
                            <div className="w-14 h-14 bg-gradient-to-br from-indigo-500 to-purple-600 rounded-2xl flex items-center justify-center text-white text-2xl font-black shadow-lg">
                                {lead.name.charAt(0).toUpperCase()}
                            </div>
                            <div>
                                <h1 className="text-2xl font-black text-slate-900">Bonjour {lead.name.split(' ')[0]} 👋</h1>
                                <p className="text-slate-500 text-sm">Dossier #{lead.id}</p>
                            </div>
                        </div>
                        <div className="flex items-center gap-2">
                            {/* Bouton pour passer en mode guidé */}
                            <button
                                onClick={() => setViewMode('guided')}
                                className="bg-indigo-100 hover:bg-indigo-200 text-indigo-700 px-3 py-2 rounded-xl text-xs font-bold flex items-center gap-2 transition-colors"
                            >
                                <Smartphone size={14} />
                                Mode Mobile
                            </button>
                            <button
                                onClick={onClose}
                                className="w-10 h-10 bg-slate-100 hover:bg-slate-200 rounded-full flex items-center justify-center transition-colors"
                            >
                                <X size={20} className="text-slate-600" />
                            </button>
                        </div>
                    </div>

                    {/* Barre de progression */}
                    <div className="bg-slate-50 rounded-2xl p-4">
                        <div className="flex items-center justify-between mb-2">
                            <div className="flex items-center gap-2">
                                <FileCheck size={18} className="text-indigo-600" />
                                <span className="font-bold text-slate-900">Dossier complété à {progressPercent}%</span>
                            </div>
                            <span className={`flex items-center gap-1.5 text-xs font-bold px-3 py-1.5 rounded-full ${statusConfig.color}`}>
                                {statusConfig.icon}
                                {statusConfig.label}
                            </span>
                        </div>
                        <div className="h-3 bg-slate-200 rounded-full overflow-hidden">
                            <div
                                className="h-full bg-gradient-to-r from-indigo-500 to-purple-500 rounded-full transition-all duration-500"
                                style={{ width: `${progressPercent}%` }}
                            />
                        </div>
                        <div className="flex justify-between text-xs text-slate-400 mt-2">
                            <span>{uploadedDocs} / {totalDocs} documents</span>
                            <span>{validatedDocs} validé(s)</span>
                        </div>
                    </div>
                </div>
            </div>

            {/* Contenu principal */}
            <div className="max-w-4xl mx-auto p-6">
                {/* Service Info */}
                <div className="bg-indigo-600 text-white rounded-2xl p-6 mb-6 relative overflow-hidden">
                    <div className="absolute top-0 right-0 opacity-10">
                        <ShieldCheck size={120} />
                    </div>
                    <h2 className="text-xl font-black mb-2 relative z-10">{lead.serviceName || lead.serviceId}</h2>
                    <p className="text-indigo-200 text-sm relative z-10">
                        Uploadez vos documents ci-dessous pour que notre équipe puisse traiter votre dossier.
                    </p>
                </div>

                {/* Checklist Documents */}
                <div className="bg-white rounded-2xl shadow-sm border border-slate-100 p-6">
                    <h3 className="font-bold text-slate-900 text-lg mb-6 flex items-center gap-2">
                        <FileText className="text-indigo-600" size={20} />
                        Ma Checklist de documents
                    </h3>

                    <div className="space-y-4">
                        {requiredDocs.map((doc) => {
                            const docState = documents[doc.id] || { id: doc.id, status: 'EMPTY' as DocumentStatus };
                            return (
                                <DocumentDropzone
                                    key={doc.id}
                                    label={doc.label + (doc.required ? ' *' : '')}
                                    status={docState.status}
                                    fileName={docState.fileName}
                                    rejectionReason={docState.rejectionReason}
                                    onUpload={(file) => handleUpload(doc.id, file)}
                                    onRemove={() => handleRemove(doc.id)}
                                />
                            );
                        })}
                    </div>

                    <p className="text-xs text-slate-400 mt-4">* Documents obligatoires</p>
                </div>

                {/* Note de sécurité */}
                <div className="mt-6 bg-emerald-50 border border-emerald-200 rounded-xl p-4 flex items-start gap-3">
                    <ShieldCheck className="text-emerald-600 flex-shrink-0 mt-0.5" size={20} />
                    <div>
                        <p className="font-bold text-emerald-800 text-sm">Vos données sont sécurisées</p>
                        <p className="text-xs text-emerald-700 mt-1">
                            Tous vos documents sont cryptés et stockés de manière sécurisée conformément au RGPD.
                        </p>
                    </div>
                </div>
            </div>

            {/* Bouton flottant Contact */}
            <button
                onClick={() => setShowChat(!showChat)}
                className="fixed bottom-6 right-6 w-16 h-16 bg-gradient-to-r from-indigo-600 to-purple-600 text-white rounded-full shadow-2xl flex items-center justify-center hover:scale-110 transition-transform z-20"
            >
                <MessageCircle size={28} />
            </button>

            {/* Mini Chat */}
            {showChat && (
                <div className="fixed bottom-24 right-6 w-80 bg-white rounded-2xl shadow-2xl border border-slate-200 overflow-hidden z-20 animate-in slide-in-from-bottom-4 duration-200">
                    <div className="bg-slate-900 text-white p-4">
                        <h4 className="font-bold">Besoin d'aide ?</h4>
                        <p className="text-slate-400 text-xs">Notre équipe vous répond sous 24h</p>
                    </div>
                    <div className="p-4">
                        <textarea
                            placeholder="Écrivez votre message..."
                            className="w-full border border-slate-200 rounded-xl p-3 text-sm resize-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none"
                            rows={3}
                        />
                        <button className="w-full mt-2 bg-indigo-600 hover:bg-indigo-700 text-white py-2 rounded-xl font-bold text-sm transition-colors">
                            Envoyer
                        </button>
                    </div>
                </div>
            )}
        </div>
    );
}

