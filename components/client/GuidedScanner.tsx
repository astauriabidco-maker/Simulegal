'use client';

import React, { useState, useRef, useCallback } from 'react';
import { DocumentAnalysisService, AnalysisResponse } from '../../services/DocumentAnalysisService';
import { LeadDocument, DocumentStatus } from '../../services/crmStore';
import {
    Camera,
    CheckCircle,
    XCircle,
    Loader2,
    ChevronLeft,
    ChevronRight,
    Upload,
    FileText,
    User,
    Home,
    CreditCard,
    Briefcase,
    GraduationCap,
    Heart,
    Shield,
    X
} from 'lucide-react';

interface DocumentStep {
    id: string;
    label: string;
    description: string;
    icon: React.ReactNode;
    required: boolean;
}

interface GuidedScannerProps {
    documents: DocumentStep[];
    existingDocs: LeadDocument[];
    onDocumentUploaded: (docId: string, file: File, result: AnalysisResponse) => void;
    onComplete: () => void;
    onClose?: () => void;
}

// Mapping des icônes par type de document
const getDocIcon = (docId: string): React.ReactNode => {
    const id = docId.toLowerCase();
    if (id.includes('passport') || id.includes('identite') || id.includes('cni')) return <User size={32} />;
    if (id.includes('domicile') || id.includes('hebergement')) return <Home size={32} />;
    if (id.includes('bancaire') || id.includes('rib')) return <CreditCard size={32} />;
    if (id.includes('travail') || id.includes('contrat') || id.includes('emploi')) return <Briefcase size={32} />;
    if (id.includes('diplome') || id.includes('etude')) return <GraduationCap size={32} />;
    if (id.includes('mariage') || id.includes('famille')) return <Heart size={32} />;
    if (id.includes('sejour') || id.includes('titre')) return <Shield size={32} />;
    return <FileText size={32} />;
};

export default function GuidedScanner({
    documents,
    existingDocs,
    onDocumentUploaded,
    onComplete,
    onClose
}: GuidedScannerProps) {
    const [currentStep, setCurrentStep] = useState(0);
    const [isAnalyzing, setIsAnalyzing] = useState(false);
    const [lastResult, setLastResult] = useState<AnalysisResponse | null>(null);
    const [showResult, setShowResult] = useState(false);
    const fileInputRef = useRef<HTMLInputElement>(null);

    const currentDoc = documents[currentStep];
    const progress = ((currentStep + 1) / documents.length) * 100;

    // Vérifie si le doc actuel est déjà uploadé
    const existingDoc = existingDocs.find(d => d.id === currentDoc?.id);
    const isCurrentValid = existingDoc?.status === 'VALID';

    // Gestion de l'upload
    const handleFileSelect = useCallback(async (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file || !currentDoc) return;

        setIsAnalyzing(true);
        setShowResult(false);

        try {
            const result = await DocumentAnalysisService.analyze(file, currentDoc.id);
            setLastResult(result);
            setShowResult(true);

            onDocumentUploaded(currentDoc.id, file, result);

            // Si valide, passe au suivant après un délai
            if (result.status === 'VALID') {
                setTimeout(() => {
                    if (currentStep < documents.length - 1) {
                        setCurrentStep(prev => prev + 1);
                        setShowResult(false);
                        setLastResult(null);
                    } else {
                        onComplete();
                    }
                }, 1500);
            }
        } catch (error) {
            console.error('[SCANNER] Erreur analyse:', error);
        } finally {
            setIsAnalyzing(false);
            // Reset input
            if (fileInputRef.current) fileInputRef.current.value = '';
        }
    }, [currentDoc, currentStep, documents.length, onDocumentUploaded, onComplete]);

    // Navigation
    const goNext = () => {
        if (currentStep < documents.length - 1) {
            setCurrentStep(prev => prev + 1);
            setShowResult(false);
            setLastResult(null);
        } else {
            onComplete();
        }
    };

    const goPrev = () => {
        if (currentStep > 0) {
            setCurrentStep(prev => prev - 1);
            setShowResult(false);
            setLastResult(null);
        }
    };

    if (!currentDoc) return null;

    return (
        <div className="fixed inset-0 bg-slate-900 flex flex-col z-50">
            {/* Header avec barre de progression */}
            <div className="safe-area-inset-top bg-slate-900">
                {/* Barre de progression style Instagram */}
                <div className="flex gap-1 px-2 pt-2">
                    {documents.map((_, index) => (
                        <div
                            key={index}
                            className="flex-1 h-1 rounded-full overflow-hidden bg-white/20"
                        >
                            <div
                                className={`h-full rounded-full transition-all duration-500 ${index < currentStep ? 'bg-white w-full' :
                                        index === currentStep ? 'bg-white' : 'w-0'
                                    }`}
                                style={{
                                    width: index === currentStep ? `${isCurrentValid ? 100 : 50}%` : undefined
                                }}
                            />
                        </div>
                    ))}
                </div>

                {/* Header */}
                <div className="flex items-center justify-between p-4">
                    <button
                        onClick={goPrev}
                        disabled={currentStep === 0}
                        className="p-2 text-white/60 disabled:opacity-30"
                    >
                        <ChevronLeft size={24} />
                    </button>
                    <span className="text-white/80 text-sm font-medium">
                        {currentStep + 1} / {documents.length}
                    </span>
                    {onClose && (
                        <button onClick={onClose} className="p-2 text-white/60">
                            <X size={24} />
                        </button>
                    )}
                </div>
            </div>

            {/* Contenu principal */}
            <div className="flex-1 flex flex-col items-center justify-center p-6 text-center">
                {/* État: Analyse en cours */}
                {isAnalyzing && (
                    <div className="flex flex-col items-center">
                        <div className="w-24 h-24 bg-blue-500/20 rounded-3xl flex items-center justify-center mb-6 animate-pulse">
                            <Loader2 className="text-blue-400 animate-spin" size={48} />
                        </div>
                        <h2 className="text-2xl font-bold text-white mb-2">Analyse en cours...</h2>
                        <p className="text-white/60">Vérification du document</p>
                    </div>
                )}

                {/* État: Résultat affiché */}
                {showResult && lastResult && !isAnalyzing && (
                    <div className="flex flex-col items-center">
                        <div className={`w-24 h-24 rounded-3xl flex items-center justify-center mb-6 ${lastResult.status === 'VALID'
                                ? 'bg-emerald-500/20'
                                : 'bg-red-500/20'
                            }`}>
                            {lastResult.status === 'VALID' ? (
                                <CheckCircle className="text-emerald-400" size={48} />
                            ) : (
                                <XCircle className="text-red-400" size={48} />
                            )}
                        </div>
                        <h2 className={`text-2xl font-bold mb-2 ${lastResult.status === 'VALID' ? 'text-emerald-400' : 'text-red-400'
                            }`}>
                            {lastResult.status === 'VALID' ? 'Document validé !' : 'Document rejeté'}
                        </h2>
                        <p className="text-white/60 max-w-xs">{lastResult.message}</p>

                        {lastResult.status !== 'VALID' && (
                            <button
                                onClick={() => fileInputRef.current?.click()}
                                className="mt-6 px-6 py-3 bg-white text-slate-900 rounded-2xl font-bold"
                            >
                                Réessayer
                            </button>
                        )}
                    </div>
                )}

                {/* État: Prêt à capturer */}
                {!isAnalyzing && !showResult && (
                    <div className="flex flex-col items-center">
                        {/* Icône du document */}
                        <div className={`w-32 h-32 rounded-3xl flex items-center justify-center mb-8 ${isCurrentValid
                                ? 'bg-emerald-500/20 text-emerald-400'
                                : 'bg-white/10 text-white'
                            }`}>
                            {isCurrentValid ? (
                                <CheckCircle size={48} />
                            ) : (
                                currentDoc.icon || getDocIcon(currentDoc.id)
                            )}
                        </div>

                        {/* Titre et description */}
                        <h2 className="text-2xl font-bold text-white mb-3">
                            {currentDoc.label}
                        </h2>
                        <p className="text-white/60 max-w-xs mb-8">
                            {currentDoc.description}
                        </p>

                        {/* Statut actuel */}
                        {isCurrentValid && (
                            <div className="flex items-center gap-2 text-emerald-400 mb-6">
                                <CheckCircle size={20} />
                                <span className="font-medium">Déjà validé</span>
                            </div>
                        )}
                    </div>
                )}
            </div>

            {/* Footer avec bouton capture */}
            <div className="safe-area-inset-bottom bg-slate-900 p-6">
                {!isAnalyzing && !showResult && (
                    <div className="flex flex-col gap-3">
                        {/* Bouton principal de capture */}
                        <button
                            onClick={() => fileInputRef.current?.click()}
                            className="w-full py-5 bg-white text-slate-900 rounded-2xl font-black text-lg flex items-center justify-center gap-3 active:scale-98 transition-transform"
                        >
                            <Camera size={24} />
                            {isCurrentValid ? 'Remplacer la photo' : 'Prendre une photo'}
                        </button>

                        {/* Bouton passer */}
                        {!currentDoc.required && (
                            <button
                                onClick={goNext}
                                className="w-full py-4 text-white/60 font-medium"
                            >
                                Passer cette étape →
                            </button>
                        )}

                        {/* Bouton suivant si déjà validé */}
                        {isCurrentValid && (
                            <button
                                onClick={goNext}
                                className="w-full py-4 bg-emerald-500 text-white rounded-2xl font-bold flex items-center justify-center gap-2"
                            >
                                Continuer
                                <ChevronRight size={20} />
                            </button>
                        )}
                    </div>
                )}

                {/* Input file caché */}
                <input
                    ref={fileInputRef}
                    type="file"
                    accept="image/*"
                    capture="environment"
                    onChange={handleFileSelect}
                    className="hidden"
                />
            </div>
        </div>
    );
}
