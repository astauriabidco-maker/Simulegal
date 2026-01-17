'use client';

import React, { useRef, useState } from 'react';
import { DocumentAnalysis, AnalysisResult } from '../../services/DocumentAnalysisService';
import {
    Upload,
    CheckCircle,
    XCircle,
    Clock,
    FileText,
    Trash2,
    AlertTriangle,
    Loader2,
    Bot,
    RefreshCw
} from 'lucide-react';

export type DocumentStatus = 'EMPTY' | 'UPLOADING' | 'ANALYZING' | 'UPLOADED' | 'VALIDATED' | 'REJECTED';

export interface DocumentVerification {
    status: 'PENDING' | 'AUTO_VALIDATED' | 'MANUAL_VALIDATED' | 'REJECTED';
    aiConfidence?: number;
    rejectionReason?: string;
    analyzedAt?: string;
}

interface DocumentDropzoneProps {
    label: string;
    status: DocumentStatus;
    fileName?: string;
    rejectionReason?: string;
    verification?: DocumentVerification;
    onUpload: (file: File, verification: DocumentVerification) => void;
    onRemove?: () => void;
    disabled?: boolean;
}

export default function DocumentDropzone({
    label,
    status,
    fileName,
    rejectionReason,
    verification,
    onUpload,
    onRemove,
    disabled = false
}: DocumentDropzoneProps) {
    const inputRef = useRef<HTMLInputElement>(null);
    const [isDragging, setIsDragging] = useState(false);
    const [internalStatus, setInternalStatus] = useState<'IDLE' | 'UPLOADING' | 'ANALYZING'>('IDLE');
    const [analysisResult, setAnalysisResult] = useState<AnalysisResult | null>(null);
    const [errorMessage, setErrorMessage] = useState<string>('');
    const [currentFile, setCurrentFile] = useState<File | null>(null);

    const handleDrop = (e: React.DragEvent) => {
        e.preventDefault();
        setIsDragging(false);
        if (disabled) return;
        const file = e.dataTransfer.files[0];
        if (file) processFile(file);
    };

    const handleDragOver = (e: React.DragEvent) => {
        e.preventDefault();
        if (!disabled) setIsDragging(true);
    };

    const handleDragLeave = () => {
        setIsDragging(false);
    };

    const handleClick = () => {
        if (disabled) return;
        if (status === 'EMPTY' || status === 'REJECTED' || internalStatus === 'IDLE') {
            inputRef.current?.click();
        }
    };

    const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (file) processFile(file);
        e.target.value = ''; // Reset pour permettre de re-sélectionner
    };

    const processFile = async (file: File) => {
        setCurrentFile(file);
        setInternalStatus('UPLOADING');
        setErrorMessage('');
        setAnalysisResult(null);

        // Petite pause pour montrer l'upload
        await new Promise(r => setTimeout(r, 300));
        setInternalStatus('ANALYZING');

        // Analyse IA
        const result = await DocumentAnalysis.analyze(file);
        setAnalysisResult(result);

        if (result.isValid) {
            // Document validé par IA
            const verificationData: DocumentVerification = {
                status: 'AUTO_VALIDATED',
                aiConfidence: result.confidence,
                analyzedAt: new Date().toISOString()
            };
            setInternalStatus('IDLE');
            onUpload(file, verificationData);
        } else {
            // Document rejeté - NE PAS sauvegarder
            setInternalStatus('IDLE');
            setErrorMessage(result.message);
        }
    };

    const handleRetry = () => {
        setAnalysisResult(null);
        setErrorMessage('');
        setCurrentFile(null);
        inputRef.current?.click();
    };

    // Détermine l'état effectif (interne ou externe)
    const effectiveStatus = internalStatus !== 'IDLE' ? internalStatus : status;

    const getStatusConfig = () => {
        // État d'erreur après analyse
        if (analysisResult && !analysisResult.isValid && internalStatus === 'IDLE') {
            return {
                borderColor: 'border-red-400',
                bgColor: 'bg-red-50',
                textColor: 'text-red-700',
                icon: <XCircle size={32} className="text-red-500" />,
                message: 'Document refusé'
            };
        }

        switch (effectiveStatus) {
            case 'EMPTY':
                return {
                    borderColor: 'border-slate-300',
                    bgColor: isDragging ? 'bg-indigo-50' : 'bg-slate-50',
                    textColor: 'text-slate-500',
                    icon: <Upload size={32} className="text-slate-400" />,
                    message: 'Glisser-déposer ou cliquer'
                };
            case 'UPLOADING':
                return {
                    borderColor: 'border-indigo-300',
                    bgColor: 'bg-indigo-50',
                    textColor: 'text-indigo-700',
                    icon: <Loader2 size={32} className="text-indigo-500 animate-spin" />,
                    message: 'Upload en cours...'
                };
            case 'ANALYZING':
                return {
                    borderColor: 'border-purple-400',
                    bgColor: 'bg-purple-50',
                    textColor: 'text-purple-700',
                    icon: <Bot size={32} className="text-purple-500 animate-pulse" />,
                    message: 'IA analyse votre document...'
                };
            case 'UPLOADED':
                // Vérifie si pré-validé par IA
                const isAiValidated = verification?.status === 'AUTO_VALIDATED';
                return {
                    borderColor: isAiValidated ? 'border-emerald-400' : 'border-amber-300',
                    bgColor: isAiValidated ? 'bg-emerald-50' : 'bg-amber-50',
                    textColor: isAiValidated ? 'text-emerald-700' : 'text-amber-700',
                    icon: isAiValidated
                        ? <CheckCircle size={32} className="text-emerald-500" />
                        : <Clock size={32} className="text-amber-500" />,
                    message: isAiValidated ? 'Pré-validé par IA' : 'En attente de validation'
                };
            case 'VALIDATED':
                return {
                    borderColor: 'border-emerald-400',
                    bgColor: 'bg-emerald-50',
                    textColor: 'text-emerald-700',
                    icon: <CheckCircle size={32} className="text-emerald-500" />,
                    message: 'Document validé ✓'
                };
            case 'REJECTED':
                return {
                    borderColor: 'border-red-400',
                    bgColor: 'bg-red-50',
                    textColor: 'text-red-700',
                    icon: <XCircle size={32} className="text-red-500" />,
                    message: rejectionReason || 'Document rejeté'
                };
            default:
                return {
                    borderColor: 'border-slate-300',
                    bgColor: 'bg-slate-50',
                    textColor: 'text-slate-500',
                    icon: <Upload size={32} className="text-slate-400" />,
                    message: 'Glisser-déposer ou cliquer'
                };
        }
    };

    const config = getStatusConfig();
    const showError = analysisResult && !analysisResult.isValid && internalStatus === 'IDLE';
    const isAiValidated = verification?.status === 'AUTO_VALIDATED';

    return (
        <div className="space-y-2">
            {/* Label */}
            <div className="flex items-center justify-between">
                <label className="font-bold text-slate-900 text-sm">{label}</label>
                <div className="flex items-center gap-2">
                    {/* Badge IA */}
                    {isAiValidated && status === 'UPLOADED' && (
                        <span className="flex items-center gap-1 text-[10px] font-bold uppercase px-2 py-0.5 rounded-full bg-purple-100 text-purple-700">
                            <Bot size={10} />
                            IA {verification?.aiConfidence}%
                        </span>
                    )}
                    {/* Badge statut */}
                    {status !== 'EMPTY' && !showError && (
                        <span className={`text-[10px] font-bold uppercase px-2 py-0.5 rounded-full ${status === 'VALIDATED' ? 'bg-emerald-100 text-emerald-700' :
                                status === 'REJECTED' ? 'bg-red-100 text-red-700' :
                                    isAiValidated ? 'bg-emerald-100 text-emerald-700' :
                                        'bg-amber-100 text-amber-700'
                            }`}>
                            {status === 'VALIDATED' ? 'Validé' : status === 'REJECTED' ? 'Rejeté' : isAiValidated ? 'Pré-validé' : 'En attente'}
                        </span>
                    )}
                </div>
            </div>

            {/* Dropzone */}
            <div
                className={`
                    relative rounded-xl border-2 border-dashed p-6 transition-all
                    ${config.borderColor} ${config.bgColor}
                    ${!disabled && (status === 'EMPTY' || status === 'REJECTED' || showError) ? 'cursor-pointer hover:border-indigo-400 hover:bg-indigo-50' : ''}
                    ${disabled ? 'opacity-50 cursor-not-allowed' : ''}
                `}
                onDrop={handleDrop}
                onDragOver={handleDragOver}
                onDragLeave={handleDragLeave}
                onClick={handleClick}
            >
                <input
                    ref={inputRef}
                    type="file"
                    className="hidden"
                    onChange={handleFileChange}
                    accept="image/*,.pdf"
                    disabled={disabled}
                />

                <div className="flex flex-col items-center text-center space-y-2">
                    {config.icon}

                    {(fileName || currentFile) && effectiveStatus !== 'EMPTY' && !showError ? (
                        <div className="flex items-center gap-2">
                            <FileText size={16} className={config.textColor} />
                            <span className={`text-sm font-medium ${config.textColor}`}>
                                {fileName || currentFile?.name}
                            </span>
                        </div>
                    ) : null}

                    <p className={`text-sm font-medium ${config.textColor}`}>{config.message}</p>

                    {/* Message d'erreur IA */}
                    {showError && (
                        <div className="mt-2 space-y-3">
                            <div className="flex items-center justify-center gap-2 text-sm text-red-600 bg-red-100 px-4 py-2 rounded-lg">
                                <AlertTriangle size={16} />
                                <span className="font-medium">{errorMessage}</span>
                            </div>
                            {analysisResult?.confidence && (
                                <p className="text-xs text-red-400">
                                    Confiance IA: {analysisResult.confidence}%
                                </p>
                            )}
                            <button
                                onClick={(e) => {
                                    e.stopPropagation();
                                    handleRetry();
                                }}
                                className="flex items-center gap-2 bg-red-600 hover:bg-red-700 text-white px-4 py-2 rounded-lg text-sm font-bold transition-colors mx-auto"
                            >
                                <RefreshCw size={16} />
                                Réessayer
                            </button>
                        </div>
                    )}

                    {/* Message de rejet externe */}
                    {status === 'REJECTED' && rejectionReason && !showError && (
                        <div className="flex items-center gap-1 text-xs text-red-600 bg-red-100 px-2 py-1 rounded-lg">
                            <AlertTriangle size={12} />
                            {rejectionReason}
                        </div>
                    )}
                </div>

                {/* Bouton Supprimer */}
                {status === 'UPLOADED' && onRemove && internalStatus === 'IDLE' && (
                    <button
                        onClick={(e) => {
                            e.stopPropagation();
                            onRemove();
                        }}
                        className="absolute top-2 right-2 w-8 h-8 bg-red-100 hover:bg-red-200 text-red-600 rounded-lg flex items-center justify-center transition-colors"
                    >
                        <Trash2 size={16} />
                    </button>
                )}
            </div>
        </div>
    );
}

