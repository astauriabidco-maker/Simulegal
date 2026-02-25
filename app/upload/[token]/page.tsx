'use client';

import React, { useState, useEffect, useRef } from 'react';
import { useParams } from 'next/navigation';
import Image from 'next/image';
import { Upload, CheckCircle, AlertCircle, FileText, Camera, Loader2, Shield, ArrowLeft } from 'lucide-react';

interface TokenInfo {
    valid: boolean;
    docId: string;
    docName: string;
    leadName: string;
    serviceName: string;
    alreadyUploaded: boolean;
    existingStatus: string | null;
}

const API_URL = 'http://localhost:4000';

export default function UploadPage() {
    const params = useParams();
    const token = params.token as string;

    const [tokenInfo, setTokenInfo] = useState<TokenInfo | null>(null);
    const [isValidating, setIsValidating] = useState(true);
    const [isInvalid, setIsInvalid] = useState(false);

    const [selectedFile, setSelectedFile] = useState<File | null>(null);
    const [previewUrl, setPreviewUrl] = useState<string | null>(null);
    const [isUploading, setIsUploading] = useState(false);
    const [uploadSuccess, setUploadSuccess] = useState(false);
    const [uploadError, setUploadError] = useState<string | null>(null);

    const fileInputRef = useRef<HTMLInputElement>(null);
    const cameraInputRef = useRef<HTMLInputElement>(null);

    // Vérifier le token au chargement
    useEffect(() => {
        const verifyToken = async () => {
            try {
                const res = await fetch(`${API_URL}/public/leads/upload/${token}`);
                if (!res.ok) {
                    setIsInvalid(true);
                    return;
                }
                const data = await res.json();
                setTokenInfo(data);
            } catch (e) {
                setIsInvalid(true);
            } finally {
                setIsValidating(false);
            }
        };
        verifyToken();
    }, [token]);

    const handleFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        setUploadError(null);

        // Vérifier le type
        const allowed = ['image/jpeg', 'image/png', 'image/webp', 'application/pdf'];
        if (!allowed.includes(file.type)) {
            setUploadError('Format non autorisé. Envoyez une image (JPG, PNG) ou un PDF.');
            return;
        }

        // Vérifier la taille (15 MB)
        if (file.size > 15 * 1024 * 1024) {
            setUploadError('Fichier trop volumineux (15 MB maximum).');
            return;
        }

        setSelectedFile(file);

        // Créer un aperçu pour les images
        if (file.type.startsWith('image/')) {
            const reader = new FileReader();
            reader.onload = (ev) => setPreviewUrl(ev.target?.result as string);
            reader.readAsDataURL(file);
        } else {
            setPreviewUrl(null);
        }
    };

    const handleUpload = async () => {
        if (!selectedFile || !token) return;

        setIsUploading(true);
        setUploadError(null);

        try {
            const formData = new FormData();
            formData.append('file', selectedFile);

            const res = await fetch(`${API_URL}/public/leads/upload/${token}`, {
                method: 'POST',
                body: formData
            });

            const result = await res.json();

            if (res.ok && result.success) {
                setUploadSuccess(true);
            } else {
                setUploadError(result.message || 'Erreur lors de l\'envoi.');
            }
        } catch (e) {
            setUploadError('Erreur de connexion. Vérifiez votre réseau et réessayez.');
        } finally {
            setIsUploading(false);
        }
    };

    // ── État de validation en cours ──
    if (isValidating) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-indigo-950 to-slate-900 flex items-center justify-center p-6">
                <div className="text-center">
                    <Loader2 className="animate-spin text-indigo-400 mx-auto mb-4" size={48} />
                    <p className="text-white/70 font-medium">Vérification du lien...</p>
                </div>
            </div>
        );
    }

    // ── Lien invalide ou expiré ──
    if (isInvalid || !tokenInfo) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-red-950 to-slate-900 flex items-center justify-center p-6">
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-10 max-w-md w-full text-center">
                    <div className="w-20 h-20 bg-red-500/20 rounded-full flex items-center justify-center mx-auto mb-6">
                        <AlertCircle size={40} className="text-red-400" />
                    </div>
                    <h1 className="text-2xl font-black text-white mb-3">Lien invalide</h1>
                    <p className="text-white/60 text-sm leading-relaxed">
                        Ce lien est expiré ou invalide. Contactez votre conseiller Simulegal pour obtenir un nouveau lien de dépôt.
                    </p>
                </div>
            </div>
        );
    }

    // ── Upload réussi ──
    if (uploadSuccess) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-emerald-950 to-slate-900 flex items-center justify-center p-6">
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-10 max-w-md w-full text-center">
                    <div className="w-24 h-24 bg-emerald-500/20 rounded-full flex items-center justify-center mx-auto mb-6 animate-pulse">
                        <CheckCircle size={48} className="text-emerald-400" />
                    </div>
                    <h1 className="text-2xl font-black text-white mb-3">Document envoyé !</h1>
                    <p className="text-white/60 text-sm leading-relaxed mb-6">
                        Votre <strong className="text-emerald-400">{tokenInfo.docName}</strong> a été déposé avec succès et rattaché à votre dossier.
                    </p>
                    <div className="bg-emerald-500/10 border border-emerald-500/20 rounded-2xl p-4">
                        <p className="text-emerald-300 text-xs font-bold">
                            ✅ Votre juriste sera notifié et examinera votre document dans les plus brefs délais.
                        </p>
                    </div>
                </div>
            </div>
        );
    }

    // ── Déjà uploadé ──
    if (tokenInfo.alreadyUploaded) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-blue-950 to-slate-900 flex items-center justify-center p-6">
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-10 max-w-md w-full text-center">
                    <div className="w-20 h-20 bg-blue-500/20 rounded-full flex items-center justify-center mx-auto mb-6">
                        <CheckCircle size={40} className="text-blue-400" />
                    </div>
                    <h1 className="text-2xl font-black text-white mb-3">Déjà déposé</h1>
                    <p className="text-white/60 text-sm leading-relaxed">
                        Votre <strong className="text-blue-400">{tokenInfo.docName}</strong> a déjà été déposé et est en cours de vérification.
                    </p>
                    <div className="bg-blue-500/10 border border-blue-500/20 rounded-2xl p-4 mt-6">
                        <p className="text-blue-300 text-xs font-bold">
                            Statut : {tokenInfo.existingStatus === 'VALID' ? '✅ Validé' : '⏳ En attente de validation'}
                        </p>
                    </div>
                </div>
            </div>
        );
    }

    // ── Page d'upload ──
    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-indigo-950 to-slate-900 flex flex-col">
            {/* Header */}
            <div className="p-6 pb-0">
                <div className="flex items-center gap-3 mb-2">
                    <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center shadow-lg shadow-indigo-600/30">
                        <Shield size={20} className="text-white" />
                    </div>
                    <div>
                        <h2 className="text-white font-black text-sm">Simulegal</h2>
                        <p className="text-white/40 text-[10px] font-bold uppercase tracking-widest">Dépôt sécurisé</p>
                    </div>
                </div>
            </div>

            {/* Content */}
            <div className="flex-1 flex items-center justify-center p-6">
                <div className="max-w-md w-full space-y-6">
                    {/* Info Card */}
                    <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-6">
                        <div className="flex items-start gap-4 mb-5">
                            <div className="w-14 h-14 bg-indigo-600/30 rounded-2xl flex items-center justify-center shrink-0">
                                <FileText size={24} className="text-indigo-400" />
                            </div>
                            <div>
                                <p className="text-white/40 text-[10px] font-bold uppercase tracking-widest mb-1">Document demandé</p>
                                <h1 className="text-xl font-black text-white leading-tight">{tokenInfo.docName}</h1>
                                <p className="text-white/50 text-xs mt-1 font-medium">
                                    Dossier de <strong className="text-white/70">{tokenInfo.leadName}</strong> • {tokenInfo.serviceName}
                                </p>
                            </div>
                        </div>

                        <div className="bg-indigo-500/10 border border-indigo-500/20 rounded-2xl p-3">
                            <p className="text-indigo-300 text-[11px] font-bold leading-relaxed">
                                📸 Prenez en photo ou sélectionnez le fichier correspondant. Assurez-vous que le document est lisible et non flou.
                            </p>
                        </div>
                    </div>

                    {/* Upload Zone */}
                    <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-6 space-y-4">
                        {!selectedFile ? (
                            <>
                                {/* Bouton Camera (mobile) */}
                                <button
                                    onClick={() => cameraInputRef.current?.click()}
                                    className="w-full bg-indigo-600 hover:bg-indigo-700 text-white rounded-2xl py-4 px-6 flex items-center justify-center gap-3 font-bold transition-all shadow-lg shadow-indigo-600/30 active:scale-[0.98]"
                                >
                                    <Camera size={22} />
                                    Prendre en photo
                                </button>
                                <input
                                    ref={cameraInputRef}
                                    type="file"
                                    accept="image/*"
                                    capture="environment"
                                    onChange={handleFileSelect}
                                    className="hidden"
                                />

                                {/* Séparateur */}
                                <div className="flex items-center gap-3">
                                    <div className="flex-1 h-px bg-white/10"></div>
                                    <span className="text-white/30 text-xs font-bold uppercase">ou</span>
                                    <div className="flex-1 h-px bg-white/10"></div>
                                </div>

                                {/* Bouton Fichier */}
                                <button
                                    onClick={() => fileInputRef.current?.click()}
                                    className="w-full bg-white/5 hover:bg-white/10 border-2 border-dashed border-white/20 hover:border-indigo-500/50 text-white/70 rounded-2xl py-4 px-6 flex items-center justify-center gap-3 font-bold transition-all active:scale-[0.98]"
                                >
                                    <Upload size={22} />
                                    Sélectionner un fichier
                                </button>
                                <input
                                    ref={fileInputRef}
                                    type="file"
                                    accept="image/jpeg,image/png,image/webp,application/pdf"
                                    onChange={handleFileSelect}
                                    className="hidden"
                                />

                                <p className="text-center text-white/30 text-[10px] font-bold">
                                    JPG, PNG ou PDF • 15 MB max
                                </p>
                            </>
                        ) : (
                            <>
                                {/* Aperçu du fichier sélectionné */}
                                <div className="bg-white/5 rounded-2xl p-4">
                                    {previewUrl ? (
                                        <Image
                                            src={previewUrl}
                                            alt="Aperçu"
                                            width={400}
                                            height={208}
                                            className="w-full max-h-52 object-contain rounded-xl mb-3"
                                            unoptimized
                                        />
                                    ) : (
                                        <div className="flex items-center gap-3 mb-3">
                                            <div className="w-12 h-12 bg-red-500/20 rounded-xl flex items-center justify-center">
                                                <FileText size={24} className="text-red-400" />
                                            </div>
                                            <div>
                                                <p className="text-white font-bold text-sm truncate max-w-[200px]">{selectedFile.name}</p>
                                                <p className="text-white/40 text-xs">{(selectedFile.size / 1024).toFixed(0)} KB • PDF</p>
                                            </div>
                                        </div>
                                    )}

                                    <div className="flex items-center gap-2">
                                        <CheckCircle size={14} className="text-emerald-400" />
                                        <span className="text-emerald-400 text-xs font-bold">Fichier prêt à envoyer</span>
                                    </div>
                                </div>

                                {/* Boutons d'action */}
                                <button
                                    onClick={handleUpload}
                                    disabled={isUploading}
                                    className="w-full bg-emerald-600 hover:bg-emerald-700 disabled:bg-slate-600 text-white rounded-2xl py-4 px-6 flex items-center justify-center gap-3 font-black transition-all shadow-lg shadow-emerald-600/20 active:scale-[0.98]"
                                >
                                    {isUploading ? (
                                        <>
                                            <Loader2 size={20} className="animate-spin" />
                                            Envoi en cours...
                                        </>
                                    ) : (
                                        <>
                                            <Upload size={20} />
                                            Envoyer le document
                                        </>
                                    )}
                                </button>

                                <button
                                    onClick={() => {
                                        setSelectedFile(null);
                                        setPreviewUrl(null);
                                    }}
                                    disabled={isUploading}
                                    className="w-full bg-white/5 hover:bg-white/10 text-white/60 rounded-2xl py-3 px-6 text-sm font-bold transition-all"
                                >
                                    Choisir un autre fichier
                                </button>
                            </>
                        )}

                        {/* Erreur */}
                        {uploadError && (
                            <div className="bg-red-500/10 border border-red-500/20 rounded-2xl p-4 flex items-start gap-3">
                                <AlertCircle size={18} className="text-red-400 shrink-0 mt-0.5" />
                                <p className="text-red-300 text-xs font-bold">{uploadError}</p>
                            </div>
                        )}
                    </div>

                    {/* Footer de confiance */}
                    <div className="text-center">
                        <p className="text-white/20 text-[10px] font-bold">
                            🔒 Connexion sécurisée • Vos données sont protégées
                        </p>
                    </div>
                </div>
            </div>
        </div>
    );
}
