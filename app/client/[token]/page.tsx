'use client';

import React, { useState, useEffect, useRef } from 'react';
import { useParams } from 'next/navigation';
import {
    Shield, Loader2, AlertCircle, CheckCircle, Clock, XCircle,
    Upload, Camera, FileText, ChevronDown, ChevronUp, ArrowUpCircle
} from 'lucide-react';

const API_URL = 'http://localhost:4000';

interface ChecklistItem {
    id: string;
    name: string;
    description: string;
    category: string;
    required: boolean;
    status: 'NOT_UPLOADED' | 'PENDING' | 'VALID' | 'REJECTED';
    fileUrl: string | null;
    uploadedAt: string | null;
    rejectionReason: string | null;
    uploadToken: string;
}

interface ClientSpaceData {
    leadId: string;
    name: string;
    serviceName: string;
    status: string;
    statusLabel: string;
    createdAt: string;
    checklist: ChecklistItem[];
    progress: {
        total: number;
        validated: number;
        pending: number;
        rejected: number;
        notUploaded: number;
        percentage: number;
    };
}

const STATUS_CONFIG: Record<string, { icon: React.ReactNode; color: string; bg: string; label: string }> = {
    NOT_UPLOADED: {
        icon: <Upload size={16} />,
        color: 'text-slate-400',
        bg: 'bg-slate-500/10 border-slate-500/20',
        label: 'À déposer'
    },
    PENDING: {
        icon: <Clock size={16} />,
        color: 'text-amber-400',
        bg: 'bg-amber-500/10 border-amber-500/20',
        label: 'En attente de validation'
    },
    VALID: {
        icon: <CheckCircle size={16} />,
        color: 'text-emerald-400',
        bg: 'bg-emerald-500/10 border-emerald-500/20',
        label: 'Validé ✓'
    },
    REJECTED: {
        icon: <XCircle size={16} />,
        color: 'text-red-400',
        bg: 'bg-red-500/10 border-red-500/20',
        label: 'Refusé — à renvoyer'
    }
};

const CATEGORY_LABELS: Record<string, string> = {
    IDENTITY: '🪪 Identité',
    RESIDENCE: '🏠 Résidence',
    FINANCIAL: '💰 Finances',
    CIVIL: '📜 État civil',
    PROFESSIONAL: '💼 Professionnel',
    EDUCATION: '🎓 Éducation',
    OTHER: '📎 Autres',
};

export default function ClientSpacePage() {
    const params = useParams();
    const token = params.token as string;

    const [data, setData] = useState<ClientSpaceData | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [isInvalid, setIsInvalid] = useState(false);

    // Upload state
    const [uploadingDocId, setUploadingDocId] = useState<string | null>(null);
    const [uploadProgress, setUploadProgress] = useState<string | null>(null);
    const [expandedDoc, setExpandedDoc] = useState<string | null>(null);
    const fileInputRef = useRef<HTMLInputElement>(null);

    const fetchData = async () => {
        try {
            const res = await fetch(`${API_URL}/public/leads/client-space/${token}`);
            if (!res.ok) { setIsInvalid(true); return; }
            const json = await res.json();
            setData(json);
        } catch { setIsInvalid(true); }
        finally { setIsLoading(false); }
    };

    useEffect(() => { fetchData(); }, [token]);

    const handleUpload = async (docItem: ChecklistItem, file: File) => {
        setUploadingDocId(docItem.id);
        setUploadProgress('Envoi en cours...');

        try {
            const formData = new FormData();
            formData.append('file', file);

            const res = await fetch(`${API_URL}/public/leads/upload/${docItem.uploadToken}`, {
                method: 'POST',
                body: formData
            });

            if (res.ok) {
                setUploadProgress('✅ Envoyé !');
                // Recharger les données pour mettre à jour le statut
                setTimeout(async () => {
                    await fetchData();
                    setUploadingDocId(null);
                    setUploadProgress(null);
                }, 1500);
            } else {
                const err = await res.json();
                setUploadProgress(`❌ ${err.message || 'Erreur'}`);
                setTimeout(() => { setUploadingDocId(null); setUploadProgress(null); }, 3000);
            }
        } catch {
            setUploadProgress('❌ Erreur réseau');
            setTimeout(() => { setUploadingDocId(null); setUploadProgress(null); }, 3000);
        }
    };

    const triggerFileSelect = (docItem: ChecklistItem, capture?: boolean) => {
        setExpandedDoc(docItem.id);
        const input = fileInputRef.current;
        if (input) {
            input.setAttribute('data-doc-id', docItem.id);
            input.setAttribute('data-upload-token', docItem.uploadToken);
            if (capture) {
                input.setAttribute('capture', 'environment');
            } else {
                input.removeAttribute('capture');
            }
            input.click();
        }
    };

    const onFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        const docId = e.target.getAttribute('data-doc-id');
        const uploadToken = e.target.getAttribute('data-upload-token');
        if (!docId || !uploadToken || !data) return;

        const docItem = data.checklist.find(d => d.id === docId);
        if (docItem) {
            handleUpload(docItem, file);
        }

        // Reset input
        e.target.value = '';
    };

    // ── Chargement ──
    if (isLoading) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-indigo-950 to-slate-900 flex items-center justify-center">
                <div className="text-center">
                    <Loader2 className="animate-spin text-indigo-400 mx-auto mb-4" size={48} />
                    <p className="text-white/60 font-medium">Chargement de votre espace...</p>
                </div>
            </div>
        );
    }

    // ── Lien invalide ──
    if (isInvalid || !data) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-red-950 to-slate-900 flex items-center justify-center p-6">
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-10 max-w-md w-full text-center">
                    <div className="w-20 h-20 bg-red-500/20 rounded-full flex items-center justify-center mx-auto mb-6">
                        <AlertCircle size={40} className="text-red-400" />
                    </div>
                    <h1 className="text-2xl font-black text-white mb-3">Lien expiré</h1>
                    <p className="text-white/60 text-sm">Ce lien d'accès est invalide ou expiré. Contactez votre conseiller Simulegal.</p>
                </div>
            </div>
        );
    }

    // Grouper les docs par catégorie
    const categories = data.checklist.reduce((acc, doc) => {
        const cat = doc.category || 'OTHER';
        if (!acc[cat]) acc[cat] = [];
        acc[cat].push(doc);
        return acc;
    }, {} as Record<string, ChecklistItem[]>);

    const progressColor = data.progress.percentage === 100 ? 'bg-emerald-500' :
        data.progress.percentage > 50 ? 'bg-indigo-500' : 'bg-amber-500';

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-indigo-950 to-slate-900">
            {/* Hidden file input */}
            <input
                ref={fileInputRef}
                type="file"
                accept="image/jpeg,image/png,image/webp,application/pdf"
                onChange={onFileChange}
                className="hidden"
            />

            {/* Header */}
            <div className="sticky top-0 z-50 bg-slate-900/80 backdrop-blur-xl border-b border-white/5">
                <div className="max-w-lg mx-auto px-6 py-4">
                    <div className="flex items-center gap-3">
                        <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center shadow-lg shadow-indigo-600/30">
                            <Shield size={20} className="text-white" />
                        </div>
                        <div>
                            <h1 className="text-white font-black text-sm">Simulegal</h1>
                            <p className="text-white/40 text-[10px] font-bold uppercase tracking-widest">Espace Client</p>
                        </div>
                    </div>
                </div>
            </div>

            <div className="max-w-lg mx-auto px-6 py-6 space-y-6">
                {/* Welcome Card */}
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-6">
                    <p className="text-white/40 text-[10px] font-bold uppercase tracking-widest mb-1">Bonjour</p>
                    <h2 className="text-2xl font-black text-white mb-1">{data.name}</h2>
                    <p className="text-indigo-400 text-sm font-bold">{data.serviceName}</p>

                    <div className="mt-4 flex items-center gap-2">
                        <span className="px-3 py-1 bg-indigo-500/20 text-indigo-300 text-xs font-black rounded-full">
                            {data.statusLabel}
                        </span>
                        <span className="text-white/30 text-xs">
                            Dossier n° {data.leadId}
                        </span>
                    </div>
                </div>

                {/* Progress Card */}
                <div className="bg-white/5 backdrop-blur-xl border border-white/10 rounded-3xl p-6">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="text-white font-bold text-sm">Progression du dossier</h3>
                        <span className="text-2xl font-black text-white">{data.progress.percentage}%</span>
                    </div>

                    {/* Progress Bar */}
                    <div className="w-full h-3 bg-white/5 rounded-full overflow-hidden mb-4">
                        <div
                            className={`h-full ${progressColor} rounded-full transition-all duration-1000 ease-out`}
                            style={{ width: `${data.progress.percentage}%` }}
                        />
                    </div>

                    {/* Stats */}
                    <div className="grid grid-cols-4 gap-2">
                        <div className="text-center">
                            <div className="text-lg font-black text-emerald-400">{data.progress.validated}</div>
                            <div className="text-[10px] text-white/40 font-bold">Validé(s)</div>
                        </div>
                        <div className="text-center">
                            <div className="text-lg font-black text-amber-400">{data.progress.pending}</div>
                            <div className="text-[10px] text-white/40 font-bold">En attente</div>
                        </div>
                        <div className="text-center">
                            <div className="text-lg font-black text-red-400">{data.progress.rejected}</div>
                            <div className="text-[10px] text-white/40 font-bold">Refusé(s)</div>
                        </div>
                        <div className="text-center">
                            <div className="text-lg font-black text-slate-400">{data.progress.notUploaded}</div>
                            <div className="text-[10px] text-white/40 font-bold">À déposer</div>
                        </div>
                    </div>
                </div>

                {/* Documents by Category */}
                {Object.entries(categories).map(([category, docs]) => (
                    <div key={category} className="space-y-3">
                        <h3 className="text-white/50 text-xs font-black uppercase tracking-widest px-2">
                            {CATEGORY_LABELS[category] || category}
                        </h3>

                        {docs.map(doc => {
                            const config = STATUS_CONFIG[doc.status];
                            const isExpanded = expandedDoc === doc.id;
                            const isCurrentlyUploading = uploadingDocId === doc.id;
                            const canUpload = doc.status === 'NOT_UPLOADED' || doc.status === 'REJECTED';

                            return (
                                <div
                                    key={doc.id}
                                    className={`bg-white/5 backdrop-blur-xl border rounded-2xl overflow-hidden transition-all ${config.bg}`}
                                >
                                    {/* Doc Header */}
                                    <button
                                        onClick={() => setExpandedDoc(isExpanded ? null : doc.id)}
                                        className="w-full flex items-center gap-3 p-4 text-left"
                                    >
                                        <div className={`${config.color} shrink-0`}>
                                            {config.icon}
                                        </div>
                                        <div className="flex-1 min-w-0">
                                            <p className="text-white font-bold text-sm truncate">{doc.name}</p>
                                            <p className={`text-xs font-bold ${config.color}`}>{config.label}</p>
                                        </div>
                                        {isExpanded ? (
                                            <ChevronUp size={16} className="text-white/30 shrink-0" />
                                        ) : (
                                            <ChevronDown size={16} className="text-white/30 shrink-0" />
                                        )}
                                    </button>

                                    {/* Doc Details (expanded) */}
                                    {isExpanded && (
                                        <div className="px-4 pb-4 space-y-3">
                                            {doc.description && (
                                                <p className="text-white/40 text-xs leading-relaxed">{doc.description}</p>
                                            )}

                                            {doc.status === 'REJECTED' && doc.rejectionReason && (
                                                <div className="bg-red-500/10 border border-red-500/20 rounded-xl p-3">
                                                    <p className="text-red-300 text-xs font-bold">
                                                        ❌ Motif du refus : {doc.rejectionReason}
                                                    </p>
                                                </div>
                                            )}

                                            {doc.uploadedAt && doc.status !== 'NOT_UPLOADED' && (
                                                <p className="text-white/30 text-[10px] font-bold">
                                                    Déposé le {new Date(doc.uploadedAt).toLocaleDateString('fr-FR', {
                                                        day: 'numeric', month: 'long', year: 'numeric', hour: '2-digit', minute: '2-digit'
                                                    })}
                                                </p>
                                            )}

                                            {/* Upload Buttons */}
                                            {canUpload && !isCurrentlyUploading && (
                                                <div className="flex gap-2">
                                                    <button
                                                        onClick={() => triggerFileSelect(doc, true)}
                                                        className="flex-1 bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl py-3 px-4 flex items-center justify-center gap-2 text-xs font-bold transition-all active:scale-[0.98]"
                                                    >
                                                        <Camera size={16} />
                                                        Photo
                                                    </button>
                                                    <button
                                                        onClick={() => triggerFileSelect(doc)}
                                                        className="flex-1 bg-white/10 hover:bg-white/15 text-white rounded-xl py-3 px-4 flex items-center justify-center gap-2 text-xs font-bold transition-all active:scale-[0.98]"
                                                    >
                                                        <FileText size={16} />
                                                        Fichier
                                                    </button>
                                                </div>
                                            )}

                                            {/* Upload in progress */}
                                            {isCurrentlyUploading && (
                                                <div className="flex items-center gap-3 py-2">
                                                    {uploadProgress?.startsWith('✅') ? (
                                                        <CheckCircle size={18} className="text-emerald-400" />
                                                    ) : uploadProgress?.startsWith('❌') ? (
                                                        <XCircle size={18} className="text-red-400" />
                                                    ) : (
                                                        <Loader2 size={18} className="animate-spin text-indigo-400" />
                                                    )}
                                                    <span className="text-white/70 text-xs font-bold">{uploadProgress}</span>
                                                </div>
                                            )}
                                        </div>
                                    )}
                                </div>
                            );
                        })}
                    </div>
                ))}

                {/* Footer */}
                <div className="text-center py-6">
                    <p className="text-white/20 text-[10px] font-bold">
                        🔒 Espace sécurisé • Connexion chiffrée • Simulegal © 2026
                    </p>
                </div>
            </div>
        </div>
    );
}
