'use client';

import React, { useState, useRef } from 'react';
import { DocumentAnalysis, AnalysisResult } from '../../services/DocumentAnalysisService';
import {
    Camera,
    CheckCircle,
    XCircle,
    RefreshCw,
    ChevronRight,
    Sparkles,
    FileText,
    Home,
    CreditCard,
    User,
    Image as ImageIcon,
    Stamp,
    Briefcase,
    GraduationCap,
    Globe,
    AlertTriangle
} from 'lucide-react';

// ============================================
// TYPES
// ============================================
export type Language = 'FR' | 'EN' | 'AR' | 'ES';
export type ScanStatus = 'WAITING' | 'SCANNING' | 'SUCCESS' | 'ERROR';

export interface DocumentStep {
    id: string;
    icon: React.ReactNode;
    required: boolean;
}

interface GuidedScannerProps {
    documents: DocumentStep[];
    onComplete: () => void;
    onDocumentScanned: (docId: string, file: File) => void;
}

// ============================================
// TRADUCTIONS MULTILINGUES
// ============================================
const TRANSLATIONS: Record<Language, Record<string, string>> = {
    FR: {
        passport: 'Votre Passeport',
        domicile: 'Justificatif de domicile',
        photos: 'Photos d\'identité',
        timbre: 'Timbre fiscal',
        travail: 'Contrat de travail',
        acte_naissance: 'Acte de naissance',
        avis_imposition: 'Avis d\'imposition',
        b2_francais: 'Diplôme français B2',
        casier: 'Casier judiciaire',
        permis_etranger: 'Permis étranger',
        traduction: 'Traduction assermentée',
        default: 'Document requis',
        snap: 'Prendre une photo',
        scanning: 'Analyse en cours...',
        success: 'Document validé !',
        error: 'Photo floue ou illisible',
        error_expired: 'Document expiré',
        error_wrong: 'Mauvais document',
        retry: 'Réessayer',
        next: 'Suivant',
        step: 'Étape',
        of: 'sur',
        instruction: 'Prenez une photo claire de',
        tip_good: '✓ Bien éclairé, net',
        tip_bad: '✗ Flou, sombre',
        complete: 'Bravo ! Tout est envoyé',
        waiting: 'Votre dossier est en cours de traitement',
        back_home: 'Retour à l\'accueil'
    },
    EN: {
        passport: 'Your Passport',
        domicile: 'Proof of address',
        photos: 'ID Photos',
        timbre: 'Tax stamp',
        travail: 'Employment contract',
        acte_naissance: 'Birth certificate',
        avis_imposition: 'Tax notice',
        b2_francais: 'French B2 diploma',
        casier: 'Criminal record',
        permis_etranger: 'Foreign license',
        traduction: 'Sworn translation',
        default: 'Required document',
        snap: 'Take a photo',
        scanning: 'Scanning...',
        success: 'Document validated!',
        error: 'Blurry or unreadable',
        error_expired: 'Expired document',
        error_wrong: 'Wrong document',
        retry: 'Retry',
        next: 'Next',
        step: 'Step',
        of: 'of',
        instruction: 'Take a clear photo of',
        tip_good: '✓ Well lit, sharp',
        tip_bad: '✗ Blurry, dark',
        complete: 'Done! All sent',
        waiting: 'Your file is being processed',
        back_home: 'Back to home'
    },
    AR: {
        passport: 'جواز السفر',
        domicile: 'إثبات العنوان',
        photos: 'صور الهوية',
        timbre: 'الطابع الضريبي',
        travail: 'عقد العمل',
        acte_naissance: 'شهادة الميلاد',
        avis_imposition: 'إشعار الضريبة',
        b2_francais: 'شهادة الفرنسية B2',
        casier: 'السجل الجنائي',
        permis_etranger: 'رخصة القيادة الأجنبية',
        traduction: 'ترجمة معتمدة',
        default: 'مستند مطلوب',
        snap: 'التقط صورة',
        scanning: 'جاري التحليل...',
        success: 'تم التحقق!',
        error: 'صورة غير واضحة',
        error_expired: 'مستند منتهي الصلاحية',
        error_wrong: 'مستند خاطئ',
        retry: 'أعد المحاولة',
        next: 'التالي',
        step: 'خطوة',
        of: 'من',
        instruction: 'التقط صورة واضحة لـ',
        tip_good: '✓ إضاءة جيدة',
        tip_bad: '✗ ضبابي، مظلم',
        complete: '!تم إرسال كل شيء',
        waiting: 'ملفك قيد المعالجة',
        back_home: 'العودة للرئيسية'
    },
    ES: {
        passport: 'Su Pasaporte',
        domicile: 'Justificante de domicilio',
        photos: 'Fotos de identidad',
        timbre: 'Timbre fiscal',
        travail: 'Contrato de trabajo',
        acte_naissance: 'Partida de nacimiento',
        avis_imposition: 'Aviso fiscal',
        b2_francais: 'Diploma francés B2',
        casier: 'Antecedentes penales',
        permis_etranger: 'Licencia extranjera',
        traduction: 'Traducción jurada',
        default: 'Documento requerido',
        snap: 'Tomar foto',
        scanning: 'Analizando...',
        success: '¡Documento validado!',
        error: 'Foto borrosa o ilegible',
        error_expired: 'Documento caducado',
        error_wrong: 'Documento incorrecto',
        retry: 'Reintentar',
        next: 'Siguiente',
        step: 'Paso',
        of: 'de',
        instruction: 'Tome una foto clara de',
        tip_good: '✓ Bien iluminado',
        tip_bad: '✗ Borroso, oscuro',
        complete: '¡Listo! Todo enviado',
        waiting: 'Su expediente está en proceso',
        back_home: 'Volver al inicio'
    }
};

// Drapeaux pour le sélecteur de langue
const FLAGS: Record<Language, string> = {
    FR: '🇫🇷',
    EN: '🇬🇧',
    AR: '🇸🇦',
    ES: '🇪🇸'
};

// Icônes par type de document
const DOC_ICONS: Record<string, React.ReactNode> = {
    passport: <Globe size={80} />,
    domicile: <Home size={80} />,
    photos: <ImageIcon size={80} />,
    timbre: <Stamp size={80} />,
    travail: <Briefcase size={80} />,
    acte_naissance: <FileText size={80} />,
    avis_imposition: <CreditCard size={80} />,
    b2_francais: <GraduationCap size={80} />,
    casier: <FileText size={80} />,
    permis_etranger: <CreditCard size={80} />,
    traduction: <FileText size={80} />,
    default: <FileText size={80} />
};

export default function GuidedScanner({
    documents,
    onComplete,
    onDocumentScanned
}: GuidedScannerProps) {
    const [currentStep, setCurrentStep] = useState(0);
    const [language, setLanguage] = useState<Language>('FR');
    const [status, setStatus] = useState<ScanStatus>('WAITING');
    const [scannedDocs, setScannedDocs] = useState<Set<string>>(new Set());
    const [errorMessage, setErrorMessage] = useState<string>('');
    const [analysisResult, setAnalysisResult] = useState<AnalysisResult | null>(null);
    const inputRef = useRef<HTMLInputElement>(null);

    const t = TRANSLATIONS[language];
    const isRTL = language === 'AR';
    const currentDoc = documents[currentStep];
    const docName = t[currentDoc?.id] || t.default;
    const isComplete = currentStep >= documents.length;

    const handleCapture = () => {
        inputRef.current?.click();
    };

    const handleFileChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        setStatus('SCANNING');
        setErrorMessage('');
        setAnalysisResult(null);

        // Utilise le service d'analyse IA
        const result = await DocumentAnalysis.analyze(file);
        setAnalysisResult(result);

        if (result.isValid) {
            setStatus('SUCCESS');
            onDocumentScanned(currentDoc.id, file);
            setScannedDocs(prev => new Set([...prev, currentDoc.id]));

            // Passage automatique après 1.5s
            setTimeout(() => {
                if (currentStep < documents.length - 1) {
                    setCurrentStep(prev => prev + 1);
                    setStatus('WAITING');
                } else {
                    onComplete();
                }
            }, 1500);
        } else {
            setStatus('ERROR');
            // Sélectionne le message selon la langue
            const msg = language === 'EN' ? result.messageEN :
                language === 'AR' ? result.messageAR :
                    language === 'ES' ? result.messageES :
                        result.message;
            setErrorMessage(msg || result.message);
        }

        // Reset input pour permettre de re-sélectionner le même fichier
        e.target.value = '';
    };

    const handleRetry = () => {
        setStatus('WAITING');
    };

    // Écran de complétion
    if (isComplete) {
        return (
            <div className={`min-h-screen bg-gradient-to-b from-emerald-500 to-emerald-600 flex flex-col items-center justify-center p-6 text-white ${isRTL ? 'rtl' : ''}`}>
                <div className="w-32 h-32 bg-white rounded-full flex items-center justify-center mb-8 shadow-2xl animate-bounce">
                    <CheckCircle className="text-emerald-500" size={64} />
                </div>
                <h1 className="text-3xl font-black mb-4 text-center">{t.complete}</h1>
                <p className="text-emerald-100 text-center mb-8">{t.waiting}</p>
                <button
                    onClick={() => window.location.href = '/'}
                    className="bg-white text-emerald-600 px-8 py-4 rounded-2xl font-black text-lg shadow-lg"
                >
                    {t.back_home}
                </button>
            </div>
        );
    }

    return (
        <div className={`min-h-screen bg-gradient-to-b from-indigo-600 to-purple-700 flex flex-col ${isRTL ? 'rtl' : ''}`}>
            {/* Header avec sélecteur de langue */}
            <div className="p-4 flex items-center justify-between">
                {/* Progress */}
                <div className="flex items-center gap-2 text-white/80">
                    <span className="font-bold">{t.step} {currentStep + 1}</span>
                    <span>{t.of}</span>
                    <span className="font-bold">{documents.length}</span>
                </div>

                {/* Language Selector */}
                <div className="flex gap-1 bg-white/10 rounded-full p-1">
                    {(Object.keys(FLAGS) as Language[]).map((lang) => (
                        <button
                            key={lang}
                            onClick={() => setLanguage(lang)}
                            className={`w-10 h-10 rounded-full flex items-center justify-center text-xl transition-all ${language === lang
                                ? 'bg-white shadow-lg scale-110'
                                : 'hover:bg-white/20'
                                }`}
                        >
                            {FLAGS[lang]}
                        </button>
                    ))}
                </div>
            </div>

            {/* Progress Bar */}
            <div className="px-6 mb-8">
                <div className="h-2 bg-white/20 rounded-full overflow-hidden">
                    <div
                        className="h-full bg-white rounded-full transition-all duration-500"
                        style={{ width: `${((currentStep) / documents.length) * 100}%` }}
                    />
                </div>
            </div>

            {/* Main Content */}
            <div className="flex-1 flex flex-col items-center justify-center px-6">
                {status === 'WAITING' && (
                    <div className="text-center animate-in fade-in duration-300">
                        {/* Icône Document */}
                        <div className="w-40 h-40 bg-white/10 rounded-3xl flex items-center justify-center mx-auto mb-8 text-white">
                            {DOC_ICONS[currentDoc?.id] || DOC_ICONS.default}
                        </div>

                        {/* Instruction */}
                        <p className="text-white/70 mb-2">{t.instruction}</p>
                        <h2 className="text-3xl font-black text-white mb-8">{docName}</h2>

                        {/* Tips Visuels */}
                        <div className="flex justify-center gap-4 mb-8">
                            <div className="bg-emerald-500/30 text-white px-4 py-2 rounded-xl text-sm font-bold">
                                {t.tip_good}
                            </div>
                            <div className="bg-red-500/30 text-white px-4 py-2 rounded-xl text-sm font-bold">
                                {t.tip_bad}
                            </div>
                        </div>
                    </div>
                )}

                {status === 'SCANNING' && (
                    <div className="text-center animate-in fade-in duration-300">
                        <div className="w-32 h-32 bg-white/20 rounded-full flex items-center justify-center mx-auto mb-8">
                            <Sparkles className="text-white animate-pulse" size={48} />
                        </div>
                        <h2 className="text-2xl font-black text-white">{t.scanning}</h2>
                    </div>
                )}

                {status === 'SUCCESS' && (
                    <div className="text-center animate-in fade-in zoom-in duration-300">
                        <div className="w-32 h-32 bg-emerald-500 rounded-full flex items-center justify-center mx-auto mb-8 shadow-2xl">
                            <CheckCircle className="text-white" size={64} />
                        </div>
                        <h2 className="text-2xl font-black text-white">{t.success}</h2>
                    </div>
                )}

                {status === 'ERROR' && (
                    <div className="text-center animate-in fade-in duration-300">
                        <div className="w-32 h-32 bg-red-500 rounded-full flex items-center justify-center mx-auto mb-8 shadow-2xl">
                            {analysisResult?.status === 'REJECTED_EXPIRED' ? (
                                <AlertTriangle className="text-white" size={64} />
                            ) : analysisResult?.status === 'REJECTED_WRONG_TYPE' ? (
                                <XCircle className="text-white" size={64} />
                            ) : (
                                <XCircle className="text-white" size={64} />
                            )}
                        </div>
                        <h2 className="text-2xl font-black text-white mb-2">
                            {analysisResult?.status === 'REJECTED_EXPIRED' ? t.error_expired :
                                analysisResult?.status === 'REJECTED_WRONG_TYPE' ? t.error_wrong :
                                    t.error}
                        </h2>
                        <p className="text-white/70 text-sm mb-6 px-8">{errorMessage}</p>
                        {analysisResult?.confidence && (
                            <p className="text-white/50 text-xs mb-4">
                                Confiance IA: {analysisResult.confidence}%
                            </p>
                        )}
                        <button
                            onClick={handleRetry}
                            className="bg-white text-red-600 px-8 py-3 rounded-2xl font-black flex items-center gap-2 mx-auto"
                        >
                            <RefreshCw size={20} />
                            {t.retry}
                        </button>
                    </div>
                )}
            </div>

            {/* Capture Button */}
            {(status === 'WAITING') && (
                <div className="p-8 pb-safe">
                    <input
                        ref={inputRef}
                        type="file"
                        accept="image/*"
                        capture="environment"
                        className="hidden"
                        onChange={handleFileChange}
                    />
                    <button
                        onClick={handleCapture}
                        className="w-24 h-24 bg-white rounded-full mx-auto flex items-center justify-center shadow-2xl active:scale-95 transition-transform"
                    >
                        <Camera className="text-indigo-600" size={40} />
                    </button>
                    <p className="text-center text-white/70 mt-4 font-medium">{t.snap}</p>
                </div>
            )}

            {/* Skip indicator (bottom dots) */}
            <div className="pb-6 flex justify-center gap-2">
                {documents.map((_, idx) => (
                    <div
                        key={idx}
                        className={`w-2 h-2 rounded-full transition-all ${idx < currentStep
                            ? 'bg-emerald-400'
                            : idx === currentStep
                                ? 'bg-white w-6'
                                : 'bg-white/30'
                            }`}
                    />
                ))}
            </div>
        </div>
    );
}
