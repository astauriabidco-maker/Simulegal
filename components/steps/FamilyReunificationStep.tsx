'use client';

import React, { useState, useMemo, useCallback, useEffect, useRef } from 'react';
import { UserProfile } from '@/types';
import {
    ArrowRight, ArrowLeft, Shield, ShieldAlert, Heart,
    Scale, ExternalLink, CheckCircle2, AlertCircle,
    ClipboardList, Pencil, Users, Wallet, Home, XCircle
} from 'lucide-react';
import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface FamilyReunificationStepProps {
    userProfile: UserProfile;
    updateProfile: (updates: Partial<UserProfile>) => void;
    onNext: () => void;
}

// ─── Page IDs ───
type PageId =
    | 'TITRE_SEJOUR'
    | 'STOP_TITRE'
    | 'POLYGAMIE'
    | 'STOP_POLYGAMIE'
    | 'SITUATION_CONJUGALE'
    | 'STOP_MARITAL'
    | 'QUI_FAIRE_VENIR'
    | 'NB_PERSONNES'
    | 'NATIONALITE'
    | 'DUREE_SEJOUR'
    | 'AAH'
    | 'RESSOURCES'
    | 'LOGEMENT'
    | 'RECAP';

export default function FamilyReunificationStep({ userProfile, updateProfile, onNext }: FamilyReunificationStepProps) {
    const [currentPage, setCurrentPage] = useState<PageId>('TITRE_SEJOUR');
    const [direction, setDirection] = useState<'forward' | 'backward'>('forward');
    const containerRef = useRef<HTMLDivElement>(null);
    const family = userProfile.family;

    const handleUpdate = useCallback((updates: Partial<UserProfile['family']>) => {
        updateProfile({ family: { ...family, ...updates } });
    }, [family, updateProfile]);

    const updateWork = useCallback((updates: Partial<UserProfile['work']>) => {
        updateProfile({ work: { ...userProfile.work, ...updates } });
    }, [userProfile.work, updateProfile]);

    // ─── Dynamic page flow ───
    const pageFlow = useMemo((): PageId[] => {
        const pages: PageId[] = ['TITRE_SEJOUR'];

        if (family.rf_has_valid_titre_sejour === false) return [...pages, 'STOP_TITRE'];
        if (family.rf_has_valid_titre_sejour !== true) return pages;

        pages.push('POLYGAMIE');
        if (family.is_polygamous === true) return [...pages, 'STOP_POLYGAMIE'];
        if (family.is_polygamous !== false) return pages;

        pages.push('SITUATION_CONJUGALE');
        if (family.rf_marital_status === 'CIVIL_PARTNER' || family.rf_marital_status === 'CONCUBIN') {
            return [...pages, 'STOP_MARITAL'];
        }
        if (!family.rf_marital_status) return pages;

        // Only married users need "who to bring"
        if (family.rf_marital_status === 'MARRIED') {
            pages.push('QUI_FAIRE_VENIR');
        }

        pages.push('NB_PERSONNES', 'NATIONALITE', 'DUREE_SEJOUR', 'AAH');

        if (family.has_handicap_allowance === false) {
            pages.push('RESSOURCES');
        }

        pages.push('LOGEMENT', 'RECAP');
        return pages;
    }, [family]);

    const currentIndex = pageFlow.indexOf(currentPage);
    const totalAnswerablePages = pageFlow.filter(p => !p.startsWith('STOP_') && p !== 'RECAP').length;
    const answeredPages = pageFlow.slice(0, currentIndex).filter(p => !p.startsWith('STOP_') && p !== 'RECAP').length;
    const progressPercent = totalAnswerablePages > 0 ? Math.round((answeredPages / totalAnswerablePages) * 100) : 0;

    const isStopPage = currentPage.startsWith('STOP_');

    // ─── Pending advance mechanism (avoids stale closure) ───
    const pendingAdvanceRef = useRef(false);
    const [, setTick] = useState(0); // dummy state to force re-render

    // When pendingAdvance is set, advance on next render (after state reconciled)
    useEffect(() => {
        if (!pendingAdvanceRef.current) return;
        pendingAdvanceRef.current = false;
        const idx = pageFlow.indexOf(currentPage);
        if (idx < pageFlow.length - 1) {
            const nextPage = pageFlow[idx + 1];
            setDirection('forward');
            setCurrentPage(nextPage);
        } else if (!isStopPage) {
            onNext();
        }
    }); // no deps — run after every render when pending

    // ─── Navigation helpers ───
    const goTo = useCallback((page: PageId, dir: 'forward' | 'backward' = 'forward') => {
        setDirection(dir);
        setCurrentPage(page);
    }, []);

    const goNext = useCallback(() => {
        const idx = pageFlow.indexOf(currentPage);
        if (idx < pageFlow.length - 1) {
            goTo(pageFlow[idx + 1], 'forward');
        } else if (!isStopPage) {
            onNext();
        }
    }, [pageFlow, currentPage, goTo, onNext, isStopPage]);

    const goPrev = useCallback(() => {
        const idx = pageFlow.indexOf(currentPage);
        if (idx > 0) {
            goTo(pageFlow[idx - 1], 'backward');
        }
    }, [pageFlow, currentPage, goTo]);

    // ─── Auto-advance: triggers AFTER next React render via ref ───
    const autoAdvance = useCallback((delay = 400) => {
        setTimeout(() => {
            pendingAdvanceRef.current = true;
            setTick(t => t + 1); // force re-render to trigger the effect
        }, delay);
    }, []);

    // ─── Min surface calculation ───
    const minSurface = 16 + Math.max(0, ((family.rf_family_members_count || 1) - 1)) * 9;
    const surfaceOk = (family.rf_housing_surface || 0) >= minSurface;

    // ─── Scroll to top on page change ───
    useEffect(() => {
        containerRef.current?.scrollTo({ top: 0, behavior: 'smooth' });
    }, [currentPage]);

    // ─── Reusable button styles ───
    const ChoiceButton = ({ selected, positive, label, onClick, subtitle }: {
        selected: boolean; positive?: boolean; label: string; onClick: () => void; subtitle?: string;
    }) => (
        <button
            onClick={onClick}
            className={cn(
                "w-full p-7 rounded-[2rem] font-black text-xl transition-all duration-300 border-2 text-left",
                "hover:scale-[1.01] active:scale-[0.99]",
                selected
                    ? positive
                        ? "bg-emerald-600 border-emerald-600 text-white shadow-xl shadow-emerald-100"
                        : "bg-rose-600 border-rose-600 text-white shadow-xl shadow-rose-100"
                    : "bg-white border-slate-100 text-slate-700 hover:border-slate-200 hover:shadow-md"
            )}
        >
            <span>{label}</span>
            {subtitle && (
                <span className={cn(
                    "block text-xs font-bold uppercase tracking-widest mt-2",
                    selected ? "opacity-70" : "text-slate-400"
                )}>{subtitle}</span>
            )}
        </button>
    );

    // ─── Page wrapper ───
    const PageWrapper = ({ children, icon, title, subtitle, article }: {
        children: React.ReactNode;
        icon: React.ReactNode;
        title: string;
        subtitle?: string;
        article?: string;
    }) => (
        <div
            key={currentPage}
            className={cn(
                "flex flex-col items-center justify-center min-h-[60vh] max-w-2xl mx-auto px-6 py-12 space-y-10",
                direction === 'forward' ? 'rf-slide-right' : 'rf-slide-left'
            )}
        >
            <div className="text-center space-y-4">
                <div className="inline-flex p-4 bg-slate-50 rounded-3xl mb-2">{icon}</div>
                <h2 className="text-3xl sm:text-4xl font-black text-slate-900 tracking-tight leading-tight">{title}</h2>
                {subtitle && <p className="text-slate-500 text-lg font-medium max-w-lg mx-auto">{subtitle}</p>}
                {article && (
                    <p className="text-indigo-500 text-xs font-black uppercase tracking-widest">{article}</p>
                )}
            </div>
            <div className="w-full space-y-5">{children}</div>
        </div>
    );

    // ─── STOP page component ───
    const StopPage = ({ title, description, article, cta, suggestion, icon: Icon }: {
        title: string; description: string; article: string; cta: string; suggestion?: string;
        icon: typeof XCircle;
    }) => (
        <div key={currentPage} className="flex flex-col items-center justify-center min-h-[60vh] max-w-2xl mx-auto px-6 py-12 rf-zoom-in">
            <div className="w-full p-10 bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900 rounded-[2.5rem] shadow-2xl text-white space-y-8">
                <div className="flex items-start gap-5">
                    <div className="p-3 bg-rose-500/20 rounded-2xl shrink-0">
                        <Icon className="w-10 h-10 text-rose-400" />
                    </div>
                    <div className="space-y-4 flex-1">
                        <div className="flex items-center gap-3 flex-wrap">
                            <span className="px-4 py-1.5 bg-rose-500 text-white text-xs font-black rounded-full uppercase tracking-wider">
                                Non éligible
                            </span>
                            <span className="text-slate-400 text-xs font-bold">{article}</span>
                        </div>
                        <h3 className="text-3xl font-black leading-tight">{title}</h3>
                        <p className="text-slate-300 font-medium leading-relaxed text-lg">{description}</p>
                        {suggestion && (
                            <p className="text-emerald-400 text-sm font-bold flex items-start gap-2 mt-4">
                                <CheckCircle2 className="w-5 h-5 shrink-0 mt-0.5" />
                                {suggestion}
                            </p>
                        )}
                    </div>
                </div>
                <button className="w-full flex items-center justify-center gap-3 px-8 py-6 bg-white/10 hover:bg-white/20 font-black rounded-2xl transition-all text-lg border border-white/10">
                    <Scale className="w-5 h-5" />
                    {cta}
                    <ExternalLink className="w-4 h-4 opacity-50" />
                </button>
            </div>
        </div>
    );

    return (
        <div ref={containerRef} className="relative">
            {/* ─── Progress bar ─── */}
            {!isStopPage && (
                <div className="max-w-sm mx-auto px-6 pt-6 mb-4">
                    <div className="flex items-center justify-between text-xs font-bold text-slate-400 mb-2">
                        <span>Question {answeredPages + 1}/{totalAnswerablePages}</span>
                        <span>{progressPercent}%</span>
                    </div>
                    <div className="h-1.5 bg-slate-100 rounded-full overflow-hidden">
                        <div
                            className="h-full bg-indigo-600 rounded-full transition-all duration-700 ease-out"
                            style={{ width: `${progressPercent}%` }}
                        />
                    </div>
                </div>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: TITRE DE SÉJOUR                                     */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'TITRE_SEJOUR' && (
                <PageWrapper
                    icon={<Shield className="w-10 h-10 text-indigo-600" />}
                    title="Disposez-vous d'un titre de séjour en cours de validité ?"
                    subtitle="Le demandeur doit justifier d'un titre de séjour d'au moins 1 an."
                    article="CESEDA L434-1"
                >
                    <ChoiceButton
                        label="Oui"
                        selected={family.rf_has_valid_titre_sejour === true}
                        positive
                        onClick={() => { handleUpdate({ rf_has_valid_titre_sejour: true }); autoAdvance(); }}
                    />
                    <ChoiceButton
                        label="Non"
                        selected={family.rf_has_valid_titre_sejour === false}
                        onClick={() => { handleUpdate({ rf_has_valid_titre_sejour: false }); autoAdvance(); }}
                    />
                </PageWrapper>
            )}

            {/* STOP: Titre de séjour */}
            {currentPage === 'STOP_TITRE' && (
                <StopPage
                    icon={Shield}
                    title="Titre de séjour requis"
                    description="Le regroupement familial nécessite un titre de séjour en cours de validité d'une durée d'au moins 1 an. Vous devez d'abord régulariser votre situation administrative."
                    article="CESEDA L434-1"
                    cta="Consulter un juriste"
                />
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: POLYGAMIE                                           */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'POLYGAMIE' && (
                <PageWrapper
                    icon={<ShieldAlert className="w-10 h-10 text-rose-500" />}
                    title="Êtes-vous en situation de polygamie ?"
                    subtitle="La polygamie est une exclusion absolue du regroupement familial."
                    article="CESEDA L434-6"
                >
                    <ChoiceButton
                        label="Non"
                        selected={family.is_polygamous === false}
                        positive
                        onClick={() => { handleUpdate({ is_polygamous: false }); autoAdvance(); }}
                    />
                    <ChoiceButton
                        label="Oui"
                        selected={family.is_polygamous === true}
                        onClick={() => { handleUpdate({ is_polygamous: true }); autoAdvance(); }}
                    />
                </PageWrapper>
            )}

            {/* STOP: Polygamie */}
            {currentPage === 'STOP_POLYGAMIE' && (
                <StopPage
                    icon={ShieldAlert}
                    title="Regroupement familial interdit"
                    description="Le regroupement familial est interdit en situation de polygamie. Cette exclusion est absolue et ne peut faire l'objet d'aucune dérogation."
                    article="CESEDA L434-6"
                    cta="Consulter un juriste"
                />
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: SITUATION CONJUGALE                                  */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'SITUATION_CONJUGALE' && (
                <PageWrapper
                    icon={<Heart className="w-10 h-10 text-pink-500" />}
                    title="Quelle est votre situation conjugale ?"
                    subtitle="Le regroupement familial concerne les conjoints mariés."
                    article="CESEDA L434-2"
                >
                    {([
                        { label: 'Marié(e)', value: 'MARRIED' as const, subtitle: 'Éligible au RF' },
                        { label: 'Célibataire', value: 'SINGLE' as const, subtitle: 'Enfants mineurs uniquement' },
                        { label: 'Pacsé(e)', value: 'CIVIL_PARTNER' as const, subtitle: 'Autre procédure disponible' },
                        { label: 'Concubin(e)', value: 'CONCUBIN' as const, subtitle: 'Autre procédure disponible' },
                    ] as const).map((opt) => (
                        <ChoiceButton
                            key={opt.value}
                            label={opt.label}
                            subtitle={opt.subtitle}
                            selected={family.rf_marital_status === opt.value}
                            positive={opt.value === 'MARRIED' || opt.value === 'SINGLE'}
                            onClick={() => { handleUpdate({ rf_marital_status: opt.value }); autoAdvance(500); }}
                        />
                    ))}
                </PageWrapper>
            )}

            {/* STOP: Marital */}
            {currentPage === 'STOP_MARITAL' && (
                <StopPage
                    icon={Heart}
                    title={family.rf_marital_status === 'CIVIL_PARTNER'
                        ? "Le PACS n'ouvre pas droit au RF"
                        : "Le concubinage n'ouvre pas droit au RF"}
                    description={family.rf_marital_status === 'CIVIL_PARTNER'
                        ? "Le regroupement familial est réservé aux conjoints mariés. En tant que pacsé(e), vous pouvez demander un titre « vie privée et familiale » — c'est une procédure différente mais accessible."
                        : "Le regroupement familial concerne uniquement les conjoints mariés. En tant que concubin(e), la procédure « vie privée et familiale » peut être une alternative si vous justifiez d'une vie commune stable."}
                    article={family.rf_marital_status === 'CIVIL_PARTNER' ? "CESEDA L434-2" : "CESEDA L434-2 / L423-23"}
                    cta="Découvrir les alternatives"
                    suggestion="Un mariage ou un PACS préalable pourrait aussi ouvrir de nouvelles voies."
                />
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: QUI FAIRE VENIR                                     */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'QUI_FAIRE_VENIR' && (
                <PageWrapper
                    icon={<Users className="w-10 h-10 text-blue-600" />}
                    title="Qui souhaitez-vous faire venir ?"
                >
                    {([
                        { label: 'Mon conjoint(e) et mes enfants', value: 'SPOUSE_AND_CHILDREN' as const },
                        { label: 'Mon conjoint(e) uniquement', value: 'SPOUSE_ONLY' as const },
                        { label: 'Mes enfants uniquement', value: 'CHILDREN_ONLY' as const },
                    ] as const).map((opt) => (
                        <ChoiceButton
                            key={opt.value}
                            label={opt.label}
                            selected={family.rf_who_to_bring === opt.value}
                            positive
                            onClick={() => { handleUpdate({ rf_who_to_bring: opt.value }); autoAdvance(400); }}
                        />
                    ))}
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: NOMBRE DE PERSONNES                                 */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'NB_PERSONNES' && (
                <PageWrapper
                    icon={<Users className="w-10 h-10 text-blue-600" />}
                    title="Combien de personnes souhaitez-vous faire venir ?"
                    subtitle={family.rf_marital_status === 'SINGLE'
                        ? "Enfants de moins de 18 ans."
                        : "Conjoint(e) et/ou enfants de moins de 18 ans."}
                >
                    <div className="grid grid-cols-4 gap-4">
                        {[1, 2, 3, 4].map((n) => (
                            <button
                                key={n}
                                onClick={() => { handleUpdate({ rf_family_members_count: n }); autoAdvance(); }}
                                className={cn(
                                    "p-8 rounded-[2rem] font-black text-3xl transition-all duration-300 border-2",
                                    "hover:scale-105 active:scale-95",
                                    family.rf_family_members_count === n
                                        ? "bg-indigo-600 border-indigo-600 text-white shadow-xl shadow-indigo-100"
                                        : "bg-white border-slate-100 text-slate-600 hover:border-indigo-200 hover:shadow-md"
                                )}
                            >
                                {n}{n === 4 ? '+' : ''}
                            </button>
                        ))}
                    </div>
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: NATIONALITÉ                                         */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'NATIONALITE' && (
                <PageWrapper
                    icon={<span className="text-4xl">🌍</span>}
                    title="Quelle est votre nationalité ?"
                    subtitle="Les ressortissants algériens bénéficient d'un accord spécifique."
                >
                    <ChoiceButton
                        label="🇩🇿  Algérienne"
                        subtitle="Accord franco-algérien — 12 mois de séjour"
                        selected={family.sponsor_nationality === 'ALGERIAN'}
                        positive
                        onClick={() => { handleUpdate({ sponsor_nationality: 'ALGERIAN' as any }); autoAdvance(); }}
                    />
                    <ChoiceButton
                        label="🌍  Autre nationalité"
                        subtitle="Droit commun CESEDA — 18 mois de séjour"
                        selected={family.sponsor_nationality === 'OTHER'}
                        positive
                        onClick={() => { handleUpdate({ sponsor_nationality: 'OTHER' as any }); autoAdvance(); }}
                    />
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: DURÉE DE SÉJOUR                                     */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'DUREE_SEJOUR' && (
                <PageWrapper
                    icon={<span className="text-4xl">⏳</span>}
                    title="Depuis combien de temps résidez-vous régulièrement en France ?"
                    subtitle={family.sponsor_nationality === 'ALGERIAN'
                        ? "Minimum requis : 12 mois (accord franco-algérien)."
                        : "Minimum requis : 18 mois (droit commun CESEDA)."}
                >
                    {([
                        { label: 'Moins de 12 mois', value: 'LESS_12' },
                        { label: '12 à 18 mois', value: '12_18' },
                        { label: 'Plus de 18 mois', value: 'MORE_18' },
                    ] as const).map((opt) => (
                        <ChoiceButton
                            key={opt.value}
                            label={opt.label}
                            selected={family.presence_duration === opt.value}
                            positive
                            onClick={() => { handleUpdate({ presence_duration: opt.value as any }); autoAdvance(); }}
                        />
                    ))}
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: AAH / ASI                                           */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'AAH' && (
                <PageWrapper
                    icon={<Wallet className="w-10 h-10 text-emerald-600" />}
                    title="Percevez-vous l'AAH ou l'ASI ?"
                    subtitle="Si oui, la condition de ressources est automatiquement remplie."
                >
                    <ChoiceButton label="Oui" selected={family.has_handicap_allowance === true} positive
                        onClick={() => { handleUpdate({ has_handicap_allowance: true }); autoAdvance(); }} />
                    <ChoiceButton label="Non" selected={family.has_handicap_allowance === false} positive
                        onClick={() => { handleUpdate({ has_handicap_allowance: false }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: RESSOURCES (source + montant groupés)               */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'RESSOURCES' && (
                <PageWrapper
                    icon={<Wallet className="w-10 h-10 text-emerald-600" />}
                    title="Quels sont vos revenus ?"
                    subtitle="L'administration vérifie la stabilité et le montant de vos ressources."
                >
                    {/* Source */}
                    <div className="space-y-3">
                        <label className="block text-sm font-black uppercase tracking-widest text-slate-400">Source principale</label>
                        <div className="grid grid-cols-2 gap-3">
                            {([
                                { label: "Travail", value: "SALARY" },
                                { label: "Retraite", value: "PENSION" },
                                { label: "Chômage", value: "OTHER" },
                                { label: "RSA / Aides", value: "RSA_ALOWANCE" },
                            ] as const).map((opt) => (
                                <button
                                    key={opt.value}
                                    onClick={() => handleUpdate({ income_source: opt.value as any })}
                                    className={cn(
                                        "p-5 rounded-2xl border-2 text-left transition-all font-bold text-base",
                                        family.income_source === opt.value
                                            ? "bg-emerald-600 border-emerald-600 text-white shadow-lg"
                                            : "bg-white border-slate-100 text-slate-600 hover:border-emerald-200"
                                    )}
                                >
                                    {opt.label}
                                </button>
                            ))}
                        </div>
                    </div>

                    {/* Amount */}
                    <div className="space-y-3 mt-6">
                        <label className="block text-sm font-black uppercase tracking-widest text-slate-400">
                            Moyenne mensuelle nette (12 derniers mois)
                        </label>
                        <div className="relative">
                            <input
                                type="number"
                                inputMode="numeric"
                                value={userProfile.work.salary_monthly_gross || ''}
                                onChange={(e) => updateWork({ salary_monthly_gross: parseInt(e.target.value) || 0 })}
                                className="w-full p-7 bg-white border-2 border-slate-200 rounded-[2rem] focus:border-emerald-500 focus:ring-4 focus:ring-emerald-50 transition-all outline-none font-black text-4xl text-slate-900 text-center"
                                placeholder="0"
                            />
                            <span className="absolute right-8 top-1/2 -translate-y-1/2 text-3xl font-black text-slate-300">€</span>
                        </div>
                        {(userProfile.work.salary_monthly_gross || 0) > 0 && (
                            <div className={cn(
                                "flex items-center justify-center gap-2 text-sm font-bold mt-3 p-3 rounded-2xl",
                                (userProfile.work.salary_monthly_gross || 0) >= 1398
                                    ? "text-emerald-700 bg-emerald-50"
                                    : "text-amber-700 bg-amber-50"
                            )}>
                                {(userProfile.work.salary_monthly_gross || 0) >= 1398 ? (
                                    <><CheckCircle2 className="w-4 h-4" /> Au-dessus du SMIC net (≥ 1 398€)</>
                                ) : (
                                    <><AlertCircle className="w-4 h-4" /> En dessous du SMIC net (&lt; 1 398€)</>
                                )}
                            </div>
                        )}
                        <p className="text-xs text-slate-400 font-medium text-center mt-2">
                            💡 Consultez vos 12 dernières fiches de paie — « Net à payer avant impôt ».
                        </p>
                    </div>
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: LOGEMENT (unifié)                                   */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'LOGEMENT' && (
                <PageWrapper
                    icon={<Home className="w-10 h-10 text-indigo-600" />}
                    title="Avez-vous un logement pour accueillir votre famille ?"
                    subtitle="L'OFII effectuera une visite pour vérifier les conditions d'accueil."
                >
                    {([
                        { label: "🏠  Oui, j'ai un logement", value: 'OWNED_RENTED' },
                        { label: "🔍  Non, je cherche encore", value: 'SEARCHING' },
                        { label: "❓  Je ne sais pas encore", value: 'UNKNOWN' },
                    ] as const).map((opt) => (
                        <button
                            key={opt.value}
                            onClick={() => handleUpdate({ housing_status: opt.value as any })}
                            className={cn(
                                "w-full p-7 rounded-[2rem] border-2 text-left transition-all font-bold text-xl",
                                "hover:scale-[1.01] active:scale-[0.99]",
                                family.housing_status === opt.value
                                    ? "bg-indigo-600 border-indigo-600 text-white shadow-xl shadow-indigo-100"
                                    : "bg-white border-slate-100 text-slate-600 hover:border-indigo-200 hover:shadow-md"
                            )}
                        >
                            {opt.label}
                        </button>
                    ))}

                    {/* Surface — only if they have a home */}
                    {family.housing_status === 'OWNED_RENTED' && (
                        <div className="mt-8 space-y-4 p-8 bg-slate-50 rounded-[2.5rem] border-2 border-slate-100 rf-zoom-in">
                            <label className="block text-sm font-black uppercase tracking-widest text-slate-400">
                                Surface du logement • Min. {minSurface} m² pour {family.rf_family_members_count || 1} pers.
                            </label>
                            <div className="relative">
                                <input
                                    type="number"
                                    inputMode="numeric"
                                    value={family.rf_housing_surface || ''}
                                    onChange={(e) => handleUpdate({ rf_housing_surface: parseInt(e.target.value) || 0 })}
                                    className="w-full p-7 bg-white border-2 border-slate-200 rounded-[2rem] focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 transition-all outline-none font-black text-4xl text-slate-900 text-center"
                                    placeholder="0"
                                />
                                <span className="absolute right-8 top-1/2 -translate-y-1/2 text-3xl font-black text-slate-300">m²</span>
                            </div>
                            {(family.rf_housing_surface || 0) > 0 && (
                                <div className={cn(
                                    "flex items-center justify-center gap-2 text-sm font-bold p-3 rounded-2xl",
                                    surfaceOk ? "text-emerald-700 bg-emerald-50" : "text-amber-700 bg-amber-50"
                                )}>
                                    {surfaceOk ? (
                                        <><CheckCircle2 className="w-4 h-4" /> Surface conforme ({family.rf_housing_surface} m² ≥ {minSurface} m²)</>
                                    ) : (
                                        <><AlertCircle className="w-4 h-4" /> Surface insuffisante ({family.rf_housing_surface} m² &lt; {minSurface} m²)</>
                                    )}
                                </div>
                            )}
                        </div>
                    )}

                    {/* Warning if no housing */}
                    {(family.housing_status === 'SEARCHING' || family.housing_status === 'UNKNOWN') && (
                        <div className="p-5 bg-amber-50 border-2 border-amber-200 rounded-2xl rf-zoom-in">
                            <p className="text-amber-700 text-sm font-bold">
                                ⚠️ Un logement conforme sera nécessaire avant la délivrance du visa.
                            </p>
                        </div>
                    )}
                </PageWrapper>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* PAGE: RÉCAPITULATIF                                       */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {currentPage === 'RECAP' && (
                <div key="recap" className="max-w-2xl mx-auto px-6 py-12 space-y-8 rf-slide-right">
                    <div className="text-center space-y-4">
                        <div className="inline-flex p-4 bg-violet-50 rounded-3xl mb-2">
                            <ClipboardList className="w-10 h-10 text-violet-600" />
                        </div>
                        <h2 className="text-3xl sm:text-4xl font-black text-slate-900">Récapitulatif</h2>
                        <p className="text-slate-500 text-lg font-medium">Vérifiez vos réponses avant de voir votre résultat.</p>
                    </div>

                    {/* Conditions préalables */}
                    <RecapCard title="Conditions Préalables" icon={<Shield className="w-5 h-5 text-emerald-500" />} onEdit={() => goTo('TITRE_SEJOUR', 'backward')}>
                        <RecapRow label="Titre de séjour" value="✅ Oui" />
                        <RecapRow label="Polygamie" value="✅ Non" />
                        <RecapRow label="Situation conjugale" value={
                            ({ MARRIED: '💍 Marié(e)', SINGLE: 'Célibataire' } as Record<string, string>)[family.rf_marital_status || ''] || '-'
                        } />
                    </RecapCard>

                    {/* Famille */}
                    <RecapCard title="Votre Famille" icon={<Users className="w-5 h-5 text-blue-500" />} onEdit={() => goTo(family.rf_marital_status === 'MARRIED' ? 'QUI_FAIRE_VENIR' : 'NB_PERSONNES', 'backward')}>
                        {family.rf_who_to_bring && (
                            <RecapRow label="Qui faire venir" value={
                                ({ SPOUSE_AND_CHILDREN: 'Conjoint + Enfants', SPOUSE_ONLY: 'Conjoint seul', CHILDREN_ONLY: 'Enfants seuls' } as Record<string, string>)[family.rf_who_to_bring] || '-'
                            } />
                        )}
                        <RecapRow label="Nb de personnes" value={`${family.rf_family_members_count || '-'}${family.rf_family_members_count === 4 ? '+' : ''}`} />
                        <RecapRow label="Nationalité" value={family.sponsor_nationality === 'ALGERIAN' ? '🇩🇿 Algérienne' : '🌍 Autre'} />
                        <RecapRow label="Durée de séjour" value={
                            ({ LESS_12: '< 12 mois', '12_18': '12 à 18 mois', MORE_18: '> 18 mois' } as Record<string, string>)[family.presence_duration || ''] || '-'
                        } />
                    </RecapCard>

                    {/* Ressources */}
                    <RecapCard title="Les Ressources" icon={<Wallet className="w-5 h-5 text-emerald-500" />} onEdit={() => goTo('AAH', 'backward')}>
                        <RecapRow label="AAH / ASI" value={family.has_handicap_allowance ? '✅ Oui' : 'Non'} />
                        {family.has_handicap_allowance === false && (
                            <>
                                <RecapRow label="Source de revenus" value={
                                    ({ SALARY: 'Travail', PENSION: 'Retraite', OTHER: 'Chômage', RSA_ALOWANCE: 'RSA/Aides' } as Record<string, string>)[family.income_source || ''] || '-'
                                } />
                                <RecapRow label="Salaire mensuel" value={`${userProfile.work.salary_monthly_gross || 0} €`}
                                    highlight={(userProfile.work.salary_monthly_gross || 0) >= 1398 ? 'green' : 'amber'} />
                            </>
                        )}
                    </RecapCard>

                    {/* Logement */}
                    <RecapCard title="Le Logement" icon={<Home className="w-5 h-5 text-indigo-500" />} onEdit={() => goTo('LOGEMENT', 'backward')}>
                        <RecapRow label="Logement disponible" value={
                            ({ OWNED_RENTED: '✅ Oui', SEARCHING: '🔍 En recherche', UNKNOWN: '❓ Incertain' } as Record<string, string>)[family.housing_status || ''] || '-'
                        } />
                        {family.housing_status === 'OWNED_RENTED' && (family.rf_housing_surface || 0) > 0 && (
                            <RecapRow label="Surface" value={`${family.rf_housing_surface} m² (min. ${minSurface} m²)`}
                                highlight={surfaceOk ? 'green' : 'amber'} />
                        )}
                    </RecapCard>
                </div>
            )}

            {/* ═══════════════════════════════════════════════════════════ */}
            {/* NAVIGATION FOOTER                                         */}
            {/* ═══════════════════════════════════════════════════════════ */}
            {!isStopPage && (
                <div className="max-w-2xl mx-auto px-6 pb-12 flex flex-col sm:flex-row items-center justify-between gap-4">
                    <button
                        onClick={goPrev}
                        disabled={currentIndex === 0}
                        className={cn(
                            "flex items-center gap-2 px-6 py-4 font-bold rounded-2xl transition-all text-sm",
                            currentIndex === 0
                                ? "opacity-0 pointer-events-none"
                                : "text-slate-400 hover:text-slate-600 hover:bg-slate-50"
                        )}
                    >
                        <ArrowLeft className="w-4 h-4" /> Précédent
                    </button>

                    {/* Show "Continuer" for pages that need manual submit — resources + logement */}
                    {(currentPage === 'RESSOURCES' || currentPage === 'LOGEMENT') && (
                        <button
                            disabled={currentPage === 'RESSOURCES'
                                ? !(family.income_source && (userProfile.work.salary_monthly_gross || 0) > 0)
                                : !family.housing_status || (family.housing_status === 'OWNED_RENTED' && !(family.rf_housing_surface || 0))
                            }
                            onClick={goNext}
                            className={cn(
                                "group w-full sm:w-auto px-10 py-5 rounded-[2rem] font-black text-lg transition-all flex items-center justify-center gap-3 shadow-xl",
                                (currentPage === 'RESSOURCES'
                                    ? !!(family.income_source && (userProfile.work.salary_monthly_gross || 0) > 0)
                                    : !!(family.housing_status && (family.housing_status !== 'OWNED_RENTED' || (family.rf_housing_surface || 0) > 0))
                                )
                                    ? "bg-indigo-600 text-white hover:bg-indigo-700 shadow-indigo-200"
                                    : "bg-slate-200 text-slate-400 cursor-not-allowed shadow-none"
                            )}
                        >
                            Continuer <ArrowRight className="w-5 h-5 group-hover:translate-x-1 transition-transform" />
                        </button>
                    )}

                    {/* Recap: final CTA */}
                    {currentPage === 'RECAP' && (
                        <button
                            onClick={onNext}
                            className="group w-full sm:w-auto px-10 py-6 bg-indigo-600 text-white rounded-[2rem] font-black text-xl transition-all flex items-center justify-center gap-3 shadow-2xl shadow-indigo-200 hover:bg-indigo-700 hover:scale-[1.02] active:scale-[0.98]"
                        >
                            VOIR MON ÉLIGIBILITÉ <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                        </button>
                    )}
                </div>
            )}
        </div>
    );
}

// ─── Recap sub-components ───
function RecapCard({ title, icon, onEdit, children }: {
    title: string; icon: React.ReactNode; onEdit: () => void; children: React.ReactNode;
}) {
    return (
        <div className="p-6 bg-white border-2 border-slate-100 rounded-3xl space-y-3">
            <div className="flex items-center justify-between">
                <h3 className="font-black text-lg text-slate-900 flex items-center gap-2">{icon} {title}</h3>
                <button onClick={onEdit} className="text-indigo-600 text-xs font-bold flex items-center gap-1 hover:underline">
                    <Pencil className="w-3 h-3" /> Modifier
                </button>
            </div>
            <div className="grid grid-cols-2 gap-y-2 gap-x-4 text-sm">{children}</div>
        </div>
    );
}

function RecapRow({ label, value, highlight }: { label: string; value: string; highlight?: 'green' | 'amber' }) {
    return (
        <>
            <div className="text-slate-400 font-bold">{label}</div>
            <div className={cn("font-bold", highlight === 'green' ? 'text-emerald-600' : highlight === 'amber' ? 'text-amber-600' : 'text-slate-800')}>
                {value}
            </div>
        </>
    );
}
