'use client';

import React, { useState, useMemo, useCallback, useEffect, useRef } from 'react';
import { UserProfile } from '@/types';
import {
    ArrowRight, ArrowLeft, Shield, ShieldAlert, Heart,
    Scale, ExternalLink, CheckCircle2, AlertCircle,
    ClipboardList, Pencil, Users, Wallet, Home, XCircle, Users2, Baby
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
    | 'QUI_FAIRE_VENIR'
    | 'SITUATION_CONJUGALE'
    | 'STOP_MARITAL'
    | 'AGE_ENFANTS'
    | 'NB_PERSONNES'
    | 'TITRE_SEJOUR'
    | 'STOP_TITRE'
    | 'POLYGAMIE'
    | 'STOP_POLYGAMIE'
    | 'NATIONALITE'
    | 'DUREE_SEJOUR'
    | 'AAH'
    | 'RESSOURCES'
    | 'LOGEMENT'
    | 'RECAP';

export default function FamilyReunificationStep({ userProfile, updateProfile, onNext }: FamilyReunificationStepProps) {
    const [currentPage, setCurrentPage] = useState<PageId>('QUI_FAIRE_VENIR'); // NOUVEAU START
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
        const pages: PageId[] = ['QUI_FAIRE_VENIR'];

        const who = family.rf_who_to_bring;

        // 1. Branche Conjoint
        if (who === 'SPOUSE_ONLY' || who === 'SPOUSE_AND_CHILDREN') {
            pages.push('SITUATION_CONJUGALE');
            if (family.rf_marital_status === 'CIVIL_PARTNER' || family.rf_marital_status === 'CONCUBIN') {
                return [...pages, 'STOP_MARITAL'];
            }
        }

        // 2. Branche Enfants
        if (who === 'CHILDREN_ONLY' || who === 'SPOUSE_AND_CHILDREN') {
            pages.push('AGE_ENFANTS'); // Nouvelle question pour check majorité
        }

        // 3. Nombre
        pages.push('NB_PERSONNES');

        // 4. Administratif (Bloquants)
        pages.push('TITRE_SEJOUR');
        if (family.rf_has_valid_titre_sejour === false) return [...pages, 'STOP_TITRE'];

        pages.push('POLYGAMIE');
        if (family.is_polygamous === true) return [...pages, 'STOP_POLYGAMIE'];

        // 5. Conditions Matérielles
        pages.push('NATIONALITE', 'DUREE_SEJOUR', 'AAH');
        if (family.has_handicap_allowance === false) {
            pages.push('RESSOURCES');
        }
        pages.push('LOGEMENT', 'RECAP');

        return pages;
    }, [family]);

    // ... (Reste de la logique de navigation identique) ...
    const currentIndex = pageFlow.indexOf(currentPage);
    const totalAnswerablePages = pageFlow.filter(p => !p.startsWith('STOP_') && p !== 'RECAP').length;
    const answeredPages = pageFlow.slice(0, currentIndex).filter(p => !p.startsWith('STOP_') && p !== 'RECAP').length;
    const progressPercent = totalAnswerablePages > 0 ? Math.round((answeredPages / totalAnswerablePages) * 100) : 0;
    const isStopPage = currentPage.startsWith('STOP_');
    const pendingAdvanceRef = useRef(false);
    const [, setTick] = useState(0);

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
    });

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

    const autoAdvance = useCallback((delay = 400) => {
        setTimeout(() => {
            pendingAdvanceRef.current = true;
            setTick(t => t + 1);
        }, delay);
    }, []);

    const minSurface = 16 + Math.max(0, ((family.rf_family_members_count || 1) - 1)) * 9;
    const surfaceOk = (family.rf_housing_surface || 0) >= minSurface;

    useEffect(() => {
        containerRef.current?.scrollTo({ top: 0, behavior: 'smooth' });
    }, [currentPage]);

    // ─── Components ───
    const ChoiceButton = ({ selected, positive, label, onClick, subtitle }: any) => (
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
                <span className={cn("block text-xs font-bold uppercase tracking-widest mt-2", selected ? "opacity-70" : "text-slate-400")}>{subtitle}</span>
            )}
        </button>
    );

    const PageWrapper = ({ children, icon, title, subtitle, article }: any) => (
        <div key={currentPage} className={cn("flex flex-col items-center justify-center min-h-[60vh] max-w-2xl mx-auto px-6 py-12 space-y-10", direction === 'forward' ? 'rf-slide-right' : 'rf-slide-left')}>
            <div className="text-center space-y-4">
                <div className="inline-flex p-4 bg-slate-50 rounded-3xl mb-2">{icon}</div>
                <h2 className="text-3xl sm:text-4xl font-black text-slate-900 tracking-tight leading-tight">{title}</h2>
                {subtitle && <p className="text-slate-500 text-lg font-medium max-w-lg mx-auto">{subtitle}</p>}
                {article && <p className="text-indigo-500 text-xs font-black uppercase tracking-widest">{article}</p>}
            </div>
            <div className="w-full space-y-5">{children}</div>
        </div>
    );

    const StopPage = ({ title, description, article, cta, suggestion, icon: Icon }: any) => (
        <div key={currentPage} className="flex flex-col items-center justify-center min-h-[60vh] max-w-2xl mx-auto px-6 py-12 rf-zoom-in">
            <div className="w-full p-10 bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900 rounded-[2.5rem] shadow-2xl text-white space-y-8">
                <div className="flex items-start gap-5">
                    <div className="p-3 bg-rose-500/20 rounded-2xl shrink-0"><Icon className="w-10 h-10 text-rose-400" /></div>
                    <div className="space-y-4 flex-1">
                        <div className="flex items-center gap-3 flex-wrap">
                            <span className="px-4 py-1.5 bg-rose-500 text-white text-xs font-black rounded-full uppercase tracking-wider">Non éligible</span>
                            <span className="text-slate-400 text-xs font-bold">{article}</span>
                        </div>
                        <h3 className="text-3xl font-black leading-tight">{title}</h3>
                        <p className="text-slate-300 font-medium leading-relaxed text-lg">{description}</p>
                        {suggestion && <p className="text-emerald-400 text-sm font-bold flex items-start gap-2 mt-4"><CheckCircle2 className="w-5 h-5 shrink-0 mt-0.5" />{suggestion}</p>}
                    </div>
                </div>
                <button className="w-full flex items-center justify-center gap-3 px-8 py-6 bg-white/10 hover:bg-white/20 font-black rounded-2xl transition-all text-lg border border-white/10">
                    <Scale className="w-5 h-5" />{cta}<ExternalLink className="w-4 h-4 opacity-50" />
                </button>
            </div>
        </div>
    );

    return (
        <div ref={containerRef} className="relative">
            {!isStopPage && (
                <div className="max-w-sm mx-auto px-6 pt-6 mb-4">
                    <div className="flex items-center justify-between text-xs font-bold text-slate-400 mb-2">
                        <span>Question {answeredPages + 1}/{totalAnswerablePages}</span>
                        <span>{progressPercent}%</span>
                    </div>
                    <div className="h-1.5 bg-slate-100 rounded-full overflow-hidden">
                        <div className="h-full bg-indigo-600 rounded-full transition-all duration-700 ease-out" style={{ width: `${progressPercent}%` }} />
                    </div>
                </div>
            )}

            {/* PAGE: QUI FAIRE VENIR (Nouveau point d'entrée émotionnel) */}
            {currentPage === 'QUI_FAIRE_VENIR' && (
                <PageWrapper icon={<Heart className="w-10 h-10 text-rose-500" />} title="Qui souhaitez-vous faire venir vivre en France ?" subtitle="Nous allons vérifier l'éligibilité de votre projet familial.">
                    <ChoiceButton label="Mon conjoint(e) et nos enfants" value="SPOUSE_AND_CHILDREN" selected={family.rf_who_to_bring === 'SPOUSE_AND_CHILDREN'} positive onClick={() => { handleUpdate({ rf_who_to_bring: 'SPOUSE_AND_CHILDREN' }); autoAdvance(); }} />
                    <ChoiceButton label="Mon conjoint(e) uniquement" value="SPOUSE_ONLY" selected={family.rf_who_to_bring === 'SPOUSE_ONLY'} positive onClick={() => { handleUpdate({ rf_who_to_bring: 'SPOUSE_ONLY' }); autoAdvance(); }} />
                    <ChoiceButton label="Mes enfants uniquement" value="CHILDREN_ONLY" selected={family.rf_who_to_bring === 'CHILDREN_ONLY'} positive onClick={() => { handleUpdate({ rf_who_to_bring: 'CHILDREN_ONLY' }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* PAGE: SITUATION CONJUGALE (Si conjoint impliqué) */}
            {currentPage === 'SITUATION_CONJUGALE' && (
                <PageWrapper icon={<Users2 className="w-10 h-10 text-indigo-500" />} title="Quelle est votre situation matrimoniale ?" subtitle="Le regroupement familial exige le mariage civil." article="CESEDA L434-2">
                    <ChoiceButton label="Marié(e) civilement" subtitle="Ou remariage légal" selected={family.rf_marital_status === 'MARRIED'} positive onClick={() => { handleUpdate({ rf_marital_status: 'MARRIED' }); autoAdvance(); }} />
                    <ChoiceButton label="Pacsé(e) / Partenaire civil" subtitle="Union civile enregistrée" selected={family.rf_marital_status === 'CIVIL_PARTNER'} onClick={() => { handleUpdate({ rf_marital_status: 'CIVIL_PARTNER' }); autoAdvance(); }} />
                    <ChoiceButton label="Concubinage / Union libre" subtitle="Vie commune sans contrat" selected={family.rf_marital_status === 'CONCUBIN'} onClick={() => { handleUpdate({ rf_marital_status: 'CONCUBIN' }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* STOP: Marital */}
            {currentPage === 'STOP_MARITAL' && (
                <StopPage icon={Heart} title={family.rf_marital_status === 'CIVIL_PARTNER' ? "Le PACS n'ouvre pas droit au Regroupement Familial" : "Le concubinage n'est pas reconnu"} description={family.rf_marital_status === 'CIVIL_PARTNER' ? "Le regroupement familial est strictement réservé aux couples mariés. Le PACS ne suffit pas pour cette procédure spécifique." : "L'union libre ne permet pas de bénéficier du regroupement familial, quelle que soit la durée de vie commune."} article="CESEDA L434-2" cta="Voir les alternatives Vie Privée" suggestion="Le mariage civil reste la voie principale pour cette procédure." />
            )}

            {/* PAGE: AGE ENFANTS (Si enfants impliqués) */}
            {currentPage === 'AGE_ENFANTS' && (
                <PageWrapper icon={<Baby className="w-10 h-10 text-blue-400" />} title="Quel âge ont vos enfants ?" subtitle="La procédure concerne les enfants mineurs." article="CESEDA L434-1">
                    <ChoiceButton label="Moins de 18 ans" subtitle="Au jour du dépôt de la demande" selected={true} positive onClick={() => autoAdvance()} />
                    <ChoiceButton label="18 ans ou plus" subtitle="Majeurs" selected={false} onClick={() => alert("Le regroupement familial ne concerne que les enfants mineurs (sauf cas très particuliers de handicap).")} />
                </PageWrapper>
            )}

            {/* PAGE: NOMBRE DE PERSONNES */}
            {currentPage === 'NB_PERSONNES' && (
                <PageWrapper icon={<Users className="w-10 h-10 text-blue-600" />} title="Combien de personnes au total ?" subtitle="Pour calculer la surface de logement nécessaire.">
                    <div className="grid grid-cols-4 gap-4">
                        {[1, 2, 3, 4].map((n) => (
                            <button key={n} onClick={() => { handleUpdate({ rf_family_members_count: n }); autoAdvance(); }} className={cn("p-8 rounded-[2rem] font-black text-3xl transition-all duration-300 border-2", "hover:scale-105 active:scale-95", family.rf_family_members_count === n ? "bg-indigo-600 border-indigo-600 text-white shadow-xl shadow-indigo-100" : "bg-white border-slate-100 text-slate-600 hover:border-indigo-200 hover:shadow-md")}>
                                {n}{n === 4 ? '+' : ''}
                            </button>
                        ))}
                    </div>
                </PageWrapper>
            )}

            {/* PAGE: TITRE DE SEJOUR (Bloquant administratif déplacé après) */}
            {currentPage === 'TITRE_SEJOUR' && (
                <PageWrapper icon={<Shield className="w-10 h-10 text-indigo-600" />} title="Votre titre de séjour actuel" subtitle="Vous devez résider régulièrement en France." article="CESEDA L434-1">
                    <ChoiceButton label="Carte de séjour (≥ 1 an)" subtitle="Ou récépissé de renouvellement" selected={family.rf_has_valid_titre_sejour === true} positive onClick={() => { handleUpdate({ rf_has_valid_titre_sejour: true }); autoAdvance(); }} />
                    <ChoiceButton label="Titre court / Récépissé 1ère demande" subtitle="Ou sans papiers" selected={family.rf_has_valid_titre_sejour === false} onClick={() => { handleUpdate({ rf_has_valid_titre_sejour: false }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* STOP: Titre */}
            {currentPage === 'STOP_TITRE' && (
                <StopPage icon={Shield} title="Titre de séjour valide requis" description="Le demandeur doit être titulaire d'un titre de séjour d'une durée de validité d'au moins un an (ou récépissé de renouvellement)." article="CESEDA L434-1" cta="Régulariser ma situation d'abord" />
            )}

            {/* PAGE: POLYGAMIE */}
            {currentPage === 'POLYGAMIE' && (
                <PageWrapper icon={<ShieldAlert className="w-10 h-10 text-rose-500" />} title="Situation de polygamie en France ?" subtitle="Question légale obligatoire." article="CESEDA L434-6">
                    <ChoiceButton label="Non, je suis en situation monogame" selected={family.is_polygamous === false} positive onClick={() => { handleUpdate({ is_polygamous: false }); autoAdvance(); }} />
                    <ChoiceButton label="Oui, je vis en état de polygamie" selected={family.is_polygamous === true} onClick={() => { handleUpdate({ is_polygamous: true }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {currentPage === 'STOP_POLYGAMIE' && (<StopPage icon={ShieldAlert} title="Exclusion absolue" description="La polygamie est un motif d'exclusion de l'ordre public français empêchant tout regroupement familial." article="CESEDA L434-6" cta="Consulter un avocat" />)}

            {/* PAGE: NATIONALITÉ */}
            {currentPage === 'NATIONALITE' && (
                <PageWrapper icon={<span className="text-4xl">🌍</span>} title="Quelle est votre nationalité ?" subtitle="Les conditions de durée de présence varient.">
                    <ChoiceButton label="🇩🇿  Algérienne" subtitle="Accord franco-algérien" selected={family.sponsor_nationality === 'ALGERIAN'} positive onClick={() => { handleUpdate({ sponsor_nationality: 'ALGERIAN' as any }); autoAdvance(); }} />
                    <ChoiceButton label="🌍  Autre nationalité (Hors UE)" subtitle="Droit commun" selected={family.sponsor_nationality === 'OTHER'} positive onClick={() => { handleUpdate({ sponsor_nationality: 'OTHER' as any }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* PAGE: DURÉE DE SÉJOUR */}
            {currentPage === 'DUREE_SEJOUR' && (
                <PageWrapper icon={<span className="text-4xl">⏳</span>} title="Présence régulière en France" subtitle={family.sponsor_nationality === 'ALGERIAN' ? "Seuil requis : 12 mois." : "Seuil requis : 18 mois."}>
                    <ChoiceButton label="Moins de 12 mois" value="LESS_12" selected={family.presence_duration === 'LESS_12'} onClick={() => { handleUpdate({ presence_duration: 'LESS_12' as any }); autoAdvance(); }} />
                    <ChoiceButton label="Entre 12 et 18 mois" value="12_18" selected={family.presence_duration === '12_18'} positive={family.sponsor_nationality === 'ALGERIAN'} onClick={() => { handleUpdate({ presence_duration: '12_18' as any }); autoAdvance(); }} />
                    <ChoiceButton label="Plus de 18 mois" value="MORE_18" selected={family.presence_duration === 'MORE_18'} positive onClick={() => { handleUpdate({ presence_duration: 'MORE_18' as any }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* PAGE: AAH */}
            {currentPage === 'AAH' && (
                <PageWrapper icon={<Wallet className="w-10 h-10 text-emerald-600" />} title="Percevez-vous l'AAH ou l'ASI ?" subtitle="Dispense de conditions de ressources (Allocation Adulte Handicapé).">
                    <ChoiceButton label="Oui, je perçois ces allocations" selected={family.has_handicap_allowance === true} positive onClick={() => { handleUpdate({ has_handicap_allowance: true }); autoAdvance(); }} />
                    <ChoiceButton label="Non, je ne les perçois pas" selected={family.has_handicap_allowance === false} positive onClick={() => { handleUpdate({ has_handicap_allowance: false }); autoAdvance(); }} />
                </PageWrapper>
            )}

            {/* PAGE: RESSOURCES */}
            {currentPage === 'RESSOURCES' && (
                <PageWrapper icon={<Wallet className="w-10 h-10 text-emerald-600" />} title="Vos ressources stables" subtitle="Moyenne sur les 12 derniers mois.">
                    <div className="space-y-3">
                        <label className="block text-sm font-black uppercase tracking-widest text-slate-400">Source principale</label>
                        <div className="grid grid-cols-2 gap-3">
                            {([{ label: "Travail (CDI/CDD)", value: "SALARY" }, { label: "Retraite", value: "PENSION" }, { label: "Chômage", value: "OTHER" }, { label: "RSA / Aides", value: "RSA_ALOWANCE" }] as const).map((opt) => (
                                <button key={opt.value} onClick={() => handleUpdate({ income_source: opt.value as any })} className={cn("p-5 rounded-2xl border-2 text-left transition-all font-bold text-base", family.income_source === opt.value ? "bg-emerald-600 border-emerald-600 text-white shadow-lg" : "bg-white border-slate-100 text-slate-600 hover:border-emerald-200")}>
                                    {opt.label}
                                </button>
                            ))}
                        </div>
                    </div>
                    <div className="space-y-3 mt-6">
                        <label className="block text-sm font-black uppercase tracking-widest text-slate-400">Montant mensuel NET</label>
                        <div className="relative">
                            <input type="number" inputMode="numeric" value={userProfile.work.salary_monthly_gross || ''} onChange={(e) => updateWork({ salary_monthly_gross: parseInt(e.target.value) || 0 })} className="w-full p-7 bg-white border-2 border-slate-200 rounded-[2rem] focus:border-emerald-500 focus:ring-4 focus:ring-emerald-50 transition-all outline-none font-black text-4xl text-slate-900 text-center" placeholder="0" />
                            <span className="absolute right-8 top-1/2 -translate-y-1/2 text-3xl font-black text-slate-300">€</span>
                        </div>
                        {(userProfile.work.salary_monthly_gross || 0) > 0 && (
                            <div className={cn("flex items-center justify-center gap-2 text-sm font-bold mt-3 p-3 rounded-2xl", (userProfile.work.salary_monthly_gross || 0) >= 1398 ? "text-emerald-700 bg-emerald-50" : "text-amber-700 bg-amber-50")}>
                                {(userProfile.work.salary_monthly_gross || 0) >= 1398 ? <><CheckCircle2 className="w-4 h-4" /> Suffisant (≥ SMIC)</> : <><AlertCircle className="w-4 h-4" /> Attention, faible</>}
                            </div>
                        )}
                    </div>
                </PageWrapper>
            )}

            {/* PAGE: LOGEMENT */}
            {currentPage === 'LOGEMENT' && (
                <PageWrapper icon={<Home className="w-10 h-10 text-indigo-600" />} title="Disponibilité d'un logement" subtitle="Surface minimale requise selon la zone et la famille.">
                    {([{ label: "✅ J'ai un logement (Locataire/Proprio)", value: 'OWNED_RENTED' }, { label: "🔍 Je cherche un logement", value: 'SEARCHING' }, { label: "⏳ Hébergé / Autre", value: 'UNKNOWN' }] as const).map((opt) => (
                        <button key={opt.value} onClick={() => handleUpdate({ housing_status: opt.value as any })} className={cn("w-full p-7 rounded-[2rem] border-2 text-left transition-all font-bold text-xl", "hover:scale-[1.01] active:scale-[0.99]", family.housing_status === opt.value ? "bg-indigo-600 border-indigo-600 text-white shadow-xl shadow-indigo-100" : "bg-white border-slate-100 text-slate-600 hover:border-indigo-200 hover:shadow-md")}>
                            {opt.label}
                        </button>
                    ))}
                    {family.housing_status === 'OWNED_RENTED' && (
                        <div className="mt-8 space-y-4 p-8 bg-slate-50 rounded-[2.5rem] border-2 border-slate-100 rf-zoom-in">
                            <label className="block text-sm font-black uppercase tracking-widest text-slate-400">Surface habitable (m²)</label>
                            <div className="relative">
                                <input type="number" inputMode="numeric" value={family.rf_housing_surface || ''} onChange={(e) => handleUpdate({ rf_housing_surface: parseInt(e.target.value) || 0 })} className="w-full p-7 bg-white border-2 border-slate-200 rounded-[2rem] focus:border-indigo-500 focus:ring-4 focus:ring-indigo-50 transition-all outline-none font-black text-4xl text-slate-900 text-center" placeholder="0" />
                                <span className="absolute right-8 top-1/2 -translate-y-1/2 text-3xl font-black text-slate-300">m²</span>
                            </div>
                            {(family.rf_housing_surface || 0) > 0 && <div className={cn("flex items-center justify-center gap-2 text-sm font-bold p-3 rounded-2xl", surfaceOk ? "text-emerald-700 bg-emerald-50" : "text-amber-700 bg-amber-50")}>{surfaceOk ? <><CheckCircle2 className="w-4 h-4" /> Surface OK</> : <><AlertCircle className="w-4 h-4" /> Trop petit ({minSurface} m² requis)</>}</div>}
                        </div>
                    )}
                </PageWrapper>
            )}

            {/* PAGE: RECAP */}
            {currentPage === 'RECAP' && (
                <div key="recap" className="max-w-2xl mx-auto px-6 py-12 space-y-8 rf-slide-right">
                    <div className="text-center space-y-4">
                        <div className="inline-flex p-4 bg-violet-50 rounded-3xl mb-2"><ClipboardList className="w-10 h-10 text-violet-600" /></div>
                        <h2 className="text-3xl sm:text-4xl font-black text-slate-900">Tout est noté !</h2>
                        <p className="text-slate-500 text-lg font-medium">Voici le résumé de votre déclaration.</p>
                    </div>
                    <RecapCard title="Projet Familial" icon={<Heart className="w-5 h-5 text-rose-500" />} onEdit={() => goTo('QUI_FAIRE_VENIR', 'backward')}>
                        <RecapRow label="Bénéficiaires" value={({ SPOUSE_AND_CHILDREN: 'Conjoint + Enfants', SPOUSE_ONLY: 'Conjoint seul', CHILDREN_ONLY: 'Enfants seuls' } as Record<string, string>)[family.rf_who_to_bring] || '-'} />
                        <RecapRow label="Statut" value={({ MARRIED: 'Marié(e)', SINGLE: 'Célibataire' } as Record<string, string>)[family.rf_marital_status || ''] || '-'} />
                    </RecapCard>
                    <RecapCard title="Situation Admin." icon={<Shield className="w-5 h-5 text-indigo-500" />} onEdit={() => goTo('TITRE_SEJOUR', 'backward')}>
                        <RecapRow label="Titre de séjour" value="✅ Valide" />
                        <RecapRow label="Nationalité" value={family.sponsor_nationality === 'ALGERIAN' ? 'Algérienne' : 'Autre (Hors UE)'} />
                        <RecapRow label="Durée séjour" value={({ LESS_12: '< 12 mois', '12_18': '12-18 mois', MORE_18: '> 18 mois' } as Record<string, string>)[family.presence_duration || ''] || '-'} />
                    </RecapCard>
                    <RecapCard title="Moyens" icon={<Wallet className="w-5 h-5 text-emerald-500" />} onEdit={() => goTo('RESSOURCES', 'backward')}>
                        <RecapRow label="Ressources" value={family.has_handicap_allowance ? 'AAH (Dispensé)' : `${userProfile.work.salary_monthly_gross} €/mois`} highlight={(userProfile.work.salary_monthly_gross || 0) >= 1398 || family.has_handicap_allowance ? 'green' : 'amber'} />
                        <RecapRow label="Logement" value={`${family.rf_housing_surface} m²`} highlight={surfaceOk ? 'green' : 'amber'} />
                    </RecapCard>
                </div>
            )}

            {/* NAVIGATION FOOTER */}
            {!isStopPage && (
                <div className="max-w-2xl mx-auto px-6 pb-12 flex flex-col sm:flex-row items-center justify-between gap-4">
                    <button onClick={goPrev} disabled={currentIndex === 0} className={cn("flex items-center gap-2 px-6 py-4 font-bold rounded-2xl transition-all text-sm", currentIndex === 0 ? "opacity-0 pointer-events-none" : "text-slate-400 hover:text-slate-600 hover:bg-slate-50")}>
                        <ArrowLeft className="w-4 h-4" /> Précédent
                    </button>
                    {(currentPage === 'RESSOURCES' || currentPage === 'LOGEMENT') && (
                        <button disabled={currentPage === 'RESSOURCES' ? !(family.income_source && (userProfile.work.salary_monthly_gross || 0) > 0) : !family.housing_status || (family.housing_status === 'OWNED_RENTED' && !(family.rf_housing_surface || 0))} onClick={goNext} className={cn("group w-full sm:w-auto px-10 py-5 rounded-[2rem] font-black text-lg transition-all flex items-center justify-center gap-3 shadow-xl", (currentPage === 'RESSOURCES' ? !!(family.income_source && (userProfile.work.salary_monthly_gross || 0) > 0) : !!(family.housing_status && (family.housing_status !== 'OWNED_RENTED' || (family.rf_housing_surface || 0) > 0))) ? "bg-indigo-600 text-white hover:bg-indigo-700 shadow-indigo-200" : "bg-slate-200 text-slate-400 cursor-not-allowed shadow-none")}>
                            Continuer <ArrowRight className="w-5 h-5 group-hover:translate-x-1 transition-transform" />
                        </button>
                    )}
                    {currentPage === 'RECAP' && (
                        <button onClick={onNext} className="group w-full sm:w-auto px-10 py-6 bg-indigo-600 text-white rounded-[2rem] font-black text-xl transition-all flex items-center justify-center gap-3 shadow-2xl shadow-indigo-200 hover:bg-indigo-700 hover:scale-[1.02] active:scale-[0.98]">
                            LANCER L'ANALYSE <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                        </button>
                    )}
                </div>
            )}
        </div>
    );
}

// ─── Recap sub-components ───
function RecapCard({ title, icon, onEdit, children }: any) {
    return (
        <div className="p-6 bg-white border-2 border-slate-100 rounded-3xl space-y-3">
            <div className="flex items-center justify-between">
                <h3 className="font-black text-lg text-slate-900 flex items-center gap-2">{icon} {title}</h3>
                <button onClick={onEdit} className="text-indigo-600 text-xs font-bold flex items-center gap-1 hover:underline"><Pencil className="w-3 h-3" /> Modifier</button>
            </div>
            <div className="grid grid-cols-2 gap-y-2 gap-x-4 text-sm">{children}</div>
        </div>
    );
}

function RecapRow({ label, value, highlight }: { label: string; value: string; highlight?: 'green' | 'amber' }) {
    return (
        <>
            <div className="text-slate-400 font-bold">{label}</div>
            <div className={cn("font-bold", highlight === 'green' ? 'text-emerald-600' : highlight === 'amber' ? 'text-amber-600' : 'text-slate-800')}>{value}</div>
        </>
    );
}
