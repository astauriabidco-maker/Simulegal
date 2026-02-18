'use client';

import React, { useState, useMemo, useCallback, useEffect, useRef } from 'react';
import { UserProfile } from '@/types';
import { WIZARD_QUESTIONS, WizardQuestion, QuestionField, QuestionOption } from './questions';
import ResultsView from '../ResultsView';
import { computeDerivedFields } from '@/lib/computeDerivedFields';
import { WizardAnalytics } from '@/services/WizardAnalytics';

interface WizardFlowProps {
    userProfile: UserProfile;
    updateProfile: (section: keyof UserProfile, data: any) => void;
    serviceId: string;
    forceAgencyId?: string;
}

/* ═══════════════════════════════════════════════════════════
   Map Q1 specific situation selection to work booleans
   ═══════════════════════════════════════════════════════════ */
function mapSpecificSituation(situationId: string) {
    return {
        is_researcher: situationId === 'RESEARCHER',
        has_hosting_agreement: false,
        is_artist: situationId === 'ARTIST',
        is_sportif_haut_niveau: situationId === 'SPORTIF',
        is_intern: situationId === 'INTERN',
        is_au_pair: situationId === 'AU_PAIR',
        is_volunteer: situationId === 'VOLUNTEER',
        is_salarie_mission: situationId === 'MISSION',
        is_ict_transfer: situationId === 'ICT',
        is_manager_or_expert: situationId === 'MANAGER',
    };
}

function getCurrentSpecificSituation(work: UserProfile['work']): string {
    if (work.is_researcher) return 'RESEARCHER';
    if (work.is_artist) return 'ARTIST';
    if (work.is_sportif_haut_niveau) return 'SPORTIF';
    if (work.is_intern) return 'INTERN';
    if (work.is_au_pair) return 'AU_PAIR';
    if (work.is_volunteer) return 'VOLUNTEER';
    if (work.is_salarie_mission) return 'MISSION';
    if (work.is_ict_transfer) return 'ICT';
    if (work.is_manager_or_expert) return 'MANAGER';
    return 'CLASSIC';
}

export default function WizardFlow({ userProfile, updateProfile, serviceId, forceAgencyId }: WizardFlowProps) {
    const [currentIndex, setCurrentIndex] = useState(0);
    const [showResults, setShowResults] = useState(false);
    const [showResumeBanner, setShowResumeBanner] = useState(false);
    const [resumeIndex, setResumeIndex] = useState(0);
    const [animDir, setAnimDir] = useState<'forward' | 'back'>('forward');
    const analyticsSessionRef = useRef<string | null>(null);

    /* ─── Active questions (filtered by condition) ─── */
    const activeQuestions = useMemo(() =>
        WIZARD_QUESTIONS.filter(q => q.condition(userProfile)),
        [userProfile]
    );

    const totalQuestions = activeQuestions.length;
    const safeIndex = Math.min(currentIndex, totalQuestions - 1);
    const currentQuestion = activeQuestions[safeIndex];
    const progress = totalQuestions > 0 ? ((safeIndex + 1) / totalQuestions) * 100 : 0;

    /* ─── localStorage draft persistence ─── */
    const DRAFT_KEY = `simulegal_wizard_draft_${serviceId || 'default'}`;

    // Restore draft on mount — show resume banner if saved progress exists
    useEffect(() => {
        try {
            const saved = localStorage.getItem(DRAFT_KEY);
            if (saved) {
                const draft = JSON.parse(saved);
                // Only restore if less than 24h old
                if (draft.ts && Date.now() - draft.ts < 24 * 60 * 60 * 1000 && draft.index > 0) {
                    setResumeIndex(draft.index);
                    setShowResumeBanner(true);
                } else {
                    localStorage.removeItem(DRAFT_KEY);
                }
            }
        } catch { /* ignore corrupt localStorage */ }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []);

    // Save draft on every navigation
    const saveDraft = useCallback((index: number) => {
        try {
            localStorage.setItem(DRAFT_KEY, JSON.stringify({ index, ts: Date.now() }));
        } catch { /* quota exceeded — ignore */ }
    }, [DRAFT_KEY]);

    // Analytics: start session on mount
    useEffect(() => {
        analyticsSessionRef.current = WizardAnalytics.startSession(serviceId, totalQuestions, showResumeBanner);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []);

    // Analytics: track question entry on navigation
    useEffect(() => {
        if (analyticsSessionRef.current && currentQuestion && !showResults) {
            WizardAnalytics.enterQuestion(analyticsSessionRef.current, currentQuestion.id, safeIndex);
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [safeIndex, showResults]);

    /* ─── Navigation ─── */
    const goNext = useCallback(() => {
        setAnimDir('forward');
        if (safeIndex >= totalQuestions - 1) {
            // Apply all derived fields via pure function
            const derived = computeDerivedFields(userProfile);
            for (const [section, data] of Object.entries(derived)) {
                if (Object.keys(data).length > 0) {
                    updateProfile(section as keyof UserProfile, data);
                }
            }
            // Clear draft on completion
            try { localStorage.removeItem(DRAFT_KEY); } catch { }
            // Analytics: mark session complete
            if (analyticsSessionRef.current) {
                WizardAnalytics.completeSession(analyticsSessionRef.current);
            }
            setShowResults(true);
        } else {
            const nextIdx = safeIndex + 1;
            setCurrentIndex(nextIdx);
            saveDraft(nextIdx);
        }
    }, [safeIndex, totalQuestions, userProfile, updateProfile, DRAFT_KEY, saveDraft]);

    const goBack = useCallback(() => {
        setAnimDir('back');
        if (showResults) {
            setShowResults(false);
        } else if (safeIndex > 0) {
            const prevIdx = safeIndex - 1;
            setCurrentIndex(prevIdx);
            saveDraft(prevIdx);
        }
    }, [safeIndex, showResults, saveDraft]);

    // Resume or restart helpers
    const handleResume = useCallback(() => {
        setCurrentIndex(resumeIndex);
        setShowResumeBanner(false);
    }, [resumeIndex]);

    const handleRestart = useCallback(() => {
        setCurrentIndex(0);
        setShowResumeBanner(false);
        try { localStorage.removeItem(DRAFT_KEY); } catch { }
    }, [DRAFT_KEY]);

    /* ─── Generic field value getter ─── */
    const getFieldValue = (field: QuestionField): any => {
        const section = userProfile[field.section] as any;
        return section?.[field.key];
    };

    /* ─── Generic field update ─── */
    const setFieldValue = (field: QuestionField, value: any) => {
        // Analytics: record interaction
        if (analyticsSessionRef.current) {
            WizardAnalytics.recordInteraction(analyticsSessionRef.current);
        }
        updateProfile(field.section, { [field.key]: value });
    };

    /* ─── Intelligent Visa → Profile Inferences ─── */
    const applyVisaInferences = useCallback((visaType: string) => {
        // Auto-populate main_situation from visa type
        const situationMap: Record<string, string> = {
            'STUDENT': 'STUDENT',
            'WORKER': 'WORKER',
            'PASSEPORT_TALENT': 'WORKER',
        };
        if (situationMap[visaType]) {
            updateProfile('work', { main_situation: situationMap[visaType] });
        }

        // VPF → pre-fill family situation (conjoint français is the most common VPF)
        if (visaType === 'VPF') {
            updateProfile('family', { spouse_nationality: 'FRENCH' });
        }

        // Sans-papiers → force titre de séjour only
        if (visaType === 'NONE') {
            updateProfile('project', { target_goal: 'RESIDENCE_PERMIT' });
        }
    }, [updateProfile]);

    /* ─── Filter visa options by nationality group ─── */
    const getFilteredOptions = useCallback((q: WizardQuestion): QuestionOption[] => {
        if (!q.options) return [];
        // Filter unrealistic visa types for refugees/stateless
        if (q.id === 'visa') {
            const group = userProfile.identity.nationality_group;
            if (group === 'REFUGEE' || group === 'STATELESS') {
                const excludeForRefugee = ['STUDENT', 'PASSEPORT_TALENT', 'VISITOR'];
                return q.options.filter(o => !excludeForRefugee.includes(o.id));
            }
        }
        return q.options;
    }, [userProfile.identity.nationality_group]);

    /* ─── Handle special fields ─── */
    const handleGridSelect = (question: WizardQuestion, optionId: string) => {
        const field = question.fields[0];

        // Special: marital status → sets spouse_nationality + PACS flag
        if (question.id === 'marital') {
            if (optionId === 'MARRIED') {
                updateProfile('family', {
                    spouse_nationality: 'NON_EU',
                    is_pacsed_with_french: false,
                });
            } else if (optionId === 'PACS') {
                updateProfile('family', {
                    spouse_nationality: 'NON_EU',
                });
            } else {
                updateProfile('family', {
                    spouse_nationality: 'NONE',
                    marriage_duration_years: 0,
                    community_of_life: false,
                    is_pacsed_with_french: false,
                });
            }
            if (question.autoAdvance) setTimeout(goNext, 300);
            return;
        }

        // Special: boolean grids (born_in_france, has_french_child, job_tension, civic_exam)
        if (field.type === 'checkbox') {
            const boolVal = optionId === 'true';
            setFieldValue(field, boolVal);
            // Also set vulnerability show flag for related fields
            if (question.id === 'french_child' && !boolVal) {
                updateProfile('family', {
                    has_french_child: false,
                    contributes_to_education: true,
                    child_residence_france: true,
                });
            }
            if (question.autoAdvance) setTimeout(goNext, 300);
            return;
        }

        // Special: specific situation Q1 → maps to booleans
        if (question.id === 'specific_situation') {
            updateProfile('work', mapSpecificSituation(optionId));
            if (question.autoAdvance) setTimeout(goNext, 300);
            return;
        }

        // Default: set field value directly
        setFieldValue(field, optionId);
        if (question.autoAdvance) setTimeout(goNext, 300);
    };

    /* ─── Get current grid value ─── */
    const getGridValue = (question: WizardQuestion): string => {
        if (question.id === 'marital') {
            if (userProfile.family.spouse_nationality === 'NONE') return 'SINGLE';
            if (userProfile.family.is_pacsed_with_french || false) return 'PACS';
            return 'MARRIED';
        }
        if (question.id === 'specific_situation') {
            return getCurrentSpecificSituation(userProfile.work);
        }
        const field = question.fields[0];
        if (field.type === 'checkbox') {
            const val = getFieldValue(field);
            return val ? 'true' : 'false';
        }
        return String(getFieldValue(field) ?? '');
    };

    /* ─── Results view ─── */
    if (showResults) {
        return (
            <div>
                <button
                    onClick={goBack}
                    className="mb-4 px-4 py-2 text-sm font-semibold text-slate-500 hover:text-slate-700 hover:bg-slate-50 rounded-lg transition-all"
                >
                    ← Modifier mes réponses
                </button>
                <ResultsView
                    userProfile={userProfile}
                    onReset={() => { setCurrentIndex(0); setShowResults(false); try { localStorage.removeItem(DRAFT_KEY); } catch { } }}
                    serviceId={serviceId}
                    forceAgencyId={forceAgencyId}
                />
            </div>
        );
    }

    if (!currentQuestion) return null;

    /* ═══════════════════════════════════════════════════════════
       RENDER
       ═══════════════════════════════════════════════════════════ */
    return (
        <div className="w-full">
            {/* ─── Resume Banner ─── */}
            {showResumeBanner && (
                <div className="mb-6 p-4 bg-gradient-to-r from-indigo-50 to-blue-50 border border-indigo-200 rounded-2xl animate-in fade-in slide-in-from-top-4 duration-500">
                    <div className="flex items-center gap-3">
                        <span className="text-2xl">💾</span>
                        <div className="flex-1">
                            <p className="text-sm font-bold text-indigo-900">Session précédente détectée</p>
                            <p className="text-xs text-indigo-600">Vous étiez à la question {resumeIndex + 1} / {totalQuestions}</p>
                        </div>
                        <button
                            onClick={handleResume}
                            className="px-4 py-2 bg-indigo-600 text-white text-xs font-bold rounded-xl hover:bg-indigo-700 transition-colors shadow-sm"
                        >
                            Reprendre →
                        </button>
                        <button
                            onClick={handleRestart}
                            className="px-4 py-2 bg-white text-slate-500 text-xs font-bold rounded-xl hover:bg-slate-50 border border-slate-200 transition-colors"
                        >
                            Recommencer
                        </button>
                    </div>
                </div>
            )}
            {/* ─── Progress Bar ─── */}
            <div className="mb-8">
                <div className="flex justify-between items-center mb-3">
                    <div className="flex items-center gap-3">
                        <span className="text-xs font-bold text-slate-400 uppercase tracking-widest">
                            Étape {safeIndex + 1}
                        </span>
                        {currentQuestion && (() => {
                            const sectionMap: Record<string, string> = {
                                goal: '🎯 Objectif', nationality: '🌍 Identité', visa: '📋 Administratif', age: '👤 Identité', born_in_france: '👤 Identité',
                                entry_date: '📅 Parcours', residence: '📅 Parcours', entered_legally: '📋 Administratif',
                                situation: '💼 Travail', specific_situation: '💼 Travail', contract: '💼 Travail', salary: '💼 Travail', work_auth: '💼 Travail', metiers_tension: '💼 Travail',
                                french: '🗣️ Intégration', civic_exam: '⚖️ Civique', republican_values: '🗣️ Intégration',
                                marital: '💍 Famille', children: '💍 Famille', family_details: '💍 Famille',
                                casier: '⚖️ Civique', eloignement: '⚖️ Civique',
                                diploma: '🎓 Études', education: '🎓 Études',
                                vulnerability: '🩺 Vulnérabilité', health: '🩺 Santé',
                                regularisation: '📋 Régularisation',
                            };
                            const label = sectionMap[currentQuestion.id] || '';
                            return label ? (
                                <span className="text-[10px] font-semibold text-indigo-400 bg-indigo-50/60 px-2 py-0.5 rounded-md">
                                    {label}
                                </span>
                            ) : null;
                        })()}
                    </div>
                    <div className="flex items-center gap-3">
                        {totalQuestions - safeIndex > 2 && (
                            <span className="text-[10px] font-semibold text-slate-300">
                                ⏱ ~{Math.max(1, Math.ceil((totalQuestions - safeIndex) * 0.2))} min
                            </span>
                        )}
                        <span className="text-xs font-bold text-indigo-600 bg-indigo-50 px-3 py-1 rounded-full">
                            {Math.round(progress)}%
                        </span>
                    </div>
                </div>
                {/* Smooth progress bar */}
                <div className="w-full bg-slate-100 rounded-full h-2.5 overflow-hidden shadow-inner">
                    <div
                        className="bg-gradient-to-r from-blue-500 via-indigo-500 to-indigo-600 h-2.5 rounded-full transition-all duration-700 ease-out shadow-sm"
                        style={{ width: `${progress}%` }}
                    />
                </div>
                {/* Step dots */}
                <div className="flex items-center gap-1 mt-2.5 justify-center">
                    {activeQuestions.map((_, i) => (
                        <div
                            key={i}
                            className={`rounded-full transition-all duration-500 ${i < safeIndex
                                ? 'w-1.5 h-1.5 bg-indigo-500'
                                : i === safeIndex
                                    ? 'w-2.5 h-2.5 bg-indigo-600 ring-2 ring-indigo-200 animate-pulse'
                                    : 'w-1.5 h-1.5 bg-slate-200'
                                }`}
                        />
                    ))}
                </div>
            </div>

            {/* ─── Question Card ─── */}
            <div
                key={currentQuestion.id}
                className={`bg-white rounded-3xl shadow-xl shadow-slate-200/60 border border-slate-100 p-8 md:p-12 min-h-[400px] flex flex-col animate-in fade-in ${animDir === 'forward' ? 'slide-in-from-right-4' : 'slide-in-from-left-4'} zoom-in-[0.98] duration-500 ease-out`}
            >
                {/* Header */}
                <div className="text-center mb-8">
                    <span className="text-5xl mb-4 block">{currentQuestion.icon}</span>
                    <h2 className="text-2xl font-black text-slate-900 mb-2">{currentQuestion.title}</h2>
                    {currentQuestion.subtitle && (
                        <p className="text-sm text-slate-400 max-w-md mx-auto">{currentQuestion.subtitle}</p>
                    )}
                </div>

                {/* ─── Field Rendering ─── */}
                <div className="flex-1 flex flex-col items-center justify-center max-w-xl mx-auto w-full">
                    {renderQuestionBody(currentQuestion)}
                </div>

                {/* ─── Navigation ─── */}
                <div className="flex justify-between items-center mt-8 pt-6 border-t border-slate-100">
                    <button
                        onClick={goBack}
                        disabled={safeIndex === 0}
                        className={`px-6 py-3 font-semibold rounded-xl transition-all ${safeIndex === 0
                            ? 'text-slate-300 cursor-not-allowed'
                            : 'text-slate-500 hover:text-slate-700 hover:bg-slate-50'
                            }`}
                    >
                        ← Retour
                    </button>

                    <button
                        onClick={goNext}
                        className="px-8 py-3 bg-indigo-600 text-white font-bold rounded-xl hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-200 hover:shadow-xl active:scale-95"
                    >
                        {safeIndex >= totalQuestions - 1 ? 'Voir les Résultats →' : 'Suivant →'}
                    </button>
                </div>
            </div>
        </div>
    );

    /* ═══════════════════════════════════════════════════════════
       QUESTION BODY RENDERERS
       ═══════════════════════════════════════════════════════════ */
    function renderQuestionBody(q: WizardQuestion) {
        switch (q.type) {
            case 'GRID':
                return renderGrid(q);
            case 'SELECT':
                return renderSelect(q);
            case 'NUMBER':
                return renderNumber(q);
            case 'DATE':
                return renderDate(q);
            case 'TOGGLE':
                return renderToggle(q);
            case 'COMPOSITE':
                return renderComposite(q);
            case 'MULTI_CHECK':
                return renderMultiCheck(q);
            default:
                return null;
        }
    }

    /* ─── GRID: Cards with icons ─── */
    function renderGrid(q: WizardQuestion) {
        const currentVal = getGridValue(q);
        const cols = q.gridCols || 3;

        return (
            <div className="w-full">
                <div className={`grid gap-3`} style={{ gridTemplateColumns: `repeat(${Math.min(cols, 5)}, minmax(0, 1fr))` }}>
                    {q.options?.map(opt => (
                        <button
                            key={opt.id}
                            onClick={() => handleGridSelect(q, opt.id)}
                            className={`flex flex-col items-center justify-center p-4 rounded-2xl border-2 transition-all duration-200 text-center min-h-[90px] ${currentVal === opt.id
                                ? 'bg-indigo-50 border-indigo-500 shadow-md scale-[1.03] ring-2 ring-indigo-200'
                                : 'bg-white border-slate-100 hover:border-indigo-200 hover:shadow-sm text-slate-500'
                                }`}
                        >
                            {opt.icon && <span className="text-2xl mb-1">{opt.icon}</span>}
                            <span className="text-xs font-bold leading-tight">{opt.label}</span>
                            {opt.description && <span className="text-[10px] text-slate-400 mt-1">{opt.description}</span>}
                        </button>
                    ))}
                </div>

                {/* Sub-questions (e.g. hosting agreement for researcher) */}
                {q.subQuestions?.map((sq, i) =>
                    sq.condition(userProfile) ? (
                        <div key={i} className="mt-4 space-y-3 animate-in fade-in slide-in-from-top-2 duration-300">
                            {sq.fields.map(f => renderCheckField(f))}
                        </div>
                    ) : null
                )}
            </div>
        );
    }

    /* ─── SELECT: Clickable list ─── */
    function renderSelect(q: WizardQuestion) {
        const field = q.fields[0];
        const currentVal = String(getFieldValue(field) ?? '');

        return (
            <div className="w-full space-y-2">
                {getFilteredOptions(q).map(opt => (
                    <button
                        key={opt.id}
                        onClick={() => {
                            setFieldValue(field, opt.id);
                            // Intelligent visa → profile inferences
                            if (q.id === 'visa') {
                                applyVisaInferences(opt.id);
                            }
                            // Sync is_pacsed_with_french when spouse nationality changes
                            if (field.key === 'spouse_nationality') {
                                const isPacs = getGridValue({ id: 'marital' } as WizardQuestion) === 'PACS';
                                updateProfile('family', {
                                    is_pacsed_with_french: isPacs && opt.id === 'FRENCH',
                                });
                            }
                            if (q.autoAdvance) setTimeout(goNext, 300);
                        }}
                        className={`w-full text-left p-4 rounded-xl border-2 transition-all duration-200 flex items-center gap-3 ${currentVal === opt.id
                            ? 'bg-indigo-50 border-indigo-500 shadow-sm ring-1 ring-indigo-200'
                            : 'bg-white border-slate-100 hover:border-indigo-200 hover:bg-slate-50'
                            }`}
                    >
                        <span className="text-lg">{opt.icon || '○'}</span>
                        <span className={`font-semibold text-sm ${currentVal === opt.id ? 'text-indigo-700' : 'text-slate-600'}`}>
                            {opt.label}
                        </span>
                        {currentVal === opt.id && (
                            <span className="ml-auto text-indigo-500">✓</span>
                        )}
                    </button>
                ))}
            </div>
        );
    }

    /* ─── NUMBER: Big centered input ─── */
    function renderNumber(q: WizardQuestion) {
        const field = q.fields[0];
        const val = getFieldValue(field) ?? 0;

        return (
            <div className="w-full max-w-xs mx-auto">
                <div className="relative">
                    <input
                        type="number"
                        value={val}
                        onFocus={(e) => e.target.select()}
                        onChange={(e) => setFieldValue(field, Number(e.target.value) || 0)}
                        placeholder={field.placeholder}
                        className="w-full px-6 py-5 text-center text-3xl font-black rounded-2xl border-2 border-slate-200 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-100 outline-none transition-all"
                        autoFocus
                    />
                    {field.suffix && (
                        <span className="absolute right-4 top-1/2 -translate-y-1/2 text-sm font-bold text-slate-400">
                            {field.suffix}
                        </span>
                    )}
                </div>
            </div>
        );
    }

    /* ─── DATE: Single date picker ─── */
    function renderDate(q: WizardQuestion) {
        const field = q.fields[0];
        const val = getFieldValue(field) ?? '';

        return (
            <div className="w-full max-w-xs mx-auto">
                <input
                    type="date"
                    value={val}
                    onChange={(e) => setFieldValue(field, e.target.value)}
                    className="w-full px-6 py-5 text-center text-xl font-bold rounded-2xl border-2 border-slate-200 focus:border-indigo-500 focus:ring-4 focus:ring-indigo-100 outline-none transition-all"
                    autoFocus
                />
            </div>
        );
    }

    /* ─── TOGGLE: Yes/No ─── */
    function renderToggle(q: WizardQuestion) {
        const field = q.fields[0];
        const val = getFieldValue(field) ?? false;

        return (
            <div className="flex gap-4 w-full max-w-sm mx-auto">
                <button
                    onClick={() => { setFieldValue(field, true); if (q.autoAdvance) setTimeout(goNext, 300); }}
                    className={`flex-1 py-5 rounded-2xl border-2 font-bold text-lg transition-all ${val ? 'bg-emerald-50 border-emerald-500 text-emerald-700 shadow-md' : 'bg-white border-slate-100 text-slate-400 hover:border-emerald-200'}`}
                >
                    ✅ Oui
                </button>
                <button
                    onClick={() => { setFieldValue(field, false); if (q.autoAdvance) setTimeout(goNext, 300); }}
                    className={`flex-1 py-5 rounded-2xl border-2 font-bold text-lg transition-all ${!val ? 'bg-rose-50 border-rose-400 text-rose-600 shadow-md' : 'bg-white border-slate-100 text-slate-400 hover:border-rose-200'}`}
                >
                    ❌ Non
                </button>
            </div>
        );
    }

    /* ─── COMPOSITE: Mixed fields ─── */
    function renderComposite(q: WizardQuestion) {
        return (
            <div className="w-full space-y-5">
                {q.fields.map(f => {
                    if (f.type === 'select' && f.options) return renderSelectField(f);
                    if (f.type === 'number') return renderNumberField(f);
                    if (f.type === 'checkbox') return renderCheckField(f);
                    return null;
                })}

                {/* Sub-questions */}
                {q.subQuestions?.map((sq, i) =>
                    sq.condition(userProfile) ? (
                        <div key={i} className="mt-2 space-y-3 pl-4 border-l-2 border-indigo-200 animate-in fade-in slide-in-from-top-2 duration-300">
                            {sq.fields.map(f => renderCheckField(f))}
                        </div>
                    ) : null
                )}
            </div>
        );
    }

    /* ─── MULTI_CHECK: Checkbox list ─── */
    function renderMultiCheck(q: WizardQuestion) {
        const anyChecked = q.fields.some(f => getFieldValue(f));

        return (
            <div className="w-full space-y-3">
                {!anyChecked && (
                    <p className="text-center text-xs text-slate-400 mb-2">
                        Aucun élément coché = non concerné. Passez à la suite si rien ne s'applique.
                    </p>
                )}
                {q.fields.map(f => renderCheckField(f))}
            </div>
        );
    }

    /* ─── Shared: individual select field ─── */
    function renderSelectField(f: QuestionField) {
        const val = getFieldValue(f) ?? '';
        return (
            <div key={`${f.section}-${f.key}`}>
                {f.label && <label className="block text-sm font-semibold text-slate-600 mb-2">{f.label}</label>}
                <select
                    value={val}
                    onChange={(e) => setFieldValue(f, e.target.value)}
                    className="w-full px-4 py-3 rounded-xl border-2 border-slate-200 focus:border-indigo-500 focus:ring-2 focus:ring-indigo-100 outline-none transition-all bg-white font-medium"
                >
                    {f.options?.map(opt => (
                        <option key={opt.id} value={opt.id}>{opt.label}</option>
                    ))}
                </select>
            </div>
        );
    }

    /* ─── Shared: individual number field ─── */
    function renderNumberField(f: QuestionField) {
        const val = getFieldValue(f) ?? 0;
        return (
            <div key={`${f.section}-${f.key}`}>
                {f.label && <label className="block text-sm font-semibold text-slate-600 mb-2">{f.label}</label>}
                <div className="relative">
                    <input
                        type="number"
                        value={val}
                        onFocus={(e) => e.target.select()}
                        onChange={(e) => {
                            const v = Number(e.target.value) || 0;
                            setFieldValue(f, v);
                            // Sync salary → financial
                            if (f.key === 'salary_monthly_gross') {
                                updateProfile('financial', { resources_monthly_average: v });
                            }
                        }}
                        placeholder={f.placeholder}
                        className="w-full px-4 py-3 rounded-xl border-2 border-slate-200 focus:border-indigo-500 focus:ring-2 focus:ring-indigo-100 outline-none transition-all font-medium"
                    />
                    {f.suffix && (
                        <span className="absolute right-4 top-1/2 -translate-y-1/2 text-xs font-bold text-slate-400">
                            {f.suffix}
                        </span>
                    )}
                </div>
            </div>
        );
    }

    /* ─── Shared: individual checkbox field ─── */
    function renderCheckField(f: QuestionField) {
        const val = getFieldValue(f) ?? false;
        return (
            <div
                key={`${f.section}-${f.key}`}
                onClick={() => {
                    setFieldValue(f, !val);
                    // Sync vulnerability fields
                    if (f.key === 'is_victim_domestic_violence') {
                        updateProfile('vulnerability', {
                            is_victim_domestic_violence: !val,
                            has_protection_order_violence: !val,
                            show_vulnerability: true,
                        });
                    }
                    if (f.key === 'personal_needs_treatment') {
                        updateProfile('health', {
                            personal_needs_treatment: !val,
                            treatment_unavailable_in_origin: !val,
                            treatment_available_origin: val,
                        });
                    }
                }}
                className={`flex items-center gap-4 p-4 rounded-xl border-2 cursor-pointer transition-all select-none ${val
                    ? 'bg-indigo-50 border-indigo-300 shadow-sm'
                    : 'bg-white border-slate-100 hover:border-indigo-200 hover:bg-slate-50'
                    }`}
            >
                <div className={`w-6 h-6 rounded-lg border-2 flex items-center justify-center flex-shrink-0 transition-all ${val ? 'bg-indigo-600 border-indigo-600' : 'border-slate-300'}`}>
                    {val && <span className="text-white text-xs font-bold">✓</span>}
                </div>
                <span className={`text-sm font-semibold ${val ? 'text-indigo-800' : 'text-slate-600'}`}>
                    {f.label}
                </span>
            </div>
        );
    }
}
