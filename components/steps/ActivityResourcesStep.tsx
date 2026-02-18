'use client';

import React from 'react';
import { UserProfile } from '@/types';

interface StepProps {
    data: UserProfile;
    update: (section: keyof UserProfile, data: any) => void;
    onNext: () => void;
    onBack: () => void;
    canNext: boolean;
}

/* ────────────────────────────────────────────
   Q1 — Situation professionnelle spécifique
   Un seul select qui remplace ~15 booleans
   mutuellement exclusifs (is_researcher,
   is_artist, is_intern, etc.)
   ──────────────────────────────────────────── */
const SPECIFIC_SITUATIONS = [
    { id: 'CLASSIC', label: 'Salarié classique', icon: '💼' },
    { id: 'RESEARCHER', label: 'Chercheur / Scientifique', icon: '🔬' },
    { id: 'ARTIST', label: 'Artiste / Interprète', icon: '🎨' },
    { id: 'SPORTIF', label: 'Sportif de haut niveau', icon: '🏅' },
    { id: 'INTERN', label: 'Stagiaire', icon: '📋' },
    { id: 'AU_PAIR', label: 'Jeune au pair', icon: '🏠' },
    { id: 'VOLUNTEER', label: 'Volontaire / Service civique', icon: '🤝' },
    { id: 'MISSION', label: 'Salarié en mission (détaché)', icon: '🌍' },
    { id: 'ICT', label: 'Transfert intra-groupe (ICT)', icon: '🏢' },
    { id: 'MANAGER', label: 'Cadre dirigeant / Manager', icon: '👔' },
] as const;

/** Map the single Q1 selection to the individual boolean fields */
function mapSpecificSituation(situationId: string) {
    return {
        is_researcher: situationId === 'RESEARCHER',
        has_hosting_agreement: false, // will be set via sub-question
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

/** Derive the current Q1 selection from state booleans */
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

export default function ActivityResourcesStep({ data, update, onNext, onBack, canNext }: StepProps) {
    const specificSit = getCurrentSpecificSituation(data.work);
    const isNaturalization = data.project.target_goal === 'NATURALIZATION' || data.project.target_goal === 'BOTH';

    return (
        <div className="p-8 flex-1 flex flex-col">
            <h2 className="text-2xl font-bold text-slate-900 mb-1">ÉTAPE 3 : ACTIVITÉ & RESSOURCES</h2>
            <p className="text-slate-500 mb-8">Le Projet : Évaluer votre apport économique, intellectuel et votre intégration.</p>

            {data.admin.current_visa_type === 'NONE' ? (
                /* ═══════════════════════════════════════════
                   Parcours Sans Papiers + Q6 Régularisation
                   ═══════════════════════════════════════════ */
                <div className="space-y-6 animate-in fade-in slide-in-from-top-4 duration-500">
                    <div className="p-6 bg-amber-50 rounded-2xl border border-amber-100 mb-4">
                        <p className="text-sm text-amber-900 leading-relaxed font-medium">
                            <span className="text-xl mr-2">ℹ️</span>
                            Parcours Régularisation : Pour les personnes sans titre de séjour, nous analysons vos chances d'obtenir une Admission Exceptionnelle au Séjour (AES).
                        </p>
                    </div>

                    <div className="flex flex-col gap-4">
                        <div className="animate-in fade-in zoom-in-95 duration-300">
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                                Type de contrat ou promesse <span className="text-red-500">*</span>
                            </label>
                            <select
                                value={data.work.contract_type}
                                onChange={(e) => update('work', { contract_type: e.target.value })}
                                className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                            >
                                <option value="NONE">Aucun contrat</option>
                                <option value="CDI">CDI / Promesse de CDI</option>
                                <option value="CDD">CDD / Promesse de CDD</option>
                            </select>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-slate-50 rounded-xl border border-slate-100 transition-all hover:bg-white hover:shadow-sm">
                            <input
                                type="checkbox"
                                id="has_payslips"
                                checked={data.work.has_payslips || false}
                                onChange={(e) => update('work', { has_payslips: e.target.checked })}
                                className="w-5 h-5 text-indigo-600 rounded focus:ring-indigo-500"
                            />
                            <label htmlFor="has_payslips" className="text-sm font-semibold text-slate-700 cursor-pointer select-none">
                                Disposez-vous de fiches de paie (Preuve d'ancienneté au travail) ?
                            </label>
                        </div>
                    </div>

                    <div className="animate-in fade-in zoom-in-95 duration-500">
                        <label className="block text-sm font-bold text-slate-700 mb-2">
                            Niveau de ressources mensuelles (€) <span className="text-red-500">*</span>
                        </label>
                        <input
                            type="number"
                            value={data.work.salary_monthly_gross}
                            onFocus={(e) => e.target.select()}
                            onChange={(e) => {
                                const val = Number(e.target.value) || 0;
                                update('work', { salary_monthly_gross: val });
                                update('financial', { resources_monthly_average: val });
                            }}
                            placeholder="Ex: 1500"
                            className="w-full px-4 py-4 rounded-xl border-2 border-slate-200 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all text-lg font-medium"
                        />
                    </div>

                    {/* ── Q6 — Régularisation (affiché si sans titre) ── */}
                    <div className="p-6 bg-orange-50 rounded-2xl border border-orange-100 animate-in fade-in slide-in-from-top-4 duration-300">
                        <h3 className="text-sm font-bold text-orange-800 uppercase tracking-widest mb-4">📋 Situation de régularisation</h3>
                        <div className="space-y-4">
                            <div>
                                <label className="block text-sm font-medium text-gray-700 mb-1">
                                    Années de présence en France <span className="text-red-500">*</span>
                                </label>
                                <input
                                    type="number"
                                    value={data.regularisation?.years_presence_france || 0}
                                    onFocus={(e) => e.target.select()}
                                    onChange={(e) => update('regularisation', { years_presence_france: Number(e.target.value) || 0 })}
                                    placeholder="Ex: 10"
                                    className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                />
                            </div>

                            <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-orange-200">
                                <input
                                    type="checkbox"
                                    id="children_schooled"
                                    checked={data.regularisation?.has_children_schooled_3y || false}
                                    onChange={(e) => update('regularisation', { has_children_schooled_3y: e.target.checked })}
                                    className="w-5 h-5 text-orange-600 rounded focus:ring-orange-500"
                                />
                                <label htmlFor="children_schooled" className="text-sm font-semibold text-orange-900 cursor-pointer select-none">
                                    J'ai des enfants scolarisés en France depuis au moins 3 ans
                                </label>
                            </div>

                            <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-orange-200">
                                <input
                                    type="checkbox"
                                    id="exceptional_talent"
                                    checked={data.regularisation?.has_exceptional_talent || false}
                                    onChange={(e) => update('regularisation', { has_exceptional_talent: e.target.checked })}
                                    className="w-5 h-5 text-orange-600 rounded focus:ring-orange-500"
                                />
                                <label htmlFor="exceptional_talent" className="text-sm font-semibold text-orange-900 cursor-pointer select-none">
                                    J'ai un talent exceptionnel ou une renommée nationale/internationale
                                </label>
                            </div>
                        </div>
                    </div>
                </div>
            ) : (
                /* ═══════════════════════════════════════════
                   Parcours classique (avec titre)
                   ═══════════════════════════════════════════ */
                <div className="contents">
                    <div className="mb-8 p-6 bg-indigo-50 rounded-2xl border border-indigo-100">
                        <label className="block text-sm font-bold text-indigo-900 mb-4 uppercase tracking-wider">
                            Quelle est votre situation principale ? <span className="text-red-500">*</span>
                        </label>
                        <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
                            {[
                                { id: 'STUDENT', label: 'Étudiant', icon: '🎓' },
                                { id: 'WORKER', label: 'Salarié', icon: '💼' },
                                { id: 'ENTREPRENEUR', label: 'Indépendant', icon: '🚀' },
                                { id: 'OTHER', label: 'Autre', icon: '✨' }
                            ].map((sit) => (
                                <button
                                    key={sit.id}
                                    onClick={() => update('work', { main_situation: sit.id })}
                                    className={`flex flex-col items-center justify-center p-4 rounded-xl border-2 transition-all ${data.work.main_situation === sit.id ? 'bg-white border-indigo-600 shadow-md scale-105' : 'bg-white/50 border-transparent hover:border-slate-200 text-slate-500'}`}
                                >
                                    <span className="text-2xl mb-2">{sit.icon}</span>
                                    <span className="text-xs font-bold">{sit.label}</span>
                                </button>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
                        {/* Condition: Student or Potential Talent Detection */}
                        {(data.work.main_situation === 'STUDENT' ||
                            data.admin.current_visa_type === 'STUDENT' ||
                            (data.work.main_situation === 'WORKER' && data.work.salary_monthly_gross > 2500)) && (
                                <div className="animate-in fade-in zoom-in-95 duration-300">
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        Niveau de diplôme <span className="text-red-500">*</span>
                                    </label>
                                    <select
                                        value={data.education.diploma_level}
                                        onChange={(e) => update('education', { diploma_level: e.target.value })}
                                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                                    >
                                        <option value="NONE">Aucun / Baccalauréat</option>
                                        <option value="LICENCE">Licence / Bachelor (Bac+3)</option>
                                        <option value="LICENCE_PRO">Licence Pro</option>
                                        <option value="MASTER">Master / Ingénieur (Bac+5)</option>
                                        <option value="SPECIALIZED_MASTER">Mastère Spécialisé / MSc (CGE)</option>
                                        <option value="PHD">Doctorat (PhD)</option>
                                    </select>

                                    {data.education.diploma_level !== 'NONE' && (
                                        <div className="flex items-center gap-3 p-4 bg-blue-50 rounded-xl border border-blue-100 mt-4 animate-in fade-in slide-in-from-top-2 duration-300">
                                            <input
                                                type="checkbox"
                                                id="has_french_higher_education_diploma"
                                                checked={data.education.has_french_higher_education_diploma || false}
                                                onChange={(e) => update('education', { has_french_higher_education_diploma: e.target.checked })}
                                                className="w-5 h-5 text-blue-600 rounded focus:ring-blue-500"
                                            />
                                            <label htmlFor="has_french_higher_education_diploma" className="text-sm font-semibold text-blue-900 cursor-pointer select-none">
                                                Ce diplôme a été obtenu en France (Réduction de stage à 2 ans)
                                            </label>
                                        </div>
                                    )}

                                    {data.work.main_situation === 'STUDENT' && (
                                        <div className="flex items-center gap-3 p-4 bg-emerald-50 rounded-xl border border-emerald-100 mt-4 animate-in fade-in slide-in-from-top-2 duration-300">
                                            <input
                                                type="checkbox"
                                                id="is_enrolled_higher_ed"
                                                checked={data.education.is_enrolled_higher_ed ?? true}
                                                onChange={(e) => update('education', { is_enrolled_higher_ed: e.target.checked })}
                                                className="w-5 h-5 text-emerald-600 rounded focus:ring-emerald-500"
                                            />
                                            <label htmlFor="is_enrolled_higher_ed" className="text-sm font-semibold text-emerald-900 cursor-pointer select-none">
                                                Actuellement inscrit dans l'enseignement supérieur
                                            </label>
                                        </div>
                                    )}
                                </div>
                            )}

                        {/* Condition: Worker */}
                        {data.work.main_situation === 'WORKER' && (
                            <div className="contents">
                                <div className="animate-in fade-in zoom-in-95 duration-300">
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        Type de contrat <span className="text-red-500">*</span>
                                    </label>
                                    <select
                                        value={data.work.contract_type}
                                        onChange={(e) => update('work', { contract_type: e.target.value })}
                                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                                    >
                                        <option value="NONE">Pas de contrat / Sans emploi</option>
                                        <option value="CDI">CDI</option>
                                        <option value="CDD">CDD</option>
                                        <option value="SEASONAL">Saisonnier</option>
                                    </select>
                                </div>

                                <div className="animate-in fade-in zoom-in-95 duration-300">
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        Salaire mensuel brut (€) <span className="text-red-500">*</span>
                                    </label>
                                    <input
                                        type="number"
                                        value={data.work.salary_monthly_gross}
                                        onFocus={(e) => e.target.select()}
                                        onChange={(e) => update('work', { salary_monthly_gross: Number(e.target.value) || 0 })}
                                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                    />
                                </div>

                            </div>
                        )}

                        {/* Condition: Entrepreneur */}
                        {data.work.main_situation === 'ENTREPRENEUR' && (
                            <div className="contents">
                                <div className="animate-in fade-in zoom-in-95 duration-300">
                                    <label className="block text-sm font-medium text-gray-700 mb-1">Investissement (€)</label>
                                    <input
                                        type="number"
                                        value={data.investment.amount}
                                        onFocus={(e) => e.target.select()}
                                        onChange={(e) => update('investment', { amount: Number(e.target.value) || 0 })}
                                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                    />
                                </div>
                                <div className="animate-in fade-in zoom-in-95 duration-300">
                                    <div className="flex items-center gap-3 p-4 bg-slate-50 rounded-xl border border-slate-100 mt-7">
                                        <input
                                            type="checkbox"
                                            id="project_viable"
                                            checked={data.work.business_project_viable || false}
                                            onChange={(e) => update('work', { business_project_viable: e.target.checked })}
                                            className="w-5 h-5 text-indigo-600 rounded"
                                        />
                                        <label htmlFor="project_viable" className="text-sm font-medium text-slate-700">
                                            Mon projet est jugé réel et sérieux / viable
                                        </label>
                                    </div>
                                </div>
                            </div>
                        )}

                        {/* Resources for Student/Entrepreneur/Other */}
                        {(data.work.main_situation === 'STUDENT' || data.work.main_situation === 'ENTREPRENEUR' || data.work.main_situation === 'OTHER') && (
                            <div className="animate-in fade-in zoom-in-95 duration-300">
                                <label className="block text-sm font-medium text-gray-700 mb-1">Revenus mensuels totaux (€)</label>
                                <input
                                    type="number"
                                    value={data.financial.resources_monthly_average}
                                    onFocus={(e) => e.target.select()}
                                    onChange={(e) => update('financial', { resources_monthly_average: Number(e.target.value) || 0 })}
                                    className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all"
                                />
                            </div>
                        )}

                        {/* Visiteur detection */}
                        {data.work.main_situation === 'OTHER' && (
                            <div className="flex items-center gap-3 p-4 bg-indigo-50 rounded-xl border border-indigo-100 animate-in fade-in slide-in-from-top-2 duration-300">
                                <input
                                    type="checkbox"
                                    id="wants_to_work"
                                    checked={data.work.wants_to_work ?? false}
                                    onChange={(e) => update('work', { wants_to_work: e.target.checked })}
                                    className="w-5 h-5 text-indigo-600 rounded focus:ring-indigo-500"
                                />
                                <label htmlFor="wants_to_work" className="text-sm font-semibold text-indigo-900 cursor-pointer select-none">
                                    Je souhaite exercer une activité professionnelle en France
                                </label>
                            </div>
                        )}
                    </div>

                    {/* ═══════════════════════════════════════════
                       Q1 — Situation professionnelle spécifique
                       (conditionnel : si Salarié ou Autre)
                       ═══════════════════════════════════════════ */}
                    {(data.work.main_situation === 'WORKER' || data.work.main_situation === 'OTHER') && (
                        <div className="mb-8 p-6 bg-violet-50 rounded-2xl border border-violet-100 animate-in fade-in slide-in-from-top-4 duration-300">
                            <h3 className="text-sm font-bold text-violet-800 uppercase tracking-widest mb-4">🎯 Situation professionnelle spécifique</h3>
                            <div className="grid grid-cols-2 md:grid-cols-5 gap-2">
                                {SPECIFIC_SITUATIONS.map((sit) => (
                                    <button
                                        key={sit.id}
                                        onClick={() => update('work', mapSpecificSituation(sit.id))}
                                        className={`flex flex-col items-center justify-center p-3 rounded-xl border-2 transition-all text-center ${specificSit === sit.id
                                            ? 'bg-white border-violet-600 shadow-md scale-[1.03]'
                                            : 'bg-white/50 border-transparent hover:border-violet-200 text-slate-500'
                                            }`}
                                    >
                                        <span className="text-xl mb-1">{sit.icon}</span>
                                        <span className="text-[10px] font-bold leading-tight">{sit.label}</span>
                                    </button>
                                ))}
                            </div>

                            {/* Sub-questions conditionnelles pour Q1 */}
                            {specificSit === 'RESEARCHER' && (
                                <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-violet-200 mt-4 animate-in fade-in slide-in-from-top-2 duration-300">
                                    <input
                                        type="checkbox"
                                        id="hosting_agreement"
                                        checked={data.work.has_hosting_agreement || false}
                                        onChange={(e) => update('work', { has_hosting_agreement: e.target.checked })}
                                        className="w-5 h-5 text-violet-600 rounded focus:ring-violet-500"
                                    />
                                    <label htmlFor="hosting_agreement" className="text-sm font-semibold text-violet-900 cursor-pointer select-none">
                                        J'ai une convention d'accueil d'un organisme de recherche
                                    </label>
                                </div>
                            )}
                        </div>
                    )}
                </div>
            )}

            {/* ═══════════════════════════════════════════
               Common Fields: French Level + Civic Exam
               ═══════════════════════════════════════════ */}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8 mt-6 pt-6 border-t border-slate-100">
                <div className="animate-in fade-in duration-500">
                    <label className="block text-sm font-medium text-gray-700 mb-1">Niveau de français</label>
                    <select
                        value={data.integration.french_level}
                        onChange={(e) => update('integration', { french_level: e.target.value })}
                        className="w-full px-4 py-3 rounded-lg border border-gray-300 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 outline-none transition-all bg-white"
                    >
                        <option value="A1">A1 (Débutant)</option>
                        <option value="A2">A2 (Élémentaire - Requis pour Titre Pluriannuel)</option>
                        <option value="B1">B1 (Intermédiaire - Requis pour Carte Résident)</option>
                        <option value="B2">B2 (Avancé - Requis pour Naturalisation)</option>
                        <option value="C1">C1/C2 (Expert)</option>
                    </select>
                </div>

                <div className="flex flex-col gap-4">
                    {/* 2026 REFORM: Civic Exam */}
                    {!(data.identity.nationality_group === 'REFUGEE' || data.identity.nationality_group === 'STATELESS') && data.identity.age <= 65 && (
                        <div className="flex items-center gap-3 p-4 bg-emerald-50 rounded-xl border border-emerald-100 animate-in fade-in duration-300">
                            <input
                                type="checkbox"
                                id="civic_exam"
                                checked={data.integration.civic_exam_passed}
                                onChange={(e) => update('integration', { civic_exam_passed: e.target.checked })}
                                className="w-5 h-5 text-emerald-600 rounded focus:ring-emerald-500"
                            />
                            <label htmlFor="civic_exam" className="text-sm font-semibold text-emerald-900 cursor-pointer select-none">
                                J'ai réussi l'examen civique (Réforme 2026)
                            </label>
                        </div>
                    )}

                    {/* Job in Tension */}
                    {((data.work.main_situation === 'WORKER' && data.admin.current_visa_type !== 'NONE') ||
                        (data.admin.current_visa_type === 'NONE' && (data.work.contract_type === 'CDI' || data.work.contract_type === 'CDD'))) && (
                            <div className="flex items-center gap-3 p-4 bg-indigo-50 rounded-xl border border-indigo-100 animate-in fade-in duration-300">
                                <input
                                    type="checkbox"
                                    id="job_tension"
                                    checked={data.work.job_in_tension_list}
                                    onChange={(e) => update('work', { job_in_tension_list: e.target.checked })}
                                    className="w-5 h-5 text-indigo-600 rounded"
                                />
                                <label htmlFor="job_tension" className="text-sm font-medium text-slate-700">
                                    Mon métier est dans la liste des métiers en tension
                                </label>
                            </div>
                        )}
                </div>
            </div>

            {/* ═══════════════════════════════════════════
               Q5 — Distinctions / Services (si naturalisation)
               ═══════════════════════════════════════════ */}
            {isNaturalization && (
                <div className="mb-8 p-6 bg-amber-50 rounded-2xl border border-amber-100 animate-in fade-in slide-in-from-top-4 duration-300">
                    <h3 className="text-sm font-bold text-amber-800 uppercase tracking-widest mb-4">🏅 Distinctions & Services à la France</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-amber-200">
                            <input
                                type="checkbox"
                                id="served_military"
                                checked={data.work.served_french_military || false}
                                onChange={(e) => update('work', { served_french_military: e.target.checked })}
                                className="w-5 h-5 text-amber-600 rounded focus:ring-amber-500"
                            />
                            <label htmlFor="served_military" className="text-sm font-semibold text-amber-900 cursor-pointer select-none">
                                J'ai servi dans l'armée française
                            </label>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-amber-200">
                            <input
                                type="checkbox"
                                id="legion_honneur"
                                checked={data.work.has_legion_honneur || false}
                                onChange={(e) => update('work', { has_legion_honneur: e.target.checked })}
                                className="w-5 h-5 text-amber-600 rounded focus:ring-amber-500"
                            />
                            <label htmlFor="legion_honneur" className="text-sm font-semibold text-amber-900 cursor-pointer select-none">
                                Décoré de la Légion d'honneur
                            </label>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-amber-200">
                            <input
                                type="checkbox"
                                id="possession_etat"
                                checked={data.nationality_extra?.possession_etat_francais || false}
                                onChange={(e) => update('nationality_extra', { possession_etat_francais: e.target.checked })}
                                className="w-5 h-5 text-amber-600 rounded focus:ring-amber-500"
                            />
                            <label htmlFor="possession_etat" className="text-sm font-semibold text-amber-900 cursor-pointer select-none">
                                Possession d'état de Français (traité comme Français depuis 10+ ans)
                            </label>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-amber-200">
                            <input
                                type="checkbox"
                                id="lost_nationality"
                                checked={data.identity.lost_french_nationality || false}
                                onChange={(e) => update('identity', { lost_french_nationality: e.target.checked })}
                                className="w-5 h-5 text-amber-600 rounded focus:ring-amber-500"
                            />
                            <label htmlFor="lost_nationality" className="text-sm font-semibold text-amber-900 cursor-pointer select-none">
                                J'ai perdu la nationalité française (réintégration)
                            </label>
                        </div>
                    </div>
                </div>
            )}

            <div className="mt-auto flex justify-between">
                <button
                    onClick={onBack}
                    className="px-6 py-3 text-gray-500 hover:text-gray-700 hover:bg-gray-100 font-semibold rounded-lg transition-all duration-200"
                >
                    Précédent
                </button>
                <button
                    onClick={onNext}
                    disabled={!canNext}
                    className={`w-full md:w-auto px-8 py-3 font-semibold rounded-lg transition-all duration-200 ${canNext
                        ? 'bg-indigo-600 text-white hover:bg-indigo-700 shadow-lg shadow-indigo-200'
                        : 'bg-slate-200 text-slate-400 cursor-not-allowed'
                        }`}
                >
                    Suivant
                </button>
            </div>
        </div>
    );
}
