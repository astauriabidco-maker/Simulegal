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

export default function ActivityResourcesStep({ data, update, onNext, onBack, canNext }: StepProps) {
    return (
        <div className="p-8 flex-1 flex flex-col">
            <h2 className="text-2xl font-bold text-slate-900 mb-1">ÉTAPE 3 : ACTIVITÉ & RESSOURCES</h2>
            <p className="text-slate-500 mb-8">Le Projet : Évaluer votre apport économique, intellectuel et votre intégration.</p>

            {data.admin.current_visa_type === 'NONE' ? (
                /* Specialized "Sans Papiers" (Sans Titre) Path */
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
                </div>
            ) : (
                /* Classic Step Path with Situation Selector */
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
                        {/* Condition: Student or Potential Talent Detection (High Salary or coming from Student visa) */}
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

                        {/* Always show resources except for some simple cases */}
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
                    </div>
                </div>
            )}

            {/* Common Fields: French Level is ALWAYS shown */}
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
                    {/* 2026 REFORM: Civic Exam (except for Refugees/Stateless or Age > 65) */}
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

                    {/* Job in Tension question: Unified for Workers or Sans-Papiers with Contract */}
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
