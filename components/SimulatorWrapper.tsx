'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import IdentityTimelineStep from '@/components/steps/IdentityTimelineStep';
import MigratoryHistoryStep from '@/components/steps/MigratoryHistoryStep';
import ActivityResourcesStep from '@/components/steps/ActivityResourcesStep';
import FamilyVulnerabilityStep from '@/components/steps/FamilyVulnerabilityStep';
import ResultsView from '@/components/ResultsView';

const INITIAL_STATE: UserProfile = {
    identity: {
        age: 25,
        nationality_group: 'NON_EU',
        born_in_france: false,
    },
    timeline: {
        entry_date: new Date().toISOString().split('T')[0],
        years_continuous_residence: 2,
    },
    admin: {
        has_valid_visa_or_permit: false,
        health_insurance: true,
        current_visa_type: 'NONE',
        entry_mode: 'STANDARD',
    },
    family: {
        spouse_nationality: 'NONE',
        marriage_duration_years: 0,
        community_of_life: false,
        is_polygamous: false,
        has_french_child: false,
    },
    work: {
        contract_type: 'NONE',
        annual_gross_salary: 0,
        salary_monthly_gross: 0,
        has_work_authorization: false,
        job_in_tension_list: false,
        company_role: 'EMPLOYEE',
        main_situation: 'WORKER',
        has_payslips: false,
        business_project_viable: false,
    },
    education: {
        diploma_level: 'NONE',
        has_french_higher_education_diploma: false,
    },
    financial: {
        resources_stable_sufficient: false,
        resources_monthly_average: 0,
        resources_annual_total: 0,
    },
    investment: {
        amount: 0,
    },
    integration: {
        french_level: 'A1',
        adheres_to_republican_values: true,
        civic_exam_passed: false,
    },
    civic: {
        clean_criminal_record: true,
        no_expulsion_order: true,
    },
    vulnerability: {
        show_vulnerability: false,
    },
    health: {},
    residence: {},
    project: {
        target_goal: 'BOTH',
    },
};

export default function SimulatorWrapper() {
    const [step, setStep] = useState(1);
    const [userProfile, setUserProfile] = useState<UserProfile>(INITIAL_STATE);

    const updateProfile = (section: keyof UserProfile, data: any) => {
        setUserProfile((prev) => {
            const updatedSection = { ...prev[section], ...data };

            // Salary & Duration Synchronization
            if (section === 'work') {
                if ('salary_monthly_gross' in data) {
                    updatedSection.annual_gross_salary = (data.salary_monthly_gross || 0) * 12;
                } else if ('annual_gross_salary' in data) {
                    updatedSection.salary_monthly_gross = Math.round((data.annual_gross_salary || 0) / 12);
                }

                if (data.contract_type === 'CDI') {
                    updatedSection.contract_duration_months = 99; // Represents indeterminate/long duration
                }
            }

            // Timeline: Auto-calculate years_continuous_residence if entry_date changes
            if (section === 'timeline' && 'entry_date' in data) {
                const entryDate = new Date(data.entry_date);
                const today = new Date();
                let diffYears = today.getFullYear() - entryDate.getFullYear();
                const m = today.getMonth() - entryDate.getMonth();
                if (m < 0 || (m === 0 && today.getDate() < entryDate.getDate())) {
                    diffYears--;
                }
                updatedSection.years_continuous_residence = Math.max(0, diffYears);
            }

            let nextProfile = { ...prev, [section]: updatedSection };

            if (nextProfile.admin.current_visa_type === 'NONE') {
                nextProfile.admin = {
                    ...nextProfile.admin,
                    has_valid_visa_or_permit: false,
                    entry_mode: 'STANDARD'
                };

                if (nextProfile.identity.nationality_group === 'EU') {
                    nextProfile.identity = { ...nextProfile.identity, nationality_group: 'NON_EU' };
                }
            } else {
                // Automation: If a visa type is selected, assume it's valid by default for the simulation
                // unless it's a specific "EXPIRED" stat (though not yet in enum)
                nextProfile.admin.has_valid_visa_or_permit = true;
            }

            if (section === 'work' && 'main_situation' in data) {
                updatedSection.is_entrepreneur = data.main_situation === 'ENTREPRENEUR';
            }

            if (section === 'work' || section === 'financial') {
                const salary = nextProfile.work.salary_monthly_gross || 0;
                const otherResources = nextProfile.financial.resources_monthly_average || 0;
                const totalMonthly = salary + otherResources;
                const SMIC = 1766.92;
                nextProfile.financial = {
                    ...nextProfile.financial,
                    resources_stable_sufficient: totalMonthly >= SMIC,
                    resources_annual_total: totalMonthly * 12
                };
            }

            return nextProfile;
        });
    };

    const nextStep = () => setStep((s) => s + 1);
    const prevStep = () => setStep((s) => s - 1);

    const isStepValid = (currentStep: number) => {
        const data = userProfile;
        switch (currentStep) {
            case 1:
                return (
                    !!data.identity.nationality_group &&
                    data.identity.age > 0 &&
                    !!data.project.target_goal
                );
            case 2:
                return (
                    !!data.timeline.entry_date &&
                    !!data.admin.current_visa_type &&
                    data.civic.clean_criminal_record !== undefined &&
                    data.civic.no_expulsion_order !== undefined
                );
            case 3:
                const { main_situation, contract_type, salary_monthly_gross } = data.work;
                if (!main_situation) return false;
                if (main_situation === 'WORKER') {
                    return !!contract_type && contract_type !== 'NONE' && (salary_monthly_gross || 0) > 0;
                }
                if (main_situation === 'STUDENT') {
                    return !!data.education.diploma_level && data.education.diploma_level !== 'NONE';
                }
                return true;
            case 4:
                return !!data.family.spouse_nationality; // Always 'NONE' or 'FRENCH' etc.
            default:
                return true;
        }
    };

    const renderStep = () => {
        const canNext = isStepValid(step);
        switch (step) {
            case 1:
                return <IdentityTimelineStep data={userProfile} update={updateProfile} onNext={nextStep} canNext={canNext} />;
            case 2:
                return <MigratoryHistoryStep data={userProfile} update={updateProfile} onNext={nextStep} onBack={prevStep} canNext={canNext} />;
            case 3:
                return <ActivityResourcesStep data={userProfile} update={updateProfile} onNext={nextStep} onBack={prevStep} canNext={canNext} />;
            case 4:
                return <FamilyVulnerabilityStep data={userProfile} update={updateProfile} onNext={nextStep} onBack={prevStep} canNext={canNext} />;
            case 5:
                return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} />;
            default:
                return null;
        }
    };

    const progress = (step / 5) * 100;

    return (
        <div className="w-full max-w-2xl mx-auto p-4 md:p-8">
            {step < 5 && (
                <div className="mb-10">
                    <div className="flex justify-between items-end mb-3">
                        <span className="text-sm font-semibold text-slate-600">
                            Étape {step}/4 : <span className="text-slate-400 font-medium">
                                {step === 1 && "Identité & Origine"}
                                {step === 2 && "Parcours Migratoire"}
                                {step === 3 && "Activité & Ressources"}
                                {step === 4 && "Famille & Vulnérabilité"}
                            </span>
                        </span>
                        <span className="text-xs font-bold text-indigo-600 bg-indigo-50 px-2 py-0.5 rounded-full">{Math.round(progress)}%</span>
                    </div>
                    <div className="w-full bg-slate-200/50 rounded-full h-2 overflow-hidden shadow-inner">
                        <div
                            className="bg-gradient-to-r from-blue-600 to-indigo-600 h-2 rounded-full transition-all duration-700 ease-out shadow-sm"
                            style={{ width: `${progress}%` }}
                        ></div>
                    </div>
                </div>
            )}

            <div className="bg-white rounded-3xl shadow-2xl shadow-slate-200/60 border border-slate-100 min-h-[500px] flex flex-col overflow-hidden">
                {renderStep()}
            </div>

            {/* Debug Panel (Dev Mode) */}
            <div className="mt-8">
                <h4 className="text-xs font-bold text-slate-400 uppercase tracking-widest mb-2">Debug Info (Real-time Profile)</h4>
                <pre className="bg-slate-900 text-emerald-400 p-6 rounded-2xl text-[10px] overflow-auto max-h-60 border border-slate-800 shadow-2xl">
                    {JSON.stringify(userProfile, null, 2)}
                </pre>
            </div>
        </div>
    );
}
