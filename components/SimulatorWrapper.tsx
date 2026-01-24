'use client';

import React, { useState } from 'react';
import { UserProfile } from '@/types';
import IdentityTimelineStep from '@/components/steps/IdentityTimelineStep';
import MigratoryHistoryStep from '@/components/steps/MigratoryHistoryStep';
import ActivityResourcesStep from '@/components/steps/ActivityResourcesStep';
import FamilyVulnerabilityStep from '@/components/steps/FamilyVulnerabilityStep';
import FamilyReunificationStep from './steps/FamilyReunificationStep';
import DrivingLicenseStep from '@/components/steps/DrivingLicenseStep';
import RdvPrefectureStep from '@/components/steps/RdvPrefectureStep';
import LegalConsultationStep from '@/components/steps/LegalConsultationStep';
import FrenchCourseStep from '@/components/steps/FrenchCourseStep';
import CivicExamStep from '@/components/steps/CivicExamStep';
import CallbackStep from '@/components/steps/CallbackStep';
import ResultsView from '@/components/ResultsView';
import { LEGAL_QUESTIONS } from '@/data/modules/legal';
import { Scale, ArrowRight } from 'lucide-react';
import { SERVICES_CATALOG } from '@/data/services';
import { CRM } from '@/services/crmStore';

const INITIAL_STATE: UserProfile = {
    identity: {
        age: 25,
        nationality_group: 'NON_EU',
        born_in_france: false,
        name: '',
        email: '',
        phone: ''
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
    driving: {
        status: undefined,
        license_country: undefined,
        residence_start_date: '',
    },
    rdv_prefecture: {
        prefecture_dept: '',
        rdv_reason: undefined,
        current_status: undefined,
    },
    rdv_juriste: {
        subject: undefined,
        mode: undefined,
    },
    french: {
        goal: undefined,
        current_level: undefined,
        location_zip: undefined,
    },
    civic_exam: {
        civic_goal: undefined,
        knowledge_level: undefined,
        location_zip: undefined,
    },
    callback: {
        callback_subject: undefined,
        callback_urgency: undefined,
        location_zip: undefined,
    }
};

interface SimulatorWrapperProps {
    serviceId?: string;
    forceAgencyId?: string;
    onComplete?: () => void;
}

export default function SimulatorWrapper({ serviceId, forceAgencyId, onComplete }: SimulatorWrapperProps) {
    const [step, setStep] = useState(1);
    const [userProfile, setUserProfile] = useState<UserProfile>({
        ...INITIAL_STATE,
        project: {
            ...INITIAL_STATE.project,
            target_goal: serviceId ? 'SERVICE' : 'BOTH'
        }
    });

    const selectedServiceId = serviceId || '';

    const isFamilyReunification = selectedServiceId === 'family_reunification';
    const isDrivingExchange = selectedServiceId === 'permis_conduire';
    const isRdvPrefecture = selectedServiceId === 'rdv_prefecture';
    const isLegalConsultation = selectedServiceId === 'rdv_juriste';
    const isFrenchCourse = selectedServiceId === 'french_course';
    const isCivicExam = selectedServiceId === 'examen_civique';
    const isCallback = selectedServiceId === 'rappel_echeances';

    // Standard: Identity(1), History(2), Activity(3), Family(4), Results(5)
    // Family/Driving/RDV/Juriste/French/Civic/Callback: Questionnaire(1), Results(2)
    const totalStepsCap = (isFamilyReunification || isDrivingExchange || isRdvPrefecture || isLegalConsultation || isFrenchCourse || isCivicExam || isCallback) ? 1 : 4;

    const updateProfile = (section: keyof UserProfile, data: any) => {
        setUserProfile((prev) => {
            let updatedSection = { ...prev[section], ...data };

            // Salary & Duration Synchronization
            if (section === 'work') {
                if ('salary_monthly_gross' in data) {
                    updatedSection.annual_gross_salary = (data.salary_monthly_gross || 0) * 12;
                } else if ('annual_gross_salary' in data) {
                    updatedSection.salary_monthly_gross = Math.round((data.annual_gross_salary || 0) / 12);
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

            if (section === 'driving') {
                updatedSection = { ...prev.driving, ...data };
            }

            if (section === 'rdv_prefecture') {
                updatedSection = { ...prev.rdv_prefecture, ...data };
            }

            if (section === 'rdv_juriste') {
                updatedSection = { ...prev.rdv_juriste, ...data };
            }

            if (section === 'french') {
                updatedSection = { ...prev.french, ...data };
            }

            if (section === 'civic_exam') {
                updatedSection = { ...prev.civic_exam, ...data };
            }

            if (section === 'callback') {
                updatedSection = { ...prev.callback, ...data };
            }

            let nextProfile = { ...prev, [section]: updatedSection };

            if (nextProfile.admin.current_visa_type === 'NONE') {
                nextProfile.admin = {
                    ...nextProfile.admin,
                    has_valid_visa_or_permit: false,
                    entry_mode: 'STANDARD'
                };
            } else {
                nextProfile.admin.has_valid_visa_or_permit = true;
            }

            if (section === 'work' || section === 'financial') {
                const salary = nextProfile.work.salary_monthly_gross || 0;
                const totalMonthly = salary;
                const SMIC = 1766.92;
                nextProfile.financial = {
                    ...nextProfile.financial,
                    resources_monthly_average: totalMonthly,
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

        // Family isolated mode
        if (isFamilyReunification) {
            if (currentStep === 1) {
                const { sponsor_nationality, presence_duration, has_handicap_allowance, housing_status, income_source } = data.family;
                const hasBasic = !!sponsor_nationality && !!presence_duration && has_handicap_allowance !== undefined && !!housing_status;
                if (!hasBasic) return false;
                if (has_handicap_allowance === false) {
                    return (data.work.salary_monthly_gross || 0) > 0 && !!income_source;
                }
                return true;
            }
            return true;
        }

        if (isDrivingExchange) {
            if (currentStep === 1) {
                const { status, license_country, residence_start_date } = data.driving;
                if (!status || !license_country) return false;
                if (status === 'STUDENT') return true;
                return !!residence_start_date && /^\d{2}\/\d{4}$/.test(residence_start_date);
            }
            return true;
        }

        if (isRdvPrefecture) {
            if (currentStep === 1) {
                const { prefecture_dept, rdv_reason } = data.rdv_prefecture;
                return !!prefecture_dept && /^\d{5}$/.test(prefecture_dept) && !!rdv_reason && rdv_reason !== 'renouvellement_anef';
            }
            return true;
        }

        if (isLegalConsultation) {
            if (currentStep === 1) {
                const { subject, mode } = data.rdv_juriste;
                return !!subject && !!mode;
            }
            return true;
        }

        if (isFrenchCourse) {
            if (currentStep === 1) {
                const { goal, current_level, location_zip } = data.french;
                return !!goal && !!current_level && !!location_zip && /^\d{5}$/.test(location_zip);
            }
            return true;
        }

        if (isCivicExam) {
            if (currentStep === 1) {
                const { civic_goal, knowledge_level, location_zip } = data.civic_exam;
                return !!civic_goal && !!knowledge_level && !!location_zip && /^\d{5}$/.test(location_zip);
            }
            return true;
        }

        if (isCallback) {
            if (currentStep === 1) {
                const { callback_subject, callback_urgency, location_zip } = data.callback;
                return !!callback_subject && !!callback_urgency && !!location_zip && /^\d{5}$/.test(location_zip);
            }
            return true;
        }

        if (isFrenchCourse) {
            if (currentStep === 1) {
                const { goal, current_level, location_zip } = data.french;
                return !!goal && !!current_level && !!location_zip && /^\d{5}$/.test(location_zip);
            }
            return true;
        }

        // Standard Mode
        switch (currentStep) {
            case 1:
                return !!data.identity.nationality_group && data.identity.age > 0 && !!data.project.target_goal;
            case 2:
                return !!data.timeline.entry_date && (!!data.admin.current_visa_type &&
                    data.civic.clean_criminal_record !== undefined &&
                    data.civic.no_expulsion_order !== undefined);
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
                return !!data.family.spouse_nationality;
            default:
                return true;
        }
    };

    const renderStep = () => {
        const canNext = isStepValid(step);

        if (isFamilyReunification) {
            switch (step) {
                case 1:
                    return <FamilyReunificationStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.family) updateProfile('family', updates.family);
                            if (updates.work) updateProfile('work', updates.work);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        if (isDrivingExchange) {
            switch (step) {
                case 1:
                    return <DrivingLicenseStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.driving) updateProfile('driving', updates.driving);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        if (isRdvPrefecture) {
            switch (step) {
                case 1:
                    return <RdvPrefectureStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.rdv_prefecture) updateProfile('rdv_prefecture', updates.rdv_prefecture);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        if (isLegalConsultation) {
            const openPaymentFlow = async (amount: number) => {
                // En mode Kiosque ou Agency, on crée juste le lead
                const leadData = {
                    name: userProfile.identity.name || 'Candidat Anonyme',
                    email: userProfile.identity.email || '',
                    phone: userProfile.identity.phone || '',
                    serviceId: selectedServiceId,
                    serviceName: SERVICES_CATALOG.find(s => s.id === selectedServiceId)?.title || 'Service Premium',
                    originAgencyId: forceAgencyId || null,
                    status: 'NEW',
                    amountPaid: 0
                };

                const lead = await CRM.saveLead(leadData);
                if (lead && onComplete) {
                    onComplete();
                } else {
                    // @ts-ignore
                    if (window.openAuthAndPaymentFlow) {
                        // @ts-ignore
                        window.openAuthAndPaymentFlow(amount / 100);
                    } else {
                        alert(`Dossier ${lead?.id} créé avec succès !`);
                    }
                }
            };

            switch (step) {
                case 1:
                    return <LegalConsultationStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.rdv_juriste) updateProfile('rdv_juriste', updates.rdv_juriste);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    const type = userProfile.rdv_juriste.mode;
                    const isRemote = type === 'remote';

                    return (
                        <div className="p-12 bg-white rounded-3xl animate-in fade-in slide-in-from-bottom-4 duration-700">
                            <div className="text-center mb-10">
                                <div className={`mx-auto w-20 h-20 flex items-center justify-center rounded-3xl mb-6 shadow-xl ${isRemote ? 'bg-blue-600 text-white' : 'bg-indigo-600 text-white'}`}>
                                    <Scale size={40} />
                                </div>
                                <h2 className="text-4xl font-black text-slate-900 tracking-tight mb-4">
                                    {isRemote ? "Consultation Juridique Express" : "Consultation Premium en Cabinet"}
                                </h2>
                                <p className="text-xl text-slate-500 font-medium max-w-2xl mx-auto leading-relaxed">
                                    {isRemote
                                        ? "Analyse de situation et stratégie par un expert dédié. Parfait pour valider un point précis."
                                        : "Étude approfondie de vos pièces et définition de la stratégie procédurale en face à face."}
                                </p>
                            </div>

                            <div className="bg-slate-50 p-8 rounded-[2rem] border border-slate-100 mb-10 shadow-inner">
                                <div className="flex justify-between items-center mb-4">
                                    <span className="text-xl font-bold text-slate-700">Tarif forfaitaire</span>
                                    <span className="text-4xl font-black text-emerald-600">
                                        {isRemote ? "69,00 €" : "99,00 €"}
                                    </span>
                                </div>
                                <div className="text-base text-slate-400 font-medium">Inclus : TVA, Honoraires expert agréé, Compte-rendu par email.</div>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-10">
                                <div className="flex items-center gap-3 p-4 bg-emerald-50 rounded-2xl text-emerald-700 font-bold border border-emerald-100">
                                    <div className="w-2 h-2 rounded-full bg-emerald-500 animate-pulse" />
                                    Expert disponible sous 24h
                                </div>
                                <div className="flex items-center gap-3 p-4 bg-blue-50 rounded-2xl text-blue-700 font-bold border border-blue-100">
                                    <div className="w-2 h-2 rounded-full bg-blue-500" />
                                    Annulation gratuite (48h)
                                </div>
                            </div>

                            <button
                                onClick={() => openPaymentFlow(isRemote ? 6900 : 9900)}
                                className="w-full py-6 bg-gradient-to-r from-blue-600 to-indigo-600 hover:from-blue-700 hover:to-indigo-700 text-white text-2xl font-black rounded-2xl transition-all shadow-2xl shadow-indigo-200 flex items-center justify-center gap-3 group"
                            >
                                <span>Réserver maintenant</span>
                                <ArrowRight size={28} className="group-hover:translate-x-1 transition-transform" />
                            </button>

                            <button
                                onClick={() => setStep(1)}
                                className="w-full mt-6 py-4 text-slate-400 font-bold hover:text-slate-600 transition-colors"
                            >
                                Revenir au questionnaire
                            </button>
                        </div>
                    );
                default:
                    return null;
            }
        }

        if (isFrenchCourse) {
            switch (step) {
                case 1:
                    return <FrenchCourseStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.french) updateProfile('french', updates.french);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        if (isCivicExam) {
            switch (step) {
                case 1:
                    return <CivicExamStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.civic_exam) updateProfile('civic_exam', updates.civic_exam);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        if (isCallback) {
            switch (step) {
                case 1:
                    return <CallbackStep
                        userProfile={userProfile}
                        updateProfile={(updates: Partial<UserProfile>) => {
                            if (updates.callback) updateProfile('callback', updates.callback);
                        }}
                        onNext={nextStep}
                    />;
                case 2:
                    return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
                default:
                    return null;
            }
        }

        // Standard Mode
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
                return <ResultsView userProfile={userProfile} onReset={() => setStep(1)} serviceId={selectedServiceId} forceAgencyId={forceAgencyId} />;
            default:
                return null;
        }
    };

    const currentTotalSteps = (isFamilyReunification || isDrivingExchange || isRdvPrefecture || isLegalConsultation || isFrenchCourse || isCivicExam || isCallback) ? 2 : 5;
    const progress = (step / currentTotalSteps) * 100;

    const [showDebug, setShowDebug] = useState(false);

    return (
        <div className="w-full max-w-4xl mx-auto p-4 md:p-8">
            {step < currentTotalSteps && (
                <div className="mb-10">
                    <div className="flex justify-between items-end mb-3">
                        <span className="text-sm font-semibold text-slate-600">
                            {isFamilyReunification ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Questionnaire d'éligibilité</span></>
                            ) : isDrivingExchange ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Questionnaire Permis</span></>
                            ) : isRdvPrefecture ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Formulaire de Commande</span></>
                            ) : isLegalConsultation ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Qualification de Consultation</span></>
                            ) : isFrenchCourse ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Localiser un centre</span></>
                            ) : isCivicExam ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Localiser un formateur</span></>
                            ) : isCallback ? (
                                <>Étape 1/1 : <span className="text-slate-400 font-medium">Qualifier mon appel</span></>
                            ) : (
                                <>Étape {step}/4 : <span className="text-slate-400 font-medium">
                                    {step === 1 && "Identité & Origine"}
                                    {step === 2 && "Parcours Migratoire"}
                                    {step === 3 && "Activité & Ressources"}
                                    {step === 4 && "Famille & Vulnérabilité"}
                                </span></>
                            )}
                        </span>
                        <div className="flex items-center gap-4">
                            <button
                                onClick={() => setShowDebug(!showDebug)}
                                className={`text-[10px] font-bold uppercase tracking-widest px-2 py-1 rounded border transition-all ${showDebug ? 'bg-slate-900 text-white border-slate-900' : 'bg-transparent text-slate-400 border-slate-200 hover:border-slate-400'}`}
                            >
                                {showDebug ? 'Debug ON' : 'Debug OFF'}
                            </button>
                            <span className="text-xs font-bold text-indigo-600 bg-indigo-50 px-2 py-0.5 rounded-full">{Math.round(progress)}%</span>
                        </div>
                    </div>
                    <div className="w-full bg-slate-200/50 rounded-full h-2 overflow-hidden shadow-inner">
                        <div
                            className="bg-gradient-to-r from-blue-600 to-indigo-600 h-2 rounded-full transition-all duration-700 ease-out shadow-sm"
                            style={{ width: `${progress}%` }}
                        ></div>
                    </div>
                </div>
            )}

            <div className="bg-white rounded-[3rem] shadow-2xl shadow-slate-200/60 border border-slate-100 min-h-[500px] flex flex-col overflow-hidden">
                {renderStep()}
            </div>

            {/* Debug Panel (Dev Mode) */}
            {showDebug && (
                <div className="mt-8 animate-in fade-in slide-in-from-top-2 duration-300">
                    <h4 className="text-xs font-bold text-slate-400 uppercase tracking-widest mb-2">Debug Info (Real-time Profile)</h4>
                    <pre className="bg-slate-900 text-emerald-400 p-6 rounded-2xl text-[10px] overflow-auto max-h-60 border border-slate-800 shadow-2xl">
                        {JSON.stringify(userProfile, null, 2)}
                    </pre>
                </div>
            )}
        </div>
    );
}
