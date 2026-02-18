/**
 * Gold-standard test fixtures — profiles that MUST match specific procedures.
 * Run these in a unit test to prevent regressions after wizard or rule changes.
 */
import { UserProfile } from '@/types';

/** Deep-partial: all nested props become optional too */
type DeepPartial<T> = { [K in keyof T]?: T[K] extends object ? DeepPartial<T[K]> : T[K] };

// Each fixture only specifies the fields that differ from INITIAL_STATE defaults.
// The test runner should deep-merge each fixture with INITIAL_STATE before evaluating.

/** Moroccan salarié CDI — should match: accord_marocain_salarie */
export const PROFILE_MAROCAIN_SALARIE: DeepPartial<UserProfile> = {
    identity: { age: 30, nationality_group: 'MOROCCAN', born_in_france: false },
    timeline: { entry_date: '01/2020', years_continuous_residence: 6 },
    admin: { current_visa_type: 'WORKER', has_valid_visa_or_permit: true, entered_legally: true, health_insurance: true },
    work: {
        main_situation: 'WORKER', contract_type: 'CDI',
        salary_monthly_gross: 2500, annual_gross_salary: 30000,
        has_work_authorization: true
    },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: false },
    family: { spouse_nationality: 'NONE', is_polygamous: false },
};
export const EXPECTED_MAROCAIN = ['accord_marocain_salarie'];

/** Conjoint de Français — should match: vpf_conjoint_francais */
export const PROFILE_CONJOINT_FRANCAIS: DeepPartial<UserProfile> = {
    identity: { age: 28, nationality_group: 'NON_EU', born_in_france: false },
    timeline: { entry_date: '06/2021', years_continuous_residence: 4 },
    admin: { current_visa_type: 'VPF', has_valid_visa_or_permit: true, entered_legally: true, health_insurance: true },
    family: {
        spouse_nationality: 'FRENCH', marriage_duration_years: 3,
        community_of_life: true, is_polygamous: false,
        spouse_kept_nationality: true,
    },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    integration: { french_level: 'A2', adheres_to_republican_values: true, civic_exam_passed: false },
};
export const EXPECTED_CONJOINT = ['vpf_conjoint_francais'];

/** Sans-papiers 10 ans — should match: aes_10_ans */
export const PROFILE_SANS_PAPIERS_10: DeepPartial<UserProfile> = {
    identity: { age: 35, nationality_group: 'NON_EU', born_in_france: false },
    timeline: { entry_date: '03/2015', years_continuous_residence: 10 },
    admin: { current_visa_type: 'NONE', has_valid_visa_or_permit: false, entered_legally: false },
    work: {
        main_situation: 'WORKER', contract_type: 'CDI',
        salary_monthly_gross: 1800, annual_gross_salary: 21600,
        has_payslips: true, has_work_authorization: false,
    },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    regularisation: { years_presence_france: 10 },
    family: { is_polygamous: false },
};
export const EXPECTED_SANS_PAPIERS = ['aes_10_ans'];

/** Naturalisation par mariage — should match: nat_declaration_mariage */
export const PROFILE_NAT_MARIAGE: DeepPartial<UserProfile> = {
    identity: { age: 40, nationality_group: 'NON_EU', born_in_france: false },
    timeline: { entry_date: '01/2018', years_continuous_residence: 8 },
    admin: { current_visa_type: 'VPF', has_valid_visa_or_permit: true, entered_legally: true, health_insurance: true },
    family: {
        spouse_nationality: 'FRENCH', marriage_duration_years: 5,
        community_of_life: true, is_polygamous: false,
        spouse_kept_nationality: true,
    },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: true },
    financial: { resources_stable_sufficient: true, resources_monthly_average: 2000, resources_annual_total: 24000 },
    project: { target_goal: 'BOTH' },
};
export const EXPECTED_NAT_MARIAGE = ['nat_declaration_mariage'];

/** Étudiant changement de statut — should match: cst_etudiant_salarie */
export const PROFILE_ETUDIANT_CST: DeepPartial<UserProfile> = {
    identity: { age: 26, nationality_group: 'NON_EU', born_in_france: false },
    timeline: { entry_date: '09/2021', years_continuous_residence: 4 },
    admin: { current_visa_type: 'STUDENT', has_valid_visa_or_permit: true, entered_legally: true, health_insurance: true },
    work: {
        main_situation: 'STUDENT', contract_type: 'CDI',
        salary_monthly_gross: 2800, annual_gross_salary: 33600,
        has_work_authorization: true,
    },
    education: {
        diploma_level: 'MASTER', is_enrolled_higher_ed: false,
        has_french_higher_education_diploma: true,
    },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    integration: { french_level: 'B2', adheres_to_republican_values: true, civic_exam_passed: false },
    family: { spouse_nationality: 'NONE', is_polygamous: false },
};
export const EXPECTED_ETUDIANT = ['cst_etudiant_salarie'];

/** Réfugié — should match: aps_reconnu_refugie */
export const PROFILE_REFUGIE: DeepPartial<UserProfile> = {
    identity: { age: 32, nationality_group: 'REFUGEE', born_in_france: false },
    timeline: { entry_date: '01/2022', years_continuous_residence: 4 },
    admin: { current_visa_type: 'RECEIPISSE', has_valid_visa_or_permit: true, entered_legally: true, health_insurance: true },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    asylum: { is_asylum_seeker: true, asylum_application_pending: false },
    family: { spouse_nationality: 'NONE', is_polygamous: false },
};
export const EXPECTED_REFUGIE = ['aps_reconnu_refugie'];
