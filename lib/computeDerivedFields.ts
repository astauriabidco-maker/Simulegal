/**
 * computeDerivedFields — Pure function to derive all rule-engine variables
 * from the raw user profile collected by the wizard.
 *
 * This function is extracted from WizardFlow.goNext so it can be tested
 * independently. It returns a partial UserProfile with only the derived
 * sections that need to be merged.
 */
import { UserProfile } from '@/types';

/** SMIC brut mensuel 2026 — à mettre à jour chaque année */
export const SMIC_BRUT_MENSUEL = 1766.92;

export interface DerivedFields {
    timeline: Partial<UserProfile['timeline']>;
    regularisation: Partial<UserProfile['regularisation']>;
    work: Partial<UserProfile['work']>;
    financial: Partial<UserProfile['financial']>;
    admin: Partial<UserProfile['admin']>;
    project: Partial<UserProfile['project']>;
    asylum: Partial<UserProfile['asylum']>;
}

export function computeDerivedFields(profile: UserProfile): DerivedFields {
    const now = new Date();
    const age = profile.identity.age || 0;

    const result: DerivedFields = {
        timeline: {},
        regularisation: {},
        work: {},
        financial: {},
        admin: {},
        project: {},
        asylum: {},
    };

    // ── Timeline from entry_date ──
    if (profile.timeline.entry_date) {
        const parts = profile.timeline.entry_date.split('/').map(Number);
        // Support both MM/YYYY and YYYY-MM-DD formats
        let m: number, y: number;
        if (parts.length === 2) {
            [m, y] = parts;
        } else {
            // ISO format fallback
            const d = new Date(profile.timeline.entry_date);
            m = d.getMonth() + 1;
            y = d.getFullYear();
        }

        if (y && m) {
            const yearsRes = Math.max(0, Math.floor(
                (now.getFullYear() - y) + (now.getMonth() + 1 - m) / 12
            ));
            const ageAtEntry = Math.max(0, age - yearsRes);

            result.regularisation.years_presence_france = yearsRes;
            result.timeline.years_continuous_residence = yearsRes;
            result.timeline.age_at_entry = ageAtEntry;

            // Naturalisation timeline sub-fields
            if (age >= 11) {
                result.timeline.years_residence_since_age_11 = Math.max(0, yearsRes - Math.max(0, 11 - ageAtEntry));
            }
            if (age >= 8) {
                result.timeline.years_residence_since_age_8 = Math.max(0, yearsRes - Math.max(0, 8 - ageAtEntry));
            }
            if (ageAtEntry <= 6) {
                result.timeline.residence_since_age_6 = true;
            }
        }
    }

    // ── Resides in France (derived from any residence) ──
    // Used by nat_declaration_mariage: 4 years if in France, 5 years if abroad
    result.timeline.resides_in_france =
        (profile.timeline.years_continuous_residence ?? 0) > 0 ||
        !!profile.timeline.entry_date;

    // ── Salary: annual from monthly ──
    const monthly = profile.work.salary_monthly_gross || 0;
    if (monthly > 0) {
        result.work.annual_gross_salary = monthly * 12;
    }

    // ── Financial: annual from monthly ──
    const monthlyRes = profile.financial.resources_monthly_average || monthly;
    if (monthlyRes > 0) {
        result.financial.resources_annual_total = monthlyRes * 12;
        result.financial.resources_stable_sufficient = monthlyRes >= SMIC_BRUT_MENSUEL;
    }

    // ── Admin inferences ──
    const visa = profile.admin.current_visa_type;
    result.admin.has_valid_visa_or_permit = visa !== 'NONE' && visa !== undefined;

    // ── Work inferences ──
    if (profile.work.main_situation === 'ENTREPRENEUR') {
        result.work.is_entrepreneur = true;
    }

    // ── Project inferences ──
    if (profile.work.business_project_viable) {
        result.project.is_real_and_serious = true;
    }

    // ── Asylum inferences ──
    // NOTE: REFUGEE/STATELESS = recognised status → NOT an asylum seeker.
    // is_asylum_seeker should only be true for people with a pending application,
    // which must be explicitly declared by the user.

    return result;
}
