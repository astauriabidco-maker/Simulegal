import { UserProfile } from '../types/index';
import { evaluateRule, findFirstFailedCondition } from './engine';
import rulesSejour from '../specs/rules_sejour.json';
import rulesNaturalisation from '../specs/rules_naturalisation.json';
import rulesFamily from '../specs/rules_family.json';

// --- UTILITY ---
let passed = 0;
let failed = 0;

function assert(name: string, actual: boolean, expected: boolean) {
    if (actual === expected) {
        console.log(`  ✅ ${name}`);
        passed++;
    } else {
        console.error(`  ❌ ${name} — attendu: ${expected}, reçu: ${actual}`);
        failed++;
    }
}

function assertNotNull(name: string, value: any) {
    if (value !== null && value !== undefined) {
        console.log(`  ✅ ${name}`);
        passed++;
    } else {
        console.error(`  ❌ ${name} — attendu: non-null, reçu: ${value}`);
        failed++;
    }
}

function assertNull(name: string, value: any) {
    if (value === null) {
        console.log(`  ✅ ${name}`);
        passed++;
    } else {
        console.error(`  ❌ ${name} — attendu: null, reçu: ${JSON.stringify(value)}`);
        failed++;
    }
}

function findRule(rules: any[], id: string) {
    const rule = rules.find((r: any) => r.id === id);
    if (!rule) {
        console.error(`  ⚠️  Règle '${id}' non trouvée !`);
        return null;
    }
    return rule;
}

// --- BASE PROFILE ---
const baseProfile: UserProfile = {
    identity: { age: 30, nationality_group: 'NON_EU', born_in_france: false },
    timeline: { entry_date: '2020-01-01', years_continuous_residence: 5 },
    admin: { has_valid_visa_or_permit: true, health_insurance: true, current_visa_type: 'WORKER' },
    family: { spouse_nationality: 'NONE', marriage_duration_years: 0, community_of_life: false, is_polygamous: false, has_french_child: false, spouse_kept_nationality: true, contributes_to_education: true },
    work: { contract_type: 'CDI', annual_gross_salary: 25000, salary_monthly_gross: 2083, has_work_authorization: true, job_in_tension_list: false, main_situation: 'WORKER' },
    education: { diploma_level: 'NONE', has_french_higher_education_diploma: false, is_enrolled_higher_ed: false },
    financial: { resources_stable_sufficient: true, resources_monthly_average: 2083, resources_annual_total: 25000 },
    investment: { amount: 0 },
    integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: true },
    civic: { clean_criminal_record: true, no_expulsion_order: true },
    vulnerability: { show_vulnerability: false },
    health: {},
    residence: {},
    project: { target_goal: 'BOTH' },
    driving: {},
    rdv_prefecture: {},
    rdv_juriste: {},
    french: {},
    civic_exam: {},
    callback: {},
    asylum: {},
    regularisation: {},
    nationality_extra: {},
};

// Deep clone helper
function profile(overrides: any = {}): UserProfile {
    const p = JSON.parse(JSON.stringify(baseProfile));
    for (const section of Object.keys(overrides)) {
        p[section] = { ...p[section], ...overrides[section] };
    }
    return p;
}

// ==========================================
// TEST SUITE 1: French Level Comparison Fix
// ==========================================
console.log('\n=== TEST 1: Comparaison sémantique des niveaux de français ===\n');

const frenchConditionGTE_A2 = { var: 'integration.french_level', op: 'GTE' as const, val: 'A2' };
const frenchConditionGTE_B1 = { var: 'integration.french_level', op: 'GTE' as const, val: 'B1' };
const frenchConditionGTE_B2 = { var: 'integration.french_level', op: 'GTE' as const, val: 'B2' };
const frenchConditionLT_B2 = { var: 'integration.french_level', op: 'LT' as const, val: 'B2' };

assert('A1 >= A2 → false', evaluateRule(profile({ integration: { french_level: 'A1' } }), frenchConditionGTE_A2), false);
assert('A2 >= A2 → true', evaluateRule(profile({ integration: { french_level: 'A2' } }), frenchConditionGTE_A2), true);
assert('B1 >= A2 → true', evaluateRule(profile({ integration: { french_level: 'B1' } }), frenchConditionGTE_A2), true);
assert('B2 >= A2 → true', evaluateRule(profile({ integration: { french_level: 'B2' } }), frenchConditionGTE_A2), true);
assert('C1 >= A2 → true', evaluateRule(profile({ integration: { french_level: 'C1' } }), frenchConditionGTE_A2), true);

assert('A1 >= B1 → false', evaluateRule(profile({ integration: { french_level: 'A1' } }), frenchConditionGTE_B1), false);
assert('A2 >= B1 → false', evaluateRule(profile({ integration: { french_level: 'A2' } }), frenchConditionGTE_B1), false);
assert('B1 >= B1 → true', evaluateRule(profile({ integration: { french_level: 'B1' } }), frenchConditionGTE_B1), true);
assert('B2 >= B1 → true', evaluateRule(profile({ integration: { french_level: 'B2' } }), frenchConditionGTE_B1), true);

assert('A2 >= B2 → false', evaluateRule(profile({ integration: { french_level: 'A2' } }), frenchConditionGTE_B2), false);
assert('B1 >= B2 → false', evaluateRule(profile({ integration: { french_level: 'B1' } }), frenchConditionGTE_B2), false);
assert('B2 >= B2 → true', evaluateRule(profile({ integration: { french_level: 'B2' } }), frenchConditionGTE_B2), true);
assert('C1 >= B2 → true', evaluateRule(profile({ integration: { french_level: 'C1' } }), frenchConditionGTE_B2), true);

assert('A1 < B2 → true', evaluateRule(profile({ integration: { french_level: 'A1' } }), frenchConditionLT_B2), true);
assert('B1 < B2 → true', evaluateRule(profile({ integration: { french_level: 'B1' } }), frenchConditionLT_B2), true);
assert('B2 < B2 → false', evaluateRule(profile({ integration: { french_level: 'B2' } }), frenchConditionLT_B2), false);

// ==========================================
// TEST SUITE 2: Séjour Rules
// ==========================================
console.log('\n=== TEST 2: Règles Titre de Séjour ===\n');

const vpfConjointRule = findRule(rulesSejour as any[], 'vpf_conjoint_francais');
if (vpfConjointRule) {
    const vpfUser = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 2, community_of_life: true, is_polygamous: false, has_french_child: false, spouse_kept_nationality: true },
        integration: { french_level: 'A2', civic_exam_passed: true },
        admin: { entered_legally: true },
    });
    assert('VPF Conjoint Français : éligible', evaluateRule(vpfUser, vpfConjointRule.conditions), true);

    const vpfUserNoKept = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 2, community_of_life: true, is_polygamous: false, has_french_child: false, spouse_kept_nationality: false },
        integration: { french_level: 'A2', civic_exam_passed: true },
    });
    assert('VPF Conjoint Français (conjoint perdu nationalité) : non éligible', evaluateRule(vpfUserNoKept, vpfConjointRule.conditions), false);
}

const vpfParentRule = findRule(rulesSejour as any[], 'vpf_parent_enfant_francais');
if (vpfParentRule) {
    const parentUser = profile({
        family: { spouse_nationality: 'NONE', has_french_child: true, child_residence_france: true, contributes_to_education: true, is_polygamous: false },
        integration: { french_level: 'A2', civic_exam_passed: true },
        admin: { entered_legally: true },
    });
    assert('VPF Parent enfant français : éligible', evaluateRule(parentUser, vpfParentRule.conditions), true);

    const parentNoContrib = profile({
        family: { spouse_nationality: 'NONE', has_french_child: true, child_residence_france: true, contributes_to_education: false, is_polygamous: false },
        integration: { french_level: 'A2', civic_exam_passed: true },
    });
    assert('VPF Parent (ne contribue pas) : non éligible', evaluateRule(parentNoContrib, vpfParentRule.conditions), false);
}

const etudiantRule = findRule(rulesSejour as any[], 'cs_etudiant');
if (etudiantRule) {
    const studentUser = profile({
        education: { diploma_level: 'LICENCE', is_enrolled_higher_ed: true },
        financial: { resources_stable_sufficient: true, resources_monthly_average: 700 },
        integration: { french_level: 'A2', civic_exam_passed: true },
        work: { main_situation: 'STUDENT' },
        admin: { current_visa_type: 'STUDENT' },
    });
    assert('CS Étudiant : éligible', evaluateRule(studentUser, etudiantRule.conditions), true);

    const studentNotEnrolled = profile({
        education: { diploma_level: 'LICENCE', is_enrolled_higher_ed: false },
        financial: { resources_stable_sufficient: true },
        integration: { french_level: 'A2', civic_exam_passed: true },
        work: { main_situation: 'STUDENT' },
    });
    assert('CS Étudiant (non inscrit) : non éligible', evaluateRule(studentNotEnrolled, etudiantRule.conditions), false);
}

const visiteurRule = findRule(rulesSejour as any[], 'cs_visiteur');
if (visiteurRule) {
    const visitorUser = profile({
        work: { wants_to_work: false, main_situation: 'OTHER' },
        financial: { resources_stable_sufficient: true, resources_monthly_average: 2000 },
        integration: { french_level: 'A2', civic_exam_passed: true },
    });
    assert('CS Visiteur : éligible', evaluateRule(visitorUser, visiteurRule.conditions), true);

    const visitorWantsWork = profile({
        work: { wants_to_work: true, main_situation: 'OTHER' },
        financial: { resources_stable_sufficient: true },
        integration: { french_level: 'A2', civic_exam_passed: true },
    });
    assert('CS Visiteur (veut travailler) : non éligible', evaluateRule(visitorWantsWork, visiteurRule.conditions), false);
}

const carteResidentConjointRule = findRule(rulesSejour as any[], 'carte_resident_conjoint_francais');
if (carteResidentConjointRule) {
    const residentConjoint = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 4, community_of_life: true, is_polygamous: false, spouse_kept_nationality: true },
        integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Carte Résident Conjoint Fr (4 ans, B1) : éligible', evaluateRule(residentConjoint, carteResidentConjointRule.conditions), true);

    const residentConjointA2 = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 4, community_of_life: true, is_polygamous: false, spouse_kept_nationality: true },
        integration: { french_level: 'A2', adheres_to_republican_values: true, civic_exam_passed: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Carte Résident Conjoint Fr (A2 < B1 requis) : non éligible', evaluateRule(residentConjointA2, carteResidentConjointRule.conditions), false);
}

// ==========================================
// TEST SUITE 3: Naturalisation Rules
// ==========================================
console.log('\n=== TEST 3: Règles Naturalisation ===\n');

const natDecretRule = findRule(rulesNaturalisation as any[], 'nat_decret_standard');
if (natDecretRule) {
    const natUser = profile({
        timeline: { years_continuous_residence: 5 },
        integration: { french_level: 'B2', adheres_to_republican_values: true, civic_exam_passed: true },
        financial: { resources_stable_sufficient: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Naturalisation décret (5 ans, B2) : éligible', evaluateRule(natUser, natDecretRule.conditions), true);

    const natUserB1 = profile({
        timeline: { years_continuous_residence: 5 },
        integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: true },
        financial: { resources_stable_sufficient: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Naturalisation décret (B1 < B2 requis) : non éligible', evaluateRule(natUserB1, natDecretRule.conditions), false);
}

const natMariageRule = findRule(rulesNaturalisation as any[], 'nat_declaration_mariage');
if (natMariageRule) {
    const natMariageUser = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 5, community_of_life: true, is_polygamous: false, spouse_kept_nationality: true },
        timeline: { years_continuous_residence: 5, resides_in_france: true },
        integration: { french_level: 'B2', adheres_to_republican_values: true, civic_exam_passed: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Naturalisation mariage (5 ans, en France, B2) : éligible', evaluateRule(natMariageUser, natMariageRule.conditions), true);

    const natMariageB1 = profile({
        family: { spouse_nationality: 'FRENCH', marriage_duration_years: 5, community_of_life: true, is_polygamous: false, spouse_kept_nationality: true },
        integration: { french_level: 'B1', adheres_to_republican_values: true, civic_exam_passed: true },
        civic: { clean_criminal_record: true, no_expulsion_order: true },
    });
    assert('Naturalisation mariage (B1 < B2 requis) : non éligible', evaluateRule(natMariageB1, natMariageRule.conditions), false);
}

// ==========================================
// TEST SUITE 4: Regroupement Familial (Family Rules)
// ==========================================
console.log('\n=== TEST 4: Règles Regroupement Familial ===\n');

const rfDroitCommun = findRule(rulesFamily as any[], 'family_reunification_eligibility');
if (rfDroitCommun) {
    const rfEligible = profile({
        identity: { age: 30 },
        family: {
            is_polygamous: false,
            rf_has_valid_titre_sejour: true,
            rf_marital_status: 'MARRIED',
            presence_duration: 'MORE_18',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            housing_status: 'OWNED_RENTED', // Required by rule
            income_source: 'SALARY',      // Required for resource check
            has_handicap_allowance: false
        },
    });
    assert('RF Droit commun : éligible (18+ mois)', evaluateRule(rfEligible, rfDroitCommun.conditions), true);

    const rfPolygamous = profile({
        identity: { age: 30 },
        family: {
            is_polygamous: true,
            rf_has_valid_titre_sejour: true,
            rf_marital_status: 'MARRIED',
            presence_duration: 'MORE_18',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            housing_status: 'OWNED_RENTED',
            income_source: 'SALARY',
        },
    });
    assert('RF Droit commun (polygame) : non éligible', evaluateRule(rfPolygamous, rfDroitCommun.conditions), false);

    const rfNoTitre = profile({
        identity: { age: 30 },
        family: {
            is_polygamous: false,
            rf_has_valid_titre_sejour: false,
            rf_marital_status: 'MARRIED',
            presence_duration: 'MORE_18',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            housing_status: 'OWNED_RENTED',
            income_source: 'SALARY',
        },
    });
    assert('RF Droit commun (sans titre séjour) : non éligible', evaluateRule(rfNoTitre, rfDroitCommun.conditions), false);

    const rfMinor = profile({
        identity: { age: 17 },
        family: {
            is_polygamous: false,
            rf_has_valid_titre_sejour: true,
            rf_marital_status: 'MARRIED',
            presence_duration: 'MORE_18',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            housing_status: 'OWNED_RENTED',
            income_source: 'SALARY',
        },
    });
    assert('RF Droit commun (mineur) : non éligible', evaluateRule(rfMinor, rfDroitCommun.conditions), false);
}

// Test: RF accord franco-algérien
const rfAlgerien = findRule(rulesFamily as any[], 'family_reunification_algerian');
if (rfAlgerien) {
    const rfAlgerienEligible = profile({
        identity: { age: 30, nationality_group: 'ALGERIAN' },
        family: {
            is_polygamous: false,
            rf_has_valid_titre_sejour: true,
            rf_marital_status: 'MARRIED',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            sponsor_nationality: 'ALGERIAN', // Explicitly needed for rule check
            housing_status: 'OWNED_RENTED',
            income_source: 'SALARY',
            presence_duration: '12_18', // Specific to Algerian agreement (12-18 months allowed)
        },
    });
    assert('RF Algérien : éligible (accord bilatéral)', evaluateRule(rfAlgerienEligible, rfAlgerien.conditions), true);

    const rfAlgerienNonAlg = profile({
        identity: { age: 30, nationality_group: 'NON_EU' },
        family: {
            is_polygamous: false,
            rf_has_valid_titre_sejour: true,
            rf_marital_status: 'MARRIED',
            rf_family_members_count: 2,
            rf_housing_type: 'PROPRE',
            rf_resources_above_smic: true,
            sponsor_nationality: 'NON_EU',
            housing_status: 'OWNED_RENTED',
            income_source: 'SALARY',
            presence_duration: '12_18',
        },
    });
    assert('RF Algérien (non algérien) : non éligible', evaluateRule(rfAlgerienNonAlg, rfAlgerien.conditions), false);
}

// ==========================================
// TEST SUITE 5: AES (Admission Exceptionnelle au Séjour)
// ==========================================
console.log('\n=== TEST 5: CSS AES (Admission Exceptionnelle) ===\n');

// 1. Métiers en tension
const aesMetiers = findRule(rulesSejour as any[], 'aes_metiers_tension');
if (aesMetiers) {
    const aesUser = profile({
        identity: { age: 25 },
        admin: { has_valid_visa_or_permit: false },
        timeline: { years_continuous_residence: 3 },
        work: { contract_type: 'CDI', has_payslips: true, salary_monthly_gross: 2000 },
    });
    // Note: checks @config for SMIC, assuming mock returns valid comparison or simplistic override in test logic if mocked.
    // In strict unit test without config context, verifying logic flow.
    // However, engine.ts might require proper context for @config. 
    // If evaluateRule handles it dynamically, we might need to mock config lookup or ensure value is high enough.
    // Assuming 2000 > SMIC.
    assert('AES Métiers Tension (3 ans, CDI, fiches paie) : éligible', evaluateRule(aesUser, aesMetiers.conditions), true);

    const aesUserShort = profile({
        identity: { age: 25 },
        admin: { has_valid_visa_or_permit: false },
        timeline: { years_continuous_residence: 1 }, // < 3
        work: { contract_type: 'CDI', has_payslips: true, salary_monthly_gross: 2000 },
    });
    assert('AES Métiers Tension (1 an résidence) : non éligible', evaluateRule(aesUserShort, aesMetiers.conditions), false);
}

// 2. Vie Privée 10 ans
const aes10ans = findRule(rulesSejour as any[], 'aes_vie_privee_familiale');
if (aes10ans) {
    const aes10User = profile({
        identity: { age: 30 },
        admin: { current_visa_type: 'NONE' },
        regularisation: { years_presence_france: 12 },
        integration: { french_level: 'A2' },
        civic: { clean_criminal_record: true },
    });
    assert('AES Vie Privée (12 ans présence, A2) : éligible', evaluateRule(aes10User, aes10ans.conditions), true);

    const aes10UserShort = profile({
        regularisation: { years_presence_france: 5 }, // < 10
        integration: { french_level: 'A2' },
        civic: { clean_criminal_record: true },
        admin: { current_visa_type: 'NONE' },
    });
    assert('AES Vie Privée (5 ans présence) : non éligible', evaluateRule(aes10UserShort, aes10ans.conditions), false);
}

// 3. Parent enfant scolarisé
const aesParent = findRule(rulesSejour as any[], 'aes_parent_enfant_scolarise');
if (aesParent) {
    const aesParentUser = profile({
        timeline: { years_continuous_residence: 6 }, // > 5
        regularisation: { has_children_schooled_3y: true },
        admin: { current_visa_type: 'NONE' },
        civic: { clean_criminal_record: true },
    });
    assert('AES Parent (5 ans résid, enfant écolier 3 ans) : éligible', evaluateRule(aesParentUser, aesParent.conditions), true);

    const aesParentShort = profile({
        timeline: { years_continuous_residence: 2 }, // < 5
        regularisation: { has_children_schooled_3y: true },
        admin: { current_visa_type: 'NONE' },
        civic: { clean_criminal_record: true },
    });
    assert('AES Parent (2 ans résid) : non éligible', evaluateRule(aesParentShort, aesParent.conditions), false);
}

// ==========================================
// TEST SUITE 6: Edge Cases (null, undefined, missing)
// ==========================================
console.log('\n=== TEST 6: Edge Cases ===\n');

// Null/undefined values
const nullCondition = { var: 'identity.age', op: 'GTE' as const, val: 18 };
const profileMissingAge = profile({ identity: { age: undefined } });
assert('Âge undefined >= 18 → false', evaluateRule(profileMissingAge, nullCondition), false);

const profileNullAge = profile({ identity: { age: null } });
assert('Âge null >= 18 → false', evaluateRule(profileNullAge, nullCondition), false);

// Missing section — deeply nested
const deepMissingCondition = { var: 'nonexistent.section.field', op: 'EQ' as const, val: true };
assert('Section inexistante → false', evaluateRule(baseProfile, deepMissingCondition), false);

// Empty AND block — should return true (vacuously true)
const emptyAndCondition = { AND: [] };
assert('AND vide → true (vacuously)', evaluateRule(baseProfile, emptyAndCondition), true);

// Empty OR block — should return false (no branch matches)
const emptyOrCondition = { OR: [] };
assert('OR vide → false', evaluateRule(baseProfile, emptyOrCondition), false);

// Boolean equality
const boolTrueCondition = { var: 'civic.clean_criminal_record', op: 'EQ' as const, val: true };
assert('Casier vierge == true → true', evaluateRule(baseProfile, boolTrueCondition), true);

const boolFalseCondition = { var: 'civic.clean_criminal_record', op: 'EQ' as const, val: false };
assert('Casier vierge == false → false', evaluateRule(baseProfile, boolFalseCondition), false);

// NEQ operator
const neqCondition = { var: 'identity.nationality_group', op: 'NEQ' as const, val: 'EU' };
assert('NON_EU != EU → true', evaluateRule(baseProfile, neqCondition), true);

const neqCondition2 = { var: 'identity.nationality_group', op: 'NEQ' as const, val: 'NON_EU' };
assert('NON_EU != NON_EU → false', evaluateRule(baseProfile, neqCondition2), false);

// ==========================================
// TEST SUITE 6: IN Operator
// ==========================================
console.log('\n=== TEST 6: Opérateur IN ===\n');

const inCondition = { var: 'identity.nationality_group', op: 'IN' as const, val: ['EU', 'NON_EU', 'ALGERIAN'] };
assert('NON_EU IN [EU, NON_EU, ALGERIAN] → true', evaluateRule(baseProfile, inCondition), true);

const inConditionFail = { var: 'identity.nationality_group', op: 'IN' as const, val: ['EU', 'ALGERIAN', 'TUNISIAN'] };
assert('NON_EU IN [EU, ALGERIAN, TUNISIAN] → false', evaluateRule(baseProfile, inConditionFail), false);

const inConditionSingle = { var: 'work.contract_type', op: 'IN' as const, val: ['CDI'] };
assert('CDI IN [CDI] → true', evaluateRule(baseProfile, inConditionSingle), true);

const inConditionEmpty = { var: 'work.contract_type', op: 'IN' as const, val: [] };
assert('CDI IN [] → false', evaluateRule(baseProfile, inConditionEmpty), false);

// ==========================================
// TEST SUITE 7: findFirstFailedCondition
// ==========================================
console.log('\n=== TEST 7: findFirstFailedCondition ===\n');

// AND block — first failure
const andConditions = {
    AND: [
        { var: 'identity.age', op: 'GTE' as const, val: 18 },       // passes
        { var: 'integration.french_level', op: 'GTE' as const, val: 'C2' },  // fails (B1 < C2)
        { var: 'civic.clean_criminal_record', op: 'EQ' as const, val: true },  // passes
    ]
};
const firstFailed = findFirstFailedCondition(baseProfile, andConditions);
assertNotNull('findFirstFailedCondition retourne la première condition échouée', firstFailed);
if (firstFailed) {
    assert('Variable échouée = french_level', firstFailed.var === 'integration.french_level', true);
    assert('Valeur attendue = C2', firstFailed.val === 'C2', true);
    assert('Valeur réelle = B1', firstFailed.userValue === 'B1', true);
}

// All pass → null
const allPassConditions = {
    AND: [
        { var: 'identity.age', op: 'GTE' as const, val: 18 },
        { var: 'civic.clean_criminal_record', op: 'EQ' as const, val: true },
    ]
};
const noFailed = findFirstFailedCondition(baseProfile, allPassConditions);
assertNull('findFirstFailedCondition retourne null si tout passe', noFailed);

// Single leaf condition that fails
const singleFail = findFirstFailedCondition(
    baseProfile,
    { var: 'identity.age', op: 'LT' as const, val: 18 }
);
assertNotNull('findFirstFailedCondition (leaf qui échoue) : non-null', singleFail);
if (singleFail) {
    assert('Variable échouée = identity.age', singleFail.var === 'identity.age', true);
}

// OR block — fails only if all children fail
const orAllFailConditions = {
    OR: [
        { var: 'identity.age', op: 'LT' as const, val: 10 },    // fails
        { var: 'identity.age', op: 'GT' as const, val: 100 },   // fails
    ]
};
const orFailed = findFirstFailedCondition(baseProfile, orAllFailConditions);
assertNotNull('findFirstFailedCondition (OR, toutes échouent) : non-null', orFailed);

const orOnePassConditions = {
    OR: [
        { var: 'identity.age', op: 'LT' as const, val: 10 },    // fails
        { var: 'identity.age', op: 'GTE' as const, val: 18 },   // passes
    ]
};
const orPassed = findFirstFailedCondition(baseProfile, orOnePassConditions);
assertNull('findFirstFailedCondition (OR, une passe) : null', orPassed);

// ==========================================
// RÉSUMÉ
// ==========================================
console.log(`\n${'='.repeat(50)}`);
console.log(`RÉSULTAT: ${passed} passés, ${failed} échoués sur ${passed + failed} tests`);
console.log(`${'='.repeat(50)}\n`);

if (failed > 0) process.exit(1);

