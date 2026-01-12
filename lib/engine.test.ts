import { UserProfile } from '../types/index';
import { evaluateRule } from './engine';
import rulesSejour from '../specs/rules_sejour.json';

// --- MOCK USER PROFILE ---
// Simulation: Tunisien, marié à une Française depuis 1 an, vie commune OK, sans casier.
const mockUser: UserProfile = {
    identity: {
        age: 30,
        nationality_group: 'TUNISIAN',
        born_in_france: false,
    },
    timeline: {
        entry_date: '2023-01-01',
        years_continuous_residence: 1,
    },
    admin: {
        has_valid_visa_or_permit: true,
        health_insurance: true,
    },
    family: {
        spouse_nationality: 'FRENCH',
        marriage_duration_years: 1, // 1 an
        community_of_life: true,
        is_polygamous: false,
        has_french_child: false,
    },
    work: {
        contract_type: 'NONE',
        annual_gross_salary: 0,
        salary_monthly_gross: 0,
        has_work_authorization: false,
        job_in_tension_list: false,
    },
    education: {
        diploma_level: 'NONE',
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
    },
    civic: {
        clean_criminal_record: true,
        no_expulsion_order: true,
    },
};

// --- TEST EXECUTION ---
console.log('--- TEST DU MOTEUR DE RÈGLES ---');

const residentCardRule = (rulesSejour as any[]).find(
    (r) => r.id === 'carte_resident_conjoint_francais'
);

if (!residentCardRule) {
    console.error("Erreur : Règle 'carte_resident_conjoint_francais' non trouvée.");
    process.exit(1);
}

console.log(`Analyse de la règle : ${residentCardRule.name}`);
const isEligible = evaluateRule(mockUser, residentCardRule.conditions);

console.log('\nRésultat du test :');
console.log(`Utilisateur Tunisien (1 an de mariage) : ${isEligible ? 'ÉLIGIBLE ✅' : 'NON ÉLIGIBLE ❌'}`);

if (!isEligible) {
    console.log('\nNote : Le test a échoué car la règle JSON actuelle demande mécaniquement 3 ans.');
    console.log('Il faudrait modifier rules_sejour.json pour inclure l\'exception tunisienne via un bloc OR.');
}
