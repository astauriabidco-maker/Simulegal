export declare class EligibilityService {
    getThresholds(): {
        smic_mensuel_brut: number;
        smic_mensuel_net: number;
        smic_annuel_brut: number;
        min_residence_naturalisation_years: number;
        min_residence_conjoint_years: number;
        min_residence_etudiant_reduc_years: number;
        logement_surface_base: number;
        logement_surface_per_person: number;
        frais_timbre_naturalisation: number;
        frais_timbre_sejour_base: number;
    };
    getRules(category: string): never[];
}
