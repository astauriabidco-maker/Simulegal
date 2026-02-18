export interface UserProfile {
  identity: {
    age: number;
    nationality_group: 'EU' | 'ALGERIAN' | 'TUNISIAN' | 'MOROCCAN' | 'NON_EU' | 'FRANCE' | 'REFUGEE' | 'STATELESS';
    born_in_france: boolean;
    lost_french_nationality?: boolean;
    name?: string;
    email?: string;
    phone?: string;
  };
  timeline: {
    entry_date: string; // ISO Date
    years_continuous_residence: number;
    age_at_entry?: number;
    years_residence_since_age_11?: number;
    years_residence_since_age_8?: number;
    residence_since_age_6?: boolean;
    resides_in_france?: boolean;
  };
  admin: {
    current_visa_type?: 'VLS-TS' | 'STUDENT' | 'VISITOR' | 'WORKER' | 'VPF' | 'PASSEPORT_TALENT' | 'RESIDENT_CARD' | 'RECEIPISSE' | 'NONE';
    has_valid_visa_or_permit: boolean;
    entry_mode?: 'FAMILY_REUNIFICATION' | 'STANDARD';
    health_insurance: boolean;
    entered_legally?: boolean; // entrée régulière sur le territoire (visa ou dispense)
  };
  family: {
    spouse_nationality: 'FRENCH' | 'EU' | 'NON_EU' | 'NONE';
    marriage_duration_years: number;
    community_of_life: boolean;
    is_polygamous: boolean;
    has_french_child: boolean;
    child_residence_france?: boolean;
    contributes_to_education?: boolean;
    is_pacsed_with_french?: boolean;
    cohabitation_duration_years?: number;
    has_french_sibling?: boolean;
    is_ascendant_of_french?: boolean;
    spouse_kept_nationality?: boolean;
    spouse_has_passport_talent?: boolean;
    // Family Reunification specific fields
    presence_duration?: 'LESS_12' | '12_18' | 'MORE_18';
    has_handicap_allowance?: boolean;
    housing_status?: 'OWNED_RENTED' | 'SEARCHING' | 'UNKNOWN';
    income_source?: 'SALARY' | 'PENSION' | 'RSA_ALOWANCE' | 'OTHER';
    sponsor_nationality?: 'ALGERIAN' | 'OTHER';
    rf_family_members_count?: number;  // nombre de personnes à faire venir
    rf_housing_surface?: number;       // surface logement en m²
    rf_has_valid_titre_sejour?: boolean; // titre de séjour en cours de validité (CESEDA L434-1)
    rf_marital_status?: 'MARRIED' | 'CIVIL_PARTNER' | 'CONCUBIN' | 'SINGLE'; // situation matrimoniale
    rf_who_to_bring?: 'SPOUSE_ONLY' | 'CHILDREN_ONLY' | 'SPOUSE_AND_CHILDREN'; // regroupement partiel/total
  };
  work: {
    contract_type: 'CDI' | 'CDD' | 'SEASONAL' | 'NONE' | 'PROMESSE';
    annual_gross_salary: number;
    salary_monthly_gross: number;
    contract_duration_months?: number;
    has_work_authorization: boolean;
    job_in_tension_list: boolean;
    is_entrepreneur?: boolean;
    business_project_viable?: boolean;
    is_innovative_company?: boolean;
    job_related_to_rd?: boolean;
    company_role?: 'MANDATAIRE' | 'EMPLOYEE' | 'FOUNDER';
    group_seniority_months?: number;
    is_ict_transfer?: boolean;
    is_manager_or_expert?: boolean;
    wants_to_work?: boolean;
    years_experience_comparable?: number;
    main_situation?: 'STUDENT' | 'WORKER' | 'ENTREPRENEUR' | 'OTHER';
    has_work_seniority_proof?: boolean;
    has_payslips?: boolean;
    is_researcher?: boolean;
    has_hosting_agreement?: boolean;
    is_artist?: boolean;
    is_sportif_haut_niveau?: boolean;
    is_intern?: boolean;
    is_au_pair?: boolean;
    is_volunteer?: boolean;
    is_salarie_mission?: boolean;
    has_work_accident_pension?: boolean;
    work_accident_rate?: number;
    served_french_military?: boolean;
    has_legion_honneur?: boolean;
  };
  education: {
    diploma_level: 'NONE' | 'LICENCE' | 'MASTER' | 'PHD' | 'SPECIALIZED_MASTER' | 'LICENCE_PRO' | 'CGE_LEVEL_1';
    is_enrolled_higher_ed?: boolean;
    years_higher_education?: number;
    years_schooling_france?: number;
    schooling_in_france_age_6_to_16?: boolean;
    has_french_higher_education_diploma?: boolean;
  };
  financial: {
    resources_stable_sufficient: boolean;
    resources_monthly_average: number;
    resources_annual_total: number;
  };
  investment: {
    amount: number;
    creates_jobs?: boolean;
  };
  integration: {
    french_level: 'A1' | 'A2' | 'B1' | 'B2' | 'C1';
    adheres_to_republican_values: boolean;
    civic_exam_passed: boolean;
  };
  civic: {
    clean_criminal_record: boolean;
    no_expulsion_order: boolean;
  };
  vulnerability: {
    has_protection_order_violence?: boolean;
    show_vulnerability?: boolean;
    is_victim_trafficking?: boolean;
    is_victim_domestic_violence?: boolean;
  };
  health: {
    child_needs_care?: boolean;
    treatment_available_origin?: boolean;
    treatment_unavailable_in_origin?: boolean;
    personal_needs_treatment?: boolean;
  };
  asylum: {
    is_asylum_seeker?: boolean;
    asylum_application_pending?: boolean;
  };
  regularisation: {
    has_children_schooled_3y?: boolean;
    has_exceptional_talent?: boolean;
    years_presence_france?: number;
  };
  nationality_extra: {
    possession_etat_francais?: boolean;
  };
  residence: {
    maintains_home_abroad?: boolean;
  };
  project: {
    target_goal?: 'NATURALIZATION' | 'RESIDENCE_PERMIT' | 'BOTH' | 'SERVICE';
    is_real_and_serious?: boolean;
  };
  driving: {
    status?: 'STUDENT' | 'TOURIST' | 'WORKER_VP' | 'EU_NATIONAL';
    license_country?: 'MAGHREB' | 'ACCORD' | 'NO_ACCORD' | 'USA_CANADA';
    residence_start_date?: string; // MM/YYYY
  };
  rdv_prefecture: {
    prefecture_dept?: string;
    rdv_reason?: 'retrait_titre' | 'premiere_demande_papier' | 'commission_medicale' | 'renouvellement_hors_ligne' | 'naturalisation' | 'echange_permis' | 'renouvellement_anef';
    current_status?: 'anef_group' | 'physique_group';
  };
  rdv_juriste: {
    subject?: 'oqtf_contentieux' | 'refus_recours' | 'conseil_dossier' | 'verification_dossier' | 'autre';
    mode?: 'remote' | 'physical';
  };
  french: {
    goal?: 'NATURALIZATION' | 'RESIDENCE' | 'PROFESSIONAL';
    current_level?: 'A1' | 'A2_B1' | 'B2';
    location_zip?: string;
  };
  civic_exam: {
    civic_goal?: 'RESIDENCE' | 'NATURALIZATION' | 'RETAKE';
    knowledge_level?: 'BEGINNER' | 'INTERMEDIATE' | 'ADVANCED';
    location_zip?: string;
  };
  callback: {
    callback_subject?: 'INFO' | 'BLOCKED' | 'URGENT_LEGAL' | 'FOLLOW_UP';
    callback_urgency?: 'ASAP' | 'TODAY' | 'PLANNED';
    location_zip?: string;
  };
}

export interface RuleCondition {
  var?: string;
  op?: 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE' | 'IN';
  val?: any;
  AND?: RuleCondition[];
  OR?: RuleCondition[];
}

export interface ProcedureRule {
  id: string;
  name: string;
  description: string;
  priority: number;
  source_ref: string;
  conditions: RuleCondition;
  /** Ranking tier: PREMIUM (best), STANDARD, FALLBACK */
  tier?: 'PREMIUM' | 'STANDARD' | 'FALLBACK';
  /** Duration of the titre in years (0 = nationality acquisition) */
  duration_years?: number;
  /** Whether the titre gives the right to work */
  gives_work_right?: boolean;
  /** Whether this path can lead to naturalisation */
  leads_to_naturalisation?: boolean;
  /** Request type: FIRST_OR_RENEWAL (default), UPGRADE (requires existing titre) */
  /** List of document IDs required for this procedure */
  documents?: string[];
  request_type?: 'FIRST_OR_RENEWAL' | 'UPGRADE';
}
