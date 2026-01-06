from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional, Union

@dataclass
class Criterion:
    id: str
    label: str
    type: str  # 'int', 'bool', 'float', 'string'
    unit: Optional[str] = None
    threshold: Optional[Union[int, float]] = None
    expected: Optional[Any] = None

@dataclass
class ExceptionRule:
    id: str
    condition: Dict[str, Any]
    modified_criteria: Dict[str, Any] = field(default_factory=dict)
    description: Optional[str] = None

@dataclass
class Procedure:
    id: str
    name: str
    description: str
    base_criteria: List[Criterion]
    documents: List[str]
    exceptions: List[ExceptionRule] = field(default_factory=list)

@dataclass
class UserSituation:
    nationality: str
    residence_duration_months: int
    marriage_duration_months: Optional[int] = None
    has_master_france: bool = False
    community_of_life: bool = False
    spouse_nationality: str = ""
    is_spouse_eu_citizen: bool = False
    entry_legal: bool = True
    job_contract_related: bool = False
    salary_annual: float = 0.0
    no_polygamy: bool = True
    french_level: str = "A0"  # A0, A1, A2, B1, B2, C1, C2
    is_adult: bool = True
    stable_resources: bool = False
    parent_of_french_child: bool = False
    is_pacs_french: bool = False
    has_job_auth: bool = False
    has_vls_ts: bool = False
    # Nouveaux attributs Phase 2
    contribution_education: bool = False
    spouse_long_stay_eu: bool = False
    legal_residence_other_eu: bool = False
    has_health_insurance: bool = False
    entered_minor_regroupement: bool = False
    born_france_or_entered_before_13: bool = False
    receives_pension_accident: bool = False
    incapacity_rate: int = 0
    hosted_community_org: bool = False
    activity_duration_years: int = 0
    humanitarian_reasons: bool = False
    protection_order: bool = False
    filed_complaint: bool = False
    no_contact_accused: bool = True
    no_threat_public_order: bool = True
    no_oqtf: bool = True
    registered_business: bool = False
    monthly_income: float = 0.0
    had_temp_card: bool = False
    cir_compliance: bool = False
    recruited_innovative_company: bool = False
    job_rd_related: bool = False
    has_degree_3_years: bool = False
    contract_duration_months: int = 0
    seniority_months: int = 0
    has_contract_france: bool = False
    research_activity: bool = False
    has_hosting_agreement: bool = False
    has_master_or_5_years_exp: bool = False
    business_project_real: bool = False
    investment_amount: float = 0.0
    # Phase 6: Niches
    has_refugee_status: bool = False
    has_international_reputation: bool = False
    has_project_in_france: bool = False
    resources_smic: bool = False
    innovative_project_recognized: bool = False
    creates_jobs_4_years: bool = False
    is_legal_representative: bool = False
    is_artist: bool = False
    meets_remuneration_threshold: bool = False
    international_reputation: bool = False
    activity_in_france: bool = False
    family_of_talent: bool = False
    seasonal_contract: bool = False
    residence_outside_france: bool = False
    has_long_stay_visa: bool = False
    child_needs_care: bool = False
    no_care_origin_country: bool = False
    parent_responsible: bool = False
    volunteer_mission: bool = False
    org_public_utility: bool = False
    commitment_leave: bool = False
    entered_regroupement_familial: bool = False
    is_algerian: bool = False
    not_algerian: bool = True
    had_resident_card: bool = False
    no_absence_3_years: bool = True
    is_veteran: bool = False
    has_combatant_card: bool = False
    ascendant_french_child: bool = False
    dependent_on_child: bool = False
    no_sufficient_resources: bool = False
    had_student_card: bool = False
    had_researcher_card: bool = False
    completed_research: bool = False
    bilateral_agreement: bool = False
    has_degree_higher_ed: bool = False
    has_vls_ts_ict: bool = False
    is_manager_expert: bool = False
    has_vls_ts_visitor: bool = False
    commitment_no_work: bool = False
    annual_resources: float = 0.0
    enrolled_higher_ed: bool = False
    monthly_resources: float = 0.0
    has_long_stay_visa_student: bool = False
    main_residence_outside: bool = False
    has_french_pension: bool = False
    no_conviction: bool = True
    adheres_republic_values: bool = True
    born_in_france: bool = False
    residence_at_18: bool = False
    residence_5_years_since_11: bool = False
    exceptional_services: bool = False
    residence_france: bool = False
    is_refugee_or_stateless: bool = False
    employment_24_months_tension: bool = False
    ascendant_of_french: bool = False
    sibling_of_french: bool = False
    schooling_france: bool = False
    residence_since_6: bool = False
    adopted_by_french: bool = False
    lives_with_adoptive_parent: bool = False
    recueilli_by_french: bool = False
    residence_with_french_years: int = 0
    lost_french_nationality: bool = False
    graduated_french_higher_ed: bool = False

    def get_attr(self, attr_id: str) -> Any:
        # Mapping des critères vers les attributs de l'utilisateur
        mapping = {
            "marriage_duration": self.marriage_duration_months,
            "residence_duration": self.residence_duration_months // 12,
            "residence_duration_years": self.residence_duration_months // 12,
            "community_of_life": self.community_of_life,
            "nationality_spouse": self.spouse_nationality == "Française",
            "is_married_to_french": (self.spouse_nationality == "Française" and self.marriage_duration_months is not None),
            "has_master_france": self.has_master_france,
            "job_contract_related": self.job_contract_related,
            "salary_annual": self.salary_annual,
            "salary_threshold": self.salary_annual,
            "spouse_is_eu_citizen": self.is_spouse_eu_citizen,
            "entry_legal": self.entry_legal,
            "no_polygamy": self.no_polygamy,
            "french_level_a2": self.french_level in ["A2", "B1", "B2", "C1", "C2"],
            "french_level_b1": self.french_level in ["B1", "B2", "C1", "C2"],
            "is_adult": self.is_adult,
            "stable_resources": self.stable_resources,
            "parent_of_french_child": self.parent_of_french_child,
            "is_pacs_french": self.is_pacs_french,
            "common_life_duration": self.marriage_duration_months if self.is_pacs_french else 0,
            "has_job_auth": self.has_job_auth,
            "has_vls_ts": self.has_vls_ts,
            # Nouveaux mappings Phase 2
            "contribution_education": self.contribution_education,
            "spouse_long_stay_eu": self.spouse_long_stay_eu,
            "legal_residence_other_eu": self.legal_residence_other_eu,
            "has_health_insurance": self.has_health_insurance,
            "entered_minor_regroupement": self.entered_minor_regroupement,
            "age_minimum": 18 if self.is_adult else 16,
            "born_france_or_entered_before_13": self.born_france_or_entered_before_13,
            "receives_pension_accident": self.receives_pension_accident,
            "incapacity_rate": self.incapacity_rate,
            "hosted_community_org": self.hosted_community_org,
            "activity_duration_years": self.activity_duration_years,
            "humanitarian_reasons": self.humanitarian_reasons,
            "protection_order": self.protection_order,
            "filed_complaint": self.filed_complaint,
            "no_contact_accused": self.no_contact_accused,
            "no_threat_public_order": self.no_threat_public_order,
            "no_oqtf": self.no_oqtf,
            "registered_business": self.registered_business,
            "monthly_income": self.monthly_income,
            "had_temp_card": self.had_temp_card,
            "cir_compliance": self.cir_compliance,
            "recruited_innovative_company": self.recruited_innovative_company,
            "job_rd_related": self.job_rd_related,
            "has_degree_3_years": self.has_degree_3_years,
            "contract_duration_months": self.contract_duration_months,
            "seniority_months": self.seniority_months,
            "has_contract_france": self.has_contract_france,
            "research_activity": self.research_activity,
            "has_hosting_agreement": self.has_hosting_agreement,
            "has_master_or_5_years_exp": self.has_master_or_5_years_exp or self.has_master_france,
            "business_project_real": self.business_project_real,
            "investment_amount": self.investment_amount,
            "resources_smic": self.resources_smic,
            "innovative_project_recognized": self.innovative_project_recognized,
            "creates_jobs_4_years": self.creates_jobs_4_years,
            "is_legal_representative": self.is_legal_representative,
            "is_artist": self.is_artist,
            "meets_remuneration_threshold": self.meets_remuneration_threshold,
            "international_reputation": self.international_reputation,
            "activity_in_france": self.activity_in_france,
            "family_of_talent": self.family_of_talent,
            "seasonal_contract": self.seasonal_contract,
            "residence_outside_france": self.residence_outside_france,
            "has_long_stay_visa": self.has_long_stay_visa,
            "child_needs_care": self.child_needs_care,
            "no_care_origin_country": self.no_care_origin_country,
            "parent_responsible": self.parent_responsible,
            "volunteer_mission": self.volunteer_mission,
            "org_public_utility": self.org_public_utility,
            "commitment_leave": self.commitment_leave,
            "entered_regroupement_familial": self.entered_regroupement_familial,
            "is_algerian": self.nationality == "Algérienne",
            "not_algerian": self.nationality != "Algérienne",
            "had_resident_card": self.had_resident_card,
            "no_absence_3_years": self.no_absence_3_years,
            "is_veteran": self.is_veteran,
            "has_combatant_card": self.has_combatant_card,
            "ascendant_french_child": self.ascendant_french_child,
            "dependent_on_child": self.dependent_on_child,
            "no_sufficient_resources": self.no_sufficient_resources,
            "had_student_card": self.had_student_card,
            "had_researcher_card": self.had_researcher_card,
            "completed_research": self.completed_research,
            "bilateral_agreement": self.bilateral_agreement,
            "has_degree_higher_ed": self.has_degree_higher_ed,
            "has_vls_ts_ict": self.has_vls_ts_ict,
            "is_manager_expert": self.is_manager_expert,
            "has_vls_ts_visitor": self.has_vls_ts_visitor,
            "commitment_no_work": self.commitment_no_work,
            "annual_resources": self.annual_resources,
            "enrolled_higher_ed": self.enrolled_higher_ed,
            "monthly_resources": self.monthly_resources,
            "has_long_stay_visa_student": self.has_long_stay_visa_student,
            "main_residence_outside": self.main_residence_outside,
            "has_french_pension": self.has_french_pension,
            "no_conviction": self.no_conviction,
            "adheres_republic_values": self.adheres_republic_values,
            "born_in_france": self.born_in_france,
            "residence_at_18": self.residence_at_18,
            "residence_5_years_since_11": self.residence_5_years_since_11,
            "exceptional_services": self.exceptional_services,
            "residence_france": self.residence_france or self.residence_duration_months > 0,
            "is_refugee_or_stateless": self.is_refugee_or_stateless,
            "employment_24_months_tension": self.employment_24_months_tension,
            "ascendant_of_french": self.ascendant_of_french,
            "sibling_of_french": self.sibling_of_french,
            "schooling_france": self.schooling_france,
            "residence_since_6": self.residence_since_6,
            "adopted_by_french": self.adopted_by_french,
            "lives_with_adoptive_parent": self.lives_with_adoptive_parent,
            "recueilli_by_french": self.recueilli_by_french,
            "residence_with_french_years": self.residence_with_french_years,
            "lost_french_nationality": self.lost_french_nationality,
            "graduated_french_higher_ed": self.graduated_french_higher_ed,
            # Phase 6
            "has_refugee_status": self.has_refugee_status,
            "has_international_reputation": self.has_international_reputation,
            "has_project_in_france": self.has_project_in_france,
            "no_threat_public_order": self.no_threat_public_order,  # Ajout explicite
        }
        return mapping.get(attr_id)
