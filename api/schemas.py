"""
Schémas Pydantic pour l'API Simulegal.
Validation des données entrantes et sortantes.
"""
from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional

# === Schémas pour l'évaluation ===

class UserSituationInput(BaseModel):
    """Données de situation utilisateur pour évaluation."""
    nationality: str = Field(..., description="Nationalité de l'utilisateur")
    residence_duration_months: int = Field(..., ge=0, description="Durée de résidence en France (mois)")
    marriage_duration_months: Optional[int] = Field(None, ge=0, description="Durée de mariage (mois)")
    has_master_france: bool = Field(False, description="Diplôme Master en France")
    community_of_life: bool = Field(False, description="Communauté de vie effective")
    spouse_nationality: str = Field("", description="Nationalité du conjoint")
    is_spouse_eu_citizen: bool = Field(False, description="Conjoint citoyen UE")
    salary_annual: float = Field(0.0, ge=0, description="Salaire annuel brut")
    no_polygamy: bool = Field(True, description="Absence de polygamie")
    french_level: str = Field("A0", description="Niveau de français (A0-C2)")
    is_adult: bool = Field(True, description="Est majeur")
    stable_resources: bool = Field(False, description="Ressources stables")
    has_health_insurance: bool = Field(False, description="Assurance maladie")
    no_conviction: bool = Field(True, description="Pas de condamnation")
    adheres_republic_values: bool = Field(True, description="Adhésion aux valeurs de la République")
    # Champs additionnels pour procédures spécifiques
    parent_of_french_child: bool = Field(False)
    is_pacs_french: bool = Field(False)
    has_job_auth: bool = Field(False)
    has_vls_ts: bool = Field(False)
    has_degree_3_years: bool = Field(False)
    contract_duration_months: int = Field(0, ge=0)
    graduated_french_higher_ed: bool = Field(False)
    is_refugee_or_stateless: bool = Field(False)
    # Metadata
    language: Optional[str] = "fr"
    # Phase 6
    has_refugee_status: bool = Field(False)
    has_international_reputation: bool = Field(False)
    has_project_in_france: bool = Field(False)
    has_residence_card: bool = Field(False)
    has_long_stay_visa: bool = Field(False)
    is_married_french: bool = Field(False)
    entry_legal: bool = Field(True)
    regular_entry: bool = Field(True)
    goal: str = Field("both", description="Objectif principal (residence | nationality | both)")
    regular_situation: bool = Field(True, description="Situation administrative régulière")
    # P1: Administrative / Special
    no_oqtf: bool = Field(True)
    has_health_insurance: bool = Field(False)
    contribution_education: bool = Field(False)
    # P1: Special Status
    is_refugee_or_stateless: bool = Field(False)
    humanitarian_reasons: bool = Field(False)
    protection_order: bool = Field(False)
    filed_complaint: bool = Field(False)
    no_contact_accused: bool = Field(True)
    # P1: Qualified Work
    investment_amount: float = Field(0.0)
    seniority_months: int = Field(0)
    contract_duration_months: int = Field(0)
    recruited_innovative_company: bool = Field(False)
    recruited_innovative_company: bool = Field(False)
    research_activity: bool = Field(False)
    # P2: Life Path & Education in France
    born_in_france: bool = Field(False)
    schooling_france: bool = Field(False)
    residence_since_6: bool = Field(False)
    residence_at_18: bool = Field(False)
    entered_minor_regroupement: bool = Field(False)
    # P2: Extended Family
    sibling_of_french: bool = Field(False)
    ascendant_of_french: bool = Field(False)
    adopted_by_french: bool = Field(False)
    recueilli_by_french: bool = Field(False)
    residence_with_french_years: int = Field(0)
    lives_with_adoptive_parent: bool = Field(False)
    # P3: Work & Talent Niche
    is_artist: bool = Field(False)
    is_legal_representative: bool = Field(False)
    is_manager_expert: bool = Field(False)
    activity_in_france: bool = Field(False)
    international_reputation: bool = Field(False)
    meets_remuneration_threshold: bool = Field(False)
    job_rd_related: bool = Field(False)
    has_degree_higher_ed: bool = Field(False)
    has_master_or_5_years_exp: bool = Field(False)
    has_hosting_agreement: bool = Field(False)
    business_project_real: bool = Field(False)
    innovative_project_recognized: bool = Field(False)
    creates_jobs_4_years: bool = Field(False)
    has_contract_france: bool = Field(False)
    seasonal_contract: bool = Field(False)
    activity_duration_years: int = Field(0)
    employment_24_months_tension: bool = Field(False)
    registered_business: bool = Field(False)
    # P3: Family, Health & Social
    ascendant_french_child: bool = Field(False)
    dependent_on_child: bool = Field(False)
    child_needs_care: bool = Field(False)
    no_care_origin_country: bool = Field(False)
    parent_responsible: bool = Field(False)
    no_sufficient_resources: bool = Field(False)
    has_french_pension: bool = Field(False)
    receives_pension_accident: bool = Field(False)
    incapacity_rate: int = Field(0)
    hosted_community_org: bool = Field(False)
    volunteer_mission: bool = Field(False)
    org_public_utility: bool = Field(False)
    # P3: Migration Path & Admin
    is_veteran: bool = Field(False)
    has_combatant_card: bool = Field(False)
    exceptional_services: bool = Field(False)
    lost_french_nationality: bool = Field(False)
    bilateral_agreement: bool = Field(False)
    had_resident_card: bool = Field(False)
    had_student_card: bool = Field(False)
    had_researcher_card: bool = Field(False)
    had_temp_card: bool = Field(False)
    completed_research: bool = Field(False)
    cir_compliance: bool = Field(True)
    no_absence_3_years: bool = Field(True)
    commitment_leave: bool = Field(False)
    commitment_no_work: bool = Field(False)
    no_threat_public_order: bool = Field(True)
    residence_5_years_since_11: bool = Field(False)
    born_france_or_entered_before_13: bool = Field(False)
    entered_regroupement_familial: bool = Field(False)
    legal_residence_other_eu: bool = Field(False)
    spouse_long_stay_eu: bool = Field(False)
    main_residence_outside: bool = Field(False)
    residence_outside_france: bool = Field(False)
    has_long_stay_visa_visitor: bool = Field(False)
    has_long_stay_visa_student: bool = Field(False)
    has_vls_ts_ict: bool = Field(False)
    has_vls_ts_visitor: bool = Field(False)
    enrolled_higher_ed: bool = Field(False)
    # Financials
    monthly_income: float = Field(0.0)
    monthly_resources: float = Field(0.0)
    annual_resources: float = Field(0.0)
    family_of_talent: bool = Field(False)

class EligibilityResult(BaseModel):
    """Résultat d'éligibilité pour une procédure."""
    procedure_id: str
    name: str
    score: float
    missing_criteria: List[str]
    applied_exception: Optional[str]
    documents: List[str]

class EvaluationResponse(BaseModel):
    """Réponse complète d'évaluation."""
    simulation_id: Optional[int] = None
    results: List[EligibilityResult]
    top_match: Optional[EligibilityResult] = None

# === Schémas pour les procédures (CRUD Admin) ===

class CriterionSchema(BaseModel):
    """Schéma d'un critère."""
    id: str
    label: str
    type: str  # 'bool', 'int', 'float'
    threshold: Optional[float] = None
    expected: Optional[Any] = None

class ExceptionSchema(BaseModel):
    """Schéma d'une exception."""
    id: str
    condition: Dict[str, Any]
    modified_criteria: Dict[str, Any] = {}
    description: Optional[str] = None

class ProcedureSchema(BaseModel):
    """Schéma complet d'une procédure."""
    id: str
    name: str
    description: str
    base_criteria: List[CriterionSchema]
    documents: List[str]
    exceptions: List[ExceptionSchema] = []

class ProcedureCreateUpdate(BaseModel):
    """Schéma pour créer/modifier une procédure."""
    name: str
    description: str
    base_criteria: List[CriterionSchema]
    documents: List[str]
    exceptions: List[ExceptionSchema] = []
