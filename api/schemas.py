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
