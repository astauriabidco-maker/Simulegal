import pytest
import os
from core.engine import EligibilityEngine
from core.models import UserSituation

@pytest.fixture
def engine():
    return EligibilityEngine("data/eligibility_criteria.json")

def test_cas_1_tunisienne_mariee(engine):
    """Cas 1 : Tunisienne mariée à un Français depuis 18 mois -> 100% (Exception accord)"""
    user = UserSituation(
        nationality="Tunisienne",
        residence_duration_months=18,
        marriage_duration_months=18,
        community_of_life=True,
        spouse_nationality="Française",
        french_level="A2",  # Nouveau critère requis
        no_polygamy=True,
        no_threat_public_order=True
    )
    results = engine.evaluate_eligibility(user)
    res = next(r for r in results if r['procedure_id'] == 'resident_france_spouse')
    assert res['score'] == 100
    assert res['applied_exception'] == 'tunisian_accord_spouse'

def test_cas_2_algerien_master(engine):
    """Cas 2 : Algérien diplômé master en France depuis 2 ans -> Passeport Talent Salarié Qualifié"""
    user = UserSituation(
        nationality="Algérienne",
        residence_duration_months=24,
        has_master_france=True,
        job_contract_related=True,
        salary_annual=45000
    )
    results = engine.evaluate_eligibility(user)
    # Le nouveau ID est passeport_talent_qualified (pas passeport_talent_scholar)
    res = next(r for r in results if r['procedure_id'] == 'passeport_talent_qualified')
    assert res['score'] == 100

def test_cas_3_conjoint_ue(engine):
    """Cas 3 : Conjoint d'UE résidant 5 ans en France -> Éligible carte résident longue durée UE"""
    user = UserSituation(
        nationality="Brésilienne",
        residence_duration_months=60,  # 5 ans
        is_spouse_eu_citizen=True,
        community_of_life=True,
        has_health_insurance=True,
        stable_resources=True,
        french_level="A2"
    )
    results = engine.evaluate_eligibility(user)
    # Le nouveau ID est resident_long_stay_eu (pas eu_citizen_family)
    res = next(r for r in results if r['procedure_id'] == 'resident_long_stay_eu')
    assert res['score'] == 100

def test_failure_insufficient_marriage(engine):
    """Test échec : Mariage trop court hors exception (non Tunisien)"""
    user = UserSituation(
        nationality="Américaine",
        residence_duration_months=20,
        marriage_duration_months=20,
        community_of_life=True,
        spouse_nationality="Française",
        french_level="A2",
        no_polygamy=True
    )
    results = engine.evaluate_eligibility(user)
    res = next(r for r in results if r['procedure_id'] == 'resident_france_spouse')
    assert res['score'] < 100
    assert any("Durée de mariage" in c for c in res['missing_criteria'])

def test_naturalisation_par_decret(engine):
    """Test Naturalisation par décret : 5 ans de résidence, niveau B1"""
    user = UserSituation(
        nationality="Marocaine",
        residence_duration_months=72,  # 6 ans
        is_adult=True,
        stable_resources=True,
        french_level="B1",
        adheres_republic_values=True,
        no_conviction=True
    )
    results = engine.evaluate_eligibility(user)
    res = next(r for r in results if r['procedure_id'] == 'naturalization_decree')
    assert res['score'] == 100

def test_naturalisation_diplome_reduit(engine):
    """Test Naturalisation avec durée réduite pour diplômé enseignement supérieur français"""
    user = UserSituation(
        nationality="Sénégalaise",
        residence_duration_months=30,  # 2.5 ans (< 5 ans standard)
        is_adult=True,
        stable_resources=True,
        french_level="B1",
        adheres_republic_values=True,
        no_conviction=True,
        graduated_french_higher_ed=True  # Exception : durée réduite à 2 ans
    )
    results = engine.evaluate_eligibility(user)
    res = next(r for r in results if r['procedure_id'] == 'naturalization_decree')
    # Avec l'exception, le seuil devrait être 2 ans (24 mois), donc 30 mois suffisent
    assert res['score'] == 100
    assert res['applied_exception'] == 'french_higher_ed_graduate'

def test_passeport_talent_blue_card(engine):
    """Test Carte Bleue Européenne"""
    user = UserSituation(
        nationality="Indienne",
        residence_duration_months=12,
        has_degree_3_years=True,
        contract_duration_months=18,
        salary_annual=55000
    )
    results = engine.evaluate_eligibility(user)
    res = next(r for r in results if r['procedure_id'] == 'passeport_talent_blue_card')
    assert res['score'] == 100
