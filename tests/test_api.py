from fastapi.testclient import TestClient
import sys
import os
import pytest

# Ajouter le répertoire racine au PYTHONPATH
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.main import app

client = TestClient(app)

def test_read_main():
    response = client.get("/")
    assert response.status_code == 200
    assert response.json() == {"message": "Bienvenue sur l'API Simulegal", "docs": "/docs"}

def test_list_procedures():
    response = client.get("/api/procedures")
    assert response.status_code == 200
    procedures = response.json()
    assert len(procedures) > 0
    assert "id" in procedures[0]
    assert "name" in procedures[0]

def test_evaluate_eligibility():
    # Cas test : Tunisienne mariée à un Français (doit avoir 100%)
    payload = {
        "nationality": "Tunisienne",
        "residence_duration_months": 24,
        "french_level": "A2",
        "salary_annual": 0,
        "marriage_duration_months": 18,
        "spouse_nationality": "Française",
        "community_of_life": True,
        "no_polygamy": True,
        "is_adult": True
    }
    response = client.post("/api/evaluate", json=payload)
    assert response.status_code == 200
    data = response.json()
    assert "results" in data
    
    # Vérifier l'éligibilité au titre conjoint de français
    res = next((r for r in data["results"] if r["procedure_id"] == "resident_france_spouse"), None)
    assert res is not None
    assert res["score"] == 100
    assert res["applied_exception"] == "tunisian_accord_spouse"

def test_evaluate_eligibility_failure():
    # Cas test : Touriste sans critères (doit échouer)
    payload = {
        "nationality": "Américaine",
        "residence_duration_months": 1,
        "french_level": "A0",
        "salary_annual": 0,
        "is_adult": True
    }
    response = client.post("/api/evaluate", json=payload)
    assert response.status_code == 200
    data = response.json()
    
    # Vérifier qu'aucun score n'est à 100% ou qu'il y a des critères manquants
    top_match = data.get("top_match")
    if top_match:
        assert top_match["score"] < 100

def test_admin_create_delete_procedure():
    # 1. Créer une procédure
    new_proc = {
        "name": "Procédure Test API",
        "description": "Description test",
        "base_criteria": [
            {"id": "test_crit", "label": "Test critère", "type": "bool", "expected": True}
        ],
        "documents": ["Doc 1"],
        "exceptions": []
    }
    response = client.post("/api/admin/procedures", json=new_proc)
    assert response.status_code == 200
    created_proc = response.json()
    proc_id = created_proc["id"]
    assert created_proc["name"] == "Procédure Test API"

    # 2. Vérifier qu'elle existe
    response = client.get(f"/api/procedures/{proc_id}")
    assert response.status_code == 200
    
    # 3. Supprimer la procédure
    response = client.delete(f"/api/admin/procedures/{proc_id}")
    assert response.status_code == 200
    
    # 4. Vérifier qu'elle n'existe plus
    response = client.get(f"/api/procedures/{proc_id}")
    assert response.status_code == 404
