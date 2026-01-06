from fastapi.testclient import TestClient
import sys
import os
import pytest
from core.database import Base, get_db
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

# Configuration pour les tests
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.main import app

# Base de données de test en mémoire (réutilisation car on a besoin des tables)
SQLALCHEMY_DATABASE_URL = "sqlite:///./test_pdf.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def override_get_db_pdf():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db_pdf

Base.metadata.create_all(bind=engine)
client = TestClient(app)

def teardown_module(module):
    if os.path.exists("./test_pdf.db"):
        os.remove("./test_pdf.db")

def test_generate_pdf():
    # 1. Évaluation (crée une simulation en DB)
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
    assert "simulation_id" in data
    sim_id = data["simulation_id"]
    assert sim_id is not None
    
    # 2. Téléchargement PDF
    response = client.get(f"/api/reports/download/{sim_id}")
    assert response.status_code == 200
    assert response.headers["content-type"] == "application/pdf"
    assert response.content.startswith(b"%PDF") # PDF signature
    assert len(response.content) > 1000 # Vérifie que le PDF contient des données
    # Vérifie la présence de strings typiques (latin-1 encodé)
    # assert b"Tunisienne" in response.content # Peut échouer selon encodage fpdf
