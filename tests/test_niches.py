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

# Base de données de test en mémoire
SQLALCHEMY_DATABASE_URL = "sqlite:///./test_niches.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def override_get_db_niches():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db_niches

Base.metadata.create_all(bind=engine)
client = TestClient(app)

def teardown_module(module):
    if os.path.exists("./test_niches.db"):
        os.remove("./test_niches.db")

def test_refugee_eligibility():
    """Test le cas d'un réfugié."""
    payload = {
        "nationality": "Soudanaise",
        "residence_duration_months": 3,
        "has_refugee_status": True,
        "is_adult": True
    }
    response = client.post("/api/evaluate", json=payload)
    assert response.status_code == 200
    data = response.json()
    top_match = data["top_match"]
    assert top_match is not None
    assert top_match["score"] == 100
    assert "Réfugié" in top_match["name"]

def test_talent_reputation_eligibility():
    """Test le cas d'un talent renommé."""
    payload = {
        "nationality": "Américaine",
        "residence_duration_months": 0,  # Obligatoire même si 0
        "has_international_reputation": True,
        "has_project_in_france": True,
        "stable_resources": True,
        "is_adult": True
    }
    response = client.post("/api/evaluate", json=payload)
    assert response.status_code == 200
    data = response.json()
    top_match = data["top_match"]
    assert top_match is not None
    assert top_match["score"] == 100
    assert "Renommée" in top_match["name"]
    assert "Talent" in top_match["name"]
