from fastapi.testclient import TestClient
import sys
import os
import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

# Configuration pour les tests
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.main import app
from core.database import Base, get_db
from core.auth_models import User

# Base de données de test en mémoire
SQLALCHEMY_DATABASE_URL = "sqlite:///./test.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def override_get_db():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db

# Créer les tables de test
Base.metadata.create_all(bind=engine)

client = TestClient(app)

def teardown_module(module):
    # Nettoyage après les tests
    if os.path.exists("./test.db"):
        os.remove("./test.db")

def test_auth_flow_and_admin_protection():
    # 1. Inscription (Register) - Deviendra Admin car 1er utilisateur
    register_payload = {
        "email": "admin@simulegal.com",
        "password": "strongpassword123",
        "full_name": "Admin User"
    }
    response = client.post("/api/auth/register", json=register_payload)
    assert response.status_code == 200
    user_data = response.json()
    assert user_data["email"] == "admin@simulegal.com"
    assert user_data["role"] == "admin"

    # 2. Connexion (Login)
    login_payload = {
        "username": "admin@simulegal.com",
        "password": "strongpassword123"
    }
    response = client.post("/api/auth/token", data=login_payload)
    assert response.status_code == 200
    token_data = response.json()
    assert "access_token" in token_data
    token = token_data["access_token"]
    headers = {"Authorization": f"Bearer {token}"}

    # 3. Accès Admin - Créer une procédure (Autorisé)
    new_proc = {
        "name": "Procédure Sécurisée",
        "description": "Test auth",
        "base_criteria": [],
        "documents": [],
        "exceptions": []
    }
    response = client.post("/api/admin/procedures", json=new_proc, headers=headers)
    assert response.status_code == 200
    assert response.json()["name"] == "Procédure Sécurisée"
    proc_id = response.json()["id"]

    # 4. Accès sans token (Non Autorisé)
    response = client.delete(f"/api/admin/procedures/{proc_id}")
    assert response.status_code == 401  # Unauthorized

    # 5. Créer un utilisateur standard
    user_payload = {
        "email": "user@simulegal.com",
        "password": "userpassword123",
        "full_name": "Standard User"
    }
    client.post("/api/auth/register", json=user_payload)
    
    # Login user standard
    response = client.post("/api/auth/token", data={"username": "user@simulegal.com", "password": "userpassword123"})
    user_token = response.json()["access_token"]
    user_headers = {"Authorization": f"Bearer {user_token}"}
    
    # 6. Tentative accès Admin par user standard (Interdit)
    response = client.delete(f"/api/admin/procedures/{proc_id}", headers=user_headers)
    assert response.status_code == 403  # Forbidden

    # 7. Suppression avec token Admin (Autorisé)
    response = client.delete(f"/api/admin/procedures/{proc_id}", headers=headers)
    assert response.status_code == 200
