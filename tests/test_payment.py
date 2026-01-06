from fastapi.testclient import TestClient
import sys
import os
import pytest
from core.database import Base, get_db
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from api.payment import verify_session # Importer pour simuler

# Configuration pour les tests
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.main import app

# Base de données de test en mémoire
SQLALCHEMY_DATABASE_URL = "sqlite:///./test_payment.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def override_get_db_payment():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db_payment

Base.metadata.create_all(bind=engine)
client = TestClient(app)

def teardown_module(module):
    if os.path.exists("./test_payment.db"):
        os.remove("./test_payment.db")

def test_freemium_flow():
    """Valide le flow complet Freemium : Gratuit masqué -> Paiement -> Débloqué."""
    
    # 1. Évaluation Gratuite
    payload = {
        "nationality": "Tunisienne",
        "residence_duration_months": 24,
        "french_level": "B2",
        "salary_annual": 30000,
        "is_adult": True
    }
    response = client.post("/api/evaluate", json=payload)
    assert response.status_code == 200
    data = response.json()
    sim_id = data["simulation_id"]
    
    # Vérifier que c'est VERROUILLÉ
    assert "masqués" in data["results"][0]["documents"][0]
    
    # 2. Vérifier accès PDF refusé
    pdf_res = client.get(f"/api/reports/download/{sim_id}")
    assert pdf_res.status_code == 403
    
    # 3. Créer lien de paiement (Mock)
    pay_res = client.post(f"/api/payment/create-checkout/{sim_id}")
    assert pay_res.status_code == 200
    assert "checkout_url" in pay_res.json()
    
    # 4. Simuler le succès du paiement (via l'endpoint de verify avec mock activé)
    # L'endpoint verify simule le succès si 'sk_test' est présent dans la clé API
    # Appelons directement l'endpoint verify avec un faux session_id
    verify_res = client.get(f"/api/payment/verify-session/cs_test_123?simulation_id={sim_id}")
    assert verify_res.status_code == 200
    assert verify_res.json()["status"] == "paid (mock)"
    
    # 5. Vérifier accès PDF DÉBLOQUÉ
    pdf_res_unlocked = client.get(f"/api/reports/download/{sim_id}")
    assert pdf_res_unlocked.status_code == 200
    assert pdf_res_unlocked.headers["content-type"] == "application/pdf"
