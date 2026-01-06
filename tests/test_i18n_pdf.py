import sys
import os
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from core.database import Base, get_db
from core.auth_models import Simulation
from core.reports import SimulegalReport
from api.main import app
from fastapi.testclient import TestClient
import json

# Setup
SQLALCHEMY_DATABASE_URL = "sqlite:///./test_i18n.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def override_get_db_i18n():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db_i18n
Base.metadata.create_all(bind=engine)
client = TestClient(app)

def test_pdf_arabic_generation():
    # 1. Créer une simulation payée en Arabe
    db = TestingSessionLocal()
    sim = Simulation(
        data=json.dumps({"nationality": "Tunisienne", "residence_duration_months": 24}),
        is_paid=True,
        language="ar",
        procedure_name="Simulation Test Arabe"
    )
    db.add(sim)
    db.commit()
    db.refresh(sim)
    
    # 2. Demander le PDF
    response = client.get(f"/api/reports/download/{sim.id}")
    assert response.status_code == 200
    assert response.headers["content-type"] == "application/pdf"
    
    # Vérifier que le fichier n'est pas vide
    assert len(response.content) > 1000
    
    print(f"Test PDF Arabe réussi : {len(response.content)} bytes")

if __name__ == "__main__":
    try:
        test_pdf_arabic_generation()
    finally:
        if os.path.exists("./test_i18n.db"):
            os.remove("./test_i18n.db")
