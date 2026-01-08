"""
Application principale FastAPI pour Simulegal.
API REST pour l'évaluation d'éligibilité aux titres de séjour.
"""
import sys
import os
import json
import io
from typing import List, Optional
from dataclasses import fields

import sentry_sdk
from sentry_sdk.integrations.fastapi import FastApiIntegration
from fastapi import FastAPI, HTTPException, Depends, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.schemas import (
    UserSituationInput, EvaluationResponse, EligibilityResult,
    ProcedureSchema, ProcedureCreateUpdate
)
import api.auth as auth
import api.payment as payment
import api.chat as chat
import api.user as user
import api.notifications_api as notifications_api
import api.sharing_api as sharing_api
import api.admin_config as admin_config
import api.services_api as services_api
import api.case_management as case_management
import api.agency_api as agency_api
from core.engine import EligibilityEngine
from core.models import UserSituation
from core.logger import audit_logger
from core.database import get_db, init_db
from core.auth_models import Simulation, User
from core.reports import SimulegalReport

# Monitoring Sentry
SENTRY_DSN = os.getenv("SENTRY_DSN")
if SENTRY_DSN:
    sentry_sdk.init(
        dsn=SENTRY_DSN,
        integrations=[FastApiIntegration()],
        traces_sample_rate=1.0,
    )

# Chemin vers les données
DATA_PATH = os.path.join(os.path.dirname(os.path.dirname(__file__)), "data", "eligibility_criteria.json")

app = FastAPI(
    title="Simulegal API",
    description="API pour l'évaluation d'éligibilité aux titres de séjour en France",
    version="1.0.0"
)

# Initialisation de la DB au démarrage
@app.on_event("startup")
async def on_startup():
    await init_db()

# Configuration CORS pour le frontend
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

app.include_router(auth.router)
app.include_router(payment.router)
app.include_router(chat.router)
app.include_router(user.router)
app.include_router(notifications_api.router)
app.include_router(sharing_api.router)
app.include_router(admin_config.router)
app.include_router(services_api.router)
app.include_router(services_api.router)
app.include_router(case_management.router)
app.include_router(agency_api.router)


# Initialisation du moteur
engine = EligibilityEngine(DATA_PATH)


# === ENDPOINTS PUBLICS ===

# === FRONTEND SERVING ===

# Montage des fichiers statiques (CSS, JS, Images)
# Le frontend est dans le dossier 'frontend' à la racine du projet
FRONTEND_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "frontend")
app.mount("/static", StaticFiles(directory=FRONTEND_DIR), name="static")

@app.get("/")
async def root():
    """Sert la page d'accueil (Landing)."""
    return FileResponse(os.path.join(FRONTEND_DIR, "index.html"))

@app.get("/login")
async def login_page():
    """Sert la page de connexion."""
    return FileResponse(os.path.join(FRONTEND_DIR, "login.html"))

@app.get("/app")
async def dashboard_page():
    """Sert le tableau de bord (nécessite auth côté client)."""
    return FileResponse(os.path.join(FRONTEND_DIR, "dashboard.html"))

@app.get("/admin")
async def admin_page():
    """Sert le tableau de bord admin."""
    return FileResponse(os.path.join(FRONTEND_DIR, "admin.html"))

@app.get("/agent")
async def agent_page():
    """Sert le portail Agent/Franchise."""
    return FileResponse(os.path.join(FRONTEND_DIR, "agent.html"))

@app.get("/kiosk")
async def kiosk_page():
    """Sert l'interface Borne Tactile (Kiosk)."""
    return FileResponse(os.path.join(FRONTEND_DIR, "kiosk.html"))

@app.get("/chat")
async def chat_page():
    """Sert l'interface de chat."""
    return FileResponse(os.path.join(FRONTEND_DIR, "chat.html"))



@app.post("/api/evaluate", response_model=EvaluationResponse)
async def evaluate_eligibility(user_input: UserSituationInput, db: AsyncSession = Depends(get_db), current_user: Optional[User] = Depends(auth.get_current_user_optional)):
    """
    Évalue l'éligibilité d'un utilisateur à toutes les procédures.
    Sauvegarde la simulation si l'évaluation est valide.
    """
    user_kwargs = user_input.model_dump()
    # Filter to only include fields defined in UserSituation dataclass
    valid_fields = {f.name for f in fields(UserSituation)}
    user_kwargs = {k: v for k, v in user_kwargs.items() if k in valid_fields}
    user = UserSituation(**user_kwargs)
    
    # Exécution du moteur
    results = engine.evaluate_eligibility(user)
    
    # Formatage résultats
    eligibility_results = [
        EligibilityResult(
            procedure_id=r["procedure_id"],
            name=r["name"],
            score=r["score"],
            missing_criteria=r["missing_criteria"],
            applied_exception=r["applied_exception"],
            documents=r["documents"]
        )
        for r in results
    ]
    
    top_match = eligibility_results[0] if eligibility_results and eligibility_results[0].score > 0 else None
    
    # Sauvegarde Simulation
    simulation = Simulation(
        user_id=current_user.id if current_user else None,
        data=user_input.model_dump_json(),
        result_score=top_match.score if top_match else 0,
        procedure_name=top_match.name if top_match else "Inconnue",
        language=user_input.language
    )
    db.add(simulation)
    await db.commit()
    await db.refresh(simulation)
    
    # Monetization Logic: Si non payé, on masque les détails cruciaux
    if not simulation.is_paid:
        for res in eligibility_results:
            if res.score > 0:
                res.documents = ["Documents masqués (Version Gratuite)"]
                res.missing_criteria = ["Critères détaillés masqués"]
                if res.applied_exception:
                    res.applied_exception = "Exception masquée"

    return EvaluationResponse(
        simulation_id=simulation.id,
        results=eligibility_results,
        top_match=top_match
    )


@app.get("/api/simulation/{simulation_id}/results", response_model=EvaluationResponse)
async def get_simulation_results(simulation_id: int, db: AsyncSession = Depends(get_db)):
    """Récupère les résultats d'éligibilité pour une simulation existante."""
    result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation introuvable")
    
    user_data = json.loads(simulation.data)
    user_situation = UserSituation(**user_data)
    
    results = engine.evaluate_eligibility(user_situation)

    eligibility_results = [
        EligibilityResult(
            procedure_id=r["procedure_id"],
            name=r["name"],
            score=r["score"],
            missing_criteria=r["missing_criteria"],
            applied_exception=r["applied_exception"],
            documents=r["documents"]
        )
        for r in results
    ]
    
    top_match = eligibility_results[0] if eligibility_results and eligibility_results[0].score > 0 else None

    if not simulation.is_paid:
        for res in eligibility_results:
            if res.score > 0:
                res.documents = ["Documents masqués (Version Gratuite)"]
                res.missing_criteria = ["Critères détaillés masqués"]
                if res.applied_exception:
                    res.applied_exception = "Exception masquée"
    
    return EvaluationResponse(
        simulation_id=simulation.id,
        results=eligibility_results,
        top_match=top_match
    )


@app.get("/api/reports/download/{simulation_id}")
async def download_report(simulation_id: int, db: AsyncSession = Depends(get_db)):
    """Génère et télécharge le rapport PDF d'une simulation."""
    result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation introuvable")
    
    if not simulation.is_paid:
        raise HTTPException(status_code=403, detail="Paiement requis pour télécharger le rapport.")

    user_data = json.loads(simulation.data)
    user_situation = UserSituation(**user_data)
    
    results = engine.evaluate_eligibility(user_situation)
    top_match = next((r for r in results if r['score'] > 0), None)
    
    pdf = SimulegalReport(user_data, top_match, results, lang=simulation.language)
    pdf_bytes = pdf.get_pdf_bytes()
    
    headers = {
        'Content-Disposition': f'attachment; filename="rapport_simulegal_{simulation_id}.pdf"'
    }
    return Response(content=pdf_bytes, media_type='application/pdf', headers=headers)


@app.get("/api/procedures", response_model=List[ProcedureSchema])
async def list_procedures():
    """Liste toutes les procédures disponibles."""
    with open(DATA_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)
    return data["procedures"]


@app.get("/api/procedures/{procedure_id}", response_model=ProcedureSchema)
async def get_procedure(procedure_id: str):
    """Récupère les détails d'une procédure spécifique."""
    with open(DATA_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    for proc in data["procedures"]:
        if proc["id"] == procedure_id:
            return proc
    
    raise HTTPException(status_code=404, detail="Procédure non trouvée")


# === ENDPOINTS ADMIN ===

@app.get("/api/admin/simulations")
async def list_all_simulations(admin: User = Depends(auth.get_current_admin), db: AsyncSession = Depends(get_db)):
    """Liste toutes les simulations utilisateurs (Vue Juriste)."""
    result = await db.execute(
        select(Simulation).order_by(Simulation.created_at.desc())
    )
    sims = result.scalars().all()
    return sims


@app.get("/api/admin/stats")
async def get_admin_stats(admin: User = Depends(auth.get_current_admin), db: AsyncSession = Depends(get_db)):
    """Statistiques avancées pour le dashboard admin."""
    from sqlalchemy import func
    from datetime import datetime, timedelta
    
    # Total simulations
    total_sims = await db.scalar(select(func.count(Simulation.id))) or 0
    
    # Simulations payées
    paid_sims = await db.scalar(select(func.count(Simulation.id)).where(Simulation.is_paid == True)) or 0
    
    # Utilisateurs total
    total_users = await db.scalar(select(func.count(User.id))) or 0
    
    # Revenus estimés (prix unitaire * payés)
    unit_price = float(os.getenv("REPORT_PRICE", "9.99"))
    revenue = paid_sims * unit_price
    
    # Simulations cette semaine
    week_ago = datetime.utcnow() - timedelta(days=7)
    this_week = await db.scalar(
        select(func.count(Simulation.id)).where(Simulation.created_at >= week_ago)
    ) or 0
    
    # Top procédures
    top_procedures_result = await db.execute(
        select(Simulation.procedure_name, func.count(Simulation.id).label('count'))
        .where(Simulation.procedure_name.isnot(None))
        .group_by(Simulation.procedure_name)
        .order_by(func.count(Simulation.id).desc())
        .limit(5)
    )
    top_procedures = [{"name": r[0] or "Inconnue", "count": r[1]} for r in top_procedures_result.all()]
    
    # Simulations par jour (7 derniers jours)
    daily_stats = []
    for i in range(7):
        day = datetime.utcnow() - timedelta(days=i)
        start = day.replace(hour=0, minute=0, second=0, microsecond=0)
        end = day.replace(hour=23, minute=59, second=59, microsecond=999999)
        count = await db.scalar(
            select(func.count(Simulation.id))
            .where(Simulation.created_at.between(start, end))
        ) or 0
        daily_stats.append({
            "date": day.strftime("%Y-%m-%d"),
            "label": day.strftime("%d/%m"),
            "count": count
        })
    
    # Score moyen
    avg_score = await db.scalar(
        select(func.avg(Simulation.result_score)).where(Simulation.result_score > 0)
    ) or 0
    
    return {
        "total_simulations": total_sims,
        "paid_simulations": paid_sims,
        "total_users": total_users,
        "conversion_rate": round((paid_sims / total_sims * 100), 1) if total_sims > 0 else 0,
        "revenue": round(revenue, 2),
        "this_week": this_week,
        "avg_score": round(avg_score, 1),
        "top_procedures": top_procedures,
        "daily_stats": daily_stats[::-1]  # Ordre chronologique
    }


@app.get("/api/admin/users")
async def get_admin_users(admin: User = Depends(auth.get_current_admin), db: AsyncSession = Depends(get_db)):
    """Liste des utilisateurs pour l'admin."""
    from sqlalchemy import func
    from sqlalchemy.orm import selectinload
    
    # Récupérer les utilisateurs avec le nombre de simulations
    result = await db.execute(
        select(User)
        .options(selectinload(User.simulations))
        .order_by(User.created_at.desc())
        .limit(100)
    )
    users = result.scalars().all()
    
    return [{
        "id": u.id,
        "email": u.email,
        "role": u.role,
        "full_name": u.full_name or "-",
        "created_at": u.created_at.isoformat() if u.created_at else None,
        "simulations_count": len(u.simulations) if u.simulations else 0
    } for u in users]



@app.post("/api/admin/procedures", response_model=ProcedureSchema)
async def create_procedure(procedure: ProcedureCreateUpdate, admin: User = Depends(auth.get_current_admin)):
    """Crée une nouvelle procédure. (Admin seulement)"""
    with open(DATA_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    new_id = procedure.name.lower().replace(" ", "_").replace("-", "_")[:30]
    
    new_proc = {
        "id": new_id,
        "name": procedure.name,
        "description": procedure.description,
        "base_criteria": [c.model_dump() for c in procedure.base_criteria],
        "documents": procedure.documents,
        "exceptions": [e.model_dump() for e in procedure.exceptions]
    }
    
    data["procedures"].append(new_proc)
    
    with open(DATA_PATH, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    
    # Recharger le moteur
    global engine
    engine = EligibilityEngine(DATA_PATH)
    
    audit_logger.info("Procedure created", extra={"audit": {"admin_email": admin.email, "procedure_id": new_id}})
    return new_proc


@app.put("/api/admin/procedures/{procedure_id}", response_model=ProcedureSchema)
async def update_procedure(procedure_id: str, procedure: ProcedureCreateUpdate, admin: User = Depends(auth.get_current_admin)):
    """Met à jour une procédure existante. (Admin seulement)"""
    with open(DATA_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    for i, proc in enumerate(data["procedures"]):
        if proc["id"] == procedure_id:
            data["procedures"][i] = {
                "id": procedure_id,
                "name": procedure.name,
                "description": procedure.description,
                "base_criteria": [c.model_dump() for c in procedure.base_criteria],
                "documents": procedure.documents,
                "exceptions": [e.model_dump() for e in procedure.exceptions]
            }
            
            with open(DATA_PATH, 'w', encoding='utf-8') as f:
                json.dump(data, f, ensure_ascii=False, indent=2)
            
            # Recharger le moteur
            global engine
            engine = EligibilityEngine(DATA_PATH)
            
            audit_logger.info("Procedure updated", extra={"audit": {"admin_email": admin.email, "procedure_id": procedure_id}})
            return data["procedures"][i]
    
    raise HTTPException(status_code=404, detail="Procédure non trouvée")


@app.delete("/api/admin/procedures/{procedure_id}")
async def delete_procedure(procedure_id: str, admin: User = Depends(auth.get_current_admin)):
    """Supprime une procédure. (Admin seulement)"""
    with open(DATA_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    original_len = len(data["procedures"])
    data["procedures"] = [p for p in data["procedures"] if p["id"] != procedure_id]
    
    if len(data["procedures"]) == original_len:
        raise HTTPException(status_code=404, detail="Procédure non trouvée")
    
    with open(DATA_PATH, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    
    # Recharger le moteur
    global engine
    engine = EligibilityEngine(DATA_PATH)
    
    audit_logger.info("Procedure deleted", extra={"audit": {"admin_email": admin.email, "procedure_id": procedure_id}})
    return {"message": f"Procédure {procedure_id} supprimée"}


# Servir les fichiers statiques du frontend
frontend_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "frontend")
if os.path.exists(frontend_path):
    app.mount("/static", StaticFiles(directory=frontend_path), name="static")


@app.get("/app")
async def serve_app():
    """Sert l'application frontend."""
    return FileResponse(os.path.join(frontend_path, "index.html"))


@app.get("/admin")
async def serve_admin():
    """Sert le dashboard admin."""
    return FileResponse(os.path.join(frontend_path, "admin.html"))


@app.get("/login")
async def serve_login():
    """Sert la page de connexion."""
    return FileResponse(os.path.join(frontend_path, "login.html"))


@app.get("/chat")
async def serve_chat():
    """Sert l'interface de chatbot IA."""
    return FileResponse(os.path.join(frontend_path, "chat.html"))


@app.get("/dashboard")
async def serve_dashboard():
    """Sert l'espace utilisateur."""
    return FileResponse(os.path.join(frontend_path, "dashboard.html"))
