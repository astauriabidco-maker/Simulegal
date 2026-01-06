from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Dict, Any
import json

from core.database import get_db
from core.auth_models import Simulation, User
from api.auth import get_current_user

router = APIRouter(prefix="/api/user", tags=["User Dashboard"])

@router.get("/simulations")
async def get_my_simulations(current_user: User = Depends(get_current_user), db: AsyncSession = Depends(get_db)):
    """Récupère l'historique des simulations de l'utilisateur."""
    result = await db.execute(
        select(Simulation).where(Simulation.user_id == current_user.id).order_by(Simulation.created_at.desc())
    )
    sims = result.scalars().all()
    
    return [
        {
            "id": s.id,
            "date": s.created_at,
            "score": s.result_score,
            "procedure": s.procedure_name,
            "is_paid": s.is_paid,
            "language": s.language
        }
        for s in sims
    ]

@router.get("/progress")
async def get_progress_stats(current_user: User = Depends(get_current_user), db: AsyncSession = Depends(get_db)):
    """Calcule la progression et génère une timeline de régularisation."""
    result = await db.execute(
        select(Simulation).where(Simulation.user_id == current_user.id).order_by(Simulation.created_at.desc())
    )
    last_sim = result.scalars().first()
    
    if not last_sim:
        return {"has_data": False}
    
    data = json.loads(last_sim.data)
    months = data.get("residence_duration_months", 0)
    has_master = data.get("has_master_france", False)
    is_married_french = data.get("spouse_nationality", "").lower() in ["française", "francaise", "french"]
    
    # Seuils standards (en mois)
    VPF_THRESHOLD = 0  # Immédiat si éligible
    RESIDENT_THRESHOLD = 60  # 5 ans
    NAT_STANDARD = 60  # 5 ans
    NAT_REDUCED_MASTER = 24  # 2 ans si Master
    NAT_REDUCED_MARRIAGE = 48  # 4 ans si marié(e) à français(e)
    
    # Déterminer le seuil de naturalisation applicable
    if has_master:
        nat_threshold = NAT_REDUCED_MASTER
        nat_reason = "Master en France"
    elif is_married_french:
        nat_threshold = NAT_REDUCED_MARRIAGE
        nat_reason = "Conjoint(e) français(e)"
    else:
        nat_threshold = NAT_STANDARD
        nat_reason = "Parcours standard"
    
    # Construire la timeline
    timeline = []
    
    # Étape 1 : Situation actuelle
    timeline.append({
        "label": "Aujourd'hui",
        "months_from_now": 0,
        "status": "current",
        "title": "Votre situation actuelle",
        "description": f"{months} mois de résidence en France"
    })
    
    # Étape 2 : Carte de Résident (5 ans)
    months_to_resident = max(0, RESIDENT_THRESHOLD - months)
    years_to_resident = months_to_resident // 12
    remaining_months = months_to_resident % 12
    
    if months_to_resident == 0:
        resident_status = "completed"
        resident_desc = "Critère de durée validé !"
    else:
        resident_status = "pending"
        resident_desc = f"Dans {years_to_resident} an(s) et {remaining_months} mois" if years_to_resident > 0 else f"Dans {remaining_months} mois"
    
    timeline.append({
        "label": f"+{years_to_resident}a{remaining_months}m" if months_to_resident > 0 else "✓",
        "months_from_now": months_to_resident,
        "status": resident_status,
        "title": "Carte de Résident (10 ans)",
        "description": resident_desc
    })
    
    # Étape 3 : Naturalisation
    months_to_nat = max(0, nat_threshold - months)
    years_to_nat = months_to_nat // 12
    remaining_months_nat = months_to_nat % 12
    
    if months_to_nat == 0:
        nat_status = "completed"
        nat_desc = f"Critère de durée validé ! ({nat_reason})"
    else:
        nat_status = "pending"
        nat_desc = f"Dans {years_to_nat} an(s) et {remaining_months_nat} mois ({nat_reason})" if years_to_nat > 0 else f"Dans {remaining_months_nat} mois ({nat_reason})"
    
    timeline.append({
        "label": f"+{years_to_nat}a{remaining_months_nat}m" if months_to_nat > 0 else "✓",
        "months_from_now": months_to_nat,
        "status": nat_status,
        "title": "Naturalisation (Citoyenneté)",
        "description": nat_desc
    })
    
    return {
        "has_data": True,
        "current_months": months,
        "timeline": timeline
    }
