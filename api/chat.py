from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from pydantic import BaseModel
from typing import Optional, Dict, Any, List
import json
import os

from core.database import get_db
from core.auth_models import Simulation
from core.chatbot import SimulegalChatbot
from core.engine import EligibilityEngine
from core.models import UserSituation
from dataclasses import fields

router = APIRouter(prefix="/api/chat", tags=["Chat"])

# Chemin vers les données d'éligibilité
DATA_PATH = os.path.join(os.path.dirname(os.path.dirname(__file__)), "data", "eligibility_criteria.json")

class ChatMessage(BaseModel):
    message: str
    simulation_id: Optional[int] = None

class ChatResponse(BaseModel):
    reply: str
    simulation_id: int
    is_finished: bool
    current_data: Dict[str, Any]

class AdviceResponse(BaseModel):
    advice: str
    top_procedure: Optional[Dict[str, Any]] = None
    all_results: List[Dict[str, Any]] = []

@router.post("/message", response_model=ChatResponse)
async def chat_message(chat_input: ChatMessage, db: AsyncSession = Depends(get_db)):
    """Traite un message utilisateur et retourne la réponse du chatbot."""
    
    simulation = None
    initial_data = {}
    
    if chat_input.simulation_id:
        result = await db.execute(select(Simulation).where(Simulation.id == chat_input.simulation_id))
        simulation = result.scalars().first()
        if simulation:
            initial_data = json.loads(simulation.data)
    
    # Initialiser le chatbot avec les données existantes
    bot = SimulegalChatbot(initial_data)
    
    # Traiter le message
    reply, is_finished = bot.process_message(chat_input.message)
    updated_data = bot.get_situation_dict()
    
    # Créer ou mettre à jour la simulation
    if not simulation:
        simulation = Simulation(
            data=json.dumps(updated_data),
            procedure_name="Chat en cours..."
        )
        db.add(simulation)
    else:
        simulation.data = json.dumps(updated_data)
    
    await db.commit()
    await db.refresh(simulation)
    
    return ChatResponse(
        reply=reply,
        simulation_id=simulation.id,
        is_finished=is_finished,
        current_data=updated_data
    )


@router.get("/advice/{simulation_id}", response_model=AdviceResponse)
async def get_chat_advice(simulation_id: int, db: AsyncSession = Depends(get_db)):
    """Génère des conseils personnalisés basés sur une simulation terminée."""
    
    result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation introuvable")
    
    # Récupérer les données de la simulation
    user_data = json.loads(simulation.data)
    
    # Préparer les données pour le moteur d'éligibilité
    # Filtrer uniquement les champs valides pour UserSituation
    valid_fields = {f.name for f in fields(UserSituation)}
    filtered_data = {}
    
    for key, value in user_data.items():
        if key in valid_fields:
            filtered_data[key] = value
    
    # Ajouter les valeurs par défaut requises si manquantes
    if "nationality" not in filtered_data:
        filtered_data["nationality"] = user_data.get("nationality", "Inconnue")
    if "residence_duration_months" not in filtered_data:
        filtered_data["residence_duration_months"] = user_data.get("residence_duration_months", 0)
    
    # Mapper les champs du chat vers les champs du moteur
    if user_data.get("is_married"):
        filtered_data["spouse_nationality"] = "Française"
        filtered_data["is_married_french"] = True
        filtered_data["community_of_life"] = True
    
    if user_data.get("salary_monthly"):
        filtered_data["salary_annual"] = user_data["salary_monthly"] * 12
    
    try:
        # Créer l'objet UserSituation et évaluer
        user_situation = UserSituation(**filtered_data)
        engine = EligibilityEngine(DATA_PATH)
        results = engine.evaluate_eligibility(user_situation)
        
        # Générer les conseils
        bot = SimulegalChatbot(user_data)
        advice = bot.generate_advice(results)
        
        # Formater les résultats
        formatted_results = [
            {
                "procedure_id": r.get("procedure_id"),
                "name": r.get("name"),
                "score": r.get("score"),
                "missing_criteria": r.get("missing_criteria", []),
                "documents": r.get("documents", [])
            }
            for r in results[:5]  # Top 5 résultats
        ]
        
        top_procedure = formatted_results[0] if formatted_results else None
        
        # Mettre à jour la simulation avec le résultat
        if top_procedure:
            simulation.procedure_name = top_procedure["name"]
            simulation.result_score = top_procedure["score"]
            await db.commit()
        
        return AdviceResponse(
            advice=advice,
            top_procedure=top_procedure,
            all_results=formatted_results
        )
        
    except Exception as e:
        # En cas d'erreur, retourner un conseil générique
        return AdviceResponse(
            advice=f"⚠️ Analyse incomplète. Certaines informations manquent pour une évaluation précise.\n\nJe vous conseille d'utiliser le formulaire complet pour une analyse détaillée.",
            top_procedure=None,
            all_results=[]
        )
