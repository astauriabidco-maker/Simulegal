from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from pydantic import BaseModel
from typing import Optional, Dict, Any
import json

from core.database import get_db
from core.auth_models import Simulation
from core.chatbot import SimulegalChatbot

router = APIRouter(prefix="/api/chat", tags=["Chat"])

class ChatMessage(BaseModel):
    message: str
    simulation_id: Optional[int] = None

class ChatResponse(BaseModel):
    reply: str
    simulation_id: int
    is_finished: bool
    current_data: Dict[str, Any]

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
