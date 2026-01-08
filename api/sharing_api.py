"""
API de partage des résultats par Email et WhatsApp.
"""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from pydantic import BaseModel
from typing import Optional
import json
import os

from core.database import get_db
from core.auth_models import Simulation
from core.notifications import NotificationService

router = APIRouter(prefix="/api/share", tags=["Sharing"])

notification_service = NotificationService()


class ShareEmailRequest(BaseModel):
    """Requête pour envoyer les résultats par email."""
    simulation_id: int
    email: str
    user_name: Optional[str] = None


class ShareWhatsAppRequest(BaseModel):
    """Requête pour envoyer les résultats par WhatsApp."""
    simulation_id: int
    phone: str  # Format international: +33612345678
    user_name: Optional[str] = None


class ShareResponse(BaseModel):
    """Réponse de l'API de partage."""
    success: bool
    message: str


@router.post("/email", response_model=ShareResponse)
async def share_by_email(
    request: ShareEmailRequest,
    db: AsyncSession = Depends(get_db)
):
    """
    Envoie un résumé des résultats de simulation par email.
    """
    # Récupérer la simulation
    result = await db.execute(
        select(Simulation).where(Simulation.id == request.simulation_id)
    )
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation introuvable")
    
    # Préparer le contenu
    procedure_name = simulation.procedure_name or "Non définie"
    score = simulation.result_score or 0
    user_name = request.user_name or "Utilisateur"
    
    try:
        # Envoyer l'email via le service de notifications
        success = await notification_service.notify_simulation_complete(
            user_email=request.email,
            user_name=user_name,
            procedure_name=procedure_name,
            score=score,
            simulation_id=simulation.id
        )
        
        if success:
            return ShareResponse(
                success=True,
                message=f"Résultats envoyés à {request.email}"
            )
        else:
            return ShareResponse(
                success=False,
                message="Échec de l'envoi. Vérifiez la configuration SMTP."
            )
            
    except Exception as e:
        return ShareResponse(
            success=False,
            message=f"Erreur: {str(e)}"
        )


@router.post("/whatsapp", response_model=ShareResponse)
async def share_by_whatsapp(
    request: ShareWhatsAppRequest,
    db: AsyncSession = Depends(get_db)
):
    """
    Envoie un résumé des résultats de simulation par WhatsApp (via Twilio).
    """
    # Récupérer la simulation
    result = await db.execute(
        select(Simulation).where(Simulation.id == request.simulation_id)
    )
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation introuvable")
    
    # Préparer le message
    procedure_name = simulation.procedure_name or "Non définie"
    score = simulation.result_score or 0
    user_name = request.user_name or "Utilisateur"
    app_url = os.getenv("APP_URL", "https://simulegal.fr")
    
    # Emoji selon le score
    if score >= 100:
        emoji = "✅"
        status = "Éligible"
    elif score >= 70:
        emoji = "⚠️"
        status = "Éligibilité partielle"
    else:
        emoji = "📋"
        status = "Analyse complète"
    
    message = f"""
{emoji} *SimuLegal - Résultats de votre simulation*

Bonjour {user_name},

Votre simulation d'éligibilité est terminée !

📌 *Procédure recommandée:* {procedure_name}
📊 *Score d'éligibilité:* {score}%
🏷️ *Statut:* {status}

👉 Consultez vos résultats détaillés:
{app_url}/dashboard?simulation_id={simulation.id}

---
SimuLegal - Votre assistant juridique en immigration
    """.strip()
    
    try:
        success = await notification_service.send_whatsapp(
            to=request.phone,
            message=message
        )
        
        if success:
            return ShareResponse(
                success=True,
                message=f"Résultats envoyés sur WhatsApp ({request.phone})"
            )
        else:
            return ShareResponse(
                success=False,
                message="Échec de l'envoi. Vérifiez la configuration Twilio."
            )
            
    except Exception as e:
        # Mode dégradé : retourner un lien de partage WhatsApp direct
        # Mode dégradé : retourner un lien de partage WhatsApp direct
        encoded_message = message.replace(' ', '%20').replace('\n', '%0A')
        whatsapp_link = f"https://wa.me/{request.phone.replace('+', '')}?text={encoded_message}"
        return ShareResponse(
            success=True,
            message=f"Twilio non configuré. Lien alternatif: {whatsapp_link[:50]}..."
        )
