"""
API de gestion des notifications pour Simulegal.
Endpoints pour lister, marquer comme lu, et supprimer les notifications.
"""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select, update
from sqlalchemy.ext.asyncio import AsyncSession
from pydantic import BaseModel
from typing import Optional, List
from datetime import datetime

from core.database import get_db
from core.auth_models import Notification, User
from api.auth import get_current_user

router = APIRouter(prefix="/api/notifications", tags=["Notifications"])



class NotificationOut(BaseModel):
    """Schéma de sortie pour une notification."""
    id: int
    type: str
    title: str
    message: str
    link: Optional[str] = None
    is_read: bool
    created_at: datetime
    
    class Config:
        from_attributes = True


class NotificationCreate(BaseModel):
    """Schéma pour créer une notification (usage interne)."""
    type: str
    title: str
    message: str
    link: Optional[str] = None


class UnreadCountOut(BaseModel):
    """Nombre de notifications non lues."""
    count: int


@router.get("/", response_model=List[NotificationOut])
async def get_notifications(
    limit: int = 20,
    offset: int = 0,
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """
    Liste les notifications de l'utilisateur connecté.
    
    - **limit**: Nombre maximum de notifications à retourner (défaut: 20)
    - **offset**: Offset pour la pagination (défaut: 0)
    """
    result = await db.execute(
        select(Notification)
        .where(Notification.user_id == current_user.id)
        .order_by(Notification.created_at.desc())
        .limit(limit)
        .offset(offset)
    )
    notifications = result.scalars().all()
    return notifications


@router.get("/unread-count", response_model=UnreadCountOut)
async def get_unread_count(
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """Retourne le nombre de notifications non lues."""
    from sqlalchemy import func
    result = await db.execute(
        select(func.count(Notification.id))
        .where(Notification.user_id == current_user.id)
        .where(Notification.is_read == False)
    )
    count = result.scalar() or 0
    return UnreadCountOut(count=count)


@router.post("/{notification_id}/read")
async def mark_as_read(
    notification_id: int,
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """Marque une notification comme lue."""
    result = await db.execute(
        select(Notification)
        .where(Notification.id == notification_id)
        .where(Notification.user_id == current_user.id)
    )
    notification = result.scalars().first()
    
    if not notification:
        raise HTTPException(status_code=404, detail="Notification introuvable")
    
    notification.is_read = True
    await db.commit()
    
    return {"status": "success", "message": "Notification marquée comme lue"}


@router.post("/read-all")
async def mark_all_as_read(
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """Marque toutes les notifications de l'utilisateur comme lues."""
    await db.execute(
        update(Notification)
        .where(Notification.user_id == current_user.id)
        .where(Notification.is_read == False)
        .values(is_read=True)
    )
    await db.commit()
    
    return {"status": "success", "message": "Toutes les notifications ont été marquées comme lues"}


@router.delete("/{notification_id}")
async def delete_notification(
    notification_id: int,
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """Supprime une notification."""
    result = await db.execute(
        select(Notification)
        .where(Notification.id == notification_id)
        .where(Notification.user_id == current_user.id)
    )
    notification = result.scalars().first()
    
    if not notification:
        raise HTTPException(status_code=404, detail="Notification introuvable")
    
    await db.delete(notification)
    await db.commit()
    
    return {"status": "success", "message": "Notification supprimée"}


@router.delete("/")
async def delete_all_notifications(
    current_user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db)
):
    """Supprime toutes les notifications de l'utilisateur."""
    result = await db.execute(
        select(Notification)
        .where(Notification.user_id == current_user.id)
    )
    notifications = result.scalars().all()
    
    for notif in notifications:
        await db.delete(notif)
    
    await db.commit()
    
    return {"status": "success", "message": "Toutes les notifications ont été supprimées"}


# === Helper Functions ===

async def create_notification(
    db: AsyncSession,
    user_id: int,
    type: str,
    title: str,
    message: str,
    link: Optional[str] = None
) -> Notification:
    """
    Crée une nouvelle notification pour un utilisateur.
    
    Args:
        db: Session de base de données
        user_id: ID de l'utilisateur
        type: Type de notification ('simulation_complete', 'reminder', 'info', 'success', 'warning')
        title: Titre de la notification
        message: Message de la notification
        link: URL d'action optionnelle
    
    Returns:
        La notification créée
    """
    notification = Notification(
        user_id=user_id,
        type=type,
        title=title,
        message=message,
        link=link
    )
    db.add(notification)
    await db.commit()
    await db.refresh(notification)
    return notification


async def notify_simulation_complete(
    db: AsyncSession,
    user_id: int,
    procedure_name: str,
    score: float,
    simulation_id: int
) -> Notification:
    """
    Crée une notification in-app pour une simulation terminée.
    
    Args:
        db: Session de base de données
        user_id: ID de l'utilisateur
        procedure_name: Nom de la procédure recommandée
        score: Score d'éligibilité
        simulation_id: ID de la simulation
    
    Returns:
        La notification créée
    """
    if score >= 100:
        emoji = "✅"
        type = "success"
    elif score >= 70:
        emoji = "⚠️"
        type = "warning"
    else:
        emoji = "📋"
        type = "info"
    
    return await create_notification(
        db=db,
        user_id=user_id,
        type=type,
        title=f"{emoji} Simulation terminée",
        message=f"Votre éligibilité pour '{procedure_name}' a été évaluée à {score:.0f}%.",
        link=f"/dashboard?simulation_id={simulation_id}"
    )
