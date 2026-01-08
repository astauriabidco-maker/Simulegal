"""
API de configuration système pour l'administrateur.
Permet de gérer les clés API et paramètres (Twilio, SMTP, Stripe).
"""
from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, update
from pydantic import BaseModel
from typing import List, Optional
import os

from core.database import get_db
from core.auth_models import SystemConfig, User
from api.auth import get_current_user

router = APIRouter(prefix="/api/admin/config", tags=["Admin Config"])

# Liste des clés autorisées et leurs descriptions par défaut
DEFAULT_CONFIGS = {
    "TWILIO_SID": {"desc": "Account SID Twilio", "secret": True},
    "TWILIO_AUTH_TOKEN": {"desc": "Auth Token Twilio", "secret": True},
    "TWILIO_PHONE": {"desc": "Numéro WhatsApp (+1415...)", "secret": False},
    "SMTP_HOST": {"desc": "Serveur SMTP (ex: smtp.gmail.com)", "secret": False},
    "SMTP_PORT": {"desc": "Port SMTP (ex: 587)", "secret": False},
    "SMTP_USER": {"desc": "Utilisateur SMTP", "secret": False},
    "SMTP_PASSWORD": {"desc": "Mot de passe SMTP", "secret": True},
    "STRIPE_SECRET_KEY": {"desc": "Clé secrète Stripe", "secret": True},
    "REPORT_PRICE": {"desc": "Prix du rapport (€)", "secret": False},
}

class ConfigItem(BaseModel):
    key: str
    value: Optional[str] = None
    description: Optional[str] = None
    is_secret: bool = False

class ConfigUpdate(BaseModel):
    key: str
    value: str

@router.get("/", response_model=List[ConfigItem])
async def get_config(
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """Récupère la configuration système. Les secrets sont masqués."""
    if current_user.role != "admin":
        raise HTTPException(status_code=403, detail="Accès réservé aux administrateurs")
    
    # Récupérer la config en base
    result = await db.execute(select(SystemConfig))
    db_configs = {c.key: c for c in result.scalars().all()}
    
    response = []
    
    # Fusionner avec les défauts et les variables d'env
    for key, info in DEFAULT_CONFIGS.items():
        db_item = db_configs.get(key)
        
        # Valeur actuelle : priorité DB > Env > Vide
        current_value = ""
        if db_item and db_item.value:
            current_value = db_item.value
        else:
            current_value = os.getenv(key, "")
            
        # Masquer si secret
        display_value = current_value
        if info["secret"] and current_value:
             display_value = "********" if len(current_value) > 4 else "****"
             
        response.append(ConfigItem(
            key=key,
            value=display_value,
            description=db_item.description if db_item else info["desc"],
            is_secret=info["secret"]
        ))
        
    return response

@router.post("/", response_model=ConfigItem)
async def update_config(
    update_data: ConfigUpdate,
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """Met à jour une clé de configuration."""
    if current_user.role != "admin":
        raise HTTPException(status_code=403, detail="Accès réservé aux administrateurs")
        
    key = update_data.key
    if key not in DEFAULT_CONFIGS:
        raise HTTPException(status_code=400, detail="Clé de configuration inconnue")
        
    # Vérifier l'existence
    result = await db.execute(select(SystemConfig).where(SystemConfig.key == key))
    config_item = result.scalars().first()
    
    if config_item:
        config_item.value = update_data.value
    else:
        config_item = SystemConfig(
            key=key, 
            value=update_data.value,
            description=DEFAULT_CONFIGS[key]["desc"],
            is_secret=DEFAULT_CONFIGS[key]["secret"]
        )
        db.add(config_item)
        
    await db.commit()
    await db.refresh(config_item)
    
    # Masquer le retour si secret
    returned_value = config_item.value
    if config_item.is_secret and returned_value:
        returned_value = "********"
        
    return ConfigItem(
        key=config_item.key,
        value=returned_value,
        description=config_item.description,
        is_secret=config_item.is_secret
    )
