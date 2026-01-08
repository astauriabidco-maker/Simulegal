from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Optional
from datetime import datetime
from pydantic import BaseModel

from core.database import get_db
from core.auth_models import User, AppointmentRequest, Consultation, Simulation
from api.auth import get_current_user

router = APIRouter(prefix="/api/services", tags=["Services Backend"])

# --- Schemas ---

class AppointmentCreate(BaseModel):
    prefecture: str
    simulation_id: Optional[int] = None
    notes: Optional[str] = None

class AppointmentOut(BaseModel):
    id: int
    prefecture: str
    status: str
    created_at: datetime
    notes: Optional[str]

    class Config:
        from_attributes = True

class ConsultationCreate(BaseModel):
    date_time: datetime

class ConsultationOut(BaseModel):
    id: int
    date_time: datetime
    lawyer_name: str
    status: str
    meeting_link: Optional[str]

    class Config:
        from_attributes = True

# --- Endpoints ---

@router.post("/appointments", response_model=AppointmentOut)
async def create_appointment_request(
    request: AppointmentCreate,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Crée une demande d'assistance RDV."""
    # Vérifier si le service a été acheté (via une simulation liée à l'user)
    # Pour simplifier, on vérifie si UNE des simulations de l'user a le service appointment
    # Ou si on passe une simulation_id spécifique
    
    has_service = False
    
    # Check global purchase history
    query = select(Simulation).where(
        (Simulation.user_id == user.id) & (Simulation.has_appointment_service == True)
    )
    result = await db.execute(query)
    purchases = result.scalars().all()
    
    if not purchases:
        raise HTTPException(status_code=403, detail="Service Assistance RDV non acheté.")

    new_request = AppointmentRequest(
        user_id=user.id,
        simulation_id=request.simulation_id,
        prefecture=request.prefecture,
        notes=request.notes,
        status="searching"
    )
    db.add(new_request)
    await db.commit()
    await db.refresh(new_request)
    return new_request

@router.get("/appointments", response_model=List[AppointmentOut])
async def get_my_appointments(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Récupère les demandes de RDV de l'utilisateur."""
    result = await db.execute(select(AppointmentRequest).where(AppointmentRequest.user_id == user.id))
    return result.scalars().all()


@router.get("/lawyer/slots")
async def get_lawyer_slots():
    """Renvoie des créneaux de disponibilité (MOCK)."""
    # Générer quelques créneaux pour les 3 prochains jours
    from datetime import timedelta
    base_time = datetime.utcnow().replace(hour=10, minute=0, second=0, microsecond=0) + timedelta(days=1)
    
    slots = []
    for day in range(3):
        for hour in [10, 11, 14, 15, 16]:
            slot_time = base_time + timedelta(days=day)
            slot_time = slot_time.replace(hour=hour)
            slots.append(slot_time.isoformat())
            
    return {"slots": slots}

@router.post("/consultations", response_model=ConsultationOut)
async def book_consultation(
    request: ConsultationCreate,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Réserve une consultation avocat."""
    # Vérifier achat
    query = select(Simulation).where(
        (Simulation.user_id == user.id) & (Simulation.has_lawyer_service == True)
    )
    result = await db.execute(query)
    purchases = result.scalars().all()
    
    if not purchases:
        raise HTTPException(status_code=403, detail="Service Consultation Avocat non acheté.")

    new_consultation = Consultation(
        user_id=user.id,
        date_time=request.date_time,
        lawyer_name="Maître Dupont (Partenaire)",
        status="scheduled",
        meeting_link="https://meet.google.com/abc-defg-hij" # Mock link
    )
    db.add(new_consultation)
    await db.commit()
    await db.refresh(new_consultation)
    return new_consultation

@router.get("/consultations", response_model=List[ConsultationOut])
async def get_my_consultations(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Récupère les consultations de l'utilisateur."""
    result = await db.execute(select(Consultation).where(Consultation.user_id == user.id))
    return result.scalars().all()
