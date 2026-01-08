from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Optional
from pydantic import BaseModel
from datetime import datetime

from core.database import get_db
from core.auth_models import User, Agency
from api.auth import get_current_user

router = APIRouter(prefix="/api/agencies", tags=["Agency Management"])

# --- Schemas ---

class AgencyCreate(BaseModel):
    name: str
    type: str = "franchise_full" # internal, franchise_full, franchise_corner
    commission_rate: float = 0.10
    address: Optional[str] = None
    city: Optional[str] = None
    manager_email: Optional[str] = None # Optional: invite manager by email directly

class AgencyOut(BaseModel):
    id: int
    name: str
    type: str
    city: Optional[str]
    created_at: datetime
    agent_count: int = 0

    class Config:
        from_attributes = True

class AddAgentRequest(BaseModel):
    email: str
    role: str = "agent" # agent, agency_manager

# --- Endpoints ---

@router.post("/", response_model=AgencyOut)
async def create_agency(
    agency_in: AgencyCreate,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Super Admin) Créer une nouvelle agence/franchise."""
    if user.role != "admin":
        raise HTTPException(403, "Seul le super-admin peut créer une agence.")

    # Check existence
    res = await db.execute(select(Agency).where(Agency.name == agency_in.name))
    if res.scalars().first():
        raise HTTPException(400, "Une agence avec ce nom existe déjà.")

    new_agency = Agency(
        name=agency_in.name,
        type=agency_in.type,
        commission_rate=agency_in.commission_rate,
        address=agency_in.address,
        city=agency_in.city
    )
    
    # Handle Manager if email provided
    if agency_in.manager_email:
        res_user = await db.execute(select(User).where(User.email == agency_in.manager_email))
        manager = res_user.scalars().first()
        if manager:
            new_agency.manager_id = manager.id
            manager.role = "agency_manager"
            # We will link agency_id after commit to get ID
    
    db.add(new_agency)
    await db.commit()
    await db.refresh(new_agency)
    
    if agency_in.manager_email and 'manager' in locals() and manager:
        manager.agency_id = new_agency.id
        await db.commit()

    return AgencyOut(
        id=new_agency.id, 
        name=new_agency.name, 
        type=new_agency.type, 
        city=new_agency.city, 
        created_at=new_agency.created_at,
        agent_count=1 if agency_in.manager_email else 0
    )

@router.get("/", response_model=List[AgencyOut])
async def list_agencies(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Admin) Lister toutes les agences."""
    if user.role != "admin":
         # TODO: If agency_manager, return only own agency?
         raise HTTPException(403, "Accès réservé admin.")

    result = await db.execute(select(Agency))
    agencies = result.scalars().all()
    
    # Helper to get counts (naive n+1 for MVP, optimize later)
    out = []
    for a in agencies:
        # Count agents
        # res_count = await db.execute(select(func.count(User.id)).where(User.agency_id == a.id))
        # count = res_count.scalar()
        out.append(AgencyOut(
            id=a.id,
            name=a.name,
            type=a.type,
            city=a.city,
            created_at=a.created_at,
            agent_count=0 # Placeholder for now
        ))
    return out

@router.post("/{agency_id}/agents")
async def add_agent_to_agency(
    agency_id: int,
    req: AddAgentRequest,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Ajouter un utilisateur existant comme Agent dans cette agence."""
    # Auth check: Super Admin OR Manager of this agency
    is_allowed = False
    if user.role == "admin":
        is_allowed = True
    elif user.role == "agency_manager" and user.agency_id == agency_id:
        is_allowed = True
        
    if not is_allowed:
        raise HTTPException(403, "Non autorisé à gérer cette agence.")

    # Get User
    res = await db.execute(select(User).where(User.email == req.email))
    target_user = res.scalars().first()
    if not target_user:
        raise HTTPException(404, "Utilisateur introuvable (il doit d'abord s'inscrire).")
        
    target_user.agency_id = agency_id
    target_user.role = req.role # agent or agency_manager
    
    await db.commit()
    return {"status": "assigned", "role": target_user.role, "agency_id": agency_id}

@router.get("/my-stats")
async def get_agency_stats(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Agent/Manager) Récupérer les stats de l'agence."""
    if user.role not in ["agent", "agency_manager"]:
        raise HTTPException(403, "Accès réservé aux agents.")
    
    if not user.agency_id:
        return {"error": "Aucune agence associée"}

    from sqlalchemy import func
    from datetime import datetime, timedelta
    from core.auth_models import Simulation, CaseFile
    
    # Get agency info
    res_agency = await db.execute(select(Agency).where(Agency.id == user.agency_id))
    agency = res_agency.scalars().first()
    
    # Count users in agency
    res_users = await db.execute(
        select(func.count(User.id)).where(User.agency_id == user.agency_id)
    )
    agent_count = res_users.scalar() or 0
    
    # Count cases this month
    month_start = datetime.utcnow().replace(day=1, hour=0, minute=0, second=0, microsecond=0)
    res_cases = await db.execute(
        select(func.count(CaseFile.id))
        .join(User)
        .where(User.agency_id == user.agency_id)
        .where(CaseFile.created_at >= month_start)
    )
    month_cases = res_cases.scalar() or 0
    
    # Calculate revenue (paid simulations from agency users)
    res_paid = await db.execute(
        select(func.count(Simulation.id))
        .join(User)
        .where(User.agency_id == user.agency_id)
        .where(Simulation.is_paid == True)
    )
    paid_count = res_paid.scalar() or 0
    
    # Revenue estimation (29€ per report)
    revenue = paid_count * 29
    commission_rate = agency.commission_rate if agency else 0.10
    commission = revenue * commission_rate
    
    return {
        "agency_name": agency.name if agency else "Inconnue",
        "agency_type": agency.type if agency else "unknown",
        "agent_count": agent_count,
        "month_cases": month_cases,
        "total_paid": paid_count,
        "revenue": revenue,
        "commission_rate": commission_rate,
        "commission": round(commission, 2)
    }
