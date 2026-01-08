from fastapi import APIRouter, Depends, HTTPException, UploadFile, File, Form
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Optional
from datetime import datetime
from pydantic import BaseModel
import shutil
import os

from core.database import get_db
from core.auth_models import User, CaseFile, CaseDocument, Simulation
from api.auth import get_current_user

router = APIRouter(prefix="/api/case", tags=["Case Management"])

# --- Schemas ---

class CaseFileOut(BaseModel):
    id: int
    status: str
    submission_type: Optional[str]
    deposit_method: str
    admin_notes: Optional[str]
    created_at: datetime
    documents: List['CaseDocumentOut'] = []

    class Config:
        from_attributes = True

class CaseDocumentOut(BaseModel):
    id: int
    name: str
    status: str
    uploaded_at: datetime

    class Config:
        from_attributes = True

# --- Endpoints ---

@router.get("/my-case", response_model=Optional[CaseFileOut])
async def get_my_case(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Récupère le dossier actif de l'utilisateur (ou None)."""
    # On cherche le dossier le plus récent
    result = await db.execute(
        select(CaseFile).where(CaseFile.user_id == user.id).order_by(CaseFile.created_at.desc())
    )
    case_file = result.scalars().first()
    
    if not case_file:
        # Auto-create if not exists but user has paid for report (MVP rule)
        # Check if any paid simulation
        sim_res = await db.execute(select(Simulation).where((Simulation.user_id == user.id) & (Simulation.is_paid == True)))
        sim = sim_res.scalars().first()
        
        if sim:
            # Create Case File
            case_file = CaseFile(
                user_id=user.id,
                simulation_id=sim.id,
                status="collecting",
                submission_type="digital" if "étudiant" in (sim.procedure_name or "").lower() else "physical" # Simple logic
            )
            db.add(case_file)
            await db.commit()
            await db.refresh(case_file)
        else:
            return None # No paid service, no case file access

    # Fetch docs needed
    # For MVP we just return what is uploaded. 
    # In real app we would join with collected docs.
    # To return documents we need to fetch them (lazy loading issue with async)
    # But Pydantic from_attributes might trigger lazy load which fails in async without explicit join or eager load.
    # We will do explicit fetch for docs to be safe
    docs_res = await db.execute(select(CaseDocument).where(CaseDocument.case_file_id == case_file.id))
    documents = docs_res.scalars().all()
    # Manually attach (hacky for Pydantic but works if we construct dict or rely on ORM relationship with eager load)
    # Let's rely on relationship if configured eagerly, or simpler: just return the case_file and let FastAPI handle it if it was eager loaded.
    # AsyncSession lazy loading is tricky. Let's force eager loading in the initial query.
    
    # Re-query with eager load
    from sqlalchemy.orm import selectinload
    result = await db.execute(
        select(CaseFile)
        .where(CaseFile.id == case_file.id)
        .options(selectinload(CaseFile.documents))
    )
    case_file = result.scalars().first()
    
    return case_file

@router.post("/upload")
async def upload_document(
    file: UploadFile = File(...),
    doc_name: str = Form(...),
    case_id: int = Form(...),
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Upload une pièce justificative."""
    # Verify ownership
    result = await db.execute(select(CaseFile).where((CaseFile.id == case_id) & (CaseFile.user_id == user.id)))
    case_file = result.scalars().first()
    if not case_file:
        raise HTTPException(404, "Dossier introuvable")

    # Save file locally
    upload_dir = f"uploads/cases/{case_file.id}"
    os.makedirs(upload_dir, exist_ok=True)
    file_path = f"{upload_dir}/{file.filename}"
    
    with open(file_path, "wb") as buffer:
        shutil.copyfileobj(file.file, buffer)
        
    # Create DB Entry
    new_doc = CaseDocument(
        case_file_id=case_file.id,
        name=doc_name,
        file_path=file_path,
        status="uploaded"
    )
    db.add(new_doc)
    await db.commit()
    
    return {"status": "success", "filename": file.filename}

@router.post("/{case_id}/submit-review")
async def submit_for_review(
    case_id: int,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """Soumettre le dossier à l'étude."""
    result = await db.execute(select(CaseFile).where((CaseFile.id == case_id) & (CaseFile.user_id == user.id)))
    case_file = result.scalars().first()
    if not case_file:
        raise HTTPException(404, "Dossier introuvable")
        
    case_file.status = "review_pending"
    await db.commit()
    return {"status": "review_pending"}

@router.post("/{case_id}/validate-deposit-mode")
async def validate_deposit_mode(
    case_id: int,
    mode: str, # physical or digital
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Admin/User) Confirmer le mode de dépôt (si choix possible)."""
    result = await db.execute(select(CaseFile).where((CaseFile.id == case_id) & (CaseFile.user_id == user.id)))
    case_file = result.scalars().first()
    if not case_file:
        raise HTTPException(404, "Dossier introuvable")

    case_file.deposit_method = mode # 'upload' (digital) or 'physical_meeting'
    # Update submission type logic if needed
    await db.commit()
    return {"mode": mode}

@router.get("/agency-cases", response_model=List[CaseFileOut])
async def get_agency_cases(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Agent/Manager) Récupérer les dossiers de l'agence."""
    if user.role not in ["agent", "agency_manager"]:
        raise HTTPException(403, "Accès réservé aux agents.")
    
    if not user.agency_id:
         return []

    query = select(CaseFile).join(User).where(User.agency_id == user.agency_id)
    
    # Eager load documents
    from sqlalchemy.orm import selectinload
    result = await db.execute(
        query.options(selectinload(CaseFile.documents))
        .order_by(CaseFile.created_at.desc())
    )
    return result.scalars().all()

# --- Admin Endpoints ---

@router.get("/all", response_model=List[CaseFileOut])
async def get_all_cases(
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Admin) Récupérer tous les dossiers."""
    if user.role != "admin":
        raise HTTPException(403, "Accès interdit")
        
    # Eager load user and documents
    from sqlalchemy.orm import selectinload
    result = await db.execute(
        select(CaseFile)
        .options(selectinload(CaseFile.documents))
        .order_by(CaseFile.created_at.desc())
    )
    return result.scalars().all()
    """(Agent/Manager) Récupérer les dossiers de l'agence."""
    if user.role not in ["agent", "agency_manager"]:
        raise HTTPException(403, "Accès réservé aux agents.")
    
    if not user.agency_id:
         return []

    # Logic: Agent sees ONLY his creations? Or all agency?
    # Spec says "Mon Portefeuille" -> His creations. 
    # Manager sees all agency.
    
    query = select(CaseFile).join(User).where(User.agency_id == user.agency_id)
    
    if user.role == "agent":
        # Filter by creator (assuming user_id in CaseFile is the Client, how do we know who CREATED it?)
        # Ah, we miss a "created_by_agent_id" field in CaseFile!
        # For now, let's assume Agent created calculation is tricky without that field.
        # Fallback: Agent sees ALL agency files for MVP collaboration?
        # User requested "Mon Portefeuille".
        pass 
        
    # Eager load documents
    from sqlalchemy.orm import selectinload
    result = await db.execute(
        query.options(selectinload(CaseFile.documents))
        .order_by(CaseFile.created_at.desc())
    )
    return result.scalars().all()

@router.post("/{case_id}/validate")
async def validate_case(
    case_id: int,
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Admin) Valider le dossier complet."""
    if user.role != "admin":
        raise HTTPException(403, "Accès interdit")
        
    result = await db.execute(select(CaseFile).where(CaseFile.id == case_id))
    case_file = result.scalars().first()
    if not case_file:
        raise HTTPException(404, "Dossier introuvable")
        
    case_file.status = "validated"
    await db.commit()
    
    # Notification Email
    from core.notifications import NotificationService
    notif_service = NotificationService()
    user_res = await db.execute(select(User).where(User.id == case_file.user_id))
    case_owner = user_res.scalars().first()
    
    if case_owner and case_owner.email:
        try:
            await notif_service.send_email(
                to_email=case_owner.email,
                subject="SimuLegal - Dossier Validé !",
                content=f"""
                Bonjour {case_owner.full_name or ''},
                
                Excellente nouvelle ! Votre dossier a été validé par nos experts. 
                
                Vous pouvez dès maintenant accéder à votre espace pour procéder au dépôt (ANEF ou RDV Préfecture).
                
                👉 Connectez-vous : {os.getenv("APP_URL", "https://simulegal.fr")}/app
                """
            )
        except Exception as e:
            print(f"Failed to send validation email: {e}")

    return {"status": "validated"}

@router.post("/doc/{doc_id}/status")
async def update_doc_status(
    doc_id: int,
    status: str = Form(...), # validated, rejected
    reason: str = Form(None),
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user)
):
    """(Admin) Valider ou rejeter un document."""
    if user.role != "admin":
        raise HTTPException(403, "Accès interdit")
        
    result = await db.execute(select(CaseDocument).where(CaseDocument.id == doc_id))
    doc = result.scalars().first()
    if not doc:
        raise HTTPException(404, "Document introuvable")
        
    doc.status = status
    # If rejected, maybe update case status to 'changes_requested'
    if status == "rejected":
        # Update parent case status
        case_res = await db.execute(select(CaseFile).where(CaseFile.id == doc.case_file_id))
        case = case_res.scalars().first()
        if case:
            case.status = "changes_requested"
            case.admin_notes = reason
            
    await db.commit()
    return {"status": status}

