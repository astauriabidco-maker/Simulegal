from fastapi import APIRouter, Depends, HTTPException, Request, Header
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from typing import Optional
import stripe
import os
from core.database import get_db
from core.auth_models import Simulation

# Configuration Stripe
# La clé doit être fournie via la variable d'environnement STRIPE_SECRET_KEY
stripe.api_key = os.getenv("STRIPE_SECRET_KEY")

router = APIRouter(prefix="/api/payment", tags=["Payment"])

DOMAIN = os.getenv("DOMAIN", "http://localhost:8000")

@router.post("/create-checkout/{simulation_id}")
async def create_checkout_session(simulation_id: int, db: AsyncSession = Depends(get_db)):
    """Crée une session de paiement Stripe pour débloquer une simulation."""
    result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation non trouvée")

    try:
        checkout_session = stripe.checkout.Session.create(
            line_items=[
                {
                    'price_data': {
                        'currency': 'eur',
                        'product_data': {
                            'name': 'Dossier Simulegal Complet',
                        },
                        'unit_amount': 990, # 9.90€
                    },
                    'quantity': 1,
                },
            ],
            mode='payment',
            success_url=DOMAIN + f'/app?success=true&session_id={{CHECKOUT_SESSION_ID}}&simulation_id={simulation_id}',
            cancel_url=DOMAIN + '/app?canceled=true',
            metadata={
                "simulation_id": str(simulation_id)
            }
        )
        
        simulation.stripe_session_id = checkout_session.id
        await db.commit()

        return {"checkout_url": checkout_session.url}
    
    except Exception as e:
        # MOCK pour le développement
        if "sk_test" in stripe.api_key:
             mock_url = DOMAIN + f'/app?success=true&session_id=cs_test_mock_123&simulation_id={simulation_id}'
             simulation.stripe_session_id = "cs_test_mock_123"
             await db.commit()
             return {"checkout_url": mock_url}

        raise HTTPException(status_code=500, detail=str(e))

@router.get("/verify-session/{session_id}")
async def verify_session(session_id: str, simulation_id: int, db: AsyncSession = Depends(get_db)):
    """Vérifie l'état d'une session (utilisé en callback)."""
    try:
        session = stripe.checkout.Session.retrieve(session_id)
        if session.payment_status == 'paid':
             result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
             simulation = result.scalars().first()
             if simulation:
                 simulation.is_paid = True
                 await db.commit()
                 return {"status": "paid"}
        
        return {"status": "unpaid"}
                 
    except Exception as e:
         # En mode dev
         if "sk_test" in stripe.api_key:
             result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
             simulation = result.scalars().first()
             if simulation:
                 simulation.is_paid = True
                 await db.commit()
             return {"status": "paid (mock)"}
             
         raise HTTPException(status_code=500, detail=str(e))
