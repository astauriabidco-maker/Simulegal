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

PRICES = {
    "report": {"amount": 990, "name": "Dossier Simulegal Complet"},
    "appointment": {"amount": 2990, "name": "Assistance Prise de RDV"},
    "lawyer": {"amount": 5990, "name": "Consultation Avocat (30 min)"}
}

@router.post("/create-checkout/{simulation_id}")
async def create_checkout_session(
    simulation_id: int, 
    service_type: str = "report", 
    db: AsyncSession = Depends(get_db)
):
    """Crée une session de paiement Stripe pour un service spécifique."""
    result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
    simulation = result.scalars().first()
    
    if not simulation:
        raise HTTPException(status_code=404, detail="Simulation non trouvée")

    if service_type not in PRICES:
        raise HTTPException(status_code=400, detail="Type de service inconnu")
        
    price_info = PRICES[service_type]

    try:
        checkout_session = stripe.checkout.Session.create(
            line_items=[
                {
                    'price_data': {
                        'currency': 'eur',
                        'product_data': {
                            'name': price_info["name"],
                        },
                        'unit_amount': price_info["amount"],
                    },
                    'quantity': 1,
                },
            ],
            mode='payment',
            # On passe le service_type dans l'URL de succès pour le traiter au retour
            success_url=DOMAIN + f'/app?success=true&session_id={{CHECKOUT_SESSION_ID}}&simulation_id={simulation_id}&service_type={service_type}',
            cancel_url=DOMAIN + '/app?canceled=true',
            metadata={
                "simulation_id": str(simulation_id),
                "service_type": service_type
            }
        )
        
        # On ne stocke le session_id que pour le rapport principal pour l'instant, 
        # ou on pourrait ajouter des champs dédiés si besoin. 
        # Ici on écrase, car c'est le dernier checkout qui compte pour la vérif immédiate.
        simulation.stripe_session_id = checkout_session.id
        await db.commit()

        return {"checkout_url": checkout_session.url}
    
    except Exception as e:
        # MOCK pour le développement
        if "sk_test" in stripe.api_key:
             mock_url = DOMAIN + f'/app?success=true&session_id=cs_test_mock_{service_type}&simulation_id={simulation_id}&service_type={service_type}'
             simulation.stripe_session_id = f"cs_test_mock_{service_type}"
             await db.commit()
             return {"checkout_url": mock_url}

        raise HTTPException(status_code=500, detail=str(e))

@router.get("/verify-session/{session_id}")
async def verify_session(
    session_id: str, 
    simulation_id: int, 
    service_type: str = "report",
    db: AsyncSession = Depends(get_db)
):
    """Vérifie l'état d'une session et active le service correspondant."""
    try:
        # Récupération de la simulation
        result = await db.execute(select(Simulation).where(Simulation.id == simulation_id))
        simulation = result.scalars().first()
        
        if not simulation:
             return {"status": "error", "message": "Simulation not found"}

        payment_success = False
        
        # 1. Vérification via Stripe (Réel)
        if "cs_test_mock" not in session_id:
            session = stripe.checkout.Session.retrieve(session_id)
            if session.payment_status == 'paid':
                payment_success = True
                # Récupérer le vrai service_type des metadata si possible
                if session.metadata and "service_type" in session.metadata:
                    service_type = session.metadata["service_type"]
        
        # 2. Vérification Mock (Dev)
        else:
            # En mode dev, on considère que si ça revient du mock, c'est payé
            if "sk_test" in stripe.api_key:
                payment_success = True

        # Activation du service
        if payment_success:
            if service_type == "report":
                simulation.is_paid = True
            elif service_type == "appointment":
                simulation.has_appointment_service = True
            elif service_type == "lawyer":
                simulation.has_lawyer_service = True
                
            await db.commit()
            return {"status": "paid", "service": service_type}
        
        return {"status": "unpaid"}
                 
    except Exception as e:
         raise HTTPException(status_code=500, detail=str(e))
