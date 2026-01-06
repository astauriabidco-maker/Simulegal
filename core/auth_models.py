from sqlalchemy import Column, Integer, String, Boolean, Float, DateTime, ForeignKey, Text
from sqlalchemy.orm import relationship
from datetime import datetime
from core.database import Base

class User(Base):
    """Modèle utilisateur pour l'authentification."""
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    email = Column(String, unique=True, index=True, nullable=False)
    password_hash = Column(String, nullable=False)
    role = Column(String, default="user")  # 'user' ou 'admin'
    full_name = Column(String, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    simulations = relationship("Simulation", back_populates="user")

class Simulation(Base):
    """Sauvegarde d'une simulation d'éligibilité."""
    __tablename__ = "simulations"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    data = Column(Text, nullable=False)  # JSON string des données d'entrée
    result_score = Column(Float, nullable=True)
    procedure_name = Column(String, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    # Monetization
    is_paid = Column(Boolean, default=False)
    stripe_session_id = Column(String, nullable=True)
    language = Column(String, default="fr")
    
    user = relationship("User", back_populates="simulations")
