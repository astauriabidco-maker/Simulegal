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
    
    # Préférences de notifications
    notifications_email = Column(Boolean, default=True)
    notifications_sms = Column(Boolean, default=False)
    phone_number = Column(String, nullable=True)
    
    # B2B / Franchise
    agency_id = Column(Integer, ForeignKey("agencies.id"), nullable=True)
    
    simulations = relationship("Simulation", back_populates="user")
    appointments = relationship("AppointmentRequest", back_populates="user")
    consultations = relationship("Consultation", back_populates="user")
    cases = relationship("CaseFile", back_populates="user")
    notifications = relationship("Notification", back_populates="user", order_by="desc(Notification.created_at)")
    
    agency = relationship("Agency", back_populates="agents", foreign_keys=[agency_id])

class Agency(Base):
    """Agence ou Franchise Simulegal."""
    __tablename__ = "agencies"
    
    id = Column(Integer, primary_key=True, index=True)
    name = Column(String, unique=True, index=True)
    type = Column(String, default="franchise_full") # internal, franchise_full, franchise_corner
    commission_rate = Column(Float, default=0.10) # 10% par défaut
    
    address = Column(String, nullable=True)
    city = Column(String, nullable=True)
    
    manager_id = Column(Integer, ForeignKey("users.id"), nullable=True)
    
    created_at = Column(DateTime, default=datetime.utcnow)
    
    agents = relationship("User", back_populates="agency", foreign_keys="User.agency_id")
    # manager = relationship("User", foreign_keys=[manager_id]) # Circular dependency risk, handle carefully if needed or just use ID

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
    
    # Notification flag
    notification_sent = Column(Boolean, default=False)
    
    # Premium Services
    has_appointment_service = Column(Boolean, default=False)
    has_lawyer_service = Column(Boolean, default=False)
    
    user = relationship("User", back_populates="simulations")

class AppointmentRequest(Base):
    """Demande d'assistance pour la prise de RDV."""
    __tablename__ = "appointment_requests"
    
    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    simulation_id = Column(Integer, ForeignKey("simulations.id"), nullable=True)
    
    prefecture = Column(String)
    status = Column(String, default="pending") # pending, searching, found, booked
    notes = Column(String, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    user = relationship("User", back_populates="appointments")

class Consultation(Base):
    """Rendez-vous avec un avocat."""
    __tablename__ = "consultations"
    
    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    
    date_time = Column(DateTime) # Date et heure du RDV
    lawyer_name = Column(String, default="Maître Simulegal")
    status = Column(String, default="scheduled") # scheduled, completed, cancelled
    meeting_link = Column(String, nullable=True) # Lien Google Meet / Zoom
    
    created_at = Column(DateTime, default=datetime.utcnow)
    
    user = relationship("User", back_populates="consultations")

class CaseFile(Base):
    """Dossier complet du client pour accompagnement."""
    __tablename__ = "case_files"
    
    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    simulation_id = Column(Integer, ForeignKey("simulations.id"), nullable=True) # Lié à une simulation spécifique
    
    status = Column(String, default="collecting") 
    # Stages: collecting (constitution), review_pending (en attente étude), 
    # changes_requested (à corriger), validated (validé), submitted (déposé)
    
    submission_type = Column(String, nullable=True) # physical, digital (ANEF)
    deposit_method = Column(String, default="upload") # upload, physical_meeting
    
    admin_notes = Column(String, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow)
    
    user = relationship("User", back_populates="cases")
    documents = relationship("CaseDocument", back_populates="case_file")

class CaseDocument(Base):
    """Document pièce jointe au dossier."""
    __tablename__ = "case_documents"
    
    id = Column(Integer, primary_key=True, index=True)
    case_file_id = Column(Integer, ForeignKey("case_files.id"))
    
    name = Column(String) # Ex: "Passeport", "Justificatif Domicile"
    file_path = Column(String) # Path local ou S3 URL
    status = Column(String, default="uploaded") # uploaded, rejected, validated
    
    uploaded_at = Column(DateTime, default=datetime.utcnow)
    
    case_file = relationship("CaseFile", back_populates="documents")


class Notification(Base):
    """Notification in-app pour l'utilisateur."""
    __tablename__ = "notifications"
    
    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    type = Column(String, nullable=False)  # 'simulation_complete', 'reminder', 'info', 'success', 'warning'
    title = Column(String, nullable=False)
    message = Column(Text, nullable=False)
    link = Column(String, nullable=True)  # URL d'action optionnelle
    is_read = Column(Boolean, default=False)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    user = relationship("User", back_populates="notifications")


class SystemConfig(Base):
    """Configuration du système stockée en base."""
    __tablename__ = "system_config"
    
    key = Column(String, primary_key=True, index=True)
    value = Column(String, nullable=True)
    description = Column(String, nullable=True)
    is_secret = Column(Boolean, default=False)

