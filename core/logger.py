import logging
import json
import sys
from logging.handlers import RotatingFileHandler
import os
from datetime import datetime

# Création du dossier logs s'il n'existe pas
LOG_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "logs")
os.makedirs(LOG_DIR, exist_ok=True)

class JsonFormatter(logging.Formatter):
    """Formateur de logs au format JSON pour l'audit."""
    def format(self, record):
        log_record = {
            "timestamp": datetime.fromtimestamp(record.created).isoformat(),
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
        }
        
        # Ajouter les champs extra s'ils existent (audit_data)
        if hasattr(record, "audit"):
            log_record["audit"] = record.audit
            
        if record.exc_info:
            log_record["exception"] = self.formatException(record.exc_info)
            
        return json.dumps(log_record)

def get_logger(name: str):
    """Récupère un logger configuré pour l'audit."""
    logger = logging.getLogger(name)
    logger.setLevel(logging.INFO)
    
    # Éviter d'ajouter plusieurs handlers si le logger existe déjà
    if not logger.handlers:
        # Handler Console
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setFormatter(JsonFormatter())
        logger.addHandler(console_handler)
        
        # Handler Fichier (Rotation 10MB, 5 backups)
        file_handler = RotatingFileHandler(
            os.path.join(LOG_DIR, "simulegal.log"),
            maxBytes=10*1024*1024,
            backupCount=5
        )
        file_handler.setFormatter(JsonFormatter())
        logger.addHandler(file_handler)
        
    return logger

# Logger principal
audit_logger = get_logger("simulegal.audit")
