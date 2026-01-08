"""
Service de notifications multi-canal pour Simulegal.
Gère l'envoi d'emails, notifications in-app, et préparation SMS.
"""
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from typing import Optional, Dict, Any
from datetime import datetime
import os


class NotificationService:
    """Service d'envoi de notifications multi-canal."""
    
    def __init__(self):
        # Configuration SMTP
        self.smtp_host = os.getenv("SMTP_HOST", "smtp.gmail.com")
        self.smtp_port = int(os.getenv("SMTP_PORT", "587"))
        self.smtp_user = os.getenv("SMTP_USER", "")
        self.smtp_password = os.getenv("SMTP_PASSWORD", "")
        self.from_email = os.getenv("FROM_EMAIL", "noreply@simulegal.fr")
        self.app_url = os.getenv("APP_URL", "http://localhost:8001")
        
        # Configuration Twilio pour WhatsApp
        self.twilio_sid = os.getenv("TWILIO_SID", "")
        self.twilio_token = os.getenv("TWILIO_AUTH_TOKEN", "")
    async def _get_config_value(self, key: str, default: str = "") -> str:
        """Récupère une valeur de config (DB > Env > Default)."""
        from core.database import AsyncSessionLocal
        from core.auth_models import SystemConfig
        from sqlalchemy import select
        
        # 1. Essayer la DB
        try:
            async with AsyncSessionLocal() as session:
                result = await session.execute(
                    select(SystemConfig).where(SystemConfig.key == key)
                )
                config = result.scalars().first()
                if config and config.value:
                    return config.value
        except Exception as e:
            print(f"[NotificationService] Erreur lecture config DB ({key}): {e}")
            
        # 2. Fallback Env
        return os.getenv(key, default)

    async def send_whatsapp(self, to: str, message: str) -> bool:
        """Envoie un message WhatsApp via l'API Twilio."""
        sid = await self._get_config_value("TWILIO_SID")
        token = await self._get_config_value("TWILIO_AUTH_TOKEN")
        phone = await self._get_config_value("TWILIO_PHONE")
        
        if not sid or not token:
            print("[NotificationService] Twilio non configuré (DB/Env), WhatsApp non envoyé")
            return False
            
        try:
            from twilio.rest import Client
            client = Client(sid, token)
            
            # Formater le numéro pour WhatsApp
            to_formatted = to if to.startswith("+") else f"+{to}"
            from_formatted = phone if phone.startswith("+") else f"+{phone}"
            
            msg = client.messages.create(
                from_=f"whatsapp:{from_formatted}",
                body=message,
                to=f"whatsapp:{to_formatted}"
            )
            
            print(f"[NotificationService] WhatsApp envoyé à {to} (SID: {msg.sid})")
            return msg.sid is not None
            
        except ImportError:
            print("[NotificationService] Twilio non installé. pip install twilio")
            return False
        except Exception as e:
            print(f"[NotificationService] Erreur WhatsApp: {e}")
            return False

    
    async def send_email(self, to: str, subject: str, body_html: str) -> bool:
        """
        Envoie un email via SMTP.
        
        Args:
            to: Adresse email du destinataire
            subject: Sujet de l'email
            body_html: Corps de l'email en HTML
        
        Returns:
            True si l'envoi a réussi, False sinon
        """
        if not self.smtp_user or not self.smtp_password:
            print("[NotificationService] SMTP non configuré, email non envoyé")
            return False
        
        try:
            msg = MIMEMultipart("alternative")
            msg["Subject"] = subject
            msg["From"] = f"SimuLegal <{self.from_email}>"
            msg["To"] = to
            
            # Version texte simple
            text_content = body_html.replace("<br>", "\n").replace("</p>", "\n\n")
            # Supprimer les balises HTML pour la version texte
            import re
            text_content = re.sub(r'<[^>]+>', '', text_content)
            
            msg.attach(MIMEText(text_content, "plain", "utf-8"))
            msg.attach(MIMEText(body_html, "html", "utf-8"))
            
            with smtplib.SMTP(self.smtp_host, self.smtp_port) as server:
                server.starttls()
                server.login(self.smtp_user, self.smtp_password)
                server.sendmail(self.from_email, to, msg.as_string())
            
            print(f"[NotificationService] Email envoyé à {to}")
            return True
            
        except Exception as e:
            print(f"[NotificationService] Erreur email: {e}")
            return False
    
    def render_simulation_result_email(
        self, 
        user_name: str, 
        procedure_name: str, 
        score: float,
        simulation_id: int
    ) -> str:
        """
        Génère le template HTML pour les résultats de simulation.
        
        Args:
            user_name: Nom de l'utilisateur
            procedure_name: Nom de la procédure recommandée
            score: Score d'éligibilité (0-100)
            simulation_id: ID de la simulation
        
        Returns:
            HTML de l'email
        """
        # Déterminer la classe de score
        if score >= 100:
            score_class = "score-high"
            score_emoji = "✅"
            score_text = "Excellente nouvelle !"
        elif score >= 70:
            score_class = "score-medium"
            score_emoji = "⚠️"
            score_text = "Vous êtes proche de l'éligibilité"
        else:
            score_class = "score-low"
            score_emoji = "❌"
            score_text = "Éligibilité partielle"
        
        dashboard_link = f"{self.app_url}/dashboard"
        
        return f"""
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        body {{ font-family: 'Segoe UI', Arial, sans-serif; background: #f0f4f8; margin: 0; padding: 20px; }}
        .container {{ max-width: 600px; margin: 0 auto; background: white; border-radius: 16px; overflow: hidden; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }}
        .header {{ background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%); padding: 2rem; text-align: center; }}
        .header h1 {{ color: white; margin: 0; font-size: 1.8rem; }}
        .header p {{ color: rgba(255,255,255,0.8); margin: 0.5rem 0 0; }}
        .content {{ padding: 2rem; }}
        .greeting {{ font-size: 1.2rem; color: #1e293b; margin-bottom: 1rem; }}
        .result-card {{ background: #f8fafc; border-radius: 12px; padding: 1.5rem; margin: 1.5rem 0; border-left: 4px solid #6366f1; }}
        .result-card h3 {{ margin: 0 0 1rem; color: #334155; }}
        .score-badge {{ display: inline-block; padding: 0.5rem 1.5rem; border-radius: 25px; font-weight: 600; font-size: 1.1rem; }}
        .score-high {{ background: #10b981; color: white; }}
        .score-medium {{ background: #f59e0b; color: white; }}
        .score-low {{ background: #ef4444; color: white; }}
        .procedure-name {{ font-size: 1.1rem; color: #6366f1; font-weight: 600; margin-top: 1rem; }}
        .cta-button {{ display: inline-block; padding: 1rem 2rem; background: linear-gradient(135deg, #6366f1, #8b5cf6); color: white; text-decoration: none; border-radius: 8px; font-weight: 600; margin-top: 1.5rem; }}
        .cta-button:hover {{ opacity: 0.9; }}
        .footer {{ padding: 1.5rem 2rem; background: #f8fafc; text-align: center; color: #64748b; font-size: 0.85rem; }}
        .footer a {{ color: #6366f1; text-decoration: none; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🏛️ SimuLegal</h1>
            <p>Votre analyse d'éligibilité est prête</p>
        </div>
        <div class="content">
            <p class="greeting">Bonjour {user_name or 'cher utilisateur'},</p>
            <p>Votre simulation d'éligibilité aux titres de séjour est terminée. Voici un résumé de vos résultats :</p>
            
            <div class="result-card">
                <h3>{score_emoji} {score_text}</h3>
                <p>Score d'éligibilité :</p>
                <span class="score-badge {score_class}">{score:.0f}%</span>
                <p class="procedure-name">📋 Procédure recommandée : {procedure_name}</p>
            </div>
            
            <p>Pour voir les détails complets, les documents requis et nos conseils personnalisés :</p>
            
            <a href="{dashboard_link}" class="cta-button">
                📊 Voir mon analyse complète
            </a>
        </div>
        <div class="footer">
            <p>Vous recevez cet email car vous avez effectué une simulation sur <a href="{self.app_url}">SimuLegal</a>.</p>
            <p>© 2026 SimuLegal - Votre assistant juridique en droit des étrangers</p>
        </div>
    </div>
</body>
</html>
"""
    
    def render_reminder_email(self, user_name: str, message: str) -> str:
        """Génère un email de rappel."""
        return f"""
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <style>
        body {{ font-family: 'Segoe UI', Arial, sans-serif; background: #f0f4f8; margin: 0; padding: 20px; }}
        .container {{ max-width: 600px; margin: 0 auto; background: white; border-radius: 16px; overflow: hidden; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }}
        .header {{ background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%); padding: 2rem; text-align: center; }}
        .header h1 {{ color: white; margin: 0; }}
        .content {{ padding: 2rem; }}
        .cta-button {{ display: inline-block; padding: 1rem 2rem; background: #6366f1; color: white; text-decoration: none; border-radius: 8px; font-weight: 600; margin-top: 1.5rem; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>⏰ Rappel SimuLegal</h1>
        </div>
        <div class="content">
            <p>Bonjour {user_name or 'cher utilisateur'},</p>
            <p>{message}</p>
            <a href="{self.app_url}/app" class="cta-button">Continuer ma simulation</a>
        </div>
    </div>
</body>
</html>
"""

    async def notify_simulation_complete(
        self, 
        user_email: str,
        user_name: str,
        procedure_name: str,
        score: float,
        simulation_id: int
    ) -> bool:
        """
        Notifie l'utilisateur que sa simulation est terminée.
        
        Args:
            user_email: Email de l'utilisateur
            user_name: Nom de l'utilisateur
            procedure_name: Procédure recommandée
            score: Score d'éligibilité
            simulation_id: ID de la simulation
        
        Returns:
            True si la notification a été envoyée
        """
        subject = f"🎯 Résultats de votre simulation - {procedure_name}"
        body = self.render_simulation_result_email(user_name, procedure_name, score, simulation_id)
        return await self.send_email(user_email, subject, body)


# Singleton global
notification_service = NotificationService()
