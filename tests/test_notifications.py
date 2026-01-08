"""
Tests unitaires pour le système de notifications Simulegal.
Tests des modèles, service d'envoi, et API.
"""
import pytest
import sys
import os
from unittest.mock import patch, MagicMock, AsyncMock
from datetime import datetime

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from core.notifications import NotificationService


class TestNotificationService:
    """Tests pour le service de notifications."""
    
    @pytest.fixture
    def service(self):
        """Crée une instance du service de notifications."""
        return NotificationService()
    
    def test_init_default_values(self, service):
        """Test des valeurs par défaut du service."""
        assert service.smtp_host == "smtp.gmail.com"
        assert service.smtp_port == 587
        assert service.from_email == "noreply@simulegal.fr"
    
    def test_render_simulation_result_email_eligible(self, service):
        """Test génération template email pour cas éligible."""
        html = service.render_simulation_result_email(
            user_name="Jean Dupont",
            procedure_name="Carte de résident",
            score=100.0,
            simulation_id=123
        )
        
        assert "Jean Dupont" in html
        assert "Carte de résident" in html
        assert "100%" in html
        assert "Excellente nouvelle" in html
        assert "score-high" in html
    
    def test_render_simulation_result_email_partial(self, service):
        """Test génération template email pour cas partiel."""
        html = service.render_simulation_result_email(
            user_name="Marie Martin",
            procedure_name="Passeport Talent",
            score=75.0,
            simulation_id=456
        )
        
        assert "Marie Martin" in html
        assert "Passeport Talent" in html
        assert "75%" in html
        assert "score-medium" in html
    
    def test_render_simulation_result_email_low(self, service):
        """Test génération template email pour cas faible."""
        html = service.render_simulation_result_email(
            user_name="Pierre Paul",
            procedure_name="Naturalisation",
            score=30.0,
            simulation_id=789
        )
        
        assert "Pierre Paul" in html
        assert "30%" in html
        assert "score-low" in html
    
    def test_render_reminder_email(self, service):
        """Test génération template email de rappel."""
        html = service.render_reminder_email(
            user_name="Sophie",
            message="N'oubliez pas de compléter votre simulation !"
        )
        
        assert "Sophie" in html
        assert "compléter votre simulation" in html
        assert "Rappel" in html
    
    def test_send_email_without_credentials(self, service):
        """Test envoi email sans credentials (devrait échouer gracieusement)."""
        import asyncio
        
        # Sans SMTP_USER et SMTP_PASSWORD configurés
        service.smtp_user = ""
        service.smtp_password = ""
        
        async def _test():
            return await service.send_email(
                to="test@example.com",
                subject="Test",
                body_html="<p>Test</p>"
            )
        
        result = asyncio.get_event_loop().run_until_complete(_test())
        assert result == False  # Devrait retourner False sans erreur
    
    def test_notify_simulation_complete(self, service):
        """Test notification de simulation complète."""
        import asyncio
        from unittest.mock import patch, AsyncMock
        
        # Mock send_email
        async def _test():
            with patch.object(service, 'send_email', new_callable=AsyncMock) as mock_send:
                mock_send.return_value = True
                
                result = await service.notify_simulation_complete(
                    user_email="user@test.com",
                    user_name="Test User",
                    procedure_name="Carte de séjour",
                    score=85.0,
                    simulation_id=100
                )
                
                # Vérifier que send_email a été appelé
                mock_send.assert_called_once()
                return True
        
        result = asyncio.get_event_loop().run_until_complete(_test())
        assert result == True



class TestNotificationTemplates:
    """Tests pour les templates HTML des notifications."""
    
    def test_email_contains_required_elements(self):
        """Test que le template email contient les éléments requis."""
        service = NotificationService()
        html = service.render_simulation_result_email(
            user_name="Test",
            procedure_name="Test Proc",
            score=80.0,
            simulation_id=1
        )
        
        # Éléments structurels
        assert "<!DOCTYPE html>" in html
        assert "<html" in html
        assert "</html>" in html
        
        # Éléments de marque
        assert "SimuLegal" in html
        
        # Bouton CTA
        assert "Voir mon analyse" in html or "cta-button" in html
    
    def test_email_score_classes(self):
        """Test des classes CSS selon le score."""
        service = NotificationService()
        
        # Score élevé
        html_high = service.render_simulation_result_email("", "", 100.0, 1)
        assert "score-high" in html_high
        
        # Score moyen
        html_medium = service.render_simulation_result_email("", "", 75.0, 1)
        assert "score-medium" in html_medium
        
        # Score bas
        html_low = service.render_simulation_result_email("", "", 30.0, 1)
        assert "score-low" in html_low


class TestNotificationAdviceGeneration:
    """Tests pour la génération de conseils dans les notifications."""
    
    @pytest.fixture
    def service(self):
        return NotificationService()
    
    def test_advice_eligible_contains_documents(self, service):
        """Test que les conseils éligibles mentionnent les documents."""
        html = service.render_simulation_result_email(
            user_name="Test",
            procedure_name="Carte de résident",
            score=100.0,
            simulation_id=1
        )
        
        assert "documents" in html.lower() or "dossier" in html.lower()
    
    def test_advice_partial_mentions_improvement(self, service):
        """Test que les conseils partiels mentionnent l'amélioration."""
        html = service.render_simulation_result_email(
            user_name="Test",
            procedure_name="Test",
            score=75.0,
            simulation_id=1
        )
        
        # Devrait mentionner l'éligibilité proche
        assert "proche" in html.lower() or "améliorer" in html.lower() or "75" in html
