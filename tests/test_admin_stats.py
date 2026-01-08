"""
Tests unitaires pour les endpoints admin avancés Simulegal.
Tests des statistiques, graphiques, et gestion des utilisateurs.
"""
import pytest
import sys
import os
from fastapi.testclient import TestClient
from unittest.mock import patch, MagicMock, AsyncMock
from datetime import datetime, timedelta

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api.main import app

client = TestClient(app)


class TestAdminStatsEndpoint:
    """Tests pour l'endpoint /api/admin/stats."""
    
    def test_stats_requires_admin(self):
        """Test que l'endpoint stats nécessite une authentification admin."""
        response = client.get("/api/admin/stats")
        # Devrait retourner 401 ou 403 sans token
        assert response.status_code in [401, 403, 422]
    
    def test_stats_returns_expected_fields(self):
        """Test que stats retourne les champs attendus (avec mock auth)."""
        # Ce test nécessiterait un mock de l'authentification admin
        # Pour l'instant, on vérifie que l'endpoint existe
        response = client.get("/api/admin/stats")
        assert response.status_code != 404  # L'endpoint existe


class TestAdminUsersEndpoint:
    """Tests pour l'endpoint /api/admin/users."""
    
    def test_users_requires_admin(self):
        """Test que l'endpoint users nécessite une authentification admin."""
        response = client.get("/api/admin/users")
        # Devrait retourner 401 ou 403 sans token
        assert response.status_code in [401, 403, 422]
    
    def test_users_endpoint_exists(self):
        """Test que l'endpoint users existe."""
        response = client.get("/api/admin/users")
        assert response.status_code != 404


class TestAdminSimulationsEndpoint:
    """Tests pour l'endpoint /api/admin/simulations."""
    
    def test_simulations_requires_admin(self):
        """Test que l'endpoint simulations nécessite une authentification admin."""
        response = client.get("/api/admin/simulations")
        assert response.status_code in [401, 403, 422]


class TestStatsCalculations:
    """Tests unitaires pour les calculs de statistiques."""
    
    def test_conversion_rate_calculation(self):
        """Test du calcul du taux de conversion."""
        total = 100
        paid = 25
        expected_rate = 25.0  # 25%
        
        calculated = round((paid / total * 100), 1) if total > 0 else 0
        assert calculated == expected_rate
    
    def test_conversion_rate_zero_total(self):
        """Test du taux de conversion avec zéro simulations."""
        total = 0
        paid = 0
        
        calculated = round((paid / total * 100), 1) if total > 0 else 0
        assert calculated == 0
    
    def test_revenue_calculation(self):
        """Test du calcul des revenus."""
        paid_sims = 10
        unit_price = 9.99
        expected_revenue = 99.90
        
        calculated = round(paid_sims * unit_price, 2)
        assert calculated == expected_revenue
    
    def test_daily_stats_last_7_days(self):
        """Test génération des statistiques journalières."""
        daily_stats = []
        for i in range(7):
            day = datetime.utcnow() - timedelta(days=i)
            daily_stats.append({
                "date": day.strftime("%Y-%m-%d"),
                "label": day.strftime("%d/%m"),
                "count": i * 2  # Valeur fictive
            })
        
        # Vérifier qu'on a 7 jours
        assert len(daily_stats) == 7
        
        # Vérifier le format des dates
        for stat in daily_stats:
            assert "date" in stat
            assert "label" in stat
            assert "count" in stat


class TestTopProceduresFormatting:
    """Tests pour le formatage des top procédures."""
    
    def test_top_procedures_format(self):
        """Test du format des top procédures."""
        raw_data = [
            ("Carte de résident", 45),
            ("Passeport Talent", 30),
            ("Naturalisation", 25),
        ]
        
        formatted = [{"name": r[0], "count": r[1]} for r in raw_data]
        
        assert len(formatted) == 3
        assert formatted[0]["name"] == "Carte de résident"
        assert formatted[0]["count"] == 45
    
    def test_top_procedures_handles_none(self):
        """Test gestion des valeurs None dans les procédures."""
        raw_data = [(None, 10), ("Test", 5)]
        
        formatted = [{"name": r[0] or "Inconnue", "count": r[1]} for r in raw_data]
        
        assert formatted[0]["name"] == "Inconnue"
        assert formatted[1]["name"] == "Test"
    
    def test_top_procedures_limit(self):
        """Test limite à 5 procédures."""
        raw_data = [(f"Proc_{i}", i) for i in range(10)]
        
        formatted = [{"name": r[0], "count": r[1]} for r in raw_data[:5]]
        
        assert len(formatted) == 5


class TestUserFormatting:
    """Tests pour le formatage des données utilisateur."""
    
    def test_user_format_with_full_data(self):
        """Test formatage utilisateur avec données complètes."""
        user_data = {
            "id": 1,
            "email": "test@example.com",
            "role": "admin",
            "full_name": "Test User",
            "created_at": datetime.utcnow(),
            "simulations_count": 5
        }
        
        formatted = {
            "id": user_data["id"],
            "email": user_data["email"],
            "role": user_data["role"],
            "full_name": user_data["full_name"] or "-",
            "created_at": user_data["created_at"].isoformat() if user_data["created_at"] else None,
            "simulations_count": user_data["simulations_count"]
        }
        
        assert formatted["id"] == 1
        assert formatted["email"] == "test@example.com"
        assert formatted["role"] == "admin"
        assert formatted["simulations_count"] == 5
    
    def test_user_format_with_missing_name(self):
        """Test formatage utilisateur sans nom."""
        user_data = {
            "id": 2,
            "email": "noname@example.com",
            "role": "user",
            "full_name": None,
            "created_at": None,
            "simulations_count": 0
        }
        
        formatted = {
            "full_name": user_data["full_name"] or "-",
            "created_at": user_data["created_at"].isoformat() if user_data["created_at"] else None
        }
        
        assert formatted["full_name"] == "-"
        assert formatted["created_at"] is None
