"""
Tests unitaires pour le chatbot enrichi Simulegal.
Tests de l'extraction d'informations, questions conditionnelles, et génération de conseils.
"""
import pytest
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from core.chatbot import SimulegalChatbot


class TestChatbotExtraction:
    """Tests pour l'extraction d'informations depuis les messages."""
    
    def test_extract_nationality_tunisienne(self):
        """Test extraction de nationalité tunisienne."""
        bot = SimulegalChatbot()
        bot._extract_info("Je suis tunisien")
        assert bot.situation.get("nationality") == "Tunisienne"
    
    def test_extract_nationality_marocaine(self):
        """Test extraction de nationalité marocaine."""
        bot = SimulegalChatbot()
        bot._extract_info("Je suis marocain depuis toujours")
        assert bot.situation.get("nationality") == "Marocaine"
    
    def test_extract_nationality_algerienne(self):
        """Test extraction de nationalité algérienne."""
        bot = SimulegalChatbot()
        bot._extract_info("Nationalité algérienne")
        assert bot.situation.get("nationality") == "Algérienne"
    
    def test_extract_residence_years(self):
        """Test extraction durée de résidence en années."""
        bot = SimulegalChatbot()
        bot._extract_info("Je vis en France depuis 5 ans")
        assert bot.situation.get("residence_duration_months") == 60
    
    def test_extract_residence_months(self):
        """Test extraction durée de résidence en mois."""
        bot = SimulegalChatbot()
        bot._extract_info("Je suis là depuis 18 mois")
        assert bot.situation.get("residence_duration_months") == 18
    
    def test_extract_salary_monthly(self):
        """Test extraction salaire mensuel."""
        bot = SimulegalChatbot()
        bot._extract_info("Je gagne environ 2500€ par mois")
        assert bot.situation.get("salary_monthly") == 2500
        assert bot.situation.get("salary_annual") == 30000
    
    def test_extract_salary_k(self):
        """Test extraction salaire avec notation 'k'."""
        bot = SimulegalChatbot()
        bot._extract_info("Mon salaire annuel est de 45k")
        assert bot.situation.get("salary_annual") == 45000
    
    def test_extract_contract_cdi(self):
        """Test extraction type de contrat CDI."""
        bot = SimulegalChatbot()
        bot._extract_info("J'ai un CDI")
        assert bot.situation.get("job_contract") == "CDI"
    
    def test_extract_contract_cdd(self):
        """Test extraction type de contrat CDD."""
        bot = SimulegalChatbot()
        bot._extract_info("Je travaille en CDD")
        assert bot.situation.get("job_contract") == "CDD"
    
    def test_extract_married_positive(self):
        """Test extraction mariage positif."""
        bot = SimulegalChatbot()
        bot._extract_info("oui je suis marié à une française")
        assert bot.situation.get("is_married") == True
    
    def test_extract_married_negative(self):
        """Test extraction mariage négatif."""
        bot = SimulegalChatbot()
        bot._extract_info("non, je ne suis pas marié")
        assert bot.situation.get("is_married") == False


class TestChatbotConditionalQuestions:
    """Tests pour les questions conditionnelles."""
    
    def test_condition_has_income(self):
        """Test condition has_income."""
        bot = SimulegalChatbot({"salary_monthly": 2500})
        assert bot._check_condition("has_income") == True
        
        bot2 = SimulegalChatbot({"salary_monthly": 0})
        assert bot2._check_condition("has_income") == False
    
    def test_condition_has_contract(self):
        """Test condition has_contract."""
        bot = SimulegalChatbot({"job_contract": "CDI"})
        assert bot._check_condition("has_contract") == True
        
        bot2 = SimulegalChatbot({"job_contract": "Freelance"})
        assert bot2._check_condition("has_contract") == False
    
    def test_condition_has_master(self):
        """Test condition has_master."""
        bot = SimulegalChatbot({"has_master_france": True})
        assert bot._check_condition("has_master") == True
    
    def test_condition_has_health_issue(self):
        """Test condition has_health_issue."""
        bot = SimulegalChatbot({"health_issue": True})
        assert bot._check_condition("has_health_issue") == True


class TestChatbotAdvice:
    """Tests pour la génération de conseils."""
    
    def test_generate_advice_eligible(self):
        """Test génération conseils pour cas éligible (100%)."""
        bot = SimulegalChatbot()
        results = [{
            "procedure_id": "test",
            "name": "Carte de résident",
            "score": 100,
            "missing_criteria": [],
            "documents": ["Passeport", "Justificatif"]
        }]
        advice = bot.generate_advice(results)
        
        assert "Excellente nouvelle" in advice
        assert "Carte de résident" in advice
        assert "100%" in advice
    
    def test_generate_advice_partial(self):
        """Test génération conseils pour cas partiel (70-99%)."""
        bot = SimulegalChatbot()
        results = [{
            "procedure_id": "test",
            "name": "Passeport Talent",
            "score": 75,
            "missing_criteria": ["Durée de résidence insuffisante"],
            "documents": []
        }]
        advice = bot.generate_advice(results)
        
        assert "proche de l'éligibilité" in advice
        assert "Points à améliorer" in advice
    
    def test_generate_advice_low(self):
        """Test génération conseils pour cas faible (<40%)."""
        bot = SimulegalChatbot()
        results = [{
            "procedure_id": "test",
            "name": "Naturalisation",
            "score": 30,
            "missing_criteria": ["Durée", "Niveau", "Ressources"],
            "documents": []
        }]
        advice = bot.generate_advice(results)
        
        assert "difficile" in advice.lower() or "pistes" in advice.lower()
    
    def test_generate_advice_empty(self):
        """Test génération conseils sans résultats."""
        bot = SimulegalChatbot()
        advice = bot.generate_advice([])
        assert "pas pu analyser" in advice.lower()


class TestChatbotFlow:
    """Tests pour le flux complet du chatbot."""
    
    def test_process_message_returns_question(self):
        """Test que process_message retourne une question."""
        bot = SimulegalChatbot()
        reply, is_finished = bot.process_message("Bonjour")
        
        assert isinstance(reply, str)
        assert len(reply) > 0
        assert is_finished == False
    
    def test_get_next_question_with_empty_situation(self):
        """Test première question avec situation vide."""
        bot = SimulegalChatbot()
        question = bot._get_next_question()
        
        assert question is not None
        assert question["field"] == "nationality"
    
    def test_get_next_question_skips_conditional(self):
        """Test que les questions conditionnelles sont sautées si condition non remplie."""
        bot = SimulegalChatbot({
            "nationality": "Tunisienne",
            "residence_duration_months": 36,
            "is_married": False,
            "parent_of_french_child": False,
            "has_master_france": False,  # Pas de master -> skip research_activity
            "salary_monthly": 0  # Pas de salaire -> skip job_contract
        })
        question = bot._get_next_question()
        
        # Ne devrait pas être une question conditionnelle sur le travail ou la recherche
        assert question is None or question["field"] not in ["job_contract", "research_activity"]
    
    def test_situation_persistence(self):
        """Test que la situation est bien persistée entre les appels."""
        initial = {"nationality": "Sénégalaise", "residence_duration_months": 24}
        bot = SimulegalChatbot(initial)
        
        bot._extract_info("J'ai un CDI à 3000€")
        
        situation = bot.get_situation_dict()
        assert situation["nationality"] == "Sénégalaise"
        assert situation["residence_duration_months"] == 24
        assert situation["job_contract"] == "CDI"
        assert situation["salary_monthly"] == 3000
