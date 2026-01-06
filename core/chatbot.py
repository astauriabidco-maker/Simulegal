import re
import json
from typing import Dict, Any, List, Optional, Tuple
from core.models import UserSituation

class SimulegalChatbot:
    """Moteur conversationnel pour l'évaluation d'éligibilité."""
    
    QUESTIONS = [
        {"field": "nationality", "text": "Pour commencer, quelle est votre nationalité ?"},
        {"field": "residence_duration_months", "text": "Depuis combien de temps (en mois ou années) résidez-vous en France ?"},
        {"field": "is_married", "text": "Êtes-vous marié(e) à un(e) citoyen(ne) français(e) ?"},
        {"field": "parent_of_french_child", "text": "Êtes-vous parent d'un enfant français mineur résidant en France ?"},
        {"field": "has_master_france", "text": "Avez-vous obtenu un diplôme de Master (ou équivalent) en France ?"},
        {"field": "salary_annual", "text": "Quel est votre salaire annuel brut approximatif ?"},
        {"field": "no_polygamy", "text": "Question importante pour le dossier : confirmez-vous l'absence de situation de polygamie ?"}
    ]

    def __init__(self, situation_data: Optional[Dict[str, Any]] = None):
        self.situation = situation_data or {}
        # Assurer les valeurs par défaut pour les booléens critiques si non présents
        if "no_polygamy" not in self.situation:
            self.situation["no_polygamy"] = True
        if "is_adult" not in self.situation:
            self.situation["is_adult"] = True

    def process_message(self, message: str) -> Tuple[str, bool]:
        """
        Analyse le message, met à jour la situation et suggère la suite.
        Retourne (Réponse, EstTerminé).
        """
        # 1. Extraction d'informations simple (Regex/Mots-clés)
        self._extract_info(message)
        
        # 2. Identifier le prochain champ manquant
        next_question = self._get_next_question()
        
        if next_question:
            return next_question["text"], False
        else:
            return "Merci pour toutes ces informations. Je dispose de suffisamment d'éléments pour analyser votre situation. Voulez-vous voir vos résultats ?", True

    def _extract_info(self, text: str):
        text = text.lower()
        
        # Nationalité (Simplifié: prend le dernier mot capitalisé ou spécifique)
        countries = ["tunisie", "tunisien", "algérie", "algérien", "maroc", "marocain", "sénégal", "france", "français"]
        for c in countries:
            if c in text:
                if "tunis" in c: self.situation["nationality"] = "Tunisienne"
                elif "algér" in c: self.situation["nationality"] = "Algérienne"
                elif "maroc" in c: self.situation["nationality"] = "Marocaine"
                elif "sénégal" in c: self.situation["nationality"] = "Sénégalaise"
                elif "franç" in c: self.situation["nationality"] = "Française"

        # Durée de résidence
        # Cherche "X ans" ou "X mois"
        years_match = re.search(r'(\d+)\s*an', text)
        if years_match:
            self.situation["residence_duration_months"] = int(years_match.group(1)) * 12
        else:
            months_match = re.search(r'(\d+)\s*mois', text)
            if months_match:
                self.situation["residence_duration_months"] = int(months_match.group(1))

        # Salaire
        money_match = re.search(r'(\d+)\s*(?:€|euro|k)', text)
        if money_match:
            val = int(money_match.group(1))
            if val < 500: val *= 1000 # Gestion des "35k"
            self.situation["salary_annual"] = val

        # Booléens (Oui/Non)
        positive = ["oui", "ouais", "exact", "tout à fait", "c'est ça", "marié", "pacsé"]
        negative = ["non", "pas du tout", "célibataire", "aucun"]
        
        # On regarde si le message répond à la dernière question posée (via le contexte optionnel)
        # Ici on fait une détection globale simple
        if any(w in text for w in positive):
            if "mari" in text or "conjoint" in text: self.situation["is_married"] = True
            if "enfant" in text: self.situation["parent_of_french_child"] = True
            if "master" in text or "diplôme" in text: self.situation["has_master_france"] = True
            
        if any(w in text for w in negative):
            if "mari" in text: self.situation["is_married"] = False

    def _get_next_question(self) -> Optional[Dict[str, str]]:
        for q in self.QUESTIONS:
            field = q["field"]
            if field not in self.situation or self.situation[field] is None:
                # Cas spécial pour les booléens qui pourraient être False
                if field == "nationality" and not self.situation.get(field): return q
                if field == "residence_duration_months" and self.situation.get(field) is None: return q
                if field in ["is_married", "parent_of_french_child", "has_master_france"] and field not in self.situation:
                    return q
        return None

    def get_situation_dict(self) -> Dict[str, Any]:
        return self.situation
