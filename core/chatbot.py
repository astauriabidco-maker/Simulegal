import re
import json
from typing import Dict, Any, List, Optional, Tuple
from core.models import UserSituation


class SimulegalChatbot:
    """Moteur conversationnel enrichi pour l'évaluation d'éligibilité."""
    
    # Questions de base + conditionnelles
    QUESTIONS = [
        # Questions de base
        {"field": "nationality", "text": "Pour commencer, quelle est votre nationalité ?"},
        {"field": "residence_duration_months", "text": "Depuis combien de temps (en mois ou années) résidez-vous en France ?"},
        {"field": "is_married", "text": "Êtes-vous marié(e) à un(e) citoyen(ne) français(e) ?"},
        {"field": "parent_of_french_child", "text": "Êtes-vous parent d'un enfant français mineur résidant en France ?"},
        {"field": "has_master_france", "text": "Avez-vous obtenu un diplôme de Master (ou supérieur) en France ?"},
        {"field": "salary_monthly", "text": "Quel est votre revenu mensuel net approximatif (en €) ?"},
        
        # Questions conditionnelles enrichies
        {"field": "job_contract", "text": "Quel type de contrat de travail avez-vous actuellement ? (CDI, CDD, Freelance, ou Sans emploi)",
         "condition": "has_income", "options": ["CDI", "CDD", "Freelance", "Sans emploi"]},
        {"field": "has_job_auth", "text": "Avez-vous une autorisation de travail en cours de validité ?",
         "condition": "has_contract"},
        {"field": "employment_24_months_tension", "text": "Travaillez-vous dans un métier en tension depuis plus de 24 mois ?",
         "condition": "has_contract"},
        {"field": "research_activity", "text": "Exercez-vous une activité de recherche ou d'enseignement supérieur ?",
         "condition": "has_master"},
        {"field": "has_hosting_agreement", "text": "Avez-vous une convention d'accueil avec un organisme de recherche agréé ?",
         "condition": "is_researcher"},
        {"field": "business_project_real", "text": "Avez-vous un projet réel de création d'entreprise en France ?",
         "condition": "is_freelance"},
        {"field": "innovative_project_recognized", "text": "Votre projet est-il reconnu comme innovant (BPI, incubateur agréé) ?",
         "condition": "has_business_project"},
        {"field": "health_issue", "text": "Avez-vous des problèmes de santé nécessitant des soins spécifiques en France ?"},
        {"field": "no_care_origin_country", "text": "Ces soins sont-ils indisponibles dans votre pays d'origine ?",
         "condition": "has_health_issue"},
        {"field": "is_refugee", "text": "Êtes-vous réfugié, apatride ou demandeur d'asile ?"},
        {"field": "regular_situation", "text": "Êtes-vous actuellement en situation régulière (visa ou titre de séjour valide) ?"},
        {"field": "no_polygamy", "text": "Question importante : confirmez-vous l'absence de situation de polygamie ?"}
    ]
    
    # Contexte juridique pour enrichir les réponses
    LEGAL_CONTEXT = {
        "nationality": "📚 Le CESEDA distingue les ressortissants selon les accords bilatéraux (Algérie, Tunisie, Maroc ont des règles spécifiques).",
        "residence_duration_months": "📚 La durée de séjour est cruciale : 5 ans pour la carte de résident, 5-10 ans pour la naturalisation.",
        "salary_monthly": "📚 Certains titres (Passeport Talent) exigent un salaire minimum de 1.5x à 2x le SMIC (~2700-3600€/mois).",
        "has_master_france": "📚 Un Master français facilite l'accès au Passeport Talent et à l'APS recherche d'emploi (2 ans).",
        "job_contract": "📚 Un CDI facilite grandement les démarches. Un CDD peut donner accès à une carte 'Travailleur Temporaire'.",
        "regular_situation": "📚 Sans titre valide, certaines procédures restent accessibles : AES, régularisation humanitaire, demande d'asile."
    }

    def __init__(self, situation_data: Optional[Dict[str, Any]] = None):
        self.situation = situation_data or {}
        self.question_index = 0
        # Assurer les valeurs par défaut pour les booléens critiques
        if "no_polygamy" not in self.situation:
            self.situation["no_polygamy"] = True
        if "is_adult" not in self.situation:
            self.situation["is_adult"] = True

    def process_message(self, message: str) -> Tuple[str, bool]:
        """
        Analyse le message, met à jour la situation et suggère la suite.
        Retourne (Réponse, EstTerminé).
        """
        # 1. Extraction d'informations
        self._extract_info(message)
        
        # 2. Identifier le prochain champ manquant (avec conditions)
        next_question = self._get_next_question()
        
        if next_question:
            # Ajouter le contexte juridique si disponible
            field = next_question.get("field", "")
            context = self.LEGAL_CONTEXT.get(field, "")
            
            response = next_question["text"]
            if context and field in ["nationality", "residence_duration_months", "salary_monthly"]:
                response = f"{context}\n\n{response}"
            
            # Ajouter les options si disponibles
            if "options" in next_question:
                response += f"\n\n💡 Options : {', '.join(next_question['options'])}"
            
            return response, False
        else:
            return "Merci pour toutes ces informations ! 🎉 Je dispose de suffisamment d'éléments pour analyser votre situation. Voulez-vous voir vos résultats et mes conseils personnalisés ?", True

    def _check_condition(self, condition: str) -> bool:
        """Vérifie si une condition est remplie pour afficher une question."""
        s = self.situation
        
        conditions = {
            "has_income": (s.get("salary_monthly") or 0) > 0 or (s.get("salary_annual") or 0) > 0,
            "has_contract": s.get("job_contract") in ["CDI", "CDD", "cdi", "cdd"],
            "has_master": s.get("has_master_france") == True,
            "is_researcher": s.get("research_activity") == True,
            "is_freelance": s.get("job_contract") in ["Freelance", "freelance", "auto-entrepreneur"],
            "has_business_project": s.get("business_project_real") == True,
            "has_health_issue": s.get("health_issue") == True,
        }
        
        return conditions.get(condition, False)

    def _extract_info(self, text: str):
        text_lower = text.lower()
        
        # Nationalité
        countries = {
            "tunisie": "Tunisienne", "tunisien": "Tunisienne", "tunisienne": "Tunisienne",
            "algérie": "Algérienne", "algérien": "Algérienne", "algérienne": "Algérienne",
            "maroc": "Marocaine", "marocain": "Marocaine", "marocaine": "Marocaine",
            "sénégal": "Sénégalaise", "sénégalais": "Sénégalaise",
            "cameroun": "Camerounaise", "camerounais": "Camerounaise",
            "côte d'ivoire": "Ivoirienne", "ivoirien": "Ivoirienne",
            "mali": "Malienne", "malien": "Malienne",
            "chine": "Chinoise", "chinois": "Chinoise",
            "inde": "Indienne", "indien": "Indienne",
            "brésil": "Brésilienne", "brésilien": "Brésilienne",
            "haïti": "Haïtienne", "haïtien": "Haïtienne"
        }
        for key, value in countries.items():
            if key in text_lower:
                self.situation["nationality"] = value
                break

        # Durée de résidence
        years_match = re.search(r'(\d+)\s*an', text_lower)
        if years_match:
            self.situation["residence_duration_months"] = int(years_match.group(1)) * 12
        else:
            months_match = re.search(r'(\d+)\s*mois', text_lower)
            if months_match:
                self.situation["residence_duration_months"] = int(months_match.group(1))

        # Salaire
        money_match = re.search(r'(\d+)\s*(?:€|euro|euros|k)', text_lower)
        if money_match:
            val = int(money_match.group(1))
            if val < 500:  # Gestion des "35k"
                val *= 1000
            if val > 10000:  # Annuel
                self.situation["salary_annual"] = val
                self.situation["salary_monthly"] = val // 12
            else:
                self.situation["salary_monthly"] = val
                self.situation["salary_annual"] = val * 12

        # Type de contrat
        if "cdi" in text_lower:
            self.situation["job_contract"] = "CDI"
        elif "cdd" in text_lower:
            self.situation["job_contract"] = "CDD"
        elif "freelance" in text_lower or "auto-entrepreneur" in text_lower or "indépendant" in text_lower:
            self.situation["job_contract"] = "Freelance"
        elif "sans emploi" in text_lower or "chômage" in text_lower or "pas de travail" in text_lower:
            self.situation["job_contract"] = "Sans emploi"

        # Booléens (Oui/Non)
        positive = ["oui", "ouais", "exact", "tout à fait", "c'est ça", "absolument", "bien sûr", "évidemment"]
        negative = ["non", "pas du tout", "jamais", "aucun", "pas vraiment"]
        
        # Détection contextuelle
        if any(w in text_lower for w in positive):
            if "mari" in text_lower or "conjoint" in text_lower or "épou" in text_lower:
                self.situation["is_married"] = True
            if "enfant" in text_lower and "français" in text_lower:
                self.situation["parent_of_french_child"] = True
            if "master" in text_lower or "diplôme" in text_lower or "bac+5" in text_lower:
                self.situation["has_master_france"] = True
            if "recherche" in text_lower or "chercheur" in text_lower or "enseignant" in text_lower:
                self.situation["research_activity"] = True
            if "santé" in text_lower or "maladie" in text_lower or "soins" in text_lower:
                self.situation["health_issue"] = True
            if "réfugié" in text_lower or "asile" in text_lower:
                self.situation["is_refugee"] = True
            if "régulier" in text_lower or "visa" in text_lower or "titre" in text_lower:
                self.situation["regular_situation"] = True
            if "projet" in text_lower and ("entreprise" in text_lower or "business" in text_lower):
                self.situation["business_project_real"] = True
            if "innovant" in text_lower or "bpi" in text_lower or "incubateur" in text_lower:
                self.situation["innovative_project_recognized"] = True
            if "autorisation" in text_lower and "travail" in text_lower:
                self.situation["has_job_auth"] = True
            if "tension" in text_lower or "pénurie" in text_lower:
                self.situation["employment_24_months_tension"] = True
            if "convention" in text_lower and "accueil" in text_lower:
                self.situation["has_hosting_agreement"] = True
            if "indisponible" in text_lower or "pas de soins" in text_lower:
                self.situation["no_care_origin_country"] = True
                
        if any(w in text_lower for w in negative):
            if "mari" in text_lower:
                self.situation["is_married"] = False
            if "enfant" in text_lower:
                self.situation["parent_of_french_child"] = False
            if "régulier" in text_lower or "papier" in text_lower:
                self.situation["regular_situation"] = False
            if "santé" in text_lower:
                self.situation["health_issue"] = False

        # Réponses simples oui/non pour la dernière question posée
        if text_lower.strip() in ["oui", "ouais", "yes", "o"]:
            self._set_last_field_true()
        elif text_lower.strip() in ["non", "no", "n", "pas"]:
            self._set_last_field_false()

    def _set_last_field_true(self):
        """Définit le dernier champ demandé à True."""
        for q in self.QUESTIONS:
            field = q["field"]
            if field not in self.situation or self.situation.get(field) is None:
                condition = q.get("condition")
                if condition is None or self._check_condition(condition):
                    self.situation[field] = True
                    break

    def _set_last_field_false(self):
        """Définit le dernier champ demandé à False."""
        for q in self.QUESTIONS:
            field = q["field"]
            if field not in self.situation or self.situation.get(field) is None:
                condition = q.get("condition")
                if condition is None or self._check_condition(condition):
                    self.situation[field] = False
                    break

    def _get_next_question(self) -> Optional[Dict[str, Any]]:
        """Retourne la prochaine question à poser, en tenant compte des conditions."""
        for q in self.QUESTIONS:
            field = q["field"]
            condition = q.get("condition")
            
            # Vérifier si la condition est remplie (ou s'il n'y a pas de condition)
            if condition and not self._check_condition(condition):
                continue
            
            # Vérifier si le champ n'est pas déjà renseigné
            if field not in self.situation or self.situation.get(field) is None:
                # Cas spécial pour les champs numériques
                if field in ["residence_duration_months", "salary_monthly"] and self.situation.get(field) == 0:
                    continue
                return q
        
        return None

    def generate_advice(self, eligibility_results: List[Dict]) -> str:
        """Génère des conseils personnalisés basés sur les résultats d'éligibilité."""
        if not eligibility_results:
            return "❌ Je n'ai pas pu analyser votre éligibilité. Essayez le formulaire complet pour plus de précision."
        
        top = eligibility_results[0]
        score = top.get("score", 0)
        name = top.get("name", "Titre de séjour")
        missing = top.get("missing_criteria", [])
        documents = top.get("documents", [])
        
        advice = f"📋 **Meilleure option identifiée** : {name}\n"
        advice += f"📊 **Score d'éligibilité** : {score:.0f}%\n\n"
        
        if score >= 100:
            advice += "✅ **Excellente nouvelle !** Vous semblez éligible à ce titre.\n\n"
            advice += "**Mes conseils :**\n"
            advice += "1️⃣ Rassemblez les documents requis (voir liste ci-dessous)\n"
            advice += "2️⃣ Prenez RDV en préfecture via le site de votre département\n"
            advice += "3️⃣ Préparez un dossier complet pour éviter les aller-retours\n\n"
            if documents:
                advice += "**📄 Documents à préparer :**\n"
                for doc in documents[:5]:
                    advice += f"• {doc}\n"
        elif score >= 70:
            advice += "⚠️ **Vous êtes proche de l'éligibilité !**\n\n"
            advice += "**Points à améliorer ou vérifier :**\n"
            for c in missing[:3]:
                advice += f"• {c}\n"
            advice += "\n💡 **Conseil** : Consultez un avocat ou une association spécialisée pour clarifier ces points."
        elif score >= 40:
            advice += "🔶 **Éligibilité partielle détectée.**\n\n"
            advice += "Plusieurs critères ne sont pas remplis. Options alternatives :\n"
            advice += "• Explorer d'autres procédures (APS, régularisation...)\n"
            advice += "• Consulter un avocat spécialisé en droit des étrangers\n"
            advice += "• Contacter une association d'aide aux migrants (GISTI, Cimade...)\n"
        else:
            advice += "❌ **L'éligibilité à ce titre semble difficile actuellement.**\n\n"
            advice += "**Pistes alternatives à explorer :**\n"
            advice += "• Demande d'asile si vous êtes persécuté dans votre pays\n"
            advice += "• Régularisation pour raisons humanitaires\n"
            advice += "• Admission exceptionnelle au séjour (AES)\n"
            advice += "• Consultation juridique gratuite en préfecture\n"
        
        return advice

    def get_situation_dict(self) -> Dict[str, Any]:
        return self.situation
