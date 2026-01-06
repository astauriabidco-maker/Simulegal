import json
import logging
from typing import List, Dict, Any, Tuple, Optional
from core.models import Procedure, Criterion, ExceptionRule, UserSituation

# Configuration des logs
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class EligibilityEngine:
    def __init__(self, criteria_path: str):
        self.procedures = self._load_procedures(criteria_path)

    def _load_procedures(self, path: str) -> List[Procedure]:
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            procedures = []
            for p_data in data['procedures']:
                criteria = [Criterion(**c) for c in p_data['base_criteria']]
                exceptions = [ExceptionRule(**e) for e in p_data.get('exceptions', [])]
                procedures.append(Procedure(
                    id=p_data['id'],
                    name=p_data['name'],
                    description=p_data['description'],
                    base_criteria=criteria,
                    documents=p_data['documents'],
                    exceptions=exceptions
                ))
            return procedures
        except Exception as e:
            logger.error(f"Erreur lors du chargement des procédures : {e}")
            return []

    def evaluate_eligibility(self, user: UserSituation) -> List[Dict[str, Any]]:
        results = []
        for procedure in self.procedures:
            score, missing_criteria, applied_exception = self._calculate_procedure_score(user, procedure)
            results.append({
                "procedure_id": procedure.id,
                "name": procedure.name,
                "score": score,
                "missing_criteria": missing_criteria,
                "applied_exception": applied_exception,
                "documents": procedure.documents
            })
        
        # Trier par score décroissant
        return sorted(results, key=lambda x: x['score'], reverse=True)

    def _calculate_procedure_score(self, user: UserSituation, procedure: Procedure) -> Tuple[float, List[str], Optional[str]]:
        missing_criteria = []
        total_weight = 0
        passed_weight = 0
        applied_exception = None

        # 1. Vérifier si une exception s'applique
        active_thresholds = {c.id: c.threshold for c in procedure.base_criteria if c.threshold is not None}
        active_expecteds = {c.id: c.expected for c in procedure.base_criteria if c.expected is not None}

        for exc in procedure.exceptions:
            # Vérifier si la condition de l'exception est remplie (ex: nationalité)
            is_match = True
            for key, val in exc.condition.items():
                if getattr(user, key, None) != val:
                    is_match = False
                    break
            
            if is_match:
                applied_exception = exc.id
                # Appliquer les seuils modifiés
                for crit_id, mods in exc.modified_criteria.items():
                    if 'threshold' in mods:
                        active_thresholds[crit_id] = mods['threshold']
                break

        # 2. Évaluer chaque critère
        for crit in procedure.base_criteria:
            user_val = user.get_attr(crit.id)
            is_valid = False
            
            if crit.type == 'bool':
                expected = active_expecteds.get(crit.id, crit.expected)
                is_valid = (user_val == expected)
            elif crit.type in ['int', 'float']:
                threshold = active_thresholds.get(crit.id, crit.threshold)
                if threshold is not None:
                    # Traiter None comme 0 pour les comparaisons numériques
                    effective_val = user_val if user_val is not None else 0
                    is_valid = (effective_val >= threshold)
                else:
                    is_valid = True # Pas de seuil défini

            total_weight += 1
            if is_valid:
                passed_weight += 1
            else:
                missing_criteria.append(f"{crit.label} (Attendu: {active_expecteds.get(crit.id, crit.expected) or active_thresholds.get(crit.id, crit.threshold)})")

        score = (passed_weight / total_weight) * 100 if total_weight > 0 else 0
        return score, missing_criteria, applied_exception
