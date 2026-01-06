import sys
import os
from core.models import UserSituation
from core.engine import EligibilityEngine
from core.report_generator import ReportGenerator

def clear_screen():
    os.system('cls' if os.name == 'nt' else 'clear')

def main():
    print("==================================================")
    print("   SIMULEGAL - Moteur d'Éligibilité Titre de Séjour")
    print("==================================================")
    
    try:
        nationality = input("Votre nationalité : ")
        months_france = int(input("Durée de résidence en France (en mois) : ") or 0)
        is_married = input("Êtes-vous marié(e) ? (o/n) : ").lower() == 'o'
        
        marriage_duration = 0
        community_life = False
        spouse_nat = ""
        is_spouse_eu = False
        
        if is_married:
            marriage_duration = int(input("Durée du mariage (en mois) : ") or 0)
            community_life = input("Vivez-vous avec votre conjoint(e) ? (o/n) : ").lower() == 'o'
            spouse_nat = input("Nationalité du conjoint : ")
            is_spouse_eu = input("Votre conjoint est-il citoyen de l'UE (hors France) ? (o/n) : ").lower() == 'o'

        has_master = input("Avez-vous un Master obtenu en France ? (o/n) : ").lower() == 'o'
        salary = float(input("Votre salaire annuel brut (si applicable) : ") or 0)
        job_related = input("Votre emploi est-il lié à votre diplôme ? (o/n) : ").lower() == 'o'
        no_polygamy = input("Vivez-vous en situation de polygamie ? (o/n) : ").lower() == 'n'
        french_level = input("Niveau de français (A1, A2, B1, B2) : ").upper()
        stable_resources = input("Disposez-vous de ressources stables ? (o/n) : ").lower() == 'o'
        parent_french = input("Êtes-vous parent d'un enfant français résidant en France ? (o/n) : ").lower() == 'o'

        user = UserSituation(
            nationality=nationality,
            residence_duration_months=months_france,
            marriage_duration_months=marriage_duration if is_married else None,
            has_master_france=has_master,
            community_of_life=community_life,
            spouse_nationality=spouse_nat,
            is_spouse_eu_citizen=is_spouse_eu,
            salary_annual=salary,
            job_contract_related=job_related,
            no_polygamy=no_polygamy,
            french_level=french_level,
            stable_resources=stable_resources,
            parent_of_french_child=parent_french
        )

        engine = EligibilityEngine("data/eligibility_criteria.json")
        results = engine.evaluate_eligibility(user)

        print("\n--- RÉSULTATS D'ÉLIGIBILITÉ ---")
        for res in results:
            if res['score'] > 0:
                print(f"[{res['score']}%] {res['name']}")
                if res['missing_criteria']:
                    print(f"    Mancant : {', '.join(res['missing_criteria'])}")
        
        save_pdf = input("\nGénérer un rapport PDF ? (o/n) : ").lower() == 'o'
        if save_pdf:
            gen = ReportGenerator(nationality)
            path = gen.generate(results, "rapport_eligibilite.pdf")
            print(f"Rapport généré avec succès : {path}")

    except ValueError as e:
        print(f"Erreur de saisie : {e}")
    except Exception as e:
        print(f"Une erreur est survenue : {e}")

if __name__ == "__main__":
    main()
