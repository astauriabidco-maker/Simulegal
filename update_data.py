import json
import sys

# Charger les données existantes
try:
    with open("data/eligibility_criteria.json", "r", encoding="utf-8") as f:
        data = json.load(f)
except FileNotFoundError:
    print("Fichier non trouvé")
    sys.exit(1)

# Mise à jour du SMIC annuel 2024 (21 203 €)
# On parcourt toutes les procédures qui utilisent un seuil de salaire
for proc in data["procedures"]:
    for crit in proc.get("base_criteria", []) + proc.get("exceptions", []):
        if hasattr(crit, "get") and crit.get("id") == "salary_annual" and crit.get("threshold"):
            # Si le seuil est proche de l'ancien SMIC (approx 20000-21000), on met à jour
            # Ou on force la mise à jour si c'est explicitement le SMIC
            if 19000 < crit["threshold"] < 22000:
                crit["threshold"] = 21203

# Ajout des nouvelles procédures (si pas déjà présentes)
existing_ids = [p["id"] for p in data["procedures"]]

new_procedures = [
    {
        "id": "resident_refugee",
        "name": "Carte de résident (10 ans) - Réfugié",
        "description": "Délivrée aux personnes bénéficiant du statut de réfugié (OFPRA/CNDA).",
        "base_criteria": [
            {
                "id": "has_refugee_status",
                "label": "Bénéficier du statut de réfugié (OFPRA/CNDA)",
                "type": "bool",
                "expected": True
            },
            {
                "id": "no_threat_public_order",
                "label": "Absence de menace pour l'ordre public",
                "type": "bool",
                "expected": True
            }
        ],
        "documents": [
            "Décision de l'OFPRA ou de la CNDA",
            "Justificatif d'état civil",
            "Justificatif de domicile"
        ],
        "exceptions": []
    },
    {
        "id": "passport_talent_reputation",
        "name": "Passeport Talent - Renommée Internationale",
        "description": "Pour les domaines scientifique, littéraire, artistique, intellectuel, éducatif ou sportif.",
        "base_criteria": [
            {
                "id": "has_international_reputation",
                "label": "Renommée nationale ou internationale établie",
                "type": "bool",
                "expected": True
            },
            {
                "id": "has_project_in_france",
                "label": "Projet d'activité en France",
                "type": "bool",
                "expected": True
            },
             {
                "id": "stable_resources",
                "label": "Ressources suffisantes",
                "type": "bool",
                "expected": True
            }
        ],
        "documents": [
            "Articles de presse",
            "Récompenses / Distinctions",
            "Attestations de professionnels reconnus",
            "Justificatifs de ressources"
        ],
        "exceptions": []
    }
]

added_count = 0
for new_proc in new_procedures:
    if new_proc["id"] not in existing_ids:
        data["procedures"].append(new_proc)
        added_count += 1

# Sauvegarde
with open("data/eligibility_criteria.json", "w", encoding="utf-8") as f:
    json.dump(data, f, ensure_ascii=False, indent=2)

print(f"Mise à jour terminée. {added_count} procédures ajoutées.")
